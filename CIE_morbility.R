library(DBI)
library(RSQLite)
library(stringr)

getGestWeeks <- function(termsW){
  resultW <- 0
  strFilterW <- "\\b((\\s?[A-Z]*\\s?){1,})?([0-9][0-9]?)\\s?(S\\s?G|SEM(ANAS?)?(\\s?(DE)?\\s?GEST?(ACION)?)?)((\\s?[A-Z]*\\s?){1,})?\\b"
  for(w in 1:length(termsW)){
    if(grepl(strFilterW, termsW[w])){
      resultW <- gsub(strFilterW,"\\3",termsW[w])
      break
    }
  }
  return(resultW)
}

isObstetric <- function(age_, ageC_, sex_, service_, terms_){
	result_ <- FALSE
	strFilter <- "\\b((EMB(ARAZ(ADAS?|OS?))?)?[0-9][0-9]?\\s?S(EMANAS?)?\\s?(DE)?\\s?G(ESTACION)?|ABORTOS?|PARTOS?|CEA?[SZ]AR[IE]AS?|EMB(ARAZ(ADAS?|OS?))?)\\b"
	if(as.numeric(ageC_) == 4 & as.numeric(sex_) == 2){
		if(as.numeric(age_) >= 10 & as.numeric(age_) <= 50){
			for(i in 1:length(terms_)){
				if(grepl(strFilter, terms_[i]) | as.numeric(service_) == 49){
					result_ <- TRUE
					break
				}
			}
		}
	}
	return(result_)
}

ageValida <- function(age__, ageC__, ageCMin__, ageCMax__, ageMin__, ageMax__){
	valida <- TRUE
	msg <- "C?digo no v?lido para la edad"
	if(!(ageC__ >= as.numeric(ageCMin__) & ageC__ <= as.numeric(ageCMax__))){
		valida <- FALSE
	}else{
		if(ageC__ == as.numeric(ageCMin__)){
			if(age__ < as.numeric(ageMin__)){
				valida <- FALSE
			}
		}else if(ageC__ == as.numeric(ageCMax__)){
			if(age__ > as.numeric(ageMax__)){
				valida <- FALSE
			}
		}
	}
	return(c(valida,msg))
}

batch_coding <- function(input, output, session){
  
  start.time <- Sys.time()
  #setwd("C:/Users/Usuario/Documents/INEC/Proyectos/Morbilit/")
  fechaHora <- format(Sys.time(),"%y%m%d%H%M%S")
  frName <- paste("Reports/resumen",".txt",sep="")
  faName <- paste("Reports/asigna",".txt",sep="")
  fcName <- paste("Reports/recodif",".txt",sep="")
  fileReport<-file(frName, "w")
  fileAsigna<-file(faName, "w")
  fileCodifica<-file(fcName, "w")
  
  db <- dbConnect(SQLite(), "CIE10morb.db")
  
  std0 <- dbGetQuery(db, "SELECT MainKey, Rank, FilterIn, FilterOut FROM Standardisation0 ORDER BY MainKey, Rank")
  std1 <- dbGetQuery(db, "SELECT MainKey, Rank, FilterIn, FilterOut FROM Standardisation1 ORDER BY MainKey, Rank")
  std2 <- dbGetQuery(db, "SELECT MainKey, Rank, FilterIn, FilterOut FROM Standardisation2 ORDER BY MainKey, Rank")
  medcod <- dbGetQuery(db, "SELECT Ident, Term1, Term2, Term3, Term4 FROM MedCod ORDER BY Term1")
  campo <- c("Term1", "Term2", "Term3", "Term4")
  icd <- c("Icd1", "Icd2", "Icd3", "Icd4")
  cTerm <- data.frame(campo, icd)
  
  
  write("PROCESO: ASIGNACI?N DE C?DIGOS CIE-10 2018",fileReport,append=TRUE)
  output$contents1 <- renderPrint("Asignando códigos generales...")
  if(nrow(medcod) > 0){
  	terminoAnt <- ""
  	IcdAnt <- ""
  	nTermExis <- 0
  	nTermCodif <- 0
  	result <- data.frame()
  	write(paste("Ident","OrdTerm","FoundIn","IcdResult","Term","FilterIn","FilterOut","TermCodif","TermNoBlank",sep=";"),fileAsigna,append=TRUE)
  	for(k in 1:nrow(cTerm)){
  	medcod <- dbGetQuery(db, paste("SELECT Ident, ", cTerm[k,"campo"], " FROM MedCod ORDER BY ", cTerm[k,"campo"], sep=""))
  	#write(paste("Ident","OrdTerm","FoundIn","IcdResult","Term","FilterIn","FilterOut","TermCodif","TermNoBlank",sep=";"),fileAsigna,append=TRUE)
  	for(i in 1:nrow(medcod)){
  		termino <- str_trim(medcod[i,cTerm[k,"campo"]])
  		output$contents1 <- renderPrint(paste("Codificando",medcod[i,"Ident"],termino,sep = ","))
  		if(termino != ""){
  			nTermExis <- nTermExis + 1
  			if(termino == terminoAnt){
  				if(nrow(result) > 0){
  					update <- dbExecute(db, paste("UPDATE MedCod SET ",cTerm[k,"icd"]," = '",IcdAnt,"' WHERE Ident = '",medcod[i,"Ident"],"'",sep=""))
  					nTermCodif <- nTermCodif + 1
  				}
  				write(paste(medcod[i,"Ident"],cTerm[k,"campo"],"Duplicado",IcdAnt,termino,"NA","NA",nTermCodif,nTermExis,sep=";"),fileAsigna,append=TRUE)
  			}else{
  				strSQL <- paste("SELECT Icd1 FROM Dictionary WHERE DiagnosisText LIKE '", termino, "'", sep="")
  				result <- dbGetQuery(db, strSQL)
  				write(paste(medcod[i,"Ident"],cTerm[k,"campo"],"Diccionario",result[,"Icd1"],termino,"NA","NA",nTermCodif,nTermExis,sep=";"),fileAsigna,append=TRUE)
  				terminoAnt <- termino
  				IcdAnt <- result[,"Icd1"]
  				if(nrow(result) > 0){
  					update <- dbExecute(db, paste("UPDATE MedCod SET ",cTerm[k,"icd"]," = '",result[,"Icd1"],"' WHERE Ident = '",medcod[i,"Ident"],"'",sep=""))
  					nTermCodif <- nTermCodif + 1
  				}else{
  					for(j in 1:nrow(std0)){
  						if(grepl(std0[j,"FilterIn"], str_trim(termino))){
  							termino = str_trim(gsub(std0[j,"FilterIn"], paste(std0[j,"FilterOut"],"",sep=""), termino))
  							strSQL <- paste("SELECT Icd1 FROM Dictionary WHERE DiagnosisText LIKE '", termino, "'", sep="")
  							write(paste(medcod[i,"Ident"],cTerm[k,"campo"],"AplicaStandard0",result[,"Icd1"],termino,paste(std0[j,"FilterIn"],"",sep=""),paste(std0[j,"FilterOut"],"",sep=""),nTermCodif,nTermExis,sep=";"),fileAsigna,append=TRUE)
  							result <- dbGetQuery(db, strSQL)
  							if(nrow(result) > 0){
  								update <- dbExecute(db, paste("UPDATE MedCod SET ",cTerm[k,"icd"]," = '",result[,"Icd1"],"' WHERE Ident = '",medcod[i,"Ident"],"'",sep=""))
  								nTermCodif <- nTermCodif + 1
  								break
  							}
  						}
  					}
  					write(paste(medcod[i,"Ident"],cTerm[k,"campo"],"ResultStandard0",result[,"Icd1"],termino,"NA","NA",nTermCodif,nTermExis,sep=";"),fileAsigna,append=TRUE)
  					IcdAnt <- result[,"Icd1"]
  					if(nrow(result) == 0){
  						for(j in 1:nrow(std1)){
  							if(grepl(std1[j,"FilterIn"], termino)){
  								termino = str_trim(gsub(std1[j,"FilterIn"], paste(std1[j,"FilterOut"],"",sep=""), termino))
  								strSQL <- paste("SELECT Icd1 FROM Dictionary WHERE DiagnosisText LIKE '", termino, "'", sep="")
  								write(paste(medcod[i,"Ident"],cTerm[k,"campo"],"AplicaStandard1",result[,"Icd1"],termino,paste(std1[j,"FilterIn"],"",sep=""),paste(std1[j,"FilterOut"],"",sep=""),nTermCodif,nTermExis,sep=";"),fileAsigna,append=TRUE)
  								result <- dbGetQuery(db, strSQL)
  								if(nrow(result) > 0){
  									update <- dbExecute(db, paste("UPDATE MedCod SET ",cTerm[k,"icd"]," = '",result[,"Icd1"],"' WHERE Ident = '",medcod[i,"Ident"],"'",sep=""))
  									nTermCodif <- nTermCodif + 1
  									break
  								}
  							}
  						}
  						write(paste(medcod[i,"Ident"],cTerm[k,"campo"],"ResultStandard1",result[,"Icd1"],termino,"NA","NA",nTermCodif,nTermExis,sep=";"),fileAsigna,append=TRUE)
  						IcdAnt <- result[,"Icd1"]
  						if(nrow(result) == 0){
  							for(j in 1:nrow(std2)){
  								if(grepl(std2[j,"FilterIn"], termino)){
  									termino = str_trim(gsub(std2[j,"FilterIn"], paste(std2[j,"FilterOut"],"",sep=""), termino))
  									strSQL <- paste("SELECT Icd1 FROM Dictionary WHERE DiagnosisText LIKE '", termino, "'", sep="")
  									write(paste(medcod[i,"Ident"],cTerm[k,"campo"],"AplicaStandard2",result[,"Icd1"],termino,paste(std2[j,"FilterIn"],"",sep=""),paste(std2[j,"FilterOut"],"",sep=""),nTermCodif,nTermExis,sep=";"),fileAsigna,append=TRUE)
  									result <- dbGetQuery(db, strSQL)
  									if(nrow(result) > 0){
  										update <- dbExecute(db, paste("UPDATE MedCod SET ",cTerm[k,"icd"]," = '",result[,"Icd1"],"' WHERE Ident = '",medcod[i,"Ident"],"'",sep=""))
  										nTermCodif <- nTermCodif + 1
  										break
  									}
  								}
  							}
  							write(paste(medcod[i,"Ident"],cTerm[k,"campo"],"ResultStandard2",result[,"Icd1"],termino,"NA","NA",nTermCodif,nTermExis,sep=";"),fileAsigna,append=TRUE)
  							IcdAnt <- result[,"Icd1"]
  						}
  					}
  				}
  			}
  		}
  	}
  	}
  }else{
  	message("No se encontraron registros en la tabla MedCod\nOperaci?n cancelada!")
  }
  nReg <- nrow(medcod)
  write(paste("Registros recorridos: ", nReg, sep=""),fileReport,append=TRUE)
  write(paste("T?rminos a codificar: ", nTermExis, sep=""),fileReport,append=TRUE)
  write(paste("T?rminos codificados: ", nTermCodif, sep=""),fileReport,append=TRUE)
  ptc = (nTermCodif / nTermExis) * 100
  write(paste("Tasa bruta de asignaci?n: ", ptc, "%", sep=""),fileReport,append=TRUE)
  
  updateSQL <- "UPDATE MedCod "
  updateSQL <- paste(updateSQL,"SET State = '', IcdAP = '', IcdAS = ''",sep="")
  update <- dbExecute(db, updateSQL)
  
  write((Sys.time() - start.time),fileReport,append=TRUE)
  write("PROCESO: IDENTIFICACION DE REGISTROS RECHAZADOS",fileReport,append=TRUE)
  output$contents1 <- renderPrint("Identificando registros rechazados...")
  updateSQL <- "UPDATE MedCod "
  updateSQL <- paste(updateSQL,"SET State = 'Rechazado' ",sep="")
  updateSQL <- paste(updateSQL,"WHERE (",sep="")
  updateSQL <- paste(updateSQL,"(Term1 <> '' AND Icd1 = '') ",sep="")
  updateSQL <- paste(updateSQL,"OR (Term2 <> '' AND Icd2 = '') ",sep="")
  updateSQL <- paste(updateSQL,"OR (Term3 <> '' AND Icd3 = '') ",sep="")
  updateSQL <- paste(updateSQL,"OR (Term4 <> '' AND Icd4 = '')",sep="")
  updateSQL <- paste(updateSQL,") ",sep="")
  updateSQL <- paste(updateSQL,"AND (State = '' OR State IS NULL)",sep="")
  numPartial <- dbExecute(db, updateSQL)
  write(paste("Registros rechazados identificados: ",numPartial,sep=""),fileReport,append=TRUE)
  
  icds <- c("","")
  colsIcdINEC <- c("IcdAP","IcdAS")
  icdExtern <- "Icd4"
  
  ident <- dbGetQuery(db, "SELECT Ident, Sex, Age, AgeCond, HospDays, Condition, Service FROM Ident ORDER BY Ident")
  medcod <- dbGetQuery(db, "SELECT Ident, Term1, Term2, Term3, Term4, Icd1, Icd2, Icd3, Icd4 FROM MedCod WHERE State = '' OR State IS NULL ORDER BY Ident")
  numCE <- 0
  
  write((Sys.time() - start.time),fileReport,append=TRUE)
  write("PROCESO: IDENTIFICACION DE CAUSAS EXTERNAS",fileReport,append=TRUE)
  for(i in 1:nrow(cTerm)){
  	dfExtern <- grep("[VWXY][0-9][0-9][0-9]?", medcod[,cTerm[i,"icd"]])
  	if(length(dfExtern) > 0){
  		for(j in 1:length(dfExtern)){
  			icds[1] <- ""
  			icds[2] <- ""
  			for(k in 1:nrow(cTerm)){
  				if(cTerm[i,"icd"] != cTerm[k,"icd"]){
  					if(medcod[dfExtern[j],cTerm[k,"icd"]] != ""){
  						for(l in 1:length(icds)){
  							if(icds[l] == ""){
  								icds[l] <- medcod[dfExtern[j],cTerm[k,"icd"]]
  								break
  							}
  						}
  					}
  				}
  			}
  			updateSQL <- "UPDATE MedCod "
  			updateSQL <- paste(updateSQL,"SET State = 'Externa', ",sep="")
  			updateSQL <- paste(updateSQL,"IcdAP = '",icds[1],"', ",sep="")
  			updateSQL <- paste(updateSQL,"IcdAS = '",icds[2],"', ",sep="")
  			updateSQL <- paste(updateSQL,"IcdCE = '",medcod[dfExtern[j],cTerm[i,"icd"]],"' ",sep="")
  			updateSQL <- paste(updateSQL,"WHERE Ident = '",medcod[dfExtern[j],"Ident"],"'",sep="")
  			update <- dbExecute(db, updateSQL)
  		}
  		numCE <- numCE + length(dfExtern)
  		write(paste("Identificados en ",cTerm[i,"icd"],": ",length(dfExtern), sep=""),fileReport,append=TRUE)
  	}
  }
  
  medcod <- dbGetQuery(db, "SELECT Ident, Term1, Term2, Term3, Term4, Icd1, Icd2, Icd3, Icd4 FROM MedCod WHERE (State = '' OR State IS NULL) ORDER BY Ident")
  write((Sys.time() - start.time),fileReport,append=TRUE)
  write("PROCESO: IDENTIFICACION DE TRAUMATISMOS Y FRACTURAS",fileReport,append=TRUE)
  output$contents1 <- renderPrint("Identificando traumatismos...")
  for(i in 1:nrow(cTerm)){
  	dfExtern <- grep("[ST][0-9][0-9][0-9]?", medcod[,cTerm[i,"icd"]])
  	if(length(dfExtern) > 0){
  		for(j in 1:length(dfExtern)){
  			updateSQL <- "UPDATE MedCod "
  			updateSQL <- paste(updateSQL,"SET State = 'Traumatismos' ",sep="")
  			updateSQL <- paste(updateSQL,"WHERE Ident = '",medcod[dfExtern[j],"Ident"],"'",sep="")
  			update <- dbExecute(db, updateSQL)
  		}
  		numCE <- numCE + length(dfExtern)
  		write(paste("Identificados en ",cTerm[i,"icd"],": ",length(dfExtern), sep=""),fileReport,append=TRUE)
  	}
  }
  
  write((Sys.time() - start.time),fileReport,append=TRUE)
  write("PROCESO: RECODIFICACION PARAMETRIZADA",fileReport,append=TRUE)
  output$contents1 <- renderPrint("Recodificando...")
  updateSQL <- "UPDATE MedCod "
  updateSQL <- paste(updateSQL,"SET State = 'Inicial' ",sep="")
  updateSQL <- paste(updateSQL,"WHERE ",sep="")
  updateSQL <- paste(updateSQL,"(State = 'Traumatismos' OR State = 'Externa' OR State = '' OR State IS NULL)",sep="")
  nComp <- dbExecute(db, updateSQL)
  write(paste("Registros con asignaci?n completa identificados: ",nComp,sep=""),fileReport,append=TRUE)
  
  #BLOQUE DE RECODIFICACION
  #write(paste("Ident","Proces","Column","IcdOrig","IcdSubst","Age","AgeCond","Sex","Result",sep=";"),fileCodifica,append=TRUE)
  medcod <- dbGetQuery(db, "SELECT Ident, Term1, Term2, Term3, Term4, Icd1, Icd2, Icd3, Icd4 FROM MedCod WHERE State = 'Inicial' ORDER BY Ident")
  cntFinal <- 0
  cntReject <- 0
  if(nrow(medcod) > 0){
  	for(i in 1:nrow(medcod)){
  		strSQL <- "SELECT AgeCond, Age, Sex, Service "
  		strSQL <- paste(strSQL,"FROM Ident ",sep="")
  		strSQL <- paste(strSQL,"WHERE Ident = '",medcod[i,"Ident"],"'",sep="")
  		ident <- dbGetQuery(db, strSQL)
  		sex <- as.numeric(ident[,"Sex"])
  		age <- as.numeric(ident[,"Age"])
  		ageC <- as.numeric(ident[,"AgeCond"])
  		service <- as.numeric(ident[,"Service"])
  		estado <- "Inicial"
  		for(j in 1:nrow(cTerm)){
  			if(medcod[i,cTerm[j,"icd"]] != ""){
  				icd_ <- medcod[i,cTerm[j,"icd"]]
  				icdNew <- icd_
  				term_ <- str_trim(medcod[i,cTerm[j,"campo"]])
  				strSQL <- "SELECT AgeCondMin, AgeCondMax, AgeMin, AgeMax, SexMin, SexMax, State, MainCond, Services "
  				strSQL <- paste(strSQL,"FROM IcdValidation ",sep="")
  				strSQL <- paste(strSQL,"WHERE Icd = '",icd_,"'",sep="")
  				icdVal <- dbGetQuery(db, strSQL)
  				strResult <- ""
  				if(nrow(icdVal) > 0){
  					#RECODIFICACION POR EDAD
  					validationAge = ageValida(age__=age,ageC__=ageC,ageCMin__=icdVal[,"AgeCondMin"],ageCMax__=icdVal[,"AgeCondMax"],ageMin__=icdVal[,"AgeMin"],ageMax__=icdVal[,"AgeMax"])
  					if(validationAge[1] == "FALSE"){
  						estado <- "Rechazado"
  						strResult <- "C?digo no v?lido para la edad"
  						strSQL <- "SELECT IcdSubst "
  						strSQL <- paste(strSQL,"FROM IcdSubst1 ",sep="")
  						strSQL <- paste(strSQL,"WHERE Icd = '",icd_,"' AND Type = 'AGE' ORDER BY Rank",sep="")
  						icdSubst <- dbGetQuery(db, strSQL)
  						for(k in 1:nrow(icdSubst)){
  							strSQL <- "SELECT AgeCondMin, AgeCondMax, AgeMin, AgeMax, SexMin, SexMax, State, MainCond, Services "
  							strSQL <- paste(strSQL,"FROM IcdValidation ",sep="")
  							strSQL <- paste(strSQL,"WHERE Icd = '",icdSubst[k,"IcdSubst"],"'",sep="")
  							icdVal2 <- dbGetQuery(db, strSQL)
  							if(nrow(icdVal2) > 0){
  								validationAge2 = ageValida(age__=age,ageC__=ageC,ageCMin__=icdVal2[,"AgeCondMin"],ageCMax__=icdVal2[,"AgeCondMax"],ageMin__=icdVal2[,"AgeMin"],ageMax__=icdVal2[,"AgeMax"])
  								if(validationAge2[1] == "TRUE"){
  									estado <- "Inicial"
  									strResult <- "Recodificaci?n exitosa debido a la edad"
  									icdNew <- icdSubst[k,"IcdSubst"]
  									medcod[i,cTerm[j,"icd"]] <- icdNew
  									break
  								}
  							}
  						}
  						write(paste(medcod[i,"Ident"],"Recodif",cTerm[j,"icd"],icd_,icdNew,age,ageC,sex,ifelse(icdNew=="",validationAge[2],strResult),sep=";"),fileCodifica,append=TRUE)
  					}
  					#VALIDACION POR SEXO
  					if(!(sex >= as.numeric(icdVal[,"SexMin"]) & sex <= as.numeric(icdVal[,"SexMax"]))){
  						estado <- "Rechazado"
  						strResult <- "C?digo no v?lido para el sexo"
  						write(paste(medcod[i,"Ident"],"Recodif",cTerm[j,"icd"],icd_,"",age,ageC,sex,strResult,sep=";"),fileCodifica,append=TRUE)
  					}
  					#RECODIFICACION OBSTETRICA
  					if(!grepl("[ZO][0-9][0-9][0-9]?",medcod[i,cTerm[j,"icd"]])){
  						if(isObstetric(age_=age, ageC=ageC, sex_=sex, service_=service, terms_=c(medcod[i,"Term1"],medcod[i,"Term2"],medcod[i,"Term3"],medcod[i,"Term4"]))){
  							icdNew <- icd_
  							strSQL <- "SELECT IcdSubst "
  							strSQL <- paste(strSQL,"FROM IcdSubst1 ",sep="")
  							strSQL <- paste(strSQL,"WHERE Icd <= '",icd_,"' AND IcdEnd >= '",icd_,"' AND Type = 'OBSTETRIC' ORDER BY Rank",sep="")
  							icdSubst <- dbGetQuery(db, strSQL)
  							strResult <- "Recodificaci?n obst?trica rechazada"
  							if(nrow(icdSubst) > 0){
  								strResult <- "Recodificaci?n obst?trica exitosa"
  								icdNew <- icdSubst[1,"IcdSubst"]
  								medcod[i,cTerm[j,"icd"]] <- icdNew
  							}
  							write(paste(medcod[i,"Ident"],"Recodif",cTerm[j,"icd"],icd_,icdNew,age,ageC,sex,strResult,sep=";"),fileCodifica,append=TRUE)
  						}
  					}
  					#RECODIFICACION OBSTETRICA POR SEMANAS DE GESTACION
  					if(grepl("O(21[0-9]|47[019])",medcod[i,cTerm[j,"icd"]])){
  					  gestWeeks <- as.numeric(getGestWeeks(termsW=c(medcod[i,"Term1"],medcod[i,"Term2"],medcod[i,"Term3"],medcod[i,"Term4"])))
  					  if( gestWeeks > 0){
  					    icd_ <- medcod[i,cTerm[j,"icd"]]
  					    strSQL <- "SELECT IcdSubst "
  					    strSQL <- paste(strSQL,"FROM IcdSubst1 ",sep="")
  					    strSQL <- paste(strSQL,"WHERE Icd <= '",icd_,"' AND IcdEnd >= '",icd_,"' AND ",sep="")
  					    strSQL <- paste(strSQL,"GWMin <= ",gestWeeks," AND GWMax >= ",gestWeeks," AND ",sep="")
  					    strSQL <- paste(strSQL,"Type = 'GESTATIONAL' ORDER BY Rank",sep="")
  					    icdSubst <- dbGetQuery(db, strSQL)
  					    strResult <- "Recodificaci?n obst?trica por semanas de gestaci?n no encontrada"
  					    if(nrow(icdSubst) > 0){
  					      strResult <- paste("Recodificaci?n obst?trica por semanas de gestaci?n = ",gestWeeks,sep="")
  					      icdNew <- icdSubst[1,"IcdSubst"]
  					      medcod[i,cTerm[j,"icd"]] <- icdNew
  					    }
  					    write(paste(medcod[i,"Ident"],"Recodif",cTerm[j,"icd"],icd_,icdNew,age,ageC,sex,strResult,sep=";"),fileCodifica,append=TRUE)
  					  }
  					}
  				}
  				updateSQL <- "UPDATE MedCod "
  				updateSQL <- paste(updateSQL,"SET ",cTerm[j,"icd"]," = '",icdNew,"', State = '",estado,"' ",sep="")
  				updateSQL <- paste(updateSQL,"WHERE ",sep="")
  				updateSQL <- paste(updateSQL,"Ident = '",medcod[i,"Ident"],"'",sep="")
  				newReject <- dbExecute(db, updateSQL)
  			}
  		}
  		if(estado == "Inicial"){
  			cntFinal <- cntFinal + 1
  		}
  	}
  }
  
  output$contents3 <- renderPrint(c(c("Code assignment completed."), c("Parameterized recoding completed.")))
  
  nRej <- nComp - cntFinal
  
  write(paste("Registros rechazados post-validaci?n: ",nRej,sep=""),fileReport,append=TRUE)
  
  write(paste("Registros para la aplicaci?n de reglas: ",cntFinal,sep=""),fileReport,append=TRUE)
  
  #BLOQUE DE REGLAS
  write((Sys.time() - start.time),fileReport,append=TRUE)
  write("PROCESO: REGLAS DE CODIFICACI?N EN MORBILIDAD",fileReport,append=TRUE)
  output$contents1 <- renderPrint("Aplicando reglas de morbilidad...")
  cntFinal_ <- 0
  medcod <- dbGetQuery(db, "SELECT Ident, Icd1, Icd2, Icd3, Icd4 FROM MedCod WHERE State = 'Inicial' ORDER BY Icd4, Icd3, Icd2, Icd1")
  for(i in 1:nrow(medcod)){
  	strSQL <- "SELECT AgeCond, Age, Sex, Service, HospDays, Condition "
  	strSQL <- paste(strSQL,"FROM Ident ",sep="")
  	strSQL <- paste(strSQL,"WHERE Ident = '",medcod[i,"Ident"],"'",sep="")
  	ident <- dbGetQuery(db, strSQL)
  	sex <- as.numeric(ident[,"Sex"])
  	age <- as.numeric(ident[,"Age"])
  	ageC <- as.numeric(ident[,"AgeCond"])
  	service <- as.numeric(ident[,"Service"])
  	condition <- as.numeric(ident[,"Condition"])
  	hospDays <- as.numeric(ident[,"HospDays"])
  	estado <- "Inicial"
  	vIcd <- c(medcod[i,"Icd1"],medcod[i,"Icd2"],medcod[i,"Icd3"],medcod[i,"Icd4"])
  	vInd <- grep("([A-U]|Z)[0-9]{2}[0-9]?",vIcd)
  	iExt <- grep("[V-Y][0-9]{2}[0-9]?",vIcd)
  	vExt <- c()
  	if(length(iExt)>0){
  	  vExt <- vIcd[iExt]
  	}
  	if(length(vInd) > 0){
  		vIcd_ <- vIcd[vInd]
  		if(length(vInd) == 1){
  			icdAP <- vIcd_[1]
  			icdAS <- ""
  			icdCE <- ""
  			strSQL <- "SELECT MainCond, External "
  			strSQL <- paste(strSQL,"FROM IcdValidation ",sep="")
  			strSQL <- paste(strSQL,"WHERE Icd = '",icdAP,"'",sep="")
  			icdValid <- dbGetQuery(db, strSQL)
  			if(nrow(icdValid) > 0){
  			  if(icdValid[,"MainCond"]){
  			    if(grepl("[ST][0-9]{2}[0-9]?",icdAP)){
  			      if(length(vExt)>0){
  			        icdCE = vExt[1]
  			      }else{
  			        icdCE = icdValid[,"External"]
  			      }
  			      strResult <- "Un solo traumatismo informado"
  			      write(paste(medcod[i,"Ident"],"SingleCondition","IcdAP",paste(icdAP,"(",icdCE,")",sep=""),"",age,ageC,sex,strResult,sep=";"),fileCodifica,append=TRUE)
  			    }else{
  			      strResult <- "Una sola afecci?n registrada"
  			      write(paste(medcod[i,"Ident"],"SingleCondition","IcdAP",icdAP,"",age,ageC,sex,strResult,sep=";"),fileCodifica,append=TRUE)
  			    }
  				  estado <- "Final"
  				  cntFinal_ <- cntFinal_ + 1
  			  }else{
  			    if(length(vExt) > 0){
    			    estado <- "Final"
    				  icdAS <- ""
    				  icdAP <- "T149"
  			      icdCE <- vExt[1]
    				  strResult <- paste(icdAS," no debe registrarse como afecci?n principal",sep="")
    				  write(paste(medcod[i,"Ident"],"SingleCondition","IcdAP",icdAS,icdAP,age,ageC,sex,strResult,sep=";"),fileCodifica,append=TRUE)
  			    }else{
  			      estado <- "Inicial"
  			      icdAS <- icdAP
  			      icdAP <- "R69"
  			      icdCE <- ""
  			      strResult <- paste(icdAS," no debe registrarse como afecci?n principal",sep="")
  			      write(paste(medcod[i,"Ident"],"SingleCondition","IcdAP",icdAS,icdAP,age,ageC,sex,strResult,sep=";"),fileCodifica,append=TRUE)
  			    }
  			  }
  			}else{
  			  strResult <- "No aceptada como afecci?n principal"
  			  write(paste(medcod[i,"Ident"],"Validation","IcdAP",icdAP,"",age,ageC,sex,strResult,sep=";"),fileCodifica,append=TRUE)
  			}
  		}else{
  			wIcd_ <- c(1:length(vIcd_))
  			dfIcd_ <- data.frame(vIcd_,wIcd_)
  			icdCE <- ""
  			#Reglas MB1, MB2
  			for(j in 1:nrow(dfIcd_)){
  				dfIcd_[j,"wIcd_"] <- 0
  				strSQL <- "SELECT Weight "
  				strSQL <- paste(strSQL,"FROM IcdServices ",sep="")
  				strSQL <- paste(strSQL,"WHERE ",sep="")
  				strSQL <- paste(strSQL,"IcdStart <= '",dfIcd_[j,"vIcd_"],"' AND ",sep="")
  				strSQL <- paste(strSQL,"IcdEnd >= '",dfIcd_[j,"vIcd_"],"' AND ",sep="")
  				strSQL <- paste(strSQL,"Service = '",service,"'",sep="")
  				icdServ <- dbGetQuery(db, strSQL)
  				if(nrow(icdServ) > 0){
  				  dfIcd_[j,"wIcd_"] <- dfIcd_[j,"wIcd_"] + icdServ[1,"Weight"]
  				}
  			}
  			strResult <- "Aplicaci?n reglas MB1, MB2: Icd/Service"
  			write(paste(medcod[i,"Ident"],"MB1,MB2","Afecciones",toString(dfIcd_),toString(dfIcd_[order(dfIcd_$wIcd_,decreasing=TRUE),]),age,ageC,sex,strResult,sep=";"),fileCodifica,append=TRUE)
  			dfIcd_ <- dfIcd_[order(dfIcd_$wIcd_,decreasing=TRUE),]
  			
  			#Aplicaci?n de Tabla 7.7 para la clasificaci?n de traumatimos
  			if(grepl("[ST][0-9]{2}[0-9]?",dfIcd_[1,"vIcd_"])){
    			strResult <- ""
    			for(j in 1:nrow(dfIcd_)){
    			  dfIcd_[j,"wIcd_"] <- 0
    			  strSQL <- "SELECT Rating "
    			  strSQL <- paste(strSQL,"FROM IcdTrauma ",sep="")
    			  strSQL <- paste(strSQL,"WHERE ",sep="")
    			  strSQL <- paste(strSQL,"IcdStart <= '",dfIcd_[j,"vIcd_"],"' AND ",sep="")
    			  strSQL <- paste(strSQL,"IcdEnd >= '",dfIcd_[j,"vIcd_"],"'",sep="")
    			  traumaRat <- dbGetQuery(db, strSQL)
    			  if(nrow(traumaRat) > 0){
    			    dfIcd_[j,"wIcd_"] <- dfIcd_[j,"wIcd_"] + (6 - traumaRat[1,"Rating"] + 1)
    			  }
    			}
    			strResult <- "Aplicaci?n tabla 7.7: Icd/Rating"
    			write(paste(medcod[i,"Ident"],"Table7.7","Traumatismos",toString(dfIcd_),toString(dfIcd_[order(dfIcd_$wIcd_,decreasing=TRUE),]),age,ageC,sex,strResult,sep=";"),fileCodifica,append=TRUE)
  			  dfIcd_ <- dfIcd_[order(dfIcd_$wIcd_,decreasing=TRUE),]
  			  if(length(vExt)>0){
  			    icdCE <- vExt[1]
  			  }else{
  			    strSQL <- "SELECT External "
  			    strSQL <- paste(strSQL,"FROM IcdValidation ",sep="")
  			    strSQL <- paste(strSQL,"WHERE Icd = '",dfIcd_[1,"vIcd_"],"'",sep="")
  			    external <- dbGetQuery(db, strSQL)
  			    if(nrow(external) > 0){
  			      icdCE <- external[1, "External"]
  			    }
  			  }
  			}
  			
  			#Clasificación de fracturas patológicas
  			
  			#Reglas MB3, MB5
  			strResult <- "No Aplica MB3, MB5"
  			isSymp <- FALSE
  			dfIcd <- dfIcd_
  		  for(j in 2:nrow(dfIcd_)){
  		    strSQL <- "SELECT Weight "
  		    strSQL <- paste(strSQL,"FROM IcdSymptoms ",sep="")
  		    strSQL <- paste(strSQL,"WHERE ",sep="")
  		    strSQL <- paste(strSQL,"IcdSymStart <= '",dfIcd_[1,"vIcd_"],"' AND ",sep="")
  		    strSQL <- paste(strSQL,"IcdSymEnd >= '",dfIcd_[1,"vIcd_"],"' AND ",sep="")
  		    strSQL <- paste(strSQL,"IcdCondStart <= '",dfIcd_[j,"vIcd_"],"' AND ",sep="")
  		    strSQL <- paste(strSQL,"IcdCondEnd >= '",dfIcd_[j,"vIcd_"],"'",sep="")
  		    icdSymp <- dbGetQuery(db, strSQL)
  		    if(nrow(icdSymp) > 0){
  		      isSymp <- TRUE
  		      strResult <- "Aplicaci?n MB3, MB5: S?ntomas"
  		      dfIcd_[j,"wIcd_"] <- dfIcd_[j,"wIcd_"] + icdSymp[1,"Weight"]
  		    }
  		  }
  		  write(paste(medcod[i,"Ident"],"MB3,MB5","Afecciones",toString(dfIcd),toString(dfIcd_),age,ageC,sex,strResult,sep=";"),fileCodifica,append=TRUE)
  		  dfIcd_ <- dfIcd_[order(dfIcd_$wIcd_,decreasing=TRUE),]
  		  
  		  #Regla MB4 y Notas
  		  strResult <- "No Aplica MB4/Notas"
  		  j <- 2
  		  while(j <= nrow(dfIcd_)){
  		    if(dfIcd_[1,"vIcd_"] == dfIcd_[j,"vIcd_"]){
  		      dfIcd_ <- dfIcd_[-c(j),]
  		      strResult <- "C?digo duplicado/Depuraci?n"
  		      j <- 1
  		    }else{
  		      if(nchar(dfIcd_[1,"vIcd_"]) == 4 & substr(dfIcd_[1,"vIcd_"],1,3) == substr(dfIcd_[j,"vIcd_"],1,3)){
  		        if(substr(dfIcd_[1,"vIcd_"],4,4) == "9" & substr(dfIcd_[j,"vIcd_"],4,4) < "9"){
  		          icdTmp <- dfIcd_[1,"vIcd_"]
  		          dfIcd_[1,"vIcd_"] <- dfIcd_[j,"vIcd_"]
  		          dfIcd_[j,"vIcd_"] <- icdTmp
  		          strResult <- "Specificity"
  		          j <- 1
  		        }
  		      }
  		      if(strResult == "No Aplica MB4/Notas"){
    		      strSQL <- "SELECT IcdFilterIn1, IcdFilterOut, Type "
    		      strSQL <- paste(strSQL,"FROM IcdSubst2 ",sep="")
    		      strSQL <- paste(strSQL,"WHERE ",sep="")
    		      strSQL <- paste(strSQL,"IcdStart <= '",dfIcd_[1,"vIcd_"],"' AND ",sep="")
    		      strSQL <- paste(strSQL,"IcdEnd >= '",dfIcd_[1,"vIcd_"],"' AND ",sep="")
    		      strSQL <- paste(strSQL,"IcdFilterIn1 <= '",dfIcd_[j,"vIcd_"],"' AND ",sep="")
    		      strSQL <- paste(strSQL,"IcdFilterIn2 >= '",dfIcd_[j,"vIcd_"],"' ",sep="")
    		      strSQL <- paste(strSQL,"ORDER BY Rank",sep="")
    		      strSQL
    		      icdSubst2 <- dbGetQuery(db, strSQL)
    		      icdSubst2
    		      if(nrow(icdSubst2) > 0){
    		        for(k in 1:nrow(icdSubst2)){
    		          icdP <- dfIcd_[1,"vIcd_"]
    	            icdS <- dfIcd_[j,"vIcd_"]
    	            if(icdSubst2[k,"Type"] == "Combination"){
    	              dfIcd_[1,"vIcd_"] <- icdSubst2[k,"IcdFilterOut"]
    	              dfIcd_ <- dfIcd_[-c(j),]
    	            }else{
    	              icdTmp <- dfIcd_[1,"vIcd_"]
    	              dfIcd_[1,"vIcd_"] <- dfIcd_[j,"vIcd_"]
    	              dfIcd_[j,"vIcd_"] <- icdTmp
    	            }
    	            strResult <- paste("Aplicaci?n MB4/Notas: ",icdSubst2[k,"Type"]," ",icdP,"/",icdS,"=",dfIcd_[1,"vIcd_"],sep="")
    	            j <- 1
    	            break
    		        }
    		      }else{
    		        strResult <- "No Aplica MB4/Notas"
    		      }
  		      }
  		    }
  		    j <- j + 1
  		  }
  		  write(paste(medcod[i,"Ident"],"MB4","Afecciones",toString(dfIcd_),"NA",age,ageC,sex,strResult,sep=";"),fileCodifica,append=TRUE)
  		  icdAP <- dfIcd_[1,"vIcd_"]
  		  if(nrow(dfIcd_) > 1){
  		    icdAS <- dfIcd_[2,"vIcd_"]
  		  }else{
  		    icdAS <- ""
  		  }
  		  strSQL <- "SELECT MainCond "
  		  strSQL <- paste(strSQL,"FROM IcdValidation ",sep="")
  		  strSQL <- paste(strSQL,"WHERE Icd = '",icdAP,"'",sep="")
  		  icdValid <- dbGetQuery(db, strSQL)
  		  if(nrow(icdValid) > 0){
  		    if(!icdValid[,"MainCond"]){
  		      estado <- "Inicial"
  		      strResult <- "No aceptado como c?digo de afecci?n principal"
  		      write(paste(medcod[i,"Ident"],"Validation","MainCondition",icdAP,"NA",age,ageC,sex,strResult,sep=";"),fileCodifica,append=TRUE)
  		    }else{
  		      cntFinal_ = cntFinal_ + 1
  		      estado <- "Final"
  		    }
  		  }else{
  		    estado <- "Inicial"
  		    strResult <- "No encontrado en el cat?logo INEC"
  		    write(paste(medcod[i,"Ident"],"Validation","IcdAP",icdAP,"NA",age,ageC,sex,strResult,sep=";"),fileCodifica,append=TRUE)
  		  }
  		}
  		if(grepl("O80[0-9]",icdAP)){
  		  if(hospDays > 3 | condition != 1){
  		    icdAP <- "O759"
  		    strResult <- "Parto normal con m?s de 3 d?as o con muerte materna"
  		    write(paste(medcod[i,"Ident"],"Recodif","IcdAP",icdAP,"",age,ageC,sex,strResult,sep=";"),fileCodifica,append=TRUE)
  		  }
  		}
  	}else{
  	  if(length(vExt) > 0){
  	    cntFinal_ <- cntFinal_ + 1
  	    estado <- "Final"
  	    icdAP <- "T149"
  	    icdAS <- ""
  	    icdCE <- vExt[1]
  	    strSQL <- "SELECT External "
  	    strSQL <- paste(strSQL,"FROM IcdValidation ",sep="")
  	    strSQL <- paste(strSQL,"WHERE Icd = '",icdCE,"'",sep="")
  	    external <- dbGetQuery(db, strSQL)
  	    if(nrow(external) > 0){
  	      icdAP <- external[1, "External"]
  	    }
  	    strResult <- "Causa externa sin traumatismos"
  	    write(paste(medcod[i,"Ident"],"NoTrauma","IcdAP",paste(icdAP,"(",icdCE,")",sep=""),"",age,ageC,sex,strResult,sep=";"),fileCodifica,append=TRUE)
  	  }else{
  	    estado <- "Inicial"
  	    icdAP <- "R69"
  	    icdAS <- ""
  	    icdCE <- ""
  	    strResult <- "No existen afecciones registradas"
  	    write(paste(medcod[i,"Ident"],"NoCondition","IcdAP",icdAP,"",age,ageC,sex,strResult,sep=";"),fileCodifica,append=TRUE)  
  	  }
  	}
  	updateSQL <- "UPDATE MedCod "
  	updateSQL <- paste(updateSQL,"SET IcdAP = '",icdAP,"', IcdAS = '",icdAS,"', IcdCE = '",icdCE,"', State = '",estado,"' ",sep="")
  	updateSQL <- paste(updateSQL,"WHERE ",sep="")
  	updateSQL <- paste(updateSQL,"Ident = '",medcod[i,"Ident"],"'",sep="")
  	update <- dbExecute(db, updateSQL)
  }
  
  output$contents4 <- renderPrint("Morbility rules applied.")
  
  write(paste("Registros en estado final: ",cntFinal_,sep=""),fileReport,append=TRUE)
  write(paste("Tasa bruta de registros codificados: ",(cntFinal_/nReg)*100,"%",sep=""),fileReport,append=TRUE)
  write(paste("Tasa ajustada de registros codificados: ",(cntFinal_/cntFinal)*100,"%",sep=""),fileReport,append=TRUE)
  dbDisconnect(db)
  
  output$contents2 <- renderPrint(c("Coding rate: ", (cntFinal_/nReg)*100))
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  time.taken
  write("PROCESO: FIN DEL PROCESAMIENTO",fileReport,append=TRUE)
  write(time.taken,fileReport,append=TRUE)
  
  close(fileAsigna)
  close(fileCodifica)
  close(fileReport)
  result_cols <- c("Resultado:", "Registros procesados:", "Tiempo:")
  result_values <- c("Procesamiento completado satisfactoriamente.",nReg,time.taken)
  result_df <- data.frame(result_cols, result_values)
  output$contents1 <- renderPrint(result_df)
  return("Proceso terminado")
}