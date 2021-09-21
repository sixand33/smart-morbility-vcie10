library(DBI)
library(RSQLite)
library(stringr)

applyParamValidation <- function(term_p, icd_f, obstetric_, age_p, age_cond_p, sex_p, gest_weeks_p){
  db_p <- dbConnect(SQLite(), "CIE10morb.db")
  icd_p <- ""
  strSQL_p <- "SELECT AgeCondMin, AgeCondMax, AgeMin, AgeMax, SexMin, SexMax, State, MainCond, Services "
  strSQL_p <- paste(strSQL_p,"FROM IcdValidation ",sep="")
  strSQL_p <- paste(strSQL_p,"WHERE Icd = '",icd_f,"'",sep="")
  icdVal <- dbGetQuery(db_p, strSQL_p)
  if(nrow(icdVal) > 0){
    #RECODIFICACI?N POR EDAD
    validationAge = ageValida(age__=age_p,
                              ageC__=age_cond_p,
                              ageCMin__=icdVal[,"AgeCondMin"],
                              ageCMax__=icdVal[,"AgeCondMax"],
                              ageMin__=icdVal[,"AgeMin"],
                              ageMax__=icdVal[,"AgeMax"])
    if(validationAge[1] == "FALSE"){
      icd_p <- paste("Edad(",icd_f,")=Error",sep="")
      strSQL_p <- "SELECT IcdSubst "
      strSQL_p <- paste(strSQL_p,"FROM IcdSubst1 ",sep="")
      strSQL_p <- paste(strSQL_p,"WHERE Icd = '",icd_f,"' AND Type = 'AGE' ORDER BY Rank",sep="")
      icdSubst <- dbGetQuery(db_p, strSQL_p)
      for(k in 1:nrow(icdSubst)){
        strSQL_p <- "SELECT AgeCondMin, AgeCondMax, AgeMin, AgeMax, SexMin, SexMax, State, MainCond, Services "
        strSQL_p <- paste(strSQL_p,"FROM IcdValidation ",sep="")
        strSQL_p <- paste(strSQL_p,"WHERE Icd = '",icdSubst[k,"IcdSubst"],"'",sep="")
        icdVal2 <- dbGetQuery(db_p, strSQL_p)
        if(nrow(icdVal2) > 0){
          validationAge2 = ageValida(age__=age_p,
                                     ageC__=age_cond_p,
                                     ageCMin__=icdVal2[,"AgeCondMin"],
                                     ageCMax__=icdVal2[,"AgeCondMax"],
                                     ageMin__=icdVal2[,"AgeMin"],
                                     ageMax__=icdVal2[,"AgeMax"])
          if(validationAge2[1] == "TRUE"){
            icd_p <- paste("Edad(",icd_f,")=",icdSubst[k,"IcdSubst"],sep="")
            icd_f <- icdSubst[k,"IcdSubst"]
            break
          }
        }
      }
    }
    #VALIDACIÓN POR SEXO
    strSQL_p <- "SELECT AgeCondMin, AgeCondMax, AgeMin, AgeMax, SexMin, SexMax, State, MainCond, Services "
    strSQL_p <- paste(strSQL_p,"FROM IcdValidation ",sep="")
    strSQL_p <- paste(strSQL_p,"WHERE Icd = '",icd_f,"'",sep="")
    icdVal <- dbGetQuery(db_p, strSQL_p)
    if(!(sex_p >= as.numeric(icdVal[,"SexMin"]) & sex_p <= as.numeric(icdVal[,"SexMax"]))){
      icd_p <- paste(icd_p,"; Sexo(",icd_f,")=Error",sep="")
    }
    if(obstetric_ == TRUE){
      #RECODIFICACIÓN OBSTÉTRICA
      print("Recodificación obstétrica")
      if(!grepl("[ZO][0-9][0-9][0-9]?",icd_f)){
        if(
          isObstetric(age_=age_p,
                       ageC=age_cond_p,
                       sex_=sex_p,
                       service_=99,
                       terms_=c(term_p)) | obstetric_ == TRUE
          )
        {
          strSQL_p <- "SELECT IcdSubst "
          strSQL_p <- paste(strSQL_p,"FROM IcdSubst1 ",sep="")
          strSQL_p <- paste(strSQL_p,"WHERE Icd <= '",icd_f,"' AND IcdEnd >= '",icd_f,"' AND Type = 'OBSTETRIC' ORDER BY Rank",sep="")
          icdSubst <- dbGetQuery(db_p, strSQL_p)
          if(nrow(icdSubst) > 0){
            icd_p <- paste(icd_p,"; Obstetricia(",icd_f,")=",icdSubst[1,"IcdSubst"],sep="")
            icd_f <- icdSubst[1,"IcdSubst"]
          }else{
            icd_p <- paste(icd_p,"; Obstetricia(",icd_f,")=Missing",sep="")
          }
        }
      }
      #RECODIFICACI?N OBST?TRICA POR SEMANAS DE GESTACI?N
      print(grepl("O(21[0-9]|47[019])",icd_f))
      print(icd_f)
      if(grepl("O(21[0-9]|47[019])",icd_f)){
        print("Semanas de gestación")
        gestWeeks <- as.numeric(getGestWeeks(termsW=c(term_p)))
        if(gest_weeks_p > gestWeeks){
          gestWeeks <- gest_weeks_p
        }
        print(gestWeeks)
        if(gestWeeks > 0){
          strSQL_p <- "SELECT IcdSubst "
          strSQL_p <- paste(strSQL_p,"FROM IcdSubst1 ",sep="")
          strSQL_p <- paste(strSQL_p,"WHERE Icd <= '",icd_f,"' AND IcdEnd >= '",icd_f,"' AND ",sep="")
          strSQL_p <- paste(strSQL_p,"GWMin <= ",gestWeeks," AND GWMax >= ",gestWeeks," AND ",sep="")
          strSQL_p <- paste(strSQL_p,"Type = 'GESTATIONAL' ORDER BY Rank",sep="")
          icdSubst <- dbGetQuery(db_p, strSQL_p)
          if(nrow(icdSubst) > 0){
            icd_p <- paste(icd_p,"; Gestacional(",icd_f,")=",icdSubst[,"IcdSubst"],sep="")
            icd_f <- icdSubst[,"IcdSubst"]
          }else{
            icd_p <- paste(icd_p,"; Gestacional(",icd_f,")=Missing",sep="")
          }
        }
      }
    }
  }
  dbDisconnect(db_p)
  return(c(icd_f, icd_p))
}

applyStandardisation <- function(std_n, term_std){
  icd_std <- ""
  db_std <- dbConnect(SQLite(), "CIE10morb.db")
  print(paste("Original: ",term_std,sep = ""))
  for(j in 1:nrow(std_n)){
    if(grepl(std_n[j,"FilterIn"], str_trim(term_std))){
      term_std = str_trim(gsub(std_n[j,"FilterIn"], paste(std_n[j,"FilterOut"],"",sep=""), term_std))
      print(paste("Estandarizado: ",term_std, " por ",std_n[j,"FilterIn"],sep = ""))
      strSQL_std <- paste("SELECT Icd1 FROM Dictionary WHERE DiagnosisText LIKE '", term_std, "'", sep="")
      result_std <- dbGetQuery(db_std, strSQL_std)
      if(nrow(result_std) > 0){
        icd_std <- result_std[,"Icd1"]
        break
      }
    }
  }
  print(paste("Código: ",icd_std,sep = ""))
  dbDisconnect(db_std)
  return(c(icd_std,term_std))
}

getGestWeeks <- function(termsW){
  resultW <- 0
  strFilterW <- "\\b((\\s?[A-Z]*\\s?){1,})?([0-9][0-9]?)\\s?S(EMANAS?)?\\s?(DE)?\\s?G(ESTACION)?\\b"
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
  strFilter <- "\\b([0-9][0-9]?\\s?S(EMANAS?)?\\s?(DE)?\\s?G(ESTACION)?|PARTOS?|CEA?[SZ]AR[IE]AS?|EMB(ARAZ(ADAS?|OS?))?)\\b"
  if(as.numeric(ageC_) == 4 & as.numeric(sex_) == 2){
    if(as.numeric(age_) >= 10 & as.numeric(age_) <= 50){
      #print(paste("Dentro del rango de edad fertil, longitud=",length(terms_),sep=""))
      for(i in 1:length(terms_)){
        #print(terms_[i])
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

assignICD10 <- function(term, validation, obstetric, sex, age, age_cond, gest_weeks){
  
  db <- dbConnect(SQLite(), "CIE10morb.db")
  std0 <- dbGetQuery(db, "SELECT MainKey, Rank, FilterIn, FilterOut FROM Standardisation0 ORDER BY MainKey, Rank")
  std1 <- dbGetQuery(db, "SELECT MainKey, Rank, FilterIn, FilterOut FROM Standardisation1 ORDER BY MainKey, Rank")
  std2 <- dbGetQuery(db, "SELECT MainKey, Rank, FilterIn, FilterOut FROM Standardisation2 ORDER BY MainKey, Rank")
  
  term_stand <- "{Empty}"
  icd_gen <- "N/A"
  icd_param <- "N/A"
  icd <- "N/A"
  
  term <- str_trim(toupper(term))
  term_orig <- term
  
  if(length(term) > 0){
    term_stand <- term
    strSQL <- paste("SELECT Icd1 FROM Dictionary WHERE DiagnosisText LIKE '", term, "'", sep="")
    result <- dbGetQuery(db, strSQL)
    if(nrow(result) > 0){
      icd_gen <- result[,"Icd1"]
    }else{
      result_std_n <- applyStandardisation(std_n = std0,term_std = term)
      icd_gen <- result_std_n[1]
      term <- result_std_n[2]
      if(!grepl("[A-Z][0-9][0-9][0-9]?",icd_gen)){
        result_std_n <- applyStandardisation(std_n = std1,term_std = term)
        icd_gen <- result_std_n[1]
        term <- result_std_n[2]
        if(!grepl("[A-Z][0-9][0-9][0-9]?",icd_gen)){
          result_std_n <- applyStandardisation(std_n = std2,term_std = term)
          icd_gen <- result_std_n[1]
          term <- result_std_n[2]
        }
      }
      term_stand <- term
      if(!grepl("[A-Z][0-9][0-9][0-9]?",icd_gen)){
        icd_gen <- "No se encontraron coincidencias"
      }
    }
    if(validation == TRUE){
      param_val <- 
        applyParamValidation(
          term_p = term_orig,
          icd_f = icd_gen,
          obstetric_ = obstetric,
          age_p = age,
          age_cond_p = age_cond,
          sex_p = sex,
          gest_weeks_p = gest_weeks
        )
      icd_param <- param_val[2]
      if(grepl("Error",icd_param)){
        icd <- "No se pudo asignar el código final debido a inconsistencias con los parámetros"
      }else{
        icd <- param_val[1]
      }
    }
  }
  dbDisconnect(db)
  return(c(term_stand,icd_gen,icd_param,icd))
}