library(shiny)
library(shinythemes)
library(DBI)
library(RSQLite)
library(shinybusy)

ui <- fluidPage(theme = shinytheme("flatly"),
    shinyjs::useShinyjs(),
    navbarPage("SmartMorbility vCIE10-2018",
           tabPanel("Codificar lote",
                    sidebarLayout(
                        wellPanel(
                            fileInput('file1', 'Archivo ~Ident~ (parámetros de validación e identificación)',accept=c('.txt')),
                            fileInput('file2', 'Archivo ~MedCod~ (términos médicos a codificar)',accept=c('.txt')),
                            actionButton("submitbutton", "Ejecutar algoritmo", class = "btn btn-primary"),
                            HTML("<a class='btn btn-default' href='/SmartMorbility'>Limpiar</a>")
                        ), #wellPanel
                        mainPanel(
                            column(style = "overflow-y:scroll; max-height: 200px",
                                   width = 12,
                                   tags$label("Estado del procesamiento",style="float: none; width: 100%;"),
                                   downloadButton('downloadData1', 'Descargar MedCod CSV'),
                                   verbatimTextOutput('contents1')
                            ),
                            column(style = "overflow-y:scroll; max-height: 200px",
                                   width = 4,
                                   tags$label("Estadísticas",style="float: none; width: 100%;"),
                                   downloadButton('downloadData2', 'Descargar TXT'),
                                   verbatimTextOutput('contents2')
                            ),
                            column(style = "overflow-y:scroll; max-height: 200px",
                                   width = 4,
                                   tags$label("Asignación de códigos",style="float: none; width: 100%;"),
                                   downloadButton('downloadData3', 'Descargar TXT'),
                                   verbatimTextOutput('contents3')
                            ),
                            column(style = "overflow-y:scroll; max-height: 200px",
                                   width = 4,
                                   tags$label("Recodificación parametrizada / Reglas morbilidad",style="float: none; width: 100%;"),
                                   downloadButton('downloadData4', 'Descargar TXT'),
                                   verbatimTextOutput('contents4')
                            )
                        )
                    ) #sidebarLayout
           ),
        tabPanel("Codificar término",
            sidebarLayout(
                sidebarPanel(
                    checkboxInput("validations","Parametrizar la codificación",value = FALSE),
                    numericInput("age","Edad cumplida:",1,min = 1,max = 125),
                    selectizeInput("cond_age","Edad en:",c("Hora(s)"=1,"Día(s)"=2,"Mes(es)"=3,"Año(s)"=4)),
                    selectizeInput("sex","Sexo:",c("Masculino"=1,"Femenino"=2)),
                    checkboxInput("obstetric","¿Embarazo?",value = FALSE),
                    numericInput("gest_weeks","Semanas de gestación:",1,min = 1,max = 43),
                    textInput("term", NULL, "",placeholder = "Ingrese aquí el término médico a codificar"),
                    actionButton("assign","Asignar código", icon=NULL)
                ),
                mainPanel(
                    h2("Resultados"),
                    h4("Término estandarizado"),
                    verbatimTextOutput("stand_term",placeholder = TRUE),
                    h4("Codificación general"),
                    verbatimTextOutput("gen_code",placeholder = TRUE),
                    h4("Codificación parametrizada"),
                    verbatimTextOutput("param_code",placeholder = TRUE),
                    h4("Código final"),
                    verbatimTextOutput("final_code",placeholder = TRUE)
                )
            )
        )
    ),
    copyright <- div(HTML("<br><table border=0 cellpadding=10 cellspacing=10 width='100%' height='50'><tr><td bgcolor='#f2f2f2' align='center'>Copyright © 2021 <a href='https://www.ecuadorencifras.gob.ec/'>INEC - Ecuador en Cifras</a>. All rights reserved.</td></tr></table>")),
    cat(as.character(copyright))
)

server <- function(input, output, session) {
    source("./CIEdify_assignment.R")
    source("./CIE_morbility.R")
    observeEvent(input$validations, {
        if(input$validations == FALSE){
            updateCheckboxInput(session = session, "obstetric", value = FALSE)
            shinyjs::disable("age")
            shinyjs::disable("cond_age")
            shinyjs::disable("sex")
            shinyjs::disable("obstetric")
        }else{
            shinyjs::enable("age")
            shinyjs::enable("cond_age")
            shinyjs::enable("sex")
        }
    })
    observeEvent(input$obstetric, {
        if(input$obstetric == FALSE){
            shinyjs::disable("gest_weeks")
        }else{
            shinyjs::enable("gest_weeks")
        }
    })
    observeEvent(input$age, {
        if(
            (input$age >= 10 & input$age <= 50) &
            input$cond_age == 4 &
            input$sex ==2
        ){
            shinyjs::enable("obstetric")
        }else{
            updateCheckboxInput(session = session, "obstetric", value = FALSE)
            shinyjs::disable("obstetric")
        }
    })
    observeEvent(input$cond_age, {
        if(
            (input$age >= 10 & input$age <= 50) &
            input$cond_age == 4 &
            input$sex ==2
        ){
            shinyjs::enable("obstetric")
        }else{
            updateCheckboxInput(session = session, "obstetric", value = FALSE)
            shinyjs::disable("obstetric")
        }
    })
    observeEvent(input$sex, {
        if(
            (input$age >= 10 & input$age <= 50) &
            input$cond_age == 4 &
            input$sex ==2
        ){
            shinyjs::enable("obstetric")
        }else{
            updateCheckboxInput(session = session, "obstetric", value = FALSE)
            shinyjs::disable("obstetric")
        }
    })
    #observeEvent(input$assign, {
    #    output$final_code <- renderText({suma(input$age, input$gest_weeks)})
    #})
    observeEvent(input$assign, {
        result <- assignICD10(
            term = input$term,
            validation = input$validations,
            obstetric = input$obstetric,
            sex = input$sex,
            age = input$age,
            age_cond = input$cond_age,
            gest_weeks = input$gest_weeks
        )
        
        output$stand_term <- renderText({result[1]})
        output$gen_code <- renderText({result[2]})
        output$param_code <- renderText({result[3]})
        output$final_code <- renderText({result[4]})
    })
    
    datasetInput <- reactive({
        
        inFile1 <- input$file1
        inFile2 <- input$file2
    
        if (is.null(inFile1) || is.null(inFile2)) {
            print("Error!: No ha seleccionado los archivos de entrada.")
        } 
        else {
            show_modal_spinner()
            print("Guardando archivos en base de datos...")
            identData <- read.table(inFile1$datapath, header = TRUE, sep = "\t")
            medcodData <- read.table(inFile2$datapath, header = TRUE, sep = "\t")
            ident_fields <- c("Ident","Sex","Age","AgeCond","HospDays","Condition","Service")
            medcod_fields <- c("Ident","Term1","Term2","Term3","Term4","Icd1","Icd2","Icd3","Icd4","IcdAP","IcdAS","IcdCE","State")
            if (all(names(identData) == ident_fields) && all(names(medcodData) == medcod_fields)){
                db <- dbConnect(SQLite(), "CIE10morb.db")
                print("Limpiando tablas...")
                dbExecute(db, "DELETE FROM Ident")
                dbExecute(db, "DELETE FROM MedCod")
                print("Almacenando registros...")
                dbWriteTable(db, name="Ident", value=identData, append=TRUE)
                dbWriteTable(db, name="MedCod", value=medcodData, append=TRUE)
                for( f in ident_fields ){
                    dbExecute(db, paste("UPDATE Ident SET ",f," = '' WHERE ",f," IS NULL",sep=""))
                }
                for( f in medcod_fields ){
                    dbExecute(db, paste("UPDATE MedCod SET ",f," = '' WHERE ",f," IS NULL",sep=""))
                }
                dbDisconnect(db)
                result <- batch_coding(input, output, session)
                print("Proceso completado.")
            }else{
                print("Error!: Los nombres de las variables no coinciden con los campos requeridos.")
            }
            remove_modal_spinner()
        }
        
    })
    
    output$contents1 <- renderPrint({
        print("El tiempo de ejecución depende del número de registros y la complejidad de la codificación.")
        if (input$submitbutton>0) {
            isolate(datasetInput())
        } else {
            return("Servidor preparado para el procesamiento.")
        }
    })
    
    datasetMedCod <- reactive({
        db <- dbConnect(SQLite(), "CIE10morb.db")
        medcod <- dbGetQuery(db, "SELECT * FROM MedCod ORDER BY Ident")
        dbDisconnect(db)
        return(medcod)
    })
    
    output$downloadData1 <- downloadHandler(
        filename = function() {
            paste("MedCod", ".csv", sep = "")
        },
        content = function(file) {
            write.csv(datasetMedCod(), file, row.names = FALSE)
        }
    )
    
    output$downloadData2 <- downloadHandler(
        filename <- function() {
            paste("statistics", "txt", sep=".")
        },
        
        content <- function(file) {
            file.copy("Reports/resumen.txt", file)
        }
    )
    
    output$downloadData3 <- downloadHandler(
        filename <- function() {
            paste("assignment", "txt", sep=".")
        },
        
        content <- function(file) {
            file.copy("Reports/asigna.txt", file)
        }
    )
    
    output$downloadData4 <- downloadHandler(
        filename <- function() {
            paste("recoding_by_rules", "txt", sep=".")
        },
        
        content <- function(file) {
            file.copy("Reports/recodif.txt", file)
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
