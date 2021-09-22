library(shiny)
library(rstudioapi)

port <- Sys.getenv('PORT')

shiny::runApp(
  appDir = dirname(rstudioapi::getSourceEditorContext()$path),
  host = '127.0.0.1',
  port = getOption("shiny.port")
)