library(shiny)
library(rstudioapi)

port <- Sys.getenv('PORT')

shiny::runApp(
  appDir = dirname(rstudioapi::getSourceEditorContext()$path),
  host = '0.0.0.0',
  port = as.numeric(port)
)