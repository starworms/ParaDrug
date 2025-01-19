# pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
# shiny::runExample("01_hello")
library(ParaDrug)
library(shiny)
shiny::runApp(appDir = list(
 ui = paradrugUI(id = "pd"), 
 server = function(input, output, session){
}))
