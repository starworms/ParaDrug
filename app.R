# pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
# shiny::runExample("01_hello")
library(shiny)
library(remotes)
remotes::install_github(repo = "starworms/ParaDrug")
library(ParaDrug)
shiny::runApp(appDir = list(
 ui = paradrugUI(id = "pd"), 
 server = function(input, output, session){
}))
