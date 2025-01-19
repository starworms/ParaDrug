# pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
# shiny::runExample("01_hello")
x = installed.packages()[, c("Package", "LibPath", "Version")]
#x = head(x)
warning(paste(sprintf("%s: %s --- %s", x[, "Package"], x[, "Version"], x[, "LibPath"]), collapse = "\n"))
#install.packages("remotes")
library(shiny)
library(remotes)
#remotes::install_github(repo = "starworms/ParaDrug")
library(ParaDrug)
shiny::runApp(appDir = list(
 ui = paradrugUI(id = "pd"), 
 server = function(input, output, session){
}))
