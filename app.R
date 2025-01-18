pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
shiny::runApp(appDir = list(
    ui = paradrugUI(id = "pd"), 
    server = function(input, output, session){
    }))
