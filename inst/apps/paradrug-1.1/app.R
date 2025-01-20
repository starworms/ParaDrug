library(shiny)
library(ParaDrug)
shinyApp(
    ui = paradrugUI(id = "pd"),
    server = paradrugServer)