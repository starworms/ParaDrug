
#' @title Paradrug Shiny app component
#' @description Main UI part of the Paradrug app
#' @param id character string with the id of the module
#' @param ... not used
#' @return 
#' \code{paradrugUI} returns the the ui elements of the Paradrug app.\cr
#' \code{paradrugServer} returns the the server components of the Paradrug app.
#' @export
#' @name paradrugShiny
#' @aliases paradrugUI paradrugServer
#' @examples 
#' if(interactive()){
#' library(ParaDrug)
#' 
#' ui <- fluidPage(
#'   paradrugUI(id = "activity")
#' )
#' server <- function(input, output, session) {
#'   paradrugServer(id = "activity")
#' }
#' shinyApp(ui, server)
#' }
paradrugUI <- function(id, ...){
}


#' @name paradrugShiny
#' @export
paradrugServer <- function(id, ...){
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
    })
}