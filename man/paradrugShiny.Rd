% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/paradrug-shiny.R
\name{paradrugShiny}
\alias{paradrugShiny}
\alias{paradrugUI}
\alias{paradrugServer}
\title{Paradrug Shiny app component}
\usage{
paradrugUI(id, ...)

paradrugServer(input, output, session)
}
\arguments{
\item{id}{character string with the id of the module}

\item{...}{not used}

\item{input}{Shiny inputs}

\item{output}{Shiny outputs}

\item{session}{Shiny session}
}
\value{
\code{paradrugUI} returns the the ui elements of the Paradrug app.\cr
\code{paradrugServer} returns the the server components of the Paradrug app.
}
\description{
Main UI part of the Paradrug app
}
\examples{
if(interactive()){
library(ParaDrug)
library(shiny)

ui <- paradrugUI()
server <- paradrugServer
shinyApp(ui, server)
}
}
