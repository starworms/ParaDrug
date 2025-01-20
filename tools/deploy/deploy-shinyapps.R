if (!interactive()) {
    rsconnect::setAccountInfo(
        name   = Sys.getenv("SHINYAPPS_ACCOUNT"),
        token  = Sys.getenv("SHINYAPPS_TOKEN"),
        secret = Sys.getenv("SHINYAPPS_SECRET"))
}
app_files = c(
    "app.R",
    "DESCRIPTION",
    "NAMESPACE",
    "R/",
    "inst/"
)
app_files = c(
    "inst/apps/paradrug-1.1/DESCRIPTION",
    "inst/apps/paradrug-1.1/ui.R",
    "inst/apps/paradrug-1.1/server.R"
)
rsconnect::deployApp(appName = "paradrug", appFiles = app_files, forceUpdate = TRUE, logLevel = "verbose", appMode = "shiny")