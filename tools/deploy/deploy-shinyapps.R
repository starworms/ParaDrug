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
rsconnect::deployApp(appName = "paradrug", appFiles = app_files, appMode = "shiny", forceUpdate = TRUE)