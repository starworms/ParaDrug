options(echo = TRUE)
if (!interactive()) {
    rsconnect::setAccountInfo(
        name   = Sys.getenv("SHINYAPPS_ACCOUNT"),
        token  = Sys.getenv("SHINYAPPS_TOKEN"),
        secret = Sys.getenv("SHINYAPPS_SECRET"))
}
app_files = c(
    "DESCRIPTION",
    "app.R",
    ".Rprofile",
    "renv.lock",
    "renv/activate.R",
    "renv/settings.json"
)
rsconnect::deployApp(appName = "paradrug", appFiles = app_files, forceUpdate = TRUE, logLevel = "verbose", appMode = "shiny")
