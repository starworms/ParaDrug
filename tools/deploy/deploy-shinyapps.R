if (!interactive()) {
    rsconnect::setAccountInfo(
        name   = Sys.getenv("SHINYAPPS_ACCOUNT"),
        token  = Sys.getenv("SHINYAPPS_TOKEN"),
        secret = Sys.getenv("SHINYAPPS_SECRET"))
}
oldwd = getwd()
setwd("inst/apps/paradrug-1.1")
source("renv/activate.R")
app_files = c(
    "DESCRIPTION",
    "app.R",
    ".Rprofile",
    "renv.lock"
)
rsconnect::deployApp(appName = "paradrug", appFiles = app_files, forceUpdate = TRUE, logLevel = "verbose", appMode = "shiny")
setwd(oldwd)