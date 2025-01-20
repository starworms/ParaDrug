options(echo = TRUE)
if (!interactive()) {
    rsconnect::setAccountInfo(
        name   = Sys.getenv("SHINYAPPS_ACCOUNT"),
        token  = Sys.getenv("SHINYAPPS_TOKEN"),
        secret = Sys.getenv("SHINYAPPS_SECRET"))
}
oldwd = getwd()
setwd("inst/apps/paradrug-1.1")
renv::restore(project = getwd(), lockfile = "renv.lock", prompt = FALSE)
source("renv/activate.R")
app_files = c(
    "DESCRIPTION",
    "app.R",
    ".Rprofile",
    "renv.lock"
)
rsconnect::deployApp(appName = "paradrug", appFiles = app_files, forceUpdate = TRUE, logLevel = "verbose", appMode = "shiny")
setwd(oldwd)