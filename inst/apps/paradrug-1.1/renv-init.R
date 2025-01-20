library(renv)
renv::settings$snapshot.type("explicit")
options(renv.snapshot.ignore.self = FALSE)
#renv::init(project = "inst/apps/paradrug-1.1")
renv::init()
## TO update package to the newest versions
if(FALSE){
    renv::install("rsconnect", lock = TRUE)
    renv::update("jsonlite", lock = TRUE)
    renv::update()
    renv::init()    
    renv::restore()
}
