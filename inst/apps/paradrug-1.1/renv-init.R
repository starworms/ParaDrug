library(renv)
## Create the .lockfile based on the DESCRIPTION file
renv::settings$snapshot.type("explicit")
options(renv.snapshot.ignore.self = FALSE)
renv::init()

## Maintenance related
if(FALSE){
    ## TO update package to the newest versions
    renv::install("rsconnect", lock = TRUE)
    renv::install("starworms/ParaDrug", lock = TRUE)
    renv::update("jsonlite", lock = TRUE)
    renv::update()
    renv::init()    
    renv::restore()
}
