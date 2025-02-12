library(renv)
## Create the .lockfile based on the DESCRIPTION file
renv::settings$snapshot.type("explicit")
options(renv.snapshot.ignore.self = FALSE)
renv::init()

##
## Maintenance related
##
if(FALSE){
    ##
    ## TO update package on shinyapps.io to the newest versions
    ## Change the working directory to the location of this file
    ## Use the remote sha in renv.lock for the ParaDrug package and push the repo again (CI will deploy this version of the package specified in that sha to shinyapps)
    ##
    renv::install("starworms/ParaDrug", lock = TRUE)
    
    ##
    ## Other - maintenance related elements (e.g. upgrade an R package to a newer version in renv)
    ##
    renv::install("rsconnect", lock = TRUE)
    renv::update("jsonlite", lock = TRUE)
    renv::update()
    renv::init()    
    renv::restore()
}
