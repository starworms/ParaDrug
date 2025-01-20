library(renv)
renv::settings$snapshot.type("explicit")
options(renv.snapshot.ignore.self = FALSE)
renv::init(project = "inst/apps/paradrug-1.1")