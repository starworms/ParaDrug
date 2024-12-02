
#' @title Create a Paradrug report
#' @description Create a Paradrug report
#' @param x an object as returned by \code{\link{read_paradrug_xls}}
#' @param params a list of options with elements
#' @param ... not used yet
#' @export
#' @return a list with elements
#' \itemize{
#' \item{report: the source file of the report}
#' \item{out: the pdf of the report}
#' \item{params: the list of params for the report}
#' }
#' @examples 
#' path   <- system.file(package = "ParaDrug", "extdata", "data", "mydata.xlsx")
#' x      <- read_paradrug_xls(path)
#' library(xlsx)
#' params <- list(
#'     Country = "a not further specified country", 
#'     Name = "Unknown", 
#'     Region = "district/province not further specified",
#'     file1 = list(datapath = system.file(package = "ParaDrug", "extdata", "data", "mydata.xlsx")),
#'     NTD = 1, Sdrug = 1, STHdrug = 1, 
#'     Shbas = "BL_KK2_AL_EPG", Shfol = "FU_KK2_AL_EPG", 
#'     Smbas = "BL_KK2_TT_EPG", Smfol = "FU_KK2_TT_EPG", 
#'     Sjbas = "BL_KK2_HW_EPG", Sjfol = "FU_KK2_HW_EPG", 
#'     Rbas = "Not recorded", Rfol = "Not recorded", 
#'     Tbas = "Not recorded", Tfol = "Not recorded", 
#'     Hbas = "Not recorded", Hfol = "Not recorded",
#'     followup = "Not recorded")
#' report <- paradrug_report(x, params = params)
paradrug_report <- function(x, params = list(), ...){
    stopifnot(inherits(x, "paradrug_rawdata"))
    report_source <- system.file(package = "ParaDrug", "apps", "paradrug-1.0", "input2.Rnw")
    if(!missing(params)){
        input <- params
    }
    oldwd <- getwd()
    setwd(system.file(package = "ParaDrug", "apps", "paradrug-1.0"))
    on.exit(setwd(oldwd))
    out <- knit2pdf(report_source, clean = TRUE)
    out <- list(
        report = basename(report_source),
        output = out,
        params = params
    )
    out
}


