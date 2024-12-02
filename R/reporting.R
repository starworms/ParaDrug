
#' @title Create a Paradrug report
#' @description Create a Paradrug report
#' @param x an object as returned by \code{\link{read_paradrug_xls}}
#' @param params a list of options with elements
#' @params version string with the version, either '1.0', or '1.1'
#' @param ... not used yet
#' @export
#' @details
#' Meaning of the terminology:
#' \itemize{
#' \item{NTD: Disease type, 1=Schistosomiasis, 2=Soil-transmitted helminthiasis}
#' \item{Sdrug: Anthelmintic drug, 1=Praziquantel (1x 40 mg/kg), 2=Other - Only relevant for Disease 'Schistosomiasis'}
#' \item{STHdrug: Anthelmintic drug, 1=Albendazole (1x 400 mg), 2=Mebendazole (1x 500 mg), 3=Other - Only relevant for Disease 'Soil-transmitted helminthiasis'}
#' \item{Shbas/Shfol: S. haematobium, in eggs per 10 ml of urine - BASELINE/FOLLOW-UP}
#' \item{Smbas/Smfol: S. mansoni, in eggs per gram of stool - BASELINE/FOLLOW-UP}
#' \item{Sjbas/Sjfol: S. japonicum, in eggs per gram of stool - BASELINE/FOLLOW-UP}
#' \item{Rbas/Rfol: Ascaris lumbricoides, in eggs per gram of stool - BASELINE/FOLLOW-UP}
#' \item{Tbas/Tfol: Trichuris trichiura, in eggs per gram of stool - BASELINE/FOLLOW-UP}
#' \item{Hbas/Hfol: Hookworms, in eggs per gram of stool - BASELINE/FOLLOW-UP}
#' }
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
paradrug_report <- function(x, params = list(), version = c("1.1", "1.0"), ...){
    stopifnot(inherits(x, "paradrug_rawdata"))
    version <- match.arg(version)
    if(version == "1.0"){
        report_source <- system.file(package = "ParaDrug", "apps", "paradrug-1.0", "input2.Rnw")
    }else if(version == "1.1"){
        report_source <- system.file(package = "ParaDrug", "apps", "paradrug-1.1", "input2.Rnw")
    }
    if(!missing(params)){
        input <- params
    }
    oldwd <- getwd()
    setwd(system.file(package = "ParaDrug", "apps", "paradrug-1.0"))
    on.exit(setwd(oldwd))
    
    PARADRUG = x
    
    out <- knit2pdf(report_source, clean = TRUE)
    out <- list(
        report = basename(report_source),
        output = out,
        params = params
    )
    out
}

