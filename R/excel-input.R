

#' @title Read xlsx data with paradrug information
#' @description Read xlsx data with paradrug information
#' @param x character string with the path to the file
#' @param sheet either the character string with the name of the sheet or the index number of the sheet. Defaults to the first sheet.
#' @export
#' @return a list with elements 
#' \itemize{
#' \item{data: data.frame with at least columns "Site", "SubjectID", "SchoolID", "Age", "Sex2", "BL_KK2_AL_EPG", "FU_KK2_AL_EPG", "BL_KK2_TT_EPG", "FU_KK2_TT_EPG", "BL_KK2_HW_EPG", "FU_KK2_HW_EPG"}
#' \item{n: the number of rows in the dataset} 
#' \item{fields: a character vector of column names in \code{data}}
#' }
#' @examples 
#' path <- system.file(package = "ParaDrug", "extdata", "data", "mydata.xlsx")
#' x    <- read_paradrug_xls(path)
read_paradrug_xls <- function(x, sheet = 1){
    stopifnot(file.exists(x))
    if(is.character(sheet)){
        data <- readxl::read_excel(x, sheet = sheet)
    }else{
        data <- readxl::read_excel(x, sheet = sheet)
    }
    data <- unclass(data)
    class(data) <- "data.frame"
    n <- nrow(data)
    out <- list(data = data, n = n, fields = as.character(unique(na.exclude(colnames(data)))))
    class(out) <- "paradrug_rawdata"
    out
}