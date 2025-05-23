% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/schistosomiasis.R
\name{plot_paradrug_schistosomiasis_eggcount}
\alias{plot_paradrug_schistosomiasis_eggcount}
\title{Plot of eggcount of Schistosomiasis}
\usage{
plot_paradrug_schistosomiasis_eggcount(
  object,
  Shbas = "BL_KK2_AL_EPG",
  Shfol = "FU_KK2_AL_EPG",
  Smbas = "BL_KK2_TT_EPG",
  Smfol = "FU_KK2_TT_EPG",
  Sjbas = "BL_KK2_HW_EPG",
  Sjfol = "FU_KK2_HW_EPG",
  ...
)
}
\arguments{
\item{object}{an object of class paradrug_rawdata as returned by \code{\link{read_paradrug_xls}}}

\item{Shbas}{column in name in object$data for Shbas/Shfol: S. haematobium, in eggs per 10 ml of urine - BASELINE/FOLLOW-UP}

\item{Shfol}{column in name in object$data for Shbas/Shfol: S. haematobium, in eggs per 10 ml of urine - BASELINE/FOLLOW-UP}

\item{Smbas}{column in name in object$data for Smbas/Smfol: S. mansoni, in eggs per gram of stool - BASELINE/FOLLOW-UP}

\item{Smfol}{column in name in object$data for Smbas/Smfol: S. mansoni, in eggs per gram of stool - BASELINE/FOLLOW-UP}

\item{Sjbas}{column in name in object$data for Shbas/Shfol: Sjbas/Sjfol: S. japonicum, in eggs per gram of stool - BASELINE/FOLLOW-UP}

\item{Sjfol}{column in name in object$data for Shbas/Shfol: Sjbas/Sjfol: S. japonicum, in eggs per gram of stool - BASELINE/FOLLOW-UP}

\item{...}{not used yet}
}
\value{
a plot with the egg counts in the trial as returned by \code{\link[graphics]{hist}}
}
\description{
Plot of eggcount of Schistosomiasis
}
\examples{
path <- system.file(package = "ParaDrug", "extdata", "data", "mydata.xlsx")
x <- read_paradrug_xls(path)
p <- plot_paradrug_schistosomiasis_eggcount(x)
}
