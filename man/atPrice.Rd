% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/atPrice.R
\name{atPrice}
\alias{atPrice}
\title{Korea Agro-Fisheries & Food Trade Corporation, Price of food, agriculture, forestry and fisheries.}
\usage{
atPrice(key, date, verbose = F)
}
\arguments{
\item{key}{character value. API key issued from <data.mafra.go.kr>. no default.}

\item{date}{character value. date which expressed like YYYYMMDD. no default.}

\item{verbose}{logical value. If TRUE, show process bar. Default is set as FALSE.}
}
\description{
atPrice function import price of food, agriculture, forestry and fisheries.
  It provide similar data to episPrice function but have different survey source.
}
\details{
The date arguments should be set as YYYYMMDD type like '20181203'.
  Like episPrice, the survey of food, agriculture, forestry and fisheries is not excuted on weekend.
  So if you insert sunday, saturday or holiday date in date argument, there will be no return.
  The API key issued from 'data.mafra.go.kr' is needed, not from 'data.go.kr'.
}
\examples{
 key <- 'your key issued from data.go.kr'

 data <- atPrice(key, date = '20191210')

}
