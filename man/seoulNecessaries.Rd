% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seoulNecessaries.R
\name{seoulNecessaries}
\alias{seoulNecessaries}
\title{data.seoul.go.kr, daily necessaries price.}
\usage{
seoulNecessaries(key, recent = T, verbose = F)
}
\arguments{
\item{key}{character value. API key issued from <www.data.go.kr>. no default.}

\item{recent}{logical value. if TRUE, download only recent 2,000 rows. Default value set as True.}

\item{verbose}{logical value. if TRUE, provide process bar. Default value set as false.}
}
\description{
seoulNecessaries function import daily necessaries's price data from traditional market and large retailer. The data updated twice a week(tuesday and friday)
}
\examples{
 key <- 'your key issued from data.seoul.go.kr'

 # example.
 data <- seoulNecessaries(key, recent = T, verbose = T)

}
