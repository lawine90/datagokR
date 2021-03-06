% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seoulBike.R
\name{seoulBike}
\alias{seoulBike}
\title{data.seoul.go.kr, realtime bike information.}
\usage{
seoulBike(key)
}
\arguments{
\item{key}{character value. API key issued from <www.data.go.kr>. no default.}
}
\description{
seoulBike function import real-time bike of all stations in seoul data. You can get how many bikes are parked right now, and the location.
}
\examples{
 key <- 'your key issued from data.seoul.go.kr'

 # example.
 data <- seoulBike(key)

}
