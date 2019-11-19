#' data.seoul.go.kr, realtime bike information.
#'
#' seoulBike function import real-time bike of all stations in seoul data. You can get how many bikes are parked right now, and the location.
#'
#' @param key character value. API key issued from <www.data.go.kr>. no default.
#'
#' @examples
#'  key <- 'your key issued from data.seoul.go.kr'
#'
#'  # example.
#'  data <- seoulBike(key)
#'
#' @export

seoulBike <- function(key){
  ### 1. parameter checking and processing.
  ## key
  if(is.null(key)){ stop("Invalid key. \n Please issue API key first and insert it to \"key\" param.") }

  ### 2. REST url for get n of pages
  urls <- c(sprintf("http://openapi.seoul.go.kr:8088/%s/json/bikeList/%s/%s/", key, 1, 1000),
           sprintf("http://openapi.seoul.go.kr:8088/%s/json/bikeList/%s/%s/", key, 1001, 2000))

  all_data <- list()
  for(i in 1:length(urls)){
    ii <- 0
    repeat{
      ii <- ii + 1
      tmp_xml <- tryCatch({jsonlite::read_json(urls[i])}, error = function(e){NULL})
      if(!is.null(tmp_xml) | ii == 15) break
    }

    if(is.null(tmp_xml)){
      stop('XML parsing fail.Please try again.')
    }else{
      loc <- tmp_xml$rentBikeStatus$row
    }

    all_data[[i]] <- data.frame(
      id = unlist( lapply(loc, function(x) ifelse(is.null(x$"stationId"), NA, x$"stationId")) ),
      name = unlist( lapply(loc, function(x) ifelse(is.null(x$"stationName"), NA, x$"stationName")) ),
      capa = as.numeric( lapply(loc, function(x) ifelse(is.null(x$"rackTotCnt"), NA, x$"rackTotCnt")) ),
      count = as.numeric( lapply(loc, function(x) ifelse(is.null(x$"parkingBikeTotCnt"), NA, x$"parkingBikeTotCnt")) ),
      share = as.numeric( lapply(loc, function(x) ifelse(is.null(x$"shared"), NA, x$"shared")) ),
      lati = as.numeric( lapply(loc, function(x) ifelse(is.null(x$"stationLatitude"), NA, x$"stationLatitude")) ),
      longi = as.numeric( lapply(loc, function(x) ifelse(is.null(x$"stationLongitude"), NA, x$"stationLongitude")) ),
      stringsAsFactors = F
    )
  }

  merged <- dplyr::bind_rows(all_data)
  for(col in colnames(merged)){
    if(class(merged[[col]]) == 'character'){
      Encoding(merged[[col]]) <- 'UTF-8'
    }
  }

  return(dplyr::as.tbl(merged))
}
