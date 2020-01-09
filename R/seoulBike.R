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
    tmp_json <- datagokR:::try_read_json(urls[i])

    if(any(class(tmp_json) %in% 'error')){
      warning('Json parsing fail.\nPlease try again.\nIt maybe caused by api key.')
      return(NULL)
    }else{
      loc <- tmp_json$rentBikeStatus$row
    }

    all_data[[i]] <- data.frame(
      id = datagokR:::find_xmlList(loc, 'stationId'),
      name = datagokR:::find_xmlList(loc, 'stationName'),
      capa = datagokR:::find_xmlList(loc, 'rackTotCnt', 'num'),
      count = datagokR:::find_xmlList(loc, 'parkingBikeTotCnt', 'num'),
      share = datagokR:::find_xmlList(loc, 'shared', 'num'),
      lati = datagokR:::find_xmlList(loc, 'stationLatitude', 'num'),
      longi = datagokR:::find_xmlList(loc, 'stationLongitude', 'num'),
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
