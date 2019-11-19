#' data.seoul.go.kr, count bus passenger.
#'
#' seoulBusCount function import the number of passenger by bus stops for a day.
#'
#' @param key character value. API key issued from <www.data.go.kr>. no default.
#' @param day character value. day value of YYYYMMDD type. Default is set -5 days
#' @param verbose logical value. if TRUE, provide process bar. Default value set as false.
#'
#' @examples
#'  key <- 'your key issued from data.seoul.go.kr'
#'
#'  # example.
#'  data <- seoulBusCount(key, day = gsub('-', '', Sys.Date()-4), verbose = T)
#'
#' @export

seoulBusCount <- function(key, day = gsub('-', '', Sys.Date()-4), verbose = F){
  ### 1. parameter checking and processing.
  ## key
  if(is.null(key)){ stop("Invalid key. \n Please issue API key first and insert it to \"key\" param.") }
  if((as.Date(day, '%Y%m%d') <= as.Date('2016-01-01')) | (as.Date(day, '%Y%m%d') > Sys.Date()-4)){
    stop("Invalid day. \n You can insert the day from 2016-01-01 to -5 days of today.")
  }

  ### 2. REST url for get n of pages
  url <- sprintf("http://openapi.seoul.go.kr:8088/%s/%s/%s/%s/%s/%s/",
                 key, 'json', 'CardBusStatisticsServiceNew', 1, 2, day)

  ii <- 0
  repeat{
    ii <- ii + 1
    tmp_xml <- tryCatch({jsonlite::read_json(url)}, error = function(e){NULL})
    if(!is.null(tmp_xml) | ii == 15) break
  }

  if(is.null(tmp_xml)){
    stop('XML parsing fail.Please try again.')
  }else{
    total <- tmp_xml$CardBusStatisticsServiceNew$list_total_count
  }

  all_data <- list()
  urls <- sprintf("http://openapi.seoul.go.kr:8088/%s/%s/%s/%s/%s/%s/",
                  key, 'json', 'CardBusStatisticsServiceNew',
                  seq(1, total, by = 1000), seq(1, total, by = 1000)+999, day)
  if(verbose == T){pb <- txtProgressBar(min = 1, length(urls), style = 3)}
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
      loc <- tmp_xml$CardBusStatisticsServiceNew$row
    }

    all_data[[i]] <- data.frame(
      bus_id = unlist( lapply(loc, function(x) ifelse(is.null(x$"BUS_ROUTE_ID"), NA, x$"BUS_ROUTE_ID")) ),
      bus_no = unlist( lapply(loc, function(x) ifelse(is.null(x$"BUS_ROUTE_NO"), NA, x$"BUS_ROUTE_NO")) ),
      bus_name = unlist( lapply(loc, function(x) ifelse(is.null(x$"BUS_ROUTE_NM"), NA, x$"BUS_ROUTE_NM")) ),
      stop_id = unlist( lapply(loc, function(x) ifelse(is.null(x$"STND_BSST_ID"), NA, x$"STND_BSST_ID")) ),
      stop_ars = unlist( lapply(loc, function(x) ifelse(is.null(x$"BSST_ARS_NO"), NA, x$"BSST_ARS_NO")) ),
      sta_id = unlist( lapply(loc, function(x) ifelse(is.null(x$"BUS_STA_ID"), NA, x$"BUS_STA_ID")) ),
      sta_name = unlist( lapply(loc, function(x) ifelse(is.null(x$"BUS_STA_NM"), NA, x$"BUS_STA_NM")) ),
      in_numb = as.numeric( lapply(loc, function(x) ifelse(is.null(x$"RIDE_PASGR_NUM"), NA, x$"RIDE_PASGR_NUM")) ),
      out_numb = as.numeric( lapply(loc, function(x) ifelse(is.null(x$"ALIGHT_PASGR_NUM"), NA, x$"ALIGHT_PASGR_NUM")) ),
      day = unlist( lapply(loc, function(x) ifelse(is.null(x$"USE_DT"), NA, x$"USE_DT")) ),
      stringsAsFactors = F
    )
    if(verbose == T){setTxtProgressBar(pb, value = i)}
  }

  merged <- dplyr::bind_rows(all_data)
  for(col in colnames(merged)){
    if(class(merged[[col]]) == 'character'){
      Encoding(merged[[col]]) <- 'UTF-8'
    }
  }

  return(dplyr::as.tbl(merged))
}
