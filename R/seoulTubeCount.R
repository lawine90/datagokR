#' data.seoul.go.kr, count bus passenger.
#'
#' seoulTubeCount function import the number of passenger by subway stations for a day.
#'
#' @param key character value. API key issued from <www.data.go.kr>. no default.
#' @param day character value. day value of YYYYMMDD type. Default is set -5 days
#'
#' @examples
#'  key <- 'your key issued from data.seoul.go.kr'
#'
#'  # example.
#'  data <- seoulTubeCount(key, day = gsub('-', '', Sys.Date()-4))
#'
#' @export

seoulTubeCount <- function(key, day = gsub('-', '', Sys.Date()-4)){
  ### 1. parameter checking and processing.
  ## key
  if(is.null(key)){ stop("Invalid key. \n Please issue API key first and insert it to \"key\" param.") }
  if((as.Date(day, '%Y%m%d') <= as.Date('2016-01-01')) | (as.Date(day, '%Y%m%d') > Sys.Date()-4)){
    stop("Invalid day. \n You can insert the day from 2016-01-01 to -5 days of today.")
  }

  ### 2. REST url for get n of pages
  url <- sprintf("http://openapi.seoul.go.kr:8088/%s/%s/%s/%s/%s/%s/",
                 key, 'json', 'CardSubwayStatsNew', 1, 1000, day)

  ii <- 0
  repeat{
    ii <- ii + 1
    tmp_xml <- tryCatch({jsonlite::read_json(url)}, error = function(e){NULL})
    if(!is.null(tmp_xml) | ii == 15) break
  }

  if(is.null(tmp_xml)){
    stop('XML parsing fail.Please try again.')
  }else{
    total <- tmp_xml$CardSubwayStatsNew$list_total_count
    loc <- tmp_xml$CardSubwayStatsNew$row
  }

  data <- data.frame(
    line = unlist( lapply(loc, function(x) ifelse(is.null(x$"LINE_NUM"), NA, x$"LINE_NUM")) ),
    station = unlist( lapply(loc, function(x) ifelse(is.null(x$"SUB_STA_NM"), NA, x$"SUB_STA_NM")) ),
    in_numb = as.numeric( lapply(loc, function(x) ifelse(is.null(x$"RIDE_PASGR_NUM"), NA, x$"RIDE_PASGR_NUM")) ),
    out_numb = as.numeric( lapply(loc, function(x) ifelse(is.null(x$"ALIGHT_PASGR_NUM"), NA, x$"ALIGHT_PASGR_NUM")) ),
    day = unlist( lapply(loc, function(x) ifelse(is.null(x$"USE_DT"), NA, x$"USE_DT")) ),
    stringsAsFactors = F
  )

  for(col in colnames(data)){
    if(class(data[[col]]) == 'character'){
      Encoding(data[[col]]) <- 'UTF-8'
    }
  }

  return(dplyr::as.tbl(data))
}
