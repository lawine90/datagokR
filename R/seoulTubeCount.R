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

  tmp_json <- datagokR:::try_read_json(url)
  if(any(class(tmp_json) %in% 'error')){
    warning('Json parsing fail.\nPlease try again.\nIt maybe caused by api key.')
    return(NULL)
  }else{
    total <- tmp_json$CardSubwayStatsNew$list_total_count
    loc <- tmp_json$CardSubwayStatsNew$row
  }

  data <- data.frame(
    line = datagokR:::find_xmlList(loc, 'LINE_NUM'),
    station = datagokR:::find_xmlList(loc, 'SUB_STA_NM'),
    in_numb = datagokR:::find_xmlList(loc, 'RIDE_PASGR_NUM'),
    out_numb = datagokR:::find_xmlList(loc, 'ALIGHT_PASGR_NUM'),
    day = datagokR:::find_xmlList(loc, 'USE_DT'),
    stringsAsFactors = F
  )

  for(col in colnames(data)){
    if(class(data[[col]]) == 'character'){
      Encoding(data[[col]]) <- 'UTF-8'
    }
  }

  return(dplyr::as.tbl(data))
}
