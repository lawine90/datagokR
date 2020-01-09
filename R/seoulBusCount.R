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

  tmp_json <- datagokR:::try_read_json(url)
  if(any(class(tmp_json) %in% 'error')){
    warning('Json parsing fail.\nPlease try again.\nIt maybe caused by api key.')
    return(NULL)
  }else{
    total <- tmp_json$CardBusStatisticsServiceNew$list_total_count
  }

  all_data <- list()
  urls <- sprintf("http://openapi.seoul.go.kr:8088/%s/%s/%s/%s/%s/%s/",
                  key, 'json', 'CardBusStatisticsServiceNew',
                  seq(1, total, by = 1000), seq(1, total, by = 1000)+999, day)
  if(verbose == T){pb <- txtProgressBar(min = 1, length(urls), style = 3)}
  for(i in 1:length(urls)){
    tmp_json <- datagokR:::try_read_json(urls[i])

    if(any(class(tmp_json) %in% 'error')){
      warning('Json parsing fail.\nPlease try again.\nIt maybe caused by api key.')
      next()
    }else{
      loc <- tmp_json$CardBusStatisticsServiceNew$row
    }

    all_data[[i]] <- data.frame(
      bus_id = datagokR:::find_xmlList(loc, 'BUS_ROUTE_ID'),
      bus_no = datagokR:::find_xmlList(loc, 'BUS_ROUTE_NO'),
      bus_name = datagokR:::find_xmlList(loc, 'BUS_ROUTE_NM'),
      stop_id = datagokR:::find_xmlList(loc, 'STND_BSST_ID'),
      stop_ars = datagokR:::find_xmlList(loc, 'BSST_ARS_NO'),
      sta_id = datagokR:::find_xmlList(loc, 'BUS_STA_ID'),
      sta_name = datagokR:::find_xmlList(loc, 'BUS_STA_NM'),
      in_numb = datagokR:::find_xmlList(loc, 'RIDE_PASGR_NUM', 'num'),
      out_numb = datagokR:::find_xmlList(loc, 'ALIGHT_PASGR_NUM', 'num'),
      day = datagokR:::find_xmlList(loc, 'USE_DT'),
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
