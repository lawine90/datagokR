#' Ministry of Environment, Real-time Particulate Matter data.
#'
#' meDust function import real-time air polution data like so2, co, o3, no2, pm10, pm25.
#'
#' @param key character value. API key issued from <www.data.go.kr>. no default.
#' @param localName character value. SiDo name in Korean character. Default is set as enc2utf8('서울')
#' @param condition character value. Refer details. Default is set as 'HOUR'
#' @param verbose logical value. If TRUE, show process bar. Default is set as FALSE.
#'
#' @details The Contidion arguments should be set as "HOUR" or "DAILY".
#'   If it is set 'HOUR', function import near window data.
#'   If it is set 'DAILY', function import data from now to -24h.
#'
#' @examples
#'  key <- 'your key issued from data.go.kr'
#'  data <- meDust(key)
#'
#' @export

meDust <- function(key, localName = enc2utf8('서울'), condition = 'HOUR', verbose = F){
  ### 1. parameter checking and processing.
  ## key
  if(is.null(key)){ stop("Invalid key. \n Please issue API key first and insert it to \"key\" param.") }
  if(!(condition %in% c('HOUR', 'DAILY'))){ stop("Invalid condition. \n Please insert right condition. It should be \"HOUR\" or \"DAILY\"") }

  locations <- enc2utf8(c("서울", "부산", "대구", "인천", "광주", "대전", "울산", "세종", "경기",
                          "강원", "충북", "충남", "전북", "전남", "경북", "경남", "제주"))
  if(!any(localName %in% locations)){
    stop(sprintf("Invalid localName. \n Please insert right localName. It should be one of %s",
         paste0(locations, collapse = ', ')))
  }else{
    localenc <- c()
    for(i in 1:length(localName)){
      localenc[i] <- utils::URLencode(enc2utf8(localName[i]))
    }
  }

  ### 2. REST url for get n of pages
  ## End Point.
  urls <- sprintf('http://openapi.airkorea.or.kr/openapi/services/rest/%s/%s?sidoName=%s&numOfRows=1000&searchCondition=%s&ServiceKey=%s',
                 'ArpltnInforInqireSvc', 'getCtprvnMesureSidoLIst', localenc, condition, key)

  ### 3. first urls's xml parsing.
  # parsing xml codes with repeat and trycatch.
  if(length(urls) == 1){verbose <- FALSE}
  if(verbose == T){pb <- txtProgressBar(min = 1, length(urls), style = 3)}

  all_data <- list()
  for(i in 1:length(urls)){
    tmp_xml <- datagokR:::try_xmlToList(urls[i])
    total <- as.numeric(tmp_xml$body$totalCount)

    if(length(total) == 0){
      warning(tmp_xml$header$resultMsg)
      next()
    }else if(total == 0){
      if(verbose == T){setTxtProgressBar(pb, value = i)}
      print('There is no data.'); next()
    }

    all_data[[i]] <- data.frame(
      time = datagokR:::find_xmlList(tmp_xml$body$items, 'dataTime'),
      city = datagokR:::find_xmlList(tmp_xml$body$items, 'cityName'),
      so2 = datagokR:::find_xmlList(tmp_xml$body$items, 'so2Value', 'num'),
      co = datagokR:::find_xmlList(tmp_xml$body$items, 'coValue', 'num'),
      o3 = datagokR:::find_xmlList(tmp_xml$body$items, 'o3Value', 'num'),
      no2 = datagokR:::find_xmlList(tmp_xml$body$items, 'no2Value', 'num'),
      pm10 = datagokR:::find_xmlList(tmp_xml$body$items, 'pm10Value', 'num'),
      pm25 = datagokR:::find_xmlList(tmp_xml$body$items, 'pm25Value', 'num'),
      stringsAsFactors = F
    )
    if(verbose == T){setTxtProgressBar(pb, value = i)}
  }

  merged <- dplyr::bind_rows(all_data)
  return(dplyr::as.tbl(merged))
}
