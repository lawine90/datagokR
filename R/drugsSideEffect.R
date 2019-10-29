#' Ministry of Food and Drug Safety, Drugs side effect information.
#'
#' drugsSideEffect function import the side effects of drugs.
#'
#' @param key character value. API key issued from <www.data.go.kr>. no default.
#'
#' @examples
#'  key <- 'your key issued from data.go.kr'
#'
#'  # example.
#'  data <- drugsSideEffect(key)
#'
#' @export

drugsSideEffect <- function(key){
  ### 1. parameter checking and processing.
  ## key
  if(is.null(key)){ stop("Invalid key. \n Please issue API key first and insert it to \"key\" param.") }

  ### 2. REST url
  ## End Point.
  url <- "http://apis.data.go.kr/1470000/MdcinSdefctInfoService/getMdcinSdefctInfoList?"

  ## generate list of urls(fxxking so many limitations...).
  # 1st, (url + key)
  urls <- paste(url, "serviceKey=", key, "&numOfRows=100", sep = "")

  ### 3. urls's xml parsing.
  # parsing xml codes with repeat and trycatch.
  ii <- 0
  repeat{
    ii <- ii + 1
    tmp_xml <- tryCatch({XML::xmlToList(urls)}, error = function(e){NULL})
    if(!is.null(tmp_xml) | ii == 15) break
  }

  ## error checking part.
  # if tmp_xml is null, stop function.
  if(is.null(tmp_xml)){
    stop('XML parsing fail.Please try again.')
  }

  if(!is.null(tmp_xml$cmmMsgHeader)){
    stop(paste(tmp_xml$cmmMsgHeader$returnAuthMsg, ".\nError Code: ",
               tmp_xml$cmmMsgHeader$returnReasonCode, sep = ""))
  }

  all_data <- data.frame(
    name_kor = unlist( lapply(tmp_xml$body$items, function(x) ifelse(is.null(x$"COL_001"), '', x$"COL_001")) ),
    name_eng = unlist( lapply(tmp_xml$body$items, function(x) ifelse(is.null(x$"COL_002"), '', x$"COL_002")) ),
    type = unlist( lapply(tmp_xml$body$items, function(x) ifelse(is.null(x$"COL_003"), '', x$"COL_003")) ),
    period = unlist( lapply(tmp_xml$body$items, function(x) ifelse(is.null(x$"COL_004"), '', x$"COL_004")) ),
    effect_kor = unlist( lapply(tmp_xml$body$items, function(x) ifelse(is.null(x$"COL_005"), '', x$"COL_005")) ),
    effect_eng = unlist( lapply(tmp_xml$body$items, function(x) ifelse(is.null(x$"COL_006"), '', x$"COL_006")) ),
    etc = unlist( lapply(tmp_xml$body$items, function(x) ifelse(is.null(x$"COL_007"), '', x$"COL_007")) ),
    stringsAsFactors = F
  )

  for(col in colnames(all_data)){
    Encoding(all_data[[col]]) <- 'UTF-8'
  }

  return(dplyr::as.tbl(all_data))
}
