#' Korea Agency of Education, Promotion and Informaion Service in Food, Agriculture, Forestry and Fisheries. Price of food, agriculture, forestry and fisheries.
#'
#' episPrice function import price of food, agriculture, forestry and fisheries.
#'   It provide similar data to atPrice function but have different survey source.
#'
#' @param key character value. API key issued from <www.data.go.kr>. no default.
#' @param date character value. date which expressed like YYYYMMDD. no default.
#' @param items numeric value. item code in epis_items data.
#' @param verbose logical value. If TRUE, show process bar. Default is set as FALSE.
#'
#' @details The date arguments should be set as YYYYMMDD type like '20181203'.
#'   The survey of food, agriculture, forestry and fisheries is not excuted on weekend.
#'   So if you insert sunday, saturday or holiday date in date argument, there will be no return.
#'
#' @examples
#'  key <- 'your key issued from data.go.kr'
#'  epis_items <- datagokR::epis_items
#'
#'  data <- episPrice(key, date = '20191203', items = head(unique(epis_items$itemCode)))
#'
#' @export

episPrice <- function(key, date, items, verbose = F){
  ### 1. parameter checking and processing.
  ## key
  if(is.null(key)){ stop("Invalid key. \n Please issue API key first and insert it to \"key\" param.") }

  ## date
  if(is.null(date)|nchar(date) != 8){
    stop("Invalid date. \n Please insert date value as YYYYMMDD into \"date\" param.")
  }else if(class(date) != 'character'){
    warning("Invalid date. \n date should be character.")
    date <- as.character(date)
  }else if(as.POSIXlt(as.Date(date, '%Y%m%d'))$wday %in% c(0,6)){
    stop("Invalid date. \n You should insert week date, not weekend.")
  }

  ## items
  if(is.null(items)){ stop("Invalid items. \n Please item code into \"item\" param. \n You can find item code from epis_items data in library.") }

  ### 2. REST url for get n of pages
  ## End Point.
  urls <- sprintf('http://apis.data.go.kr/B552895/openapi/service/%s/%s?serviceKey=%s&pageNo=1&numOfRows=1000&examinDe=%s&examinCd=7&prdlstCd=%s',
                  'OrgPriceExaminService', 'getExaminPriceList', key, date, items)
  if(length(urls) == 1){verbose <- F}
  if(verbose == T){pb <- txtProgressBar(min = 1, length(urls), style = 3)}

  ### 3. urls's xml parsing.
  all_data <- list()
  for(i in 1:length(urls)){
    ii <- 0
    repeat{
      ii <- ii + 1
      tmp_xml <- tryCatch({XML::xmlToList(urls[i])}, error = function(e){NULL})
      if(!is.null(tmp_xml) | ii == 15) break
    }

    if(is.null(tmp_xml)){
      stop('XML parsing fail.Please try again.')
    }
    if(!is.null(tmp_xml$cmmMsgHeader)){
      return(tmp_xml$cmmMsgHeader$returnAuthMsg)
    }

    all_data[[i]] <- data.frame(
      locCode = unlist( lapply(tmp_xml$body$items, function(x) ifelse(is.null(x$"areaCode"), '', x$"areaCode")) ),
      locName = unlist( lapply(tmp_xml$body$items, function(x) ifelse(is.null(x$"areaNm"), '', x$"areaNm")) ),
      mrkCode = unlist( lapply(tmp_xml$body$items, function(x) ifelse(is.null(x$"stdMrktCode"), '', x$"stdMrktCode")) ),
      mrkName = unlist( lapply(tmp_xml$body$items, function(x) ifelse(is.null(x$"stdMrktNm"), '', x$"stdMrktNm")) ),
      itmCode = unlist( lapply(tmp_xml$body$items, function(x) ifelse(is.null(x$"stdPrdlstCode"), '', x$"stdPrdlstCode")) ),
      itmName = unlist( lapply(tmp_xml$body$items, function(x) ifelse(is.null(x$"stdPrdlstNm"), '', x$"stdPrdlstNm")) ),
      spcCode = unlist( lapply(tmp_xml$body$items, function(x) ifelse(is.null(x$"stdSpciesCode"), '', x$"stdSpciesCode")) ),
      spcName = unlist( lapply(tmp_xml$body$items, function(x) ifelse(is.null(x$"stdSpciesNm"), '', x$"stdSpciesNm")) ),
      yesPrice = as.integer( lapply(tmp_xml$body$items, function(x) ifelse(is.null(x$"bfrtPric"), '', x$"bfrtPric")) ),
      todPrice = as.integer( lapply(tmp_xml$body$items, function(x) ifelse(is.null(x$"todayPric"), '', x$"todayPric")) ),
      unit = unlist( lapply(tmp_xml$body$items, function(x) ifelse(is.null(x$"examinUnitNm"), '', x$"examinUnitNm")) ),

      exmGradeCode = unlist( lapply(tmp_xml$body$items, function(x) ifelse(is.null(x$"examinGradCode"), '', x$"examinGradCode")) ),
      exmGradeName = unlist( lapply(tmp_xml$body$items, function(x) ifelse(is.null(x$"examinGradNm"), '', x$"examinGradNm")) ),
      stringsAsFactors = F
    )
    if(verbose == T){setTxtProgressBar(pb, value = i)}
  }

  merged <- dplyr::bind_rows(all_data)
  return(dplyr::as.tbl(merged))
}
