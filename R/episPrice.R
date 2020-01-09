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
    tmp_xml <- datagokR:::try_read_xml(urls[i])
    total <- as.numeric(xml2::xml_text(xml2::xml_find_all(tmp_xml, '//totalCount')))

    if(is.na(total)){
      warning(xml2::xml_text(xml2::xml_find_all(tmp_xml, '//returnAuthMsg')))
    }else if(total == 0){
      print('There is no data.')
      next()
    }

    item <- xml2::xml_find_all(tmp_xml, '//item')
    all_data[[i]] <- data.frame(
      locCode = datagokR:::find_xml(item, './areaCode'),
      locName = datagokR:::find_xml(item, "./areaNm"),
      mrkCode = datagokR:::find_xml(item, "./stdMrktCode"),
      mrkName = datagokR:::find_xml(item, "./stdMrktNm"),
      itmCode = datagokR:::find_xml(item, "./stdPrdlstCode"),
      itmName = datagokR:::find_xml(item, "./stdPrdlstNm"),
      spcCode = datagokR:::find_xml(item, "./stdSpciesCode"),
      spcName = datagokR:::find_xml(item, "./stdSpciesNm"),

      yesPrice = datagokR:::find_xml(item, "./bfrtPric", 'num'),
      todPrice = datagokR:::find_xml(item, "./todayPric", 'num'),
      unit = datagokR:::find_xml(item, "./examinUnitNm"),

      exmGradeCode = datagokR:::find_xml(item, "./examinGradCode", 'num'),
      exmGradeName = datagokR:::find_xml(item, "./examinGradNm"),
      stringsAsFactors = F
    )
    if(verbose == T){setTxtProgressBar(pb, value = i)}
  }

  merged <- dplyr::bind_rows(all_data)
  return(dplyr::as.tbl(merged))
}
