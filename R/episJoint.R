#' Korea Agency of Education, Promotion and Informaion Service in Food, Agriculture, Forestry and Fisheries. Price of food, agriculture, forestry and fisheries from joint market.
#'
#' episJoint function import food, agriculture, forestry and fisheries price in joint market.
#'   This function need API key issued from 'data.mafra.go.kr', not 'data.go.kr'.
#'
#' @param key character value. API key issued from <data.mafra.go.kr>. no default.
#' @param date character value. date which expressed like YYYYMMDD. no default.
#' @param verbose logical value. If TRUE, show process bar. Default is set as FALSE.
#'
#' @details The date arguments should be set as YYYYMMDD type like '20181203'.
#'   The API key issued from 'data.mafra.go.kr' is needed, not from 'data.go.kr'.
#'
#' @examples
#'  key <- 'your key issued from data.mafra.go.kr'
#'  data <- episJoint(key, date = '20191203')
#'
#' @export

episJoint <- function(key, date, verbose = F){
  ### 1. parameter checking and processing.
  ## key
  if(is.null(key)){ stop("Invalid key. \n Please issue API key first and insert it to \"key\" param.") }

  ## date
  if(is.null(date)|nchar(date) != 8){
    stop("Invalid date. \n Please insert date value as YYYYMMDD into \"date\" param.")
  }else if(class(date) != 'character'){
    warning("Invalid date. \n date should be character.")
    date <- as.character(date)
  }

  ### 2. REST url for get n of pages
  ## End Point.
  url <- sprintf('http://211.237.50.150:7080/openapi/%s/xml/%s/1/1?AUCNG_DE=%s',
                  key, 'Grid_20160624000000000349_1', date)

  tmp_xml <- datagokR:::try_read_xml(url)
  total <- as.numeric(xml2::xml_text(xml2::xml_find_all(tmp_xml, '//totalCnt')))

  if(is.na(total)){
    warning(xml2::xml_text(xml2::xml_find_all(tmp_xml, '//message')),
            '\nThe function return NULL')
    return(NULL)
  }else if(total == 0){
    print('There is no data. Please check regist_numb')
    return(NULL)
  }

  if(total == 0){
    print(sprintf('There is no data at %s', as.Date(date, '%Y%m%d')))
    return(NULL)
  }

  all_data <- list()
  urls <- sprintf('http://211.237.50.150:7080/openapi/%s/xml/%s/%s/%s?AUCNG_DE=%s',
                  key, 'Grid_20160624000000000349_1',
                  trimws(format(seq(1, total, by = 1000), scientific = F)),
                  trimws(format(seq(1, total, by = 1000)+999, scientific = F)), date)

  if(length(urls) == 1){verbose <- F}
  if(verbose == T){pb <- txtProgressBar(min = 1, length(urls), style = 3)}

  ### 3. urls's xml parsing.
  all_data <- list()
  for(i in 1:length(urls)){
    tmp_xml <- datagokR:::try_read_xml(urls[i])

    if(is.na(as.numeric(xml2::xml_text(xml2::xml_find_all(tmp_xml, '//totalCnt'))))){
      xml2::xml_text(xml2::xml_find_all(tmp_xml, '//message'))
      next()
    }

    row <- xml2::xml_find_all(tmp_xml, '//row')
    all_data[[i]] <- data.frame(
      date = date,
      jmrkName = datagokR:::find_xml(row, './CPR_NM'),
      jmrkCode = datagokR:::find_xml(row, './CPR_CD'),
      jmrkType = datagokR:::find_xml(row, './CPR_TYPE_NM'),
      orgName = datagokR:::find_xml(row, './SANNM'),
      orgCode = datagokR:::find_xml(row, './SANCO'),

      itmName = datagokR:::find_xml(row, './PRDLST_NM'),
      itmCode = datagokR:::find_xml(row, './PRDLST_CD'),
      spcName = datagokR:::find_xml(row, './SPCIES_NM'),
      spcCode = datagokR:::find_xml(row, './SPCIES_CD'),

      grdName = datagokR:::find_xml(row, './GRAD'),
      grdCode = datagokR:::find_xml(row, './GRAD_CD'),

      unit = datagokR:::find_xml(row, './DELNGBUNDLE_QY', 'num'),
      stdd = datagokR:::find_xml(row, './STNDRD'),

      minPrice = datagokR:::find_xml(row, './MUMM_AMT', 'num'),
      avgPrice = datagokR:::find_xml(row, './AVRG_AMT', 'num'),
      maxPrice = datagokR:::find_xml(row, './MXMM_AMT', 'num'),

      vol = datagokR:::find_xml(row, './DELNG_QY', 'num'),
      cnt = datagokR:::find_xml(row, './AUC_CO', 'num'),

      stringsAsFactors = F
    )
    if(verbose == T){setTxtProgressBar(pb, value = i)}
  }

  merged <- dplyr::bind_rows(all_data)
  return(dplyr::as.tbl(merged))
}
