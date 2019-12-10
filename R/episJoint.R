#' Korea Agency of Education, Promotion and Informaion Service in Food, Agriculture, Forestry and Fisheries. Price of food, agriculture, forestry and fisheries from joint market.
#'
#' episJoint function import food, agriculture, forestry and fisheries price in joint market.
#'   This function need API key issued from 'data.mafra.go.kr', not 'data.go.kr'.
#'
#' @param key character value. API key issued from <www.data.go.kr>. no default.
#' @param date character value. date which expressed like YYYYMMDD. no default.
#' @param verbose logical value. If TRUE, show process bar. Default is set as FALSE.
#'
#' @details The date arguments should be set as YYYYMMDD type like '20181203'.
#'   The API key issued from 'data.mafra.go.kr' is needed, not 'data.go.kr'.
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

  ii <- 0
  repeat{
    ii <- ii + 1
    tmp_xml <- tryCatch({XML::xmlToList(url)}, error = function(e){NULL})
    if(!is.null(tmp_xml) | ii == 15) break
  }

  if(is.null(tmp_xml)){
    stop('XML parsing fail.Please try again.')
  }
  if(!is.null(tmp_xml)){
    total <- as.numeric(tmp_xml$totalCnt)
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
    ii <- 0
    repeat{
      ii <- ii + 1
      tmp_xml <- tryCatch({xml2::read_xml(urls[i], encoding = 'UTF-8')}, error = function(e){NULL})
      if(!is.null(tmp_xml) | ii == 15) break
    }

    if(is.null(tmp_xml)){
      stop('XML parsing fail.Please try again.')
    }
    if(!is.null(tmp_xml$cmmMsgHeader)){
      return(tmp_xml$cmmMsgHeader$returnAuthMsg)
    }

    all_data[[i]] <- data.frame(
      date = date,
      jmrkName = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//CPR_NM')),
      jmrkCode = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//CPR_CD')),
      jmrkType = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//CPR_TYPE_NM')),
      orgName = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//SANNM')),
      orgCode = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//SANCO')),

      itmName = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//PRDLST_NM')),
      itmCode = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//PRDLST_CD')),
      spcName = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//SPCIES_NM')),
      spcCode = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//SPCIES_CD')),

      grdName = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//GRAD')),
      grdCode = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//GRAD_CD')),

      unit = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//DELNGBUNDLE_QY')),
      stdd = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//STNDRD')),

      minPrice = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//MUMM_AMT')),
      avgPrice = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//AVRG_AMT')),
      maxPrice = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//MXMM_AMT')),

      vol = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//DELNG_QY')),
      cnt = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//AUC_CO')),

      stringsAsFactors = F
    )
    if(verbose == T){setTxtProgressBar(pb, value = i)}
  }

  merged <- dplyr::bind_rows(all_data)
  return(dplyr::as.tbl(merged))
}
