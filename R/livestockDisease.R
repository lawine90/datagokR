#' Ministry of Agriculture Food and Rural Affairs, Livestock's disease information.
#'
#' livestockDisease function import data about livestock's disease information.
#'   It provide name of disease, location of farm, the number of livestocks which get disease, and so on.
#'   This function need API key issued from 'data.mafra.go.kr', not 'data.go.kr'.
#'
#' @param key character value. API key issued from <data.mafra.go.kr>. no default.
#' @param fromDate date value. 8-digits date that start of searching period.
#' @param toDate date value. 8-digits date that end of searching period.
#' @param verbose logical value. if TRUE, provide process bar. Default value set as false.
#'
#' @examples
#'  key <- 'your key issued from data.go.kr'
#'
#'  # example.
#'  data <- livestockDisease(key, as.Date('2019-10-01'), as.Date('2019-10-20'), verbose = T)
#'
#' @export

livestockDisease <- function(key, fromDate = NULL, toDate = NULL, verbose = F){
  ### 1. parameter checking and processing.
  ## key
  if(is.null(key)){ stop("Invalid key. \n Please issue API key first and insert it to 'key' param.") }

  ## fromDate, toDate
  if(is.null(fromDate)|is.null(toDate)){ stop("Invalid date. \n Please insert right 'fromDate', 'toDate' params(ex: 2019-10-29).") }
  if(fromDate > toDate){ stop("Invalid date. \n toDate should bigger than fromDate.") }

  ### 2. REST url for get n of pages
  ## generate list of urls(fxxking so many limitations...).
  # 1st, (url + key)
  # 2nd, (url + key) + fromDate, toDate
  base <- sprintf('http://211.237.50.150:7080/openapi/%s/xml/Grid_20151204000000000316_1/1/200', key)
  urls <- paste(base, sprintf('OCCRRNC_DE=%s', gsub('-', '', seq.Date(fromDate, toDate, by = 'day'))), sep = '?')

  ### 3. first urls's xml parsing.
  # parsing xml codes with repeat and trycatch.
  if(length(urls) == 1){verbose <- FALSE}
  if(verbose == T){pb <- txtProgressBar(min = 1, length(urls), style = 3)}

  all_data <- list()
  for(i in 1:length(urls)){
    # try access to url.
    tmp_xml <- datagokR:::try_read_xml(urls[i])
    total <- as.numeric(xml2::xml_text(xml2::xml_find_all(tmp_xml, '//totalCnt')))

    # if access fail, stop it.
    if(is.na(total)){
      warning(xml2::xml_text(xml2::xml_find_all(tmp_xml, '//message')))
      if(verbose == T){setTxtProgressBar(pb, value = i)}
      next()
    }else if(total == 0){
      if(verbose == T){setTxtProgressBar(pb, value = i)}
      print('There is no data.'); next()
    }

    row <- xml2::xml_find_all(tmp_xml, '//row')
    all_data[[i]] <- data.frame(
      occr_no = datagokR:::find_xml(row, './ICTSD_OCCRRNC_NO'),
      dizz_name = datagokR:::find_xml(row, './LKNTS_NM'),
      farm_name = datagokR:::find_xml(row, './FARM_NM'),
      farm_code = datagokR:::find_xml(row, './FARM_LOCPLC_LEGALDONG_CODE'),
      farm_addr = datagokR:::find_xml(row, './FARM_LOCPLC'),
      occr_date = as.Date(datagokR:::find_xml(row, './OCCRRNC_DE'), '%Y%m%d'),
      occr_n = as.numeric(datagokR:::find_xml(row, './OCCRRNC_LVSTCKCNT')),
      lvst_code = datagokR:::find_xml(row, './LVSTCKSPC_CODE'),
      lvst_type = datagokR:::find_xml(row, './LVSTCKSPC_NM'),
      diag_code = datagokR:::find_xml(row, './DGNSS_ENGN_CODE'),
      diag_name = datagokR:::find_xml(row, './DGNSS_ENGN_NM'),
      clos_date = as.Date(datagokR:::find_xml(row, './CESSATION_DE'), '%Y%m%d'),
      stringsAsFactors = F
    )
    if(verbose == T){setTxtProgressBar(pb, value = i)}
  }

  merged <- dplyr::bind_rows(all_data)
  for(col in colnames(merged)){
    if(class(merged[[col]]) == 'character'){
      Encoding(merged[[col]]) <- 'UTF-8'
      merged[[col]][merged[[col]] == ''] <- NA
    }
  }

  return(dplyr::as.tbl(merged))
}
