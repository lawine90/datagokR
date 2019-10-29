#' Ministry of Agriculture Food and Rural Affairs, Livestock's disease information.
#'
#' livestockDisease function import data about livestock's disease information. It provide name of disease, location of farm, the number of livestocks which get disease, and so on.
#'
#' @param key character value. API key issued from <www.data.go.kr>. no default.
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

  period <- gsub('-', '', seq.Date(fromDate, toDate, by = 'day'))
  urls <- paste(base, sprintf('OCCRRNC_DE=%s', period), sep = '?')

  ### 3. first urls's xml parsing.
  # parsing xml codes with repeat and trycatch.
  if(verbose == T){pb <- txtProgressBar(min = 1, length(urls), style = 3)}

  all_data <- list()
  for(i in 1:length(urls)){
    # try access to url.
    ii <- 0
    repeat{
      ii <- ii + 1
      tmp_xml <- tryCatch({xml2::read_xml(urls[i], encoding = 'UTF-8')}, error = function(e){NULL})
      if(!is.null(tmp_xml) | ii == 15) break
    }

    # if access fail, stop it.
    if(is.null(tmp_xml)){
      stop('XML parsing fail.Please try again.')
    }else{
      count <- xml2::xml_text(xml2::xml_find_all(tmp_xml, '//totalCnt'))
      count <- as.numeric(count)
    }

    # if total count is zero, skip
    if(count == 0){
      if(verbose == T){setTxtProgressBar(pb, value = i)}
      next
    }

    all_data[[i]] <- data.frame(
      occr_no = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//ICTSD_OCCRRNC_NO')),
      dizz_name = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//LKNTS_NM')),
      farm_name = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//FARM_NM')),
      farm_code = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//FARM_LOCPLC_LEGALDONG_CODE')),
      farm_addr = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//FARM_LOCPLC')),
      occr_date = as.Date(xml2::xml_text(xml2::xml_find_all(tmp_xml, '//OCCRRNC_DE')), '%Y%m%d'),
      occr_n = as.numeric(xml2::xml_text(xml2::xml_find_all(tmp_xml, '//OCCRRNC_LVSTCKCNT'))),
      lvst_code = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//LVSTCKSPC_CODE')),
      lvst_type = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//LVSTCKSPC_NM')),
      diag_code = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//DGNSS_ENGN_CODE')),
      diag_name = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//DGNSS_ENGN_NM')),
      clos_date = as.Date(xml2::xml_text(xml2::xml_find_all(tmp_xml, '//CESSATION_DE')), '%Y%m%d'),
      stringsAsFactors = F
    )
    if(verbose == T){setTxtProgressBar(pb, value = i)}
  }

  merged <- bind_rows(all_data)
  for(col in colnames(merged)){
    if(class(merged[[col]]) == 'character'){
      Encoding(merged[[col]]) <- 'UTF-8'
      merged[[col]][merged[[col]] == ''] <- NA
    }
  }

  return(dplyr::as.tbl(merged))
}
