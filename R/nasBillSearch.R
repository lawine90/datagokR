#' National Assembly Secretariat, Recent proposed bill list searching by congressman name.
#'
#' nasBillSearch function import proposed bill list searching by name of congressman.
#'
#' @param key character value. API key issued from <www.data.go.kr>. no default.
#' @param name character value. the name of congressman.
#'
#' @examples
#'  key <- 'your key issued from data.go.kr'
#'
#'  # example.
#'  data <- nasBillSearch(key, name = enc2utf8('경대수'))
#'
#' @export

nasBillSearch <- function(key, name = NULL){
  ### 1. parameter checking and processing.
  ## key
  if(is.null(key)){ stop("Invalid key. \n Please issue API key first and insert it to 'key' param.") }

  ### 2. REST url for get n of pages
  ## generate list of urls(fxxking so many limitations...).
  # 1st, (url + key)
  url <- sprintf('http://apis.data.go.kr/9710000/%s/%s?ServiceKey=%s&numOfRows=%s',
                 'BillInfoService2', 'getMotionLawList', key, 1000)

  # 2nd, name
  if(!is.null(name)){
    url <- sprintf('%s&mem_name=%s', url, utils::URLencode(enc2utf8(name)))
  }


  ### 3. first urls's xml parsing.
  # parsing xml codes with repeat and trycatch.
  ii <- 0
  repeat{
    ii <- ii + 1
    tmp_xml <- tryCatch({xml2::read_xml(url, encoding = 'UTF-8')}, error = function(e){NULL})
    if(!is.null(tmp_xml) | ii == 15) break
  }

  # if access fail, stop it.
  if(is.null(tmp_xml)){
    stop('XML parsing fail.Please try again.')
  }

  data <- data.frame(
    id = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//billId')),
    no = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//billNo')),
    name = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//billName')),

    proposer = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//proposer')),
    propose_type = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//coactKind')),
    propose_date = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//proposeDt')),

    vote_date = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//procDt')),
    vote_result = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//generalResult')),
    condition = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//procStageCd')),
    stringsAsFactors = F
  )

  for(col in colnames(data)){
    if(class(data[[col]]) == 'character'){
      Encoding(data[[col]]) <- 'UTF-8'
      data[[col]][data[[col]] == ''] <- NA
    }
  }

  return(dplyr::as.tbl(data))
}