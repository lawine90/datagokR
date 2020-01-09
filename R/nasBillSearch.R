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
  tmp_xml <- datagokR:::try_read_xml(url)

  # if access fail, stop it.
  if(length(xml2::xml_text(xml2::xml_find_all(tmp_xml, '//errMsg'))) != 0){
    warning(xml2::xml_text(xml2::xml_find_all(tmp_xml, '//returnAuthMsg')))
    return(NULL)
  }

  item <- xml2::xml_find_all(tmp_xml, '//item')
  data <- data.frame(
    id = datagokR:::find_xml(item, './billId'),
    no = datagokR:::find_xml(item, './billNo'),
    name = datagokR:::find_xml(item, './billName'),

    proposer = datagokR:::find_xml(item, './proposer'),
    propose_type = datagokR:::find_xml(item, './coactKind'),
    propose_date = datagokR:::find_xml(item, './proposeDt'),

    vote_date = datagokR:::find_xml(item, './procDt'),
    vote_result = datagokR:::find_xml(item, './generalResult'),
    condition = datagokR:::find_xml(item, './procStageCd'),
    stringsAsFactors = F
  )

  return(dplyr::as.tbl(data))
}
