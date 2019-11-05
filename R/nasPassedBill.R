#' National Assembly Secretariat, Recent passed bill list.
#'
#' nasPassedBill function import passed bill list of recent(about 6 months).
#'
#' @param key character value. API key issued from <www.data.go.kr>. no default.
#'
#' @examples
#'  key <- 'your key issued from data.go.kr'
#'
#'  # example.
#'  data <- nasPassedBill(key)
#'
#' @export

nasPassedBill <- function(key){
  ### 1. parameter checking and processing.
  ## key
  if(is.null(key)){ stop("Invalid key. \n Please issue API key first and insert it to 'key' param.") }

  ### 2. REST url for get n of pages
  ## generate list of urls(fxxking so many limitations...).
  # 1st, (url + key)
  url <- sprintf('http://apis.data.go.kr/9710000/%s/%s?ServiceKey=%s&numOfRows=%s',
                 'BillInfoService2', 'getRecentPasageList', key, 500)

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

    proposer = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//proposerKind')),
    propose_date = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//proposeDt')),
    committee = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//committeeName')),

    vote_date = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//procDt')),
    vote_result = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//generalResult')),
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
