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
  tmp_xml <- datagokR:::try_read_xml(url)
  msg <- datagokR:::find_xml(tmp_xml, '//returnAuthMsg')

  if(!is.na(msg)){
    warning(msg, '\nThe function return NULL')
    return(NULL)
  }

  data <- data.frame(
    id = datagokR:::find_xml(tmp_xml, '//billId'),
    no = datagokR:::find_xml(tmp_xml, '//billNo'),
    name = datagokR:::find_xml(tmp_xml, '//billName'),

    proposer = datagokR:::find_xml(tmp_xml, '//proposerKind'),
    propose_date = datagokR:::find_xml(tmp_xml, '//proposeDt'),
    committee = datagokR:::find_xml(tmp_xml, '//committeeName'),

    vote_date = datagokR:::find_xml(tmp_xml, '//procDt'),
    vote_result = datagokR:::find_xml(tmp_xml, '//generalResult'),
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
