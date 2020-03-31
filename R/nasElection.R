#' National Assembly Secretariat, election information.
#'
#' nasElection function import detail information of election like date, id, type and so on.
#'
#' @param key character value. API key issued from <www.data.go.kr>. no default.
#'
#' @examples
#'  key <- 'your key issued from data.go.kr'
#'
#'  # example.
#'  data <- nasElection(key)
#'
#' @export

nasElection <- function(key){
  ### 1. parameter checking and processing.
  ## key
  if(is.null(key)){ stop("Invalid key. \n Please issue API key first and insert it to 'key' param.") }

  ### 2. REST url for get n of pages
  ## generate list of urls(fxxking so many limitations...).
  # 1st, (url + key)
  url <- sprintf('http://apis.data.go.kr/9760000/%s/%s?ServiceKey=%s&numOfRows=%s',
                 'CommonCodeService', 'getCommonSgCodeList', key, 100)

  ### 3. first urls's xml parsing.
  # parsing xml codes with repeat and trycatch.
  tmp_xml <- datagokR:::try_read_xml(url)

  # if access fail, stop it.
  if(length(xml2::xml_text(xml2::xml_find_all(tmp_xml, '//errMsg'))) != 0){
    warning(xml2::xml_text(xml2::xml_find_all(tmp_xml, '//returnAuthMsg')))
    return(NULL)
  }

  item <- xml2::xml_find_all(tmp_xml, '//item')
  data <- xml_to_dataframe(item)
  # data <- data.frame(
  #   sgId = datagokR:::find_xml(item, './sgId'),
  #   sgName = datagokR:::find_xml(item, './sgName'),
  #   sgTypecode = datagokR:::find_xml(item, './sgTypecode'),
  #   sgVotedate = datagokR:::find_xml(item, './sgVotedate'),
  #   stringsAsFactors = F
  # )
  for(col in colnames(data)){
    if(class(data[[col]]) == 'character'){
      Encoding(data[[col]]) <- 'UTF-8'
      data[[col]][data[[col]] == ''] <- NA
    }
  }
  return(dplyr::as.tbl(data))
}
