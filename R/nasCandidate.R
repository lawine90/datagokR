#' National Assembly Secretariat, candidate data.
#'
#' nasCandidate function import profile of election candidates.
#'
#' @param key character value. API key issued from <www.data.go.kr>. no default.
#' @param election_id character value. the election id which means election date.
#' @param election_type numeric value. the election type.
#' @param is_pre boolean value. if TRUE, import pre-candidates data.
#'
#' @examples
#'  key <- 'your key issued from data.go.kr'
#'
#'  # example.
#'  data <- nasCandidate(key, election_id = '20180613', election_type = 4)
#'
#' @export

nasCandidate <- function(key, election_id = NULL, election_type = NULL, is_pre = FALSE){
  ### 1. parameter checking and processing.
  ## key
  if(is.null(key)){ stop("Invalid key. \n Please issue API key first and insert it to 'key' param.") }

  ### 2. REST url for get n of pages
  ## generate list of urls(fxxking so many limitations...).
  # 1st, (url + key)
  middle <- ifelse(is_pre, 'getPoelpcddRegistSttusInfoInqire', 'getPofelcddRegistSttusInfoInqire')
  url <- sprintf('http://apis.data.go.kr/9760000/%s/%s?ServiceKey=%s&numOfRows=%s&',
                 'PofelcddInfoInqireService', middle, key, 100)

  # 2nd, name
  if(!is.null(election_id) & !is.null(election_type)){
    url <- sprintf('%ssgId=%s&sgTypecode=%s', url, election_id, election_type)
  }else{
    warning('Insert election_id and electino_type.\nYou can check it from nasElection function.')
  }


  ### 3. first urls's xml parsing.
  # parsing xml codes with repeat and trycatch.
  tmp_xml <- datagokR:::try_read_xml(url)
  data <- list()

  # if access fail, stop it.
  if(length(xml2::xml_text(xml2::xml_find_all(tmp_xml, '//errMsg'))) != 0){
    warning(xml2::xml_text(xml2::xml_find_all(tmp_xml, '//returnAuthMsg')))
    return(NULL)
  }else{
    total <- as.numeric(xml2::xml_text(xml2::xml_find_all(tmp_xml, '//totalCount')))
    if(total == 0){
      print('There is no candidate information.')
      return(NULL)
    }
  }

  item <- xml2::xml_find_all(tmp_xml, '//item')
  data[[1]] <- datagokR:::xml_to_dataframe(item)
  if(total > 100){
    pageNo <- ceiling(total/100)
    for(i in 2:pageNo){
      tmp_xml <- datagokR:::try_read_xml(sprintf('%s&pageNo=%d', url, i))
      item <- xml2::xml_find_all(tmp_xml, '//item')
      data[[i]] <- datagokR:::xml_to_dataframe(item)
    }
  }

  data <- dplyr::bind_rows(data)
  for(col in colnames(data)){
    if(class(data[[col]]) == 'character'){
      Encoding(data[[col]]) <- 'UTF-8'
      data[[col]][data[[col]] == ''] <- NA
    }
  }
  return(dplyr::as.tbl(data))
}
