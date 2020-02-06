#' National Assembly Secretariat, polling place data.
#'
#' nasPolling function import information such as address, name and so on about polling place.
#'
#' @param key character value. API key issued from <www.data.go.kr>. no default.
#' @param election_id character value. the election id which means election date.
#' @param localeName character value. Korean Si/Do name.
#' @param is_pre boolean value. If you want to get data of early voting place, set TRUE.
#'
#' @examples
#'  key <- 'your key issued from data.go.kr'
#'
#'  # example.
#'  data <- nasPolling(key, election_id = '20190403',
#'                     localeName = enc2utf8('서울'))
#'
#' @export

nasPolling <- function(key, election_id = NULL, localeName = enc2utf8('서울'), is_pre = FALSE){
  ### 1. parameter checking and processing.
  ## key
  if(is.null(key)){ stop("Invalid key. \n Please issue API key first and insert it to 'key' param.") }

  ### 2. REST url for get n of pages
  ## generate list of urls(fxxking so many limitations...).
  # 1st, (url + key)
  middle <- ifelse(is_pre, 'getPrePolplcOtlnmapTrnsportInfoInqire',
                           'getPolplcOtlnmapTrnsportInfoInqire')
  url <- sprintf('http://apis.data.go.kr/9760000/%s/%s?serviceKey=%s&numOfRows=%s&',
                 'PolplcInfoInqireService2', middle, key, 100)

  # 2nd, name
  if(!is.null(election_id) & !is.null(localeName)){
    url <- sprintf('%ssgId=%s&sdName=%s', url, election_id, utils::URLencode(enc2utf8(localeName)))
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
  }else{ # even if access success, terminate function if total is zero.
    total <- as.numeric(xml2::xml_text(xml2::xml_find_all(tmp_xml, '//totalCount')))
    if(total == 0){
      print('There is no polling place information.')
      return(NULL)
    }
  } # else statement

  # get data.
  item <- xml2::xml_find_all(tmp_xml, '//item')
  data[[1]] <- datagokR:::xml_to_dataframe(item)
  if(total > 100){
    pageNo <- ceiling(total/100)
    for(i in 2:pageNo){
      tmp_xml <- datagokR:::try_read_xml(sprintf('%s&pageNo=%d', url, i))
      item <- xml2::xml_find_all(tmp_xml, '//item')
      data[[i]] <- datagokR:::xml_to_dataframe(item)
    } # for statement
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
