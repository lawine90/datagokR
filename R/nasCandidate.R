#' National Assembly Secretariat, candidate data.
#'
#' nasCandidate function import profile of election candidates.
#'
#' @param key character value. API key issued from <www.data.go.kr>. no default.
#' @param election_id character value. the election id which means election date.
#' @param election_type numeric value. the election type.
#'
#' @examples
#'  key <- 'your key issued from data.go.kr'
#'
#'  # example.
#'  data <- nasCandidate(key, election_id = '20180613', election_type = 4)
#'
#' @export

nasCandidate <- function(key, election_id = NULL, election_type = NULL){
  ### 1. parameter checking and processing.
  ## key
  if(is.null(key)){ stop("Invalid key. \n Please issue API key first and insert it to 'key' param.") }

  ### 2. REST url for get n of pages
  ## generate list of urls(fxxking so many limitations...).
  # 1st, (url + key)
  url <- sprintf('http://apis.data.go.kr/9760000/%s/%s?ServiceKey=%s&numOfRows=%s&',
                 'PofelcddInfoInqireService', 'getPofelcddRegistSttusInfoInqire', key, 100)

  # 2nd, name
  if(!is.null(election_id) & !is.null(election_type)){
    url <- sprintf('%ssgId=%s&sgTypecode=%s', url, election_id, election_type)
  }else{
    warning('Insert election_id and electino_type.\nYou can check it from nasElection function.')
  }


  ### 3. first urls's xml parsing.
  # parsing xml codes with repeat and trycatch.
  tmp_xml <- datagokR:::try_read_xml(url)

  # if access fail, stop it.
  if(length(xml2::xml_text(xml2::xml_find_all(tmp_xml, '//errMsg'))) != 0){
    warning(xml2::xml_text(xml2::xml_find_all(tmp_xml, '//returnAuthMsg')))
    return(NULL)
  }else{
    total <- as.numeric(xml2::xml_text(xml2::xml_find_all(tmp_xml, '//totalCount')))
    pageNo <- floor(total/100)
  }

  data <- list()
  for(i in 1:pageNo){
    tmp_xml <- datagokR:::try_read_xml(sprintf('%s&pageNo=%d', url, i))
    item <- xml2::xml_find_all(tmp_xml, '//item')
    data[[i]] <- data.frame(
      huboid = datagokR:::find_xml(item, './huboid'),
      sdName = datagokR:::find_xml(item, './sdName'),
      sggName = datagokR:::find_xml(item, './sggName'),
      wiwName = datagokR:::find_xml(item, './wiwName'),
      giho = datagokR:::find_xml(item, './giho'),
      gihoSangse = datagokR:::find_xml(item, './gihoSangse'),
      jdName = datagokR:::find_xml(item, './jdName'),
      name = datagokR:::find_xml(item, './name'),
      hanjaName = datagokR:::find_xml(item, './hanjaName'),
      gender = datagokR:::find_xml(item, './gender'),
      birthday = datagokR:::find_xml(item, './birthday'),
      age = datagokR:::find_xml(item, './age', 'num'),
      jobId = datagokR:::find_xml(item, './jobId'),
      job = datagokR:::find_xml(item, './job'),
      eduId = datagokR:::find_xml(item, './eduId'),
      edu = datagokR:::find_xml(item, './edu'),
      career1 = datagokR:::find_xml(item, './career1'),
      career2 = datagokR:::find_xml(item, './career2'),
      status = datagokR:::find_xml(item, './status'),
      stringsAsFactors = F
    )
  }

  for(col in colnames(data)){
    if(class(data[[col]]) == 'character'){
      Encoding(data[[col]]) <- 'UTF-8'
      data[[col]][data[[col]] == ''] <- NA
    }
  }

  return(dplyr::as.tbl(data))
}
