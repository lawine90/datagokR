#' National Assembly Secretariat, Congressman information.
#'
#' nasCongressman1 function import brief data about congressman's name, constituency, and so on.
#' If you need more specific information about congressman, please use nasCongressman2 function.
#'
#' @param key character value. API key issued from <www.data.go.kr>. no default.
#'
#' @examples
#'  key <- 'your key issued from data.go.kr'
#'
#'  # example.
#'  data <- nasCongressman1(key)
#'
#' @export

nasCongressman1 <- function(key){
  ### 1. parameter checking and processing.
  ## key
  if(is.null(key)){ stop("Invalid key. \n Please issue API key first and insert it to 'key' param.") }

  ### 2. REST url for get n of pages
  ## generate list of urls(fxxking so many limitations...).
  # 1st, (url + key)
  url <- sprintf('http://apis.data.go.kr/9710000/NationalAssemblyInfoService/%s?ServiceKey=%s&numOfRows=%s',
                 'getMemberCurrStateList', key, '500')

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
    code_dept = datagokR:::find_xml(item, './deptCd'),
    code_numb = datagokR:::find_xml(item, './num'),
    name_kr = datagokR:::find_xml(item, './empNm'),
    name_en = datagokR:::find_xml(item, './engNm'),
    name_ch = datagokR:::find_xml(item, './hjNm'),
    poto_link = datagokR:::find_xml(item, './jpgLink'),
    numb_elec = datagokR:::find_xml(item, './reeleGbnNm'),
    csty = datagokR:::find_xml(item, './origNm'),
    stringsAsFactors = F
  )

  data <- dplyr::as.tbl(data)
  for(col in colnames(data)){
    if(class(data[[col]]) == 'character'){
      Encoding(data[[col]]) <- 'UTF-8'
      data[[col]][data[[col]] == ''] <- NA
    }
  }
  return(data)
}
