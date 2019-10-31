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
    code_dept = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//deptCd')),
    code_numb = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//num')),
    name_kr = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//empNm')),
    name_en = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//engNm')),
    name_ch = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//hjNm')),
    poto_link = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//jpgLink')),
    numb_elec = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//reeleGbnNm')),
    csty = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//origNm')),
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
