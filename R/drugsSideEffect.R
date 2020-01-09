#' Ministry of Food and Drug Safety, Drugs side effect information.
#'
#' drugsSideEffect function import the side effects of drugs.
#'
#' @param key character value. API key issued from <www.data.go.kr>. no default.
#'
#' @examples
#'  key <- 'your key issued from data.go.kr'
#'  data <- drugsSideEffect(key)
#'
#' @export

drugsSideEffect <- function(key){
  ### 1. parameter checking and processing.
  ## key
  if(is.null(key)){ stop("Invalid key. \n Please issue API key first and insert it to \"key\" param.") }

  ### 2. REST url
  ## End Point.
  url <- "http://apis.data.go.kr/1470000/MdcinSdefctInfoService/getMdcinSdefctInfoList?"

  ## generate list of urls(fxxking so many limitations...).
  # 1st, (url + key)
  urls <- paste(url, "serviceKey=", key, "&numOfRows=100", sep = "")

  ### 3. urls's xml parsing.
  # parsing xml codes with repeat and trycatch.
  tmp_xml <- datagokR:::try_read_xml(urls)
  total <- as.numeric(xml2::xml_text(xml2::xml_find_all(tmp_xml, '//totalCount')))

  if(is.na(total)){
    warning(xml2::xml_text(xml2::xml_find_all(tmp_xml, '//returnAuthMsg')),
            '\nThe function return NULL')
    return(NULL)
  }else if(total == 0){
    print('There is no data.')
    return(NULL)
  }

  item <- xml2::xml_find_all(tmp_xml, '//item')
  all_data <- data.frame(
    name_kor = datagokR:::find_xml(item, './COL_001'),
    name_eng = datagokR:::find_xml(item, './COL_002'),
    type = datagokR:::find_xml(item, './COL_003'),
    period = datagokR:::find_xml(item, './COL_004'),
    effect_kor = datagokR:::find_xml(item, './COL_005'),
    effect_eng = datagokR:::find_xml(item, './COL_006'),
    etc = datagokR:::find_xml(item, './COL_007'),
    stringsAsFactors = F
  )

  for(col in colnames(all_data)){
    Encoding(all_data[[col]]) <- 'UTF-8'
  }
  return(dplyr::as.tbl(all_data))
}
