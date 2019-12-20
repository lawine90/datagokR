#' Health Insurance Review & Assessment Service, Cancer Surgery Mortality Assessment.
#'
#' hiraCancerAssess function import hosipital assessment grade data about liver, gastric cancer surgery mortality.
#'
#' @param key character value. API key issued from <www.data.go.kr>. no default.
#'
#' @examples
#'  key <- 'your key issued from data.go.kr'
#'
#'  # example.
#'  data <- hiraCancerAssess(key)
#'
#' @export

hiraCancerAssess <- function(key){
  ### 1. parameter checking and processing.
  ## key
  if(is.null(key)){ stop("Invalid key. \n Please issue API key first and insert it to 'key' param.") }

  ### 2. REST url for get n of pages
  ## generate list of urls(fxxking so many limitations...).
  # 1st, (url + key)
  url <- sprintf('http://apis.data.go.kr/B551182/%s/%s?ServiceKey=%s&numOfRows=%s',
                 'hospAsmRstInfoService', 'getCnrSoprDeathAsmRstList', key, 1000)

  ### 3. first urls's xml parsing.
  # parsing xml codes with repeat and trycatch.
  tmp_xml <- datagokR:::try_xmlToList(url)
  total <- as.numeric(tmp_xml$body$totalCount)

  if(!is.null(tmp_xml$cmmMsgHeader)){
    warning(tmp_xml$cmmMsgHeader$returnAuthMsg, '\nThe function return NULL')
    return(NULL)
  }else if(total == 0){
    print('There is no data. Please check regist_numb')
    return(NULL)
  }

  data <- data.frame(
    type_code = datagokR:::find_xmlList(tmp_xml$body$items, 'clCd'),
    type = datagokR:::find_xmlList(tmp_xml$body$items, 'clCdNm'),
    name = datagokR:::find_xmlList(tmp_xml$body$items, 'yadmNm'),

    addr_code = datagokR:::find_xmlList(tmp_xml$body$items, 'sgguCd'),
    addr_name1 = datagokR:::find_xmlList(tmp_xml$body$items, 'sidoCdNm'),
    addr_name2 = datagokR:::find_xmlList(tmp_xml$body$items, 'sgguCdNm'),

    liver = datagokR:::find_xmlList(tmp_xml$body$items, 'asmGrd1', 'num'),
    gastric = datagokR:::find_xmlList(tmp_xml$body$items, 'asmGrd2', 'num'),
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
