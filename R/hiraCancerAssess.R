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
  ii <- 0
  repeat{
    ii <- ii + 1
    tmp_xml <- tryCatch({httr::GET(url) %>% httr::content(as = "parsed", encoding = 'UTF-8')},
                        error = function(e){NULL})
    if(!is.null(tmp_xml) | ii == 15) break
  }

  # if access fail, stop it.
  if(is.null(tmp_xml)){
    stop('XML parsing fail.Please try again.')
  }

  location <- tmp_xml$response$body$items$item
  data <- data.frame(
    type_code = lapply(location, function(x) ifelse(is.null(x$clCd), "NA", x$clCd)) %>% unlist,
    type = lapply(location, function(x) ifelse(is.null(x$clCdNm), "NA", x$clCdNm)) %>% unlist,
    name = lapply(location, function(x) ifelse(is.null(x$yadmNm), "NA", x$yadmNm)) %>% unlist,

    addr_code = lapply(location, function(x) ifelse(is.null(x$sgguCd), "NA", x$sgguCd)) %>% unlist,
    addr_name1 = lapply(location, function(x) ifelse(is.null(x$sidoCdNm), "NA", x$sidoCdNm)) %>% unlist,
    addr_name2 = lapply(location, function(x) ifelse(is.null(x$sgguCdNm), "NA", x$sgguCdNm)) %>% unlist,

    liver = lapply(location, function(x) ifelse(is.null(x$asmGrd1), NA, x$asmGrd1)) %>% unlist,
    gastric = lapply(location, function(x) ifelse(is.null(x$asmGrd2), NA, x$asmGrd2)) %>% unlist,
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
