#' Ministry of Food and Drug Safety, Quasi-drug products information.
#'
#' quasiDrugs function import Quasi-drug product information like name, manufacturer, effect, and so on.
#'
#' @param key character value. API key issued from <www.data.go.kr>. no default.
#' @param verbose logical value. if TRUE, provide process bar. Default value set as false.
#'
#' @examples
#'  key <- 'your key issued from data.go.kr'
#'
#'  # example 1 searching by localeCode.
#'  data <- quasiDrugs(key, verbose = T)
#'
#' @export

quasiDrugs <- function(key, verbose = F){
  ### 1. parameter checking and processing.
  ## key
  if(is.null(key)){ stop("Invalid key. \n Please issue API key first and insert it to \"key\" param.") }

  ### 2. REST url for get n of pages
  ## End Point.
  base <- "http://apis.data.go.kr/1471057/NonMdcinPrductPrmisnInfoService/getNonMdcinPrductPrmisnInfoList?"

  ## generate list of urls(fxxking so many limitations...).
  # 1st, (url + key)
  url <- sprintf("%sserviceKey=%s&numOfRows=100&pageNo=%s", base, key, '1')

  ii <- 0
  repeat{
    ii <- ii + 1
    tmp_xml <- tryCatch({XML::xmlToList(url)}, error = function(e){NULL})
    if(!is.null(tmp_xml) | ii == 15) break
  }

  if(is.null(tmp_xml)){
    stop('XML parsing fail.Please try again.')
  }

  # if(!is.null(tmp_xml$cmmMsgHeader)){
  #   stop(paste(tmp_xml$cmmMsgHeader$returnAuthMsg, ".\nError Code: ",
  #              tmp_xml$cmmMsgHeader$returnReasonCode, sep = ""))
  # }

  # the number of pages.
  nofpage <- (as.numeric(tmp_xml$body$totalCount)/as.numeric(tmp_xml$body$numOfRows))
  nofpage <- ceiling(nofpage)

  urls <- lapply(1:nofpage, function(x) sprintf("%sserviceKey=%s&numOfRows=100&pageNo=%s", base, key, x))
  urls <- unlist(urls)

  ### 3. first urls's xml parsing.
  # parsing xml codes with repeat and trycatch.
  if(verbose == T){pb <- txtProgressBar(min = 1, length(urls), style = 3)}

  all_data <- list()
  for(i in 1:length(urls)){
    ii <- 0
    repeat{
      ii <- ii + 1
      tmp_xml <- tryCatch({xml2::read_xml(urls[i], encoding = 'UTF-8')}, error = function(e){NULL})
      if(!is.null(tmp_xml) | ii == 15) break
    }

    if(is.null(tmp_xml)){
      stop('XML parsing fail.Please try again.')
    }
    #children <- read_xml(urls[i], encoding = 'UTF-8') %>% xml_find_all('//NB_DOC_DATA//SECTION')
    #lapply(children, function(x) html_nodes(x, 'ARTICLE') %>% xml_attrs('title') %>% unlist %>% unname)

    all_data[[i]] <- data.frame(
      item_code = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//ITEM_SEQ')),
      item_name = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//ITEM_NAME')),
      effect = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//EE_DOC_DATA')),
      usage = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//UD_DOC_DATA')),
      notice = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//NB_DOC_DATA')),
      type_code = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//CLASS_NO')),
      type_name = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//CLASS_NO_NAME')),
      firm = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//ENTP_NAME')),
      stringsAsFactors = F
    )
    if(verbose == T){setTxtProgressBar(pb, value = i)}
  }

  merged <- bind_rows(all_data)
  for(col in colnames(merged)){
    Encoding(merged[[col]]) <- 'UTF-8'
    merged[[col]][merged[[col]] == 'N/A'] <- NA
    try(
      merged[[col]] <- as.numeric(merged[[col]]),
      silent = T
    )
  }

  return(dplyr::as.tbl(merged))
}
