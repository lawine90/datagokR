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
#'  # example.
#'  data <- quasiDrugs(key, verbose = T)
#'
#' @export

quasiDrugs <- function(key, verbose = F){
  ### 1. parameter checking and processing.
  ## key
  if(is.null(key)){ stop("Invalid key. \n Please issue API key first and insert it to \"key\" param.") }

  ### 2. REST url for get n of pages
  ## End Point.
  url <- sprintf("http://apis.data.go.kr/1471057/%s/%s?serviceKey=%s&numOfRows=100&pageNo=%s",
                 'NonMdcinPrductPrmisnInfoService', 'getNonMdcinPrductPrmisnInfoList', key, '1')

  tmp_xml <- datagokR:::try_xmlToList(url)
  nofpage <- (as.numeric(tmp_xml$body$totalCount)/as.numeric(tmp_xml$body$numOfRows))
  nofpage <- ceiling(nofpage)

  if(length(nofpage) == 0){
    warning(tmp_xml$cmmMsgHeader$returnAuthMsg)
    return(NULL)
  }

  urls <- lapply(1:nofpage, function(x) sprintf("http://apis.data.go.kr/1471057/%s/%s?serviceKey=%s&numOfRows=100&pageNo=%s",
                                                'NonMdcinPrductPrmisnInfoService', 'getNonMdcinPrductPrmisnInfoList', key, x))
  urls <- unlist(urls)

  ### 3. first urls's xml parsing.
  # parsing xml codes with repeat and trycatch.
  if(length(urls) == 1){verbose <- FALSE}
  if(verbose == T){pb <- txtProgressBar(min = 1, length(urls), style = 3)}

  all_data <- list()
  for(i in 1:length(urls)){
    tmp_xml <- datagokR:::try_read_xml(urls[i])

    if(is.null(datagokR:::find_xml(tmp_xml, '//totalCount'))){
      warning(datagokR:::find_xml(tmp_xml, '//returnAuthMsg'))
      next()
    }

    all_data[[i]] <- data.frame(
      item_code = datagokR:::find_xml(tmp_xml, '//ITEM_SEQ'),
      item_name = datagokR:::find_xml(tmp_xml, '//ITEM_NAME'),
      effect = datagokR:::find_xml(tmp_xml, '//EE_DOC_DATA'),
      usage = datagokR:::find_xml(tmp_xml, '//UD_DOC_DATA'),
      notice = datagokR:::find_xml(tmp_xml, '//NB_DOC_DATA'),
      type_code = datagokR:::find_xml(tmp_xml, '//CLASS_NO'),
      type_name = datagokR:::find_xml(tmp_xml, '//CLASS_NO_NAME'),
      firm = datagokR:::find_xml(tmp_xml, '//ENTP_NAME'),
      stringsAsFactors = F
    )
    if(verbose == T){setTxtProgressBar(pb, value = i)}
  }

  merged <- dplyr::bind_rows(all_data)
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
