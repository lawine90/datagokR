#' National Assembly Secretariat, Recent proposed bill list searching by congressman name.
#'
#' nasBillProposer function import proposed bill list searching by name of congressman.
#'
#' @param key character value. API key issued from <www.data.go.kr>. no default.
#' @param name character value. the name of congressman.
#' @param verbose logical value. If TRUE, show process bar. Default is set as FALSE.
#'
#' @examples
#'  key <- 'your key issued from data.go.kr'
#'
#'  # example.
#'  data <- nasBillProposer(key, id = 'PRC_L1E6Z0K1F2V8U1N7M3V5N1Y6V6G5U3')
#'
#' @export

nasBillProposer <- function(key, id = NULL, verbose = FALSE){
  ### 1. parameter checking and processing.
  ## key
  if(is.null(key)){ stop("Invalid key. \n Please issue API key first and insert it to 'key' param.") }
  if(is.null(id)){ stop("Invalid id. \n Please insert bill id. You can check bill id from nasBillSearch function.") }

  ### 2. REST url for get n of pages
  ## generate list of urls(fxxking so many limitations...).
  # 1st, (url + key)
  urls <- sprintf('http://apis.data.go.kr/9710000/%s/%s?ServiceKey=%s&bill_id=%s',
                  'BillInfoService2', 'getBillPetitionMemberList', key, id)

  if(length(urls) == 1){verbose <- F}
  if(verbose == T){pb <- txtProgressBar(min = 1, length(urls), style = 3)}

  ### 3. first urls's xml parsing.
  # parsing xml codes with repeat and trycatch.
  all_data <- list()
  for(i in 1:length(urls)){
    tmp_xml <- datagokR:::try_read_xml(urls[i])

    # if access fail, stop it.
    if(is.na(datagokR:::find_xml(tmp_xml, '//resultCode'))){
      warning(datagokR:::find_xml(tmp_xml, '//returnAuthMsg'))
      next()
    }

    all_data[[i]] <- data.frame(
      id = id[i],
      type1 = datagokR:::find_xml(tmp_xml, '//gbn1'),
      type2 = datagokR:::find_xml(tmp_xml, '//gbn2'),

      conType = datagokR:::find_xml(tmp_xml, '//gbnCd'),
      name = datagokR:::find_xml(tmp_xml, '//memName'),
      party = datagokR:::find_xml(tmp_xml, '//polyNm'),
      stringsAsFactors = F
    )
    if(verbose == T){setTxtProgressBar(pb, value = i)}
  }

  data <- dplyr::as.tbl(dplyr::bind_rows(all_data))
  for(col in colnames(data)){
    if(class(data[[col]]) == 'character'){
      Encoding(data[[col]]) <- 'UTF-8'
      data[[col]][data[[col]] == ''] <- NA
    }
  }

  return(dplyr::as.tbl(data))
}
