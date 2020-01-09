#' National Pension Service, Search company's information with corporate registration number.
#'
#' npsCorp function import data about corporation's information. See details
#'
#' @details The information contains location, type, unique id and so on.
#'   The "id" in imported data is used at npsCorp function.
#'   Because the "regist_numb" argument is ambiguous(not a full corporate registration number),
#'   you should search target company from imported data by "name".
#'
#' @param key character value. API key issued from <www.data.go.kr>. no default.
#' @param regist_numb character value. first 6 number of company's corporate registration number.
#' @param verbose logical value. If TRUE, show process bar. Default is set as FALSE.
#'
#' @examples
#'  key <- 'your key issued from data.go.kr'
#'
#'  # example.
#'  data <- npsCorp(key, regist_numb = 208812)
#'
#' @export

npsCorp <- function(key, regist_numb = NULL, verbose = FALSE){
  ### 1. parameter checking and processing.
  ## key
  if(is.null(key)){ stop("Invalid key. \n Please issue API key first and insert it to 'key' param.") }
  if(is.null(regist_numb)){ stop("Invalid regist_numb \n Please insert 6-digit regist_numb.") }

  ### 2. REST url for get n of pages
  ## generate list of urls(fxxking so many limitations...).
  # 1st, (url + key)
  url <- sprintf('http://apis.data.go.kr/B552015/%s/%s?serviceKey=%s&bzowr_rgst_no=%s',
                 'NpsBplcInfoInqireService', 'getBassInfoSearch', key, regist_numb)

  ### 3-1. get total count.
  tmp_xml <- datagokR:::try_read_xml(url)
  total <- as.numeric(xml2::xml_text(xml2::xml_find_all(tmp_xml, '//totalCount')))

  if(length(total) == 0){
    warning(xml2::xml_text(xml2::xml_find_all(tmp_xml, '//returnAuthMsg')),
            '\nThe function return NULL')
    return(NULL)
  }else if(total == 0){
    print('There is no data. Please check regist_numb')
    return(NULL)
  }

  all_data <- list()
  urls <- sprintf('http://apis.data.go.kr/B552015/%s/%s?serviceKey=%s&bzowr_rgst_no=%s&numOfRows=1000&pageNo=%s',
                  'NpsBplcInfoInqireService', 'getBassInfoSearch', key, regist_numb, 1:ceiling(total/1000))

  if(length(urls) == 1){verbose <- F}
  if(verbose == T){pb <- txtProgressBar(min = 1, length(urls), style = 3)}

  ### 3-2. all urls xml parsing.
  all_data <- list()
  for(i in 1:length(urls)){
    tmp_xml <- datagokR:::try_read_xml(urls[i])

    item <- xml2::xml_find_all(tmp_xml, '//item')
    all_data[[i]] <- data.frame(
      upDate = datagokR:::find_xml(item, './dataCrtYm'),
      registNumb = datagokR:::find_xml(item, './bzowrRgstNo'),
      id = datagokR:::find_xml(item, './seq'),
      name = datagokR:::find_xml(item, './wkplNm'),

      join = ifelse(datagokR:::find_xml(item, './wkplJnngStcd') == '1', '등록', '탈퇴'),
      type = ifelse(datagokR:::find_xml(item, './wkplStylDvcd') == '1', '법인', '개인'),

      addr = datagokR:::find_xml(item, './wkplRoadNmDtlAddr'),
      addrCode1 = datagokR::find_xml(item, './ldongAddrMgplDgCd'),
      addrCode2 = datagokR::find_xml(item, './ldongAddrMgplSgguCd'),
      addrCode3 = datagokR::find_xml(item, './ldongAddrMgplSgguEmdCd'),

      stringsAsFactors = F
    )
    if(verbose == T){setTxtProgressBar(pb, value = i)}
  }

  merged <- dplyr::bind_rows(all_data)
  return(dplyr::as.tbl(merged))
}
