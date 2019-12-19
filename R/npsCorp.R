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

  tmp_xml <- datagokR:::try_read_xml(url)
  total <- as.numeric(datagokR:::find_xml(tmp_xml, '//totalCount'))

  if(is.na(total)){
    warning(datagokR:::find_xml(tmp_xml, '//returnAuthMsg'), '\nThe function return NULL')
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

  ### 3. urls's xml parsing.
  all_data <- list()
  for(i in 1:length(urls)){
    tmp_xml <- datagokR:::try_read_xml(urls[i])

    all_data[[i]] <- data.frame(
      upDate = datagokR:::find_xml(tmp_xml, '//dataCrtYm'),
      registNumb = datagokR:::find_xml(tmp_xml, '//bzowrRgstNo'),
      id = datagokR:::find_xml(tmp_xml, '//seq'),
      name = datagokR:::find_xml(tmp_xml, '//wkplNm'),

      join = ifelse(datagokR:::find_xml(tmp_xml, '//wkplJnngStcd') == '1', '등록', '탈퇴'),
      type = ifelse(datagokR:::find_xml(tmp_xml, '//wkplStylDvcd') == '1', '법인', '개인'),

      addr = datagokR:::find_xml(tmp_xml, '//wkplRoadNmDtlAddr'),
      addrCode1 = datagokR::find_xml(tmp_xml, '//ldongAddrMgplDgCd'),
      addrCode2 = datagokR::find_xml(tmp_xml, '//ldongAddrMgplSgguCd'),
      addrCode3 = datagokR::find_xml(tmp_xml, '//ldongAddrMgplSgguEmdCd'),

      stringsAsFactors = F
    )
    if(verbose == T){setTxtProgressBar(pb, value = i)}
  }

  merged <- dplyr::bind_rows(all_data)
  return(dplyr::as.tbl(merged))
}
