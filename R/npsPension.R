#' National Pension Service, Information about corporation's national pension member.
#'
#' npsPension function import data about corporation's national pension information.
#'
#' @param key character value. API key issued from <www.data.go.kr>. no default.
#' @param id corporation's unique id which can find from npsCorp function.
#'
#' @examples
#'  key <- 'your key issued from data.go.kr'
#'
#'  # example.
#'  data <- npsPension(key, id = 22626108)
#'
#' @export

npsPension <- function(key = NULL, id = NULL){
  ### 1. parameter checking and processing.
  if(is.null(key)){ stop("Empty key. \n Please issue API key first and insert it to 'key' param.") }
  if(is.null(id)){ stop("Empty id. \n Please insert id. You can find corporation's id from npsCorp function.") }

  ### 2. REST url.
  url <- sprintf('http://apis.data.go.kr/B552015/%s/%s?ServiceKey=%s&seq=%s',
                 'NpsBplcInfoInqireService', 'getDetailInfoSearch', key, id)

  ### 3. first urls's xml parsing.
  tmp_xml <- datagokR:::try_read_xml(url)
  msg <- datagokR:::find_xml(tmp_xml, '//returnAuthMsg')

  if(!is.na(msg)){
    warning(msg, '\nThe function return NULL')
    return(NULL)
  }

  data <- data.frame(
    registNumb = datagokR:::find_xml(tmp_xml, '//bzowrRgstNo'),
    name = datagokR:::find_xml(tmp_xml, '//wkplNm'),
    wrkCode = datagokR:::find_xml(tmp_xml, '//wkplIntpCd'),
    wrkName = datagokR:::find_xml(tmp_xml, '//vldtVlKrnNm'),

    join = ifelse(datagokR:::find_xml(tmp_xml, '//wkplJnngStcd') == '1', '등록', '탈퇴'),
    inDate = datagokR:::find_xml(tmp_xml, '//adptDt'),
    outDate = datagokR:::find_xml(tmp_xml, '//scsnDt'),
    type = ifelse(datagokR:::find_xml(tmp_xml, '//wkplStylDvcd') == '1', '법인', '개인'),

    member = datagokR:::find_xml(tmp_xml, '//jnngpCnt'),
    amnt = datagokR:::find_xml(tmp_xml, '//crrmmNtcAmt'),

    addr = datagokR:::find_xml(tmp_xml, '//wkplRoadNmDtlAddr'),
    addrCode1 = datagokR::find_xml(tmp_xml, '//ldongAddrMgplDgCd'),
    addrCode2 = datagokR::find_xml(tmp_xml, '//ldongAddrMgplSgguCd'),
    addrCode3 = datagokR::find_xml(tmp_xml, '//ldongAddrMgplSgguEmdCd'),

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
