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
  msg <- xml2::xml_text(xml2::xml_find_all(tmp_xml, '//returnAuthMsg'))

  if(length(msg) != 0){
    warning(msg, '\nThe function return NULL')
    return(NULL)
  }

  item <- xml2::xml_find_all(tmp_xml, '//item')
  data <- data.frame(
    registNumb = datagokR:::find_xml(item, './bzowrRgstNo'),
    name = datagokR:::find_xml(item, './wkplNm'),
    wrkCode = datagokR:::find_xml(item, './wkplIntpCd'),
    wrkName = datagokR:::find_xml(item, './vldtVlKrnNm'),

    join = ifelse(datagokR:::find_xml(item, './wkplJnngStcd') == '1', '등록', '탈퇴'),
    inDate = datagokR:::find_xml(item, './adptDt'),
    outDate = datagokR:::find_xml(item, './scsnDt'),
    type = ifelse(datagokR:::find_xml(item, './wkplStylDvcd') == '1', '법인', '개인'),

    member = datagokR:::find_xml(item, './jnngpCnt'),
    amnt = datagokR:::find_xml(item, './crrmmNtcAmt'),

    addr = datagokR:::find_xml(item, './wkplRoadNmDtlAddr'),
    addrCode1 = datagokR::find_xml(item, './ldongAddrMgplDgCd'),
    addrCode2 = datagokR::find_xml(item, './ldongAddrMgplSgguCd'),
    addrCode3 = datagokR::find_xml(item, './ldongAddrMgplSgguEmdCd'),

    stringsAsFactors = F
  )
  return(dplyr::as.tbl(data))
}
