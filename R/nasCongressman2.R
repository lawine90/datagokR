#' National Assembly Secretariat, Congressman information.
#'
#' nasCongressman2 function import detailed data about congressman.
#' You need to know department code and unique code that can be acheived from nasCongressman1.
#' department code is "code_dept" and unique code is "code_numb" from nasCongressman1 function.
#'
#' @param key character value. API key issued from <www.data.go.kr>. no default.
#' @param code_dept character value. It is "code_dept" column which can be acheived from nasCongressman1.
#' @param code_numb character value. It is "code_numb" column which can be acheived from nasCongressman1.
#'
#' @examples
#'  key <- 'your key issued from data.go.kr'
#'
#'  # example.
#'  data <- nasCongressman2(key, code_dept = '9770931', code_numb = '2952')
#'
#' @export

nasCongressman2 <- function(key, code_dept, code_numb){
  ### 1. parameter checking and processing.
  ## key
  if(is.null(key)){ stop("Invalid key. \n Please issue API key first and insert it to 'key' param.") }

  ## codes
  if(is.null(code_dept)|is.null(code_numb)){
    stop("Invalid code. \n Please insert all codes. You can get it with nasCongressman1 function.")
  }

  ### 2. REST url for get n of pages
  ## generate list of urls(fxxking so many limitations...).
  # 1st, (url + key)
  url <- sprintf('http://apis.data.go.kr/9710000/%s/%s?ServiceKey=%s&numOfRows=%s&dept_cd=%s&num=%s',
                 'NationalAssemblyInfoService', 'getMemberDetailInfoList', key, 100, code_dept, code_numb)

  ### 3. first urls's xml parsing.
  # parsing xml codes with repeat and trycatch.
  tmp_xml <- datagokR:::try_read_xml(url)

  # if access fail, stop it.
  if(length(xml2::xml_text(xml2::xml_find_all(tmp_xml, '//errMsg'))) != 0){
    warning(xml2::xml_text(xml2::xml_find_all(tmp_xml, '//returnAuthMsg')))
    return(NULL)
  }

  item <- xml2::xml_find_all(tmp_xml, '//item')
  data <- data.frame(
    name_kr = datagokR:::find_xml(item, './empNm'),
    name_en = datagokR:::find_xml(item, './engNm'),
    name_ch = datagokR:::find_xml(item, './hjNm'),
    birth = datagokR:::find_xml(item, './bthDate'),
    hobby = datagokR:::find_xml(item, './hbbyCd'),
    able = datagokR:::find_xml(item, './examCd'),

    title = datagokR:::find_xml(item, './memTitle'),

    party = datagokR:::find_xml(item, './polyNm'),
    csty = datagokR:::find_xml(item, './origNm'),
    board = datagokR:::find_xml(item, './shrtNm'),

    elec_numb = datagokR:::find_xml(item, './reeleGbnNm'),
    elec_vol = datagokR:::find_xml(item, './electionNum'),

    off_tel = datagokR:::find_xml(item, './assemTel'),
    off_web = datagokR:::find_xml(item, './assemHomep'),
    off_mail = datagokR:::find_xml(item, './assemEmail'),

    staf_adv = datagokR:::find_xml(item, './staff'),
    staf_sec1 = datagokR:::find_xml(item, './secretary2'),
    staf_sec2 = datagokR:::find_xml(item, './secretary'),
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
