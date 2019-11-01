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
  ii <- 0
  repeat{
    ii <- ii + 1
    tmp_xml <- tryCatch({xml2::read_xml(url, encoding = 'UTF-8')}, error = function(e){NULL})
    if(!is.null(tmp_xml) | ii == 15) break
  }

  # if access fail, stop it.
  if(is.null(tmp_xml)){
    stop('XML parsing fail.Please try again.')
  }

  data <- data.frame(
    name_kr = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//empNm')),
    name_en = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//engNm')),
    name_ch = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//hjNm')),
    birth = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//bthDate')),
    hobby = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//hbbyCd')),
    able = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//examCd')),

    title = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//memTitle')),

    party = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//polyNm')),
    csty = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//origNm')),
    board = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//shrtNm')),

    elec_numb = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//reeleGbnNm')),
    elec_vol = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//electionNum')),

    off_tel = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//assemTel')),
    off_web = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//assemHomep')),
    off_mail = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//assemEmail')),

    staf_adv = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//staff')),
    staf_sec1 = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//secretary2')),
    staf_sec2 = xml2::xml_text(xml2::xml_find_all(tmp_xml, '//secretary')),
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
