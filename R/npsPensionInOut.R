#' National Pension Service, Information about corporation's national pension member in/out count.
#'
#' npsPensionInOut function import data about the number of corporation's national pension's number.
#'
#' @param key character value. API key issued from <www.data.go.kr>. no default.
#' @param id corporation's unique id which can find from npsCorp function.
#' @param month 6-digit month like 201911.
#'
#' @examples
#'  key <- 'your key issued from data.go.kr'
#'
#'  # example.
#'  data <- npsPensionInOut(key, id = '22626108', month = '201911')
#'
#' @export

npsPensionInOut <- function(key = NULL, id = NULL, month = NULL){
  ### 1. parameter checking and processing.
  if(is.null(key)){ stop("Empty key. \n Please issue API key first and insert it to 'key' param.") }
  if(is.null(id)){ stop("Empty id. \n Please insert id. You can find corporation's id from npsCorp function.") }
  if(is.null(month)){
    month <- gsub('-|\\d{2}$', '', as.Date(format(Sys.Date(), '%Y-%m-01'))-1)
    warning("Empty month. \n You should insert 6-digit month like 201911. \n Automatically insert previous month.")
  }else if(nchar(month) != 6){
    month <- gsub('-|\\d{2}$', '', as.Date(format(Sys.Date(), '%Y-%m-01'))-1)
    warning("Invalid month. \n You should insert 6-digit month like 201911. \n Automatically insert previous month.")
  }

  ### 2. REST url.
  url <- sprintf('http://apis.data.go.kr/B552015/%s/%s?ServiceKey=%s&seq=%s&date_crt_ym=%s',
                 'NpsBplcInfoInqireService', 'getPdAcctoSttusInfoSearch', key, id, month)

  ### 3. first urls's xml parsing.
  tmp_xml <- datagokR:::try_read_xml(url)
  msg <- datagokR:::find_xml(tmp_xml, '//returnAuthMsg')

  if(!is.na(msg)){
    warning(msg, '\nThe function return NULL')
    return(NULL)
  }

  data <- data.frame(
    month = month,
    memberIn = as.numeric(datagokR:::find_xml(tmp_xml, '//nwAcqzrCnt')),
    memberOut = as.numeric(datagokR:::find_xml(tmp_xml, '//lssJnngpCnt')),
    stringsAsFactors = F
  )
  return(dplyr::as.tbl(data))
}
