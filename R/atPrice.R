#' Korea Agro-Fisheries & Food Trade Corporation, Price of food, agriculture, forestry and fisheries.
#'
#' atPrice function import price of food, agriculture, forestry and fisheries.
#'   It provide similar data to episPrice function but have different survey source.
#'
#' @param key character value. API key issued from <www.data.go.kr>. no default.
#' @param date character value. date which expressed like YYYYMMDD. no default.
#' @param verbose logical value. If TRUE, show process bar. Default is set as FALSE.
#'
#' @details The date arguments should be set as YYYYMMDD type like '20181203'.
#'   Like episPrice, the survey of food, agriculture, forestry and fisheries is not excuted on weekend.
#'   So if you insert sunday, saturday or holiday date in date argument, there will be no return.
#'   The API key issued from 'data.mafra.go.kr' is needed, not from 'data.go.kr'.
#'
#' @examples
#'  key <- 'your key issued from data.go.kr'
#'
#'  data <- atPrice(key, date = '20191210')
#'
#' @export

atPrice <- function(key, date, verbose = F){
  ### 1. parameter checking and processing.
  ## key
  if(is.null(key)){ stop("Invalid key. \n Please issue API key first and insert it to \"key\" param.") }

  ## date
  if(is.null(date)|nchar(date) != 8){
    stop("Invalid date. \n Please insert date value as YYYYMMDD into \"date\" param.")
  }else if(class(date) != 'character'){
    warning("Invalid date. \n date should be character.")
    date <- as.character(date)
  }

  ### 2. REST url for get n of pages
  ## End Point.
  url <- sprintf('http://211.237.50.150:7080/openapi/%s/xml/%s/1/1?EXAMIN_DE=%s',
                 key, 'Grid_20141225000000000163_1', date)

  ii <- 0
  repeat{
    ii <- ii + 1
    tmp_xml <- tryCatch({XML::xmlToList(url)}, error = function(e){NULL})
    if(!is.null(tmp_xml) | ii == 15) break
  }

  if(is.null(tmp_xml)){
    stop('XML parsing fail.Please try again.')
  }
  if(!is.null(tmp_xml)){
    total <- as.numeric(tmp_xml$totalCnt)
  }

  if(total == 0){
    print(sprintf('There is no data at %s', as.Date(date, '%Y%m%d')))
    return(NULL)
  }

  all_data <- list()
  urls <- sprintf('http://211.237.50.150:7080/openapi/%s/xml/%s/%s/%s?EXAMIN_DE=%s',
                  key, 'Grid_20141225000000000163_1',
                  trimws(format(seq(1, total, by = 1000), scientific = F)),
                  trimws(format(seq(1, total, by = 1000)+999, scientific = F)), date)

  if(length(urls) == 1){verbose <- F}
  if(verbose == T){pb <- txtProgressBar(min = 1, length(urls), style = 3)}

  ### 3. urls's xml parsing.
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
    if(!is.null(tmp_xml$cmmMsgHeader)){
      return(tmp_xml$cmmMsgHeader$returnAuthMsg)
    }

    all_data[[i]] <- data.frame(
      date = date,

      locName = datagokR::find_xml(tmp_xml, '//AREA_NM'),
      locCode = datagokR::find_xml(tmp_xml, '//AREA_CD'),

      mrkName = datagokR::find_xml(tmp_xml, '//MRKT_NM'),
      mrkCode = datagokR::find_xml(tmp_xml, '//MRKT_CD'),

      catName = datagokR::find_xml(tmp_xml, '//FRMPRD_CATGORY_NM'),
      catCode = datagokR::find_xml(tmp_xml, '//FRMPRD_CATGORY_CD'),
      itmName = datagokR::find_xml(tmp_xml, '//PRDLST_NM'),
      itmCode = datagokR::find_xml(tmp_xml, '//PRDLST_CD'),
      spcName = datagokR::find_xml(tmp_xml, '//SPCIES_NM'),
      spcCode = datagokR::find_xml(tmp_xml, '//SPCIES_CD'),

      grdName = datagokR::find_xml(tmp_xml, '//GRAD_NM'),
      grdCode = datagokR::find_xml(tmp_xml, '//GRAD_CD'),

      unit = datagokR::find_xml(tmp_xml, '//EXAMIN_UNIT'),
      price = as.numeric(datagokR::find_xml(tmp_xml, '//AMT')),

      stringsAsFactors = F
    )
    if(verbose == T){setTxtProgressBar(pb, value = i)}
  }

  merged <- dplyr::bind_rows(all_data)
  return(dplyr::as.tbl(merged))
}
