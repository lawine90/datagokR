#' Ministry of Land Infrastructure and Transport, land transaction data.
#'
#' molitLand function import the actual transition data of house. The function also provide simple visualization using plotly.
#'
#' @param key character value. API key issued from <www.data.go.kr>
#' @param year numeric value. the year of real trade
#' @param month numeric value. the month of real trade. default is NULL
#' @param localeCode numeric value. SiGunGu code which means legal area.
#' @param localeName character value. SiGunGu name wich means legal area. It should be Korean.
#' @param slow logical value. if TRUE, give sleep inbetween importing. default is FALSE
#' @param viz logical value. if TRUE, provide simple 3d visualization result. x: date, y: price, z: the number of contract.
#'
#' @return two data.frame for meta-data and imported data.
#' two vectors for error urls and all urls. visualization.
#'
#' @details If month value is NULL, all data of the year will imported.\cr
#'    Between localeCode and localeName, one of these parameters should be inserted. \cr
#'    The localeCode parameter recommended five numeric value.
#'
#' @examples
#'  # example 1 searching by localeCode.
#'  data <- molitLand(key = "my_key", year = 2018, month = 1,
#'                    localeCode = 11110, slow = T, viz = F)
#'
#'  # example 2 searching by localeName
#'  data <- molitLand(key = "my_key", year = 2018, month = 1:6,
#'                    localeName = enc2utf8("서울"), slow = F, viz = T)
#'
#' @importFrom dplyr %>%
#' @importFrom stats runif
#'
#' @export

# utils::globalVariables(c(".data"), add = F)
molitLand <- function(key, year, month = NULL, localeCode = NULL, localeName = NULL, slow = F, viz = F){
  ### 1. parameter checking.
  if(is.null(key)){ stop("Invalid key. Please issue API key first and insert it to \"key\" param.") }
  if(!is.numeric(year) & nchar(year) != 4){ stop("Invalid year. Please insert right \"year\" param(ex: 2018)") }
  if(is.null(localeCode) & is.null(localeName)){ stop("Invalid locale. Please insert at least one params between \"localeCode\" and \"localeName\"") }
  if(!is.null(localeCode) & (mean(nchar(localeCode)) > 5)){
    warning("Five numeric value is recommended for \"localeCode\" param.")
    localeCode <- substr(localeCode, 1, 5) %>% as.numeric
  }

  ### 2. REST url.
  ## End Point.
  url <- sprintf("http://openapi.molit.go.kr/%s/service/rest/%s/%s?",
                 "OpenAPI_ToolInstallPackage", "RTMSOBJSvc", "getRTMSDataSvcLandTrade")

  ## date
  datelst <- seq.Date(from = as.Date(paste(year, '-01-01', sep = "")),
                      to = as.Date(paste(year, '-12-01', sep = "")),
                      by = 'month')
  datelst <- as.character(datelst)
  datelst <- substr(gsub("-", "", datelst), start = 1, stop = 6)

  if(!is.null(month)){
    datelst <- datelst[gsub(year, "", datelst) %in% sprintf("%02d", as.numeric(month))]
  }

  ## locale
  if(is.null(localeCode) & !is.null(localeName)){
    localeName <- gsub("시\\b|도\\b", "", localeName) %>% paste(collapse = "|")
    localeCode <- datagokR::molit_locale_code[grepl(localeName, datagokR::molit_locale_code$name),]
    localeCode <- localeCode[localeCode$exist == "존재",] %>% dplyr::select("code") %>% unlist %>% as.numeric
  }

  ## generate list of urls.
  urls <- lapply(datelst, function(x) paste(url, "serviceKey=", key, "&DEAL_YMD=", x, sep = "")) %>%
    lapply(function(x) paste(x, "&LAWD_CD=", localeCode, sep = "")) %>% unlist

  ### 3. urls's xml parsing.
  all.data <- list(); length(all.data) <- length(urls)                 # define data.frame for importing.
  pb <- utils::txtProgressBar(min = 0, length(urls), style = 3)

  ## xml data parsing as list form.
  for(i in 1:length(urls)){
    tmp.xml <- datagokR:::try_GET_content(urls[[i]])

    # if tmp.xml is error, go next.
    if(tmp.xml$response$header$resultMsg != "NORMAL SERVICE."){
      next
    }

    Count <- tmp.xml$response$body$totalCount
    if(slow){
      Sys.sleep(stats::runif(1, 0, 1.5))
    }

    # if the number of trade is 0, skip.
    # set location object differently according to the number of trade(1 or over.)
    if(Count == 0){
      utils::setTxtProgressBar(pb, value = i)
      next
    }else if(Count == 1){
      location <- tmp.xml$response$body$items
    }else{
      location <- tmp.xml$response$body$items$item
    } # end of if statement.

    all.data[[i]] <- data.frame(
      code = trimws(datagokR:::find_xmlList(location, '지역코드')),
      gu = trimws(datagokR:::find_xmlList(location, '시군구')),
      dong = trimws(datagokR:::find_xmlList(location, '법정동')),

      tradeYear = trimws(datagokR:::find_xmlList(location, '년')),
      tradeMonth = trimws(datagokR:::find_xmlList(location, '월')),
      tradeDay = trimws(datagokR:::find_xmlList(location, '일')),
      tradeType = trimws(datagokR:::find_xmlList(location, '구분')),
      price = as.numeric(trimws(gsub(",", "", datagokR:::find_xmlList(location, '거래금액')))),

      landUsage = trimws(datagokR:::find_xmlList(location, '지목')),

      area = trimws(datagokR:::find_xmlList(location, '거래면적')),
      type = trimws(datagokR:::find_xmlList(location, '용도지역')),
      stringsAsFactors = F
    )

    utils::setTxtProgressBar(pb, value = i)
  } # end of loop i.

  result <- dplyr::as.tbl(dplyr::bind_rows(all.data))

  return(result)
  cat("\nJobs Done.\n")
} # end of function.
