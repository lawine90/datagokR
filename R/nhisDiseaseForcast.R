#' National Health Insurance Service, The Disease forcasting data.
#'
#' nhisDiseaseForcast function import the disease forcasting data about 3 days.
#'
#' @param key character value. API key issued from <www.data.go.kr>. no default.
#' @param localeCode numeric value. SiGunGu code which means legal area. one of localeCode or localeName should be inserted.
#' @param localeName character value. SiGunGu name wich means legal area. one of localeCode or localeName should be inserted. It should be Korean.
#' @param type character value. decide the type of disease. it should be one of
#'             "Influenza", "Eye", "Food", "Asthma", "Skin", or "All". see details.
#' @param slow logical value. if TRUE, give sleep inbetween importing. default is TRUE.
#' @param viz logical value. if TRUE, provide simple 2d visualization result. x: date, y: mean index.
#' @param verbose logical value. if TRUE, provide process bar. Default value set as false.
#'
#' @return data.frame and visualization.
#'
#' @details nhisDiseaseForcast function import forcasted count of treatment and risk of five diseases.\cr
#'  It is calculated by National Health Insurance Service.\cr
#'  Explanation about "type" as follow.\cr
#'  "Influenza" = Predicted number of patients and risk of influenza from the day to the day after tomorrow.\cr
#'  "Eye" = Predicted number of patients and risk of eye disease at the day.\cr
#'  "Food" = Predicted number of patients and risk of food poisoning from the day to the day after tomorrow.\cr
#'  "Asthma" = Predicted number of patients and risk of asthma at the day.\cr
#'  "Skin" = Predicted number of patients and risk of skin disease from the day to the day after tomorrow.\cr\cr
#'  localeCode argument should be first 2-digit value of SiGunGu code. For example, "11" means Seoul and "12" means Busan.
#'
#'
#' @examples
#'  key <- 'your key issued from data.go.kr'
#'
#'  # example 1 searching by localeCode.
#'  data <- nhisDiseaseForcast(key, localeCode = c(11, 41), type = "Asthma", slow = T)
#'
#'  # example 2 searching by localeName
#'  data <- nhisDiseaseForcast(key, localeName = c(enc2utf8('수원')), type = "All", slow = T)
#'
#' @importFrom dplyr %>%
#'
#' @export

nhisDiseaseForcast <- function(key, localeCode = NULL, localeName = NULL, type, slow = F, viz = F, verbose = F){
  ### 1. parameter checking and processing.
  ## key
  if(is.null(key)){ stop("Invalid key. \n Please issue API key first and insert it to \"key\" param.") }

  ## localeCode, localeName
  if(is.null(localeCode) & is.null(localeName)){
    stop("Invalid locale. \n Please insert at least one params between \"localeCode\" and \"localeName\".")
  }
  if(!is.null(localeCode) & (mean(nchar(format(localeCode, scientific = F))) != 2)){
    stop("Invalid localeCode. \n Please insert right \"localeCode\". It should be 2-digit numeric values.")
  }

  ## type
  all.type <- 1:5; names(all.type) <- c("Influenza", "Eye", "Food", "Asthma", "Skin")
  if(!(type %in% c("Influenza", "Eye", "Food", "Asthma", "Skin", "All")) ){
    stop('Invalid type. \n \"type\" param should be one of "Influenza", "Eye", "Food", "Asthma", "Skin", or "All"')
  }
  if(type == "All"){
    type <- c("Influenza", "Eye", "Food", "Asthma", "Skin")
  }


  ### 2. REST url
  ## End Point.
  url <- "http://apis.data.go.kr/B550928/dissForecastInfoSvc/getDissForecastInfo?"

  ## locale
  if(is.null(localeCode) & !is.null(localeName)){
    localeName <- paste(gsub("시\\b|도\\b|구\\b", "", localeName), collapse = "|")
    localeCode <- datagokR::molit_locale_code[grepl(localeName, datagokR::molit_locale_code$name),] %>%
      dplyr::select("code") %>% unlist %>% gsub(pattern = "^(\\d{2}).*", replacement = "\\1") %>% unique
  }else if(!is.null(localeCode)){
    localeCode <- format(localeCode, scientific = FALSE)
  }

  ## generate list of urls(fxxking so many limitations...).
  # 1st, (url + key)
  # 2nd, (url + key) + type. datelst by type condition.
  # 3rd, ((url + key) + type) + localeCode.
  urls <- paste(url, "serviceKey=", key, "&numOfRows=300&pageNo=1&type=xml&dissCd=", sep = "")
  urls <- outer(urls, all.type[type], paste, sep = "") %>% as.vector %>% paste("&znCd=", sep = "")
  urls <- outer(urls, localeCode, paste, sep = "") %>% as.vector


  ### 3. urls's xml parsing.
  all.data <- list(); length(all.data) <- length(urls)
  recomand <- list(); length(recomand) <- length(urls)
  meta <- data.frame(url = urls, count = "", message = "", stringsAsFactors = F) %>% # define data.frame for meta-data.
    dplyr::as.tbl()

  if(length(urls) == 1){verbose <- FALSE}
  if(verbose == T){pb <- utils::txtProgressBar(min = 1, length(urls), style = 3)}

  ## xml data parsing as list form.
  for(i in 1:length(urls)){
    # parsing xml codes with repeat and trycatch.
    tmp_xml <- datagokR:::try_xmlToList(urls[i])

    # if tmp_xml is error, go next.
    if(!is.null(tmp_xml$cmmMsgHeader$returnAuthMsg)){
      meta[i,]$count <- "error"
      meta[i,]$message <- tmp_xml$cmmMsgHeader$returnAuthMsg
      next()
    }

    # meta-data.
    meta[i,]$count <- tmp_xml$body$totalCount

    if(slow){
      Sys.sleep(stats::runif(1, 0, 1.5))
    }

    # if meta[i,]$count is "error" or 0, skip.
    if(meta[i,]$count %in% c("error", "0")){
      if(verbose == T){setTxtProgressBar(pb, value = i)}
      next
    }else{
      location <- tmp_xml$body$items

      all.data[[i]] <- data.frame(
        diss = datagokR:::find_xmlList(tmp_xml$body$items, 'dissCd'),
        date = datagokR:::find_xmlList(tmp_xml$body$items, 'dt'),
        locale = datagokR:::find_xmlList(tmp_xml$body$items, 'lowrnkZnCd'),
        cnt = datagokR:::find_xmlList(tmp_xml$body$items, 'cnt', 'num'),
        risk = datagokR:::find_xmlList(tmp_xml$body$items, 'risk', 'num'),
        stringsAsFactors = F
      ) %>% dplyr::as.tbl()

      recomand[[i]] <- data.frame(
        diss = datagokR:::find_xmlList(tmp_xml$body$items, 'dissCd'),
        risk = datagokR:::find_xmlList(tmp_xml$body$items, 'risk'),
        rcmd = datagokR:::find_xmlList(tmp_xml$body$items, 'dissRiskXpln'),
        stringsAsFactors = F
      ) %>% dplyr::as.tbl()
    } # if statement regarding to count.
    if(verbose == T){utils::setTxtProgressBar(pb, value = i)}
  } # end of loop i.

  ### 4. merge data by index type.
  data <- dplyr::bind_rows(all.data)
  recomand <- dplyr::bind_rows(recomand)
  recomand <- recomand[!duplicated(recomand),]
  recomand <- recomand[order(recomand$diss, recomand$risk),]

  result <- list(
    meta = meta,
    data = data,
    recommanded = recomand,
    plot = NULL
  )

  return(result)
}
