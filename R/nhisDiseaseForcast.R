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
#'  localeCode argument should be first 2-digit value of SiGunGu code like "11" for Seoul or "12" for Busan.
#'
#'
#' @examples
#'  key <- 'your key issued from data.go.kr'
#'
#'  # example 1 searching by localeCode.
#'  data <- nhisDiseaseForcast(key, localeCode = c(11, 41), type = "Asthma", slow = T)
#'
#'  # example 2 searching by localeName
#'  data <- nhisDiseaseForcast(key, localeName = c("수원"), type = "All", slow = T)
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr as.tbl
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr inner_join
#' @importFrom dplyr left_join
#' @importFrom dplyr right_join
#' @importFrom dplyr mutate
#' @importFrom dplyr n
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#' @importFrom utils globalVariables
#' @importFrom utils data
#' @importFrom magrittr set_colnames
#' @importFrom stats runif
#' @importFrom XML xmlToList
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
    localeName <- gsub("시\\b|도\\b|구\\b", "", localeName) %>% paste(collapse = "|")
    localeCode <- datagokR::molit_locale_code[grepl(localeName, datagokR::molit_locale_code$name),] %>%
      select("code") %>% unlist %>% gsub(pattern = "^(\\d{2}).*", replacement = "\\1") %>% unique
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
    as.tbl

  if(verbose == T){pb <- txtProgressBar(min = 1, length(urls), style = 3)}

  ## xml data parsing as list form.
  for(i in 1:length(urls)){
    # parsing xml codes with repeat and trycatch.
    ii <- 0
    repeat{
      ii <- ii + 1
      tmp.xml <- tryCatch(
        {
          xmlToList(urls[[i]])
        }, error = function(e){
          NULL
        }
      )

      if(slow){
        Sys.sleep(runif(1, 0, 2.5))
      }
      if(!is.null(tmp.xml) | ii == 15) break
    }

    # if tmp.xml is error, go next.
    if(is.null(tmp.xml)) {
      meta[i,]$count <- "error"
      meta[i,]$message <- "xml_null"
      next
    }

    # meta-data.
    meta[i,]$count <- ifelse(is.null(tmp.xml$body$totalCount)|
                               is.na(tmp.xml$body$totalCount),
                             "error", tmp.xml$body$totalCount)
    meta[i,]$message <- ifelse(is.null(tmp.xml$header$resultMsg)|
                                 is.na(tmp.xml$header$resultMsg),
                               "error", tmp.xml$header$resultMsg)

    if(slow){
      Sys.sleep(runif(1, 0, 1.5))
    }

    # if meta[i,]$count is "error" or 0, skip.
    if(meta[i,]$count %in% c("error", "0")){
      if(verbose == T){setTxtProgressBar(pb, value = i)}
      next
    }else{
      location <- tmp.xml$body$items

      all.data[[i]] <- data.frame(
        diss = unlist( lapply(location, function(x) ifelse(is.null(x$"dissCd"), NA, x$"dissCd")) ),
        date = unlist( lapply(location, function(x) ifelse(is.null(x$"dt"), NA, x$"dt")) ),
        locale = unlist( lapply(location, function(x) ifelse(is.null(x$"lowrnkZnCd"), NA, x$"lowrnkZnCd")) ),
        cnt = unlist( lapply(location, function(x) ifelse(is.null(x$"cnt"), NA, x$"cnt")) ),
        risk = unlist( lapply(location, function(x) ifelse(is.null(x$"risk"), NA, x$"risk")) ),
        stringsAsFactors = F
      ) %>% as.tbl

      recomand[[i]] <- data.frame(
        diss = unlist( lapply(location, function(x) ifelse(is.null(x$"dissCd"), NA, x$"dissCd")) ),
        risk = unlist( lapply(location, function(x) ifelse(is.null(x$"risk"), NA, x$"risk")) ),
        rcmd = unlist( lapply(location, function(x) ifelse(is.null(x$"dissRiskXpln"), NA, x$"dissRiskXpln")) ),
        stringsAsFactors = F
      ) %>% as.tbl

      recomand[[i]] <- recomand[[i]][!duplicated(recomand[[i]]),]
    } # if statement regarding to count.
    if(verbose == T){setTxtProgressBar(pb, value = i)}
  } # end of loop i.


  ### 4. merge data by index type.
  data <- bind_rows(all.data)
  recomand <- bind_rows(recomand)
  recomand <- recomand[!duplicated(recomand),] %>% arrange(diss, risk)

  result <- list(
    meta = meta,
    data = data,
    recommanded = recomand,
    plot = NULL
  )

  return(result)
}
