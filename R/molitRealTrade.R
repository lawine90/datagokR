#' Ministry of Land Infrastructure and Transport actual transition the information
#'
#' molitRealTrade function import the actual transition data of house on the law.
#'
#' @param key character value. API key issued from <www.data.go.kr>
#' @param year numeric value. the year of real trade
#' @param month numeric value. the month of real trade. default is NULL
#' @param locale numeric value. SiGunGu code which means legal area.
#' @param houseType character value. decide the type of house. it should be one of "apart", "multi", or "detached".
#' @param tradeType character value. decide the type of trade. it should be one of "trade" or "rent".
#' @param slow logical value. if TURE, give sleep inbetween importing. default is FALSE
#'
#' @return data.frame
#'
#' @details If month value is NULL, all data of the year will imported. The locale parameter recommended five numeric value.
#'
#' @examples
#'  data <- molitRealTrade(key = "my_key", year = 2018, month = 1, locale = 11110,
#'                         houseType = "apart", tradeType = "realtrade", slow = T)
#' @export
molitRealTrade <- function(key, year, month = NULL, locale, houseType, tradeType, slow = F){
  suppressWarnings(suppressMessages(library(dplyr)))
  suppressWarnings(suppressMessages(library(bindrcpp)))
  suppressWarnings(suppressMessages(library(httr))) # god bless GET and content function TT

  ### 1. parameter checking.
  if(is.null(key)){ stop("Invalid key. Please issue API key first and insert it to \"key\" param.") }
  if(!is.numeric(year) & nchar(year) != 4){ stop("Invalid year. Please insert right \"year\" param(ex: 2018)") }
  if(mean(nchar(locale)) > 5){
    warning("Five numeric value is recommended for \"locale\" param.")
    locale <- substr(locale, 1, 5) %>% as.numeric
  }
  if(!(houseType %in% c("apart", "multi", "detached"))){
    stop('Invalid houseType. \"houseType\" param should be one of "apart", "multi", or "detached"')
  }
  if(!(tradeType %in% c("trade", "rent"))){
    stop('Invalid tradeType. \"tradeType\" param should be one of "trade" or "rent"')
  }

  ### 2. REST url.
  ## End Point.
  url <- paste(
    "http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvc",
    ifelse(houseType == "apart", "Apt", ifelse(houseType == "multi", "RH", "SH")),
    tools::toTitleCase(tradeType), "?", sep = ""
  )

  ## date
  datelst <- seq.Date(from = as.Date(paste(year, '-01-01', sep = "")), to = as.Date(paste(year, '-12-01', sep = "")), by = 'month') %>%
    as.character %>% gsub("-", "", .) %>% substr(., 1, 6)

  if(!is.null(month)){
    datelst <- datelst[gsub(year, "", datelst) %in% sprintf("%02d", month)]
  }

  ## generate list of urls.
  urls <- lapply(datelst, function(x) paste(url, "serviceKey=", key, "&DEAL_YMD=", x, sep = "")) %>%
    lapply(., function(x) paste(x, "&LAWD_CD=", locale, sep = ""))
  names(urls) <- datelst

  ### 3. urls's xml parsing.
  all.data <- list(); length(all.data) <- length(datelst)
  pb <- txtProgressBar(min = 1, max = (length(locale) * length(datelst)), style = 3)
  cnt <- 0

  ## xml data parsing as list form.
  for(i in 1:length(datelst)){
    for(j in 1:length(locale)){
      tmp.xml <- GET(urls[[i]][j]) %>% content(., as = "parsed", encoding = 'UTF-8')
      Count <- tmp.xml$response$body$totalCount

      if(slow){
        Sys.sleep(runif(1, 0, 1.5))
      }

      # if the number of trade is 0, skip.
      # set location object differently according to the number of trade(1 or over.)
      if(Count == 0){

        cnt <- cnt + 1
        setTxtProgressBar(pb, value = cnt)

        next
      }else if(Count == 1){
        location <- tmp.xml$response$body$items
      }else{
        location <- tmp.xml$response$body$items$item
      } # end of if statement.

      if(tradeType == "trade"){
        if(houseType == 'apart'){
          tmp.data <- data.frame(
            Code = character(Count), Add_Code = character(Count), Dong = character(Count),
            Apart = character(Count), Price = numeric(Count), Cons_year = numeric(Count),
            excArea = numeric(Count), Floor = numeric(Count), Trade_year = numeric(Count),
            Trade_month = numeric(Count), Trade_day = character(Count),
            stringsAsFactors = F
          )

          for(k in 1:Count){
            tmp.data[k,1] <- location[[k]]$`지역코드` %>% ifelse(is.null(.), NA, .) %>% as.character
            tmp.data[k,2] <- location[[k]]$`지번` %>% ifelse(is.null(.), NA, .) %>% trimws %>% as.character
            tmp.data[k,3] <- location[[k]]$`법정동` %>% ifelse(is.null(.), NA, .) %>% trimws %>% as.character
            tmp.data[k,4] <- location[[k]]$`아파트` %>% ifelse(is.null(.), NA, .) %>% trimws %>% as.character
            tmp.data[k,6] <- location[[k]]$`건축년도` %>% ifelse(is.null(.), NA, .) %>% as.numeric
            tmp.data[k,7] <- location[[k]]$`전용면적` %>% ifelse(is.null(.), NA, .) %>% as.numeric
            tmp.data[k,8] <- location[[k]]$`층` %>% ifelse(is.null(.), NA, .) %>% as.numeric
            tmp.data[k,9] <- location[[k]]$`년` %>% ifelse(is.null(.), NA, .) %>% as.numeric
            tmp.data[k,10] <- location[[k]]$`월` %>% ifelse(is.null(.), NA, .) %>% as.numeric
            tmp.data[k,11] <- location[[k]]$`일` %>% ifelse(is.null(.), NA, .) %>% trimws %>% as.character

            tmp.data[k,5] <- location[[k]]$`거래금액` %>% trimws %>% gsub(',', '', .) %>%
              ifelse(is.null(.), NA, .) %>% as.numeric
          } # end of loop k.
        }else if(houseType == 'multi'){
          tmp.data <- data.frame(
            Code = character(Count), Add_Code = character(Count), Dong = character(Count),
            Multi = character(Count), Price = numeric(Count), Cons_year = numeric(Count),
            excArea = numeric(Count), lndArea = numeric(Count), Floor = numeric(Count),
            Trade_year = numeric(Count), Trade_month = numeric(Count), Trade_day = character(Count),
            stringsAsFactors = F
          )

          for(k in 1:Count){
            tmp.data[k,1] <- location[[k]]$`지역코드` %>% ifelse(is.null(.), NA, .) %>% as.character
            tmp.data[k,2] <- location[[k]]$`지번` %>% ifelse(is.null(.), NA, .) %>% trimws %>% as.character
            tmp.data[k,3] <- location[[k]]$`법정동` %>% ifelse(is.null(.), NA, .) %>% trimws %>% as.character
            tmp.data[k,4] <- location[[k]]$`연립다세대` %>% ifelse(is.null(.), NA, .) %>% trimws %>% as.character
            tmp.data[k,6] <- location[[k]]$`건축년도` %>% ifelse(is.null(.), NA, .) %>% as.numeric
            tmp.data[k,7] <- location[[k]]$`전용면적` %>% ifelse(is.null(.), NA, .) %>% as.numeric
            tmp.data[k,8] <- location[[k]]$`대지권면적` %>% ifelse(is.null(.), NA, .) %>% as.numeric
            tmp.data[k,9] <- location[[k]]$`층` %>% ifelse(is.null(.), NA, .) %>% as.numeric
            tmp.data[k,10] <- location[[k]]$`년` %>% ifelse(is.null(.), NA, .) %>% as.numeric
            tmp.data[k,11] <- location[[k]]$`월` %>% ifelse(is.null(.), NA, .) %>% as.numeric
            tmp.data[k,12] <- location[[k]]$`일` %>% ifelse(is.null(.), NA, .) %>% trimws %>% as.character

            tmp.data[k,5] <- location[[k]]$`거래금액` %>% trimws %>% gsub(',', '', .) %>%
              ifelse(is.null(.), NA, .) %>% as.numeric
          } # end of loop k.
        }else if(houseType == 'detached'){
          tmp.data <- data.frame(
            Code = character(Count), Dong = character(Count), Type = character(Count),
            Price = numeric(Count), Cons_year = numeric(Count), totArea = numeric(Count),
            lndArea = numeric(Count), Trade_year = numeric(Count), Trade_month = numeric(Count),
            Trade_day = character(Count), stringsAsFactors = F
          )

          for(k in 1:Count){
            tmp.data[k,1] <- location[[k]]$`지역코드` %>% ifelse(is.null(.), NA, .) %>% as.character
            tmp.data[k,2] <- location[[k]]$`법정동` %>% ifelse(is.null(.), NA, .) %>% trimws %>% as.character
            tmp.data[k,3] <- location[[k]]$`주택유형` %>% ifelse(is.null(.), NA, .) %>% trimws %>% as.character
            tmp.data[k,5] <- location[[k]]$`건축년도` %>% ifelse(is.null(.), NA, .) %>% as.numeric
            tmp.data[k,6] <- location[[k]]$`연면적` %>% ifelse(is.null(.), NA, .) %>% as.numeric
            tmp.data[k,7] <- location[[k]]$`대지면적` %>% ifelse(is.null(.), NA, .) %>% as.numeric
            tmp.data[k,8] <- location[[k]]$`년` %>% ifelse(is.null(.), NA, .) %>% as.numeric
            tmp.data[k,9] <- location[[k]]$`월` %>% ifelse(is.null(.), NA, .) %>% as.numeric
            tmp.data[k,10] <- location[[k]]$`일` %>% ifelse(is.null(.), NA, .) %>% trimws %>% as.character

            tmp.data[k,4] <- location[[k]]$`거래금액` %>% trimws %>% gsub(',', '', .) %>%
              ifelse(is.null(.), NA, .) %>% as.numeric
          } # end of loop k.
        } # end of if statement.
      }else{
        if(houseType == 'apart'){
          tmp.data <- data.frame(
            Code = character(Count), Add_Code = character(Count), Dong = character(Count),
            Apart = character(Count), rentPrice = numeric(Count), depoPrice = numeric(Count),
            Cons_year = numeric(Count), excArea = numeric(Count), Floor = numeric(Count),
            Trade_year = numeric(Count), Trade_month = numeric(Count), Trade_day = character(Count),
            stringsAsFactors = F
          )

          for(k in 1:Count){
            tmp.data[k,1] <- location[[k]]$`지역코드` %>% ifelse(is.null(.), NA, .) %>% as.character
            tmp.data[k,2] <- location[[k]]$`지번` %>% ifelse(is.null(.), NA, .) %>% trimws %>% as.character
            tmp.data[k,3] <- location[[k]]$`법정동` %>% ifelse(is.null(.), NA, .) %>% trimws %>% as.character
            tmp.data[k,4] <- location[[k]]$`아파트` %>% ifelse(is.null(.), NA, .) %>% trimws %>% as.character
            tmp.data[k,7] <- location[[k]]$`건축년도` %>% ifelse(is.null(.), NA, .) %>% as.numeric
            tmp.data[k,8] <- location[[k]]$`전용면적` %>% ifelse(is.null(.), NA, .) %>% as.numeric
            tmp.data[k,9] <- location[[k]]$`층` %>% ifelse(is.null(.), NA, .) %>% as.numeric
            tmp.data[k,10] <- location[[k]]$`년` %>% ifelse(is.null(.), NA, .) %>% as.numeric
            tmp.data[k,11] <- location[[k]]$`월` %>% ifelse(is.null(.), NA, .) %>% as.numeric
            tmp.data[k,12] <- location[[k]]$`일` %>% ifelse(is.null(.), NA, .) %>% trimws %>% as.character

            tmp.data[k,5] <- location[[k]]$`월세금액` %>% trimws %>% gsub(',', '', .) %>%
              ifelse(is.null(.), NA, .) %>% as.numeric
            tmp.data[k,6] <- location[[k]]$`보증금액` %>% trimws %>% gsub(',', '', .) %>%
              ifelse(is.null(.), NA, .) %>% as.numeric
          } # end of loop k.
        }else if(houseType == 'multi'){
          tmp.data <- data.frame(
            Code = character(Count), Add_Code = character(Count), Dong = character(Count),
            Multi = character(Count), rentPrice = numeric(Count), depoPrice = numeric(Count),
            Cons_year = numeric(Count), excArea = numeric(Count), Floor = numeric(Count),
            Trade_year = numeric(Count), Trade_month = numeric(Count), Trade_day = character(Count),
            stringsAsFactors = F
          )

          for(k in 1:Count){
            tmp.data[k,1] <- location[[k]]$`지역코드` %>% ifelse(is.null(.), NA, .) %>% as.character
            tmp.data[k,2] <- location[[k]]$`지번` %>% ifelse(is.null(.), NA, .) %>% trimws %>% as.character
            tmp.data[k,3] <- location[[k]]$`법정동` %>% ifelse(is.null(.), NA, .) %>% trimws %>% as.character
            tmp.data[k,4] <- location[[k]]$`연립다세대` %>% ifelse(is.null(.), NA, .) %>% trimws %>% as.character
            tmp.data[k,7] <- location[[k]]$`건축년도` %>% ifelse(is.null(.), NA, .) %>% as.numeric
            tmp.data[k,8] <- location[[k]]$`전용면적` %>% ifelse(is.null(.), NA, .) %>% as.numeric
            tmp.data[k,9] <- location[[k]]$`층` %>% ifelse(is.null(.), NA, .) %>% as.numeric
            tmp.data[k,10] <- location[[k]]$`년` %>% ifelse(is.null(.), NA, .) %>% as.numeric
            tmp.data[k,11] <- location[[k]]$`월` %>% ifelse(is.null(.), NA, .) %>% as.numeric
            tmp.data[k,12] <- location[[k]]$`일` %>% ifelse(is.null(.), NA, .) %>% trimws %>% as.character

            tmp.data[k,5] <- location[[k]]$`월세금액` %>% trimws %>% gsub(',', '', .) %>%
              ifelse(is.null(.), NA, .) %>% as.numeric
            tmp.data[k,6] <- location[[k]]$`보증금액` %>% trimws %>% gsub(',', '', .) %>%
              ifelse(is.null(.), NA, .) %>% as.numeric
          } # end of loop k.
        }else if(houseType == 'detached'){
          tmp.data <- data.frame(
            Code = character(Count), Dong = character(Count),
            rentPrice = numeric(Count), depoPrice = numeric(Count),
            contArea = numeric(Count), Trade_year = numeric(Count),
            Trade_month = numeric(Count), Trade_day = character(Count),
            stringsAsFactors = F
          )

          for(k in 1:Count){
            tmp.data[k,1] <- location[[k]]$`지역코드` %>% ifelse(is.null(.), NA, .) %>% as.character
            tmp.data[k,2] <- location[[k]]$`법정동` %>% ifelse(is.null(.), NA, .) %>% trimws %>% as.character
            tmp.data[k,5] <- location[[k]]$`계약면적` %>% ifelse(is.null(.), NA, .) %>% as.numeric
            tmp.data[k,6] <- location[[k]]$`년` %>% ifelse(is.null(.), NA, .) %>% as.numeric
            tmp.data[k,7] <- location[[k]]$`월` %>% ifelse(is.null(.), NA, .) %>% as.numeric
            tmp.data[k,8] <- location[[k]]$`일` %>% ifelse(is.null(.), NA, .) %>% trimws %>% as.character

            tmp.data[k,3] <- location[[k]]$`월세금액` %>% trimws %>% gsub(',', '', .) %>%
              ifelse(is.null(.), NA, .) %>% as.numeric
            tmp.data[k,4] <- location[[k]]$`보증금액` %>% trimws %>% gsub(',', '', .) %>%
              ifelse(is.null(.), NA, .) %>% as.numeric
          } # end of loop k.
        } # end of if statement.
      }

      if(is.null(all.data[[i]])){
        all.data[[i]] <- tmp.data
      }else{
        all.data[[i]] <- bind_rows(all.data[[i]], tmp.data)
      } # end of if statement.

      cnt <- cnt + 1
      setTxtProgressBar(pb, value = cnt)
    } # end of loop j.
  } # end of loop i.

  merging <- bind_rows(all.data) %>% as.tbl
  return(merging)
  cat("\nJobs Done.\n")
} # end of function.
