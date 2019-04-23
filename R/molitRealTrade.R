#' Ministry of Land Infrastructure and Transport, real estate transaction data.
#'
#' molitRealTrade function import the actual transition data of house. The function also provide simple visualization using plotly.
#'
#' @param key character value. API key issued from <www.data.go.kr>
#' @param year numeric value. the year of real trade
#' @param month numeric value. the month of real trade. default is NULL
#' @param localeCode numeric value. SiGunGu code which means legal area.
#' @param localeName character value. SiGunGu name wich means legal area.
#' @param houseType character value. decide the type of house. it should be one of "apart", "multi", or "detached".
#' @param tradeType character value. decide the type of trade. it should be one of "trade" or "rent".
#' @param slow logical value. if TRUE, give sleep inbetween importing. default is FALSE
#' @param viz logical value. if TRUE, provide simple 3d visualization result. x: date, y: price, z: the number of contract.
#'
#' @return data.frame and visualization.
#'
#' @details If month value is NULL, all data of the year will imported.
#'    Between localeCode and localeName, one of these parameters should be inserted. The localeCode parameter recommended five numeric value.
#'    houseType parameter means the type of house. "apart" means Apartment(아파트), "multi" means Multiplex house(연립다세대), and "detached" means detached house(단독주택)
#'
#' @examples
#'  # example 1 searching by localeCode.
#'  data <- molitRealTrade(key = "my_key", year = 2018, month = 1, localeCode = 11110,
#'                         houseType = "apart", tradeType = "trade", slow = T, viz = F)
#'
#'  # example 2 searching by localeName
#'  data <- molitRealTrade(key = "my_key", year = 2018, month = 1:6, localeName = "서울",
#'                         houseType = "apart", tradeType = "rent", slow = F, viz = T)
#' @export
molitRealTrade <- function(key, year, month = NULL, localeCode = NULL, localeName = NULL,
                           houseType, tradeType, slow = F, viz = F){
  suppressWarnings(suppressMessages(library(dplyr)))
  suppressWarnings(suppressMessages(library(bindrcpp)))
  suppressWarnings(suppressMessages(library(httr)))

  ### 1. parameter checking.
  if(is.null(key)){ stop("Invalid key. Please issue API key first and insert it to \"key\" param.") }
  if(!is.numeric(year) & nchar(year) != 4){ stop("Invalid year. Please insert right \"year\" param(ex: 2018)") }
  if(is.null(localeCode) & is.null(localeName)){ stop("Invalid locale. Please insert at least one params between \"localeCode\" and \"localeName\"") }
  if(!is.null(localeCode) & (mean(nchar(localeCode)) > 5)){
    warning("Five numeric value is recommended for \"localeCode\" param.")
    localeCode <- substr(localeCode, 1, 5) %>% as.numeric
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

  ## locale
  if(is.null(localeCode) & !is.null(localeName)){
    data("molit_realTrade") # should be updated regularly.
    localeName <- gsub("시\\b|도\\b", "", localeName) %>% paste(., collapse = "|")
    localeCode <- molit_locale_code[grepl(localeName, molit_locale_code$name),] %>%
      filter(exist == "존재") %>% select(code) %>% unlist %>% as.numeric
  }

  ## generate list of urls.
  urls <- lapply(datelst, function(x) paste(url, "serviceKey=", key, "&DEAL_YMD=", x, sep = "")) %>%
    lapply(., function(x) paste(x, "&LAWD_CD=", localeCode, sep = "")) %>% unlist

  ### 3. urls's xml parsing.
  all.data <- list(); length(all.data) <- length(urls)
  pb <- txtProgressBar(min = 1, length(urls), style = 3)

  ## xml data parsing as list form.
  for(i in 1:length(urls)){
    tmp.xml <- GET(urls[[i]]) %>% content(., as = "parsed", encoding = 'UTF-8')
    Count <- tmp.xml$response$body$totalCount

    if(slow){
      Sys.sleep(runif(1, 0, 1.5))
    }

    # if the number of trade is 0, skip.
    # set location object differently according to the number of trade(1 or over.)
    if(Count == 0){
      setTxtProgressBar(pb, value = i)
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

    setTxtProgressBar(pb, value = i)
  } # end of loop i.

  result <- list(
    data = bind_rows(all.data) %>% as.tbl %>% mutate(Code = as.integer(Code)) %>%
      left_join(., molit_locale_code[,c('code', 'name')], by = c("Code" = "code"))
  )

  if(viz == T){
    library(plotly)

    tmp.m <- result$data[,grepl("name|Dong|Price|Trade", colnames(result$data))]
    tmp.m <- tmp.m %>% mutate(Date = Trade_day %>% strsplit(., "~") %>%
                      lapply(., function(x) as.numeric(x) %>% mean %>% round) %>%
                      paste(Trade_year, Trade_month, ., sep = "-") %>% as.Date,
                      Trade_year = NULL, Trade_month = NULL, Trade_day = NULL,
                      Location = as.factor(name))

    if(tradeType == "trade"){
      tmp.m.g <- tmp.m %>% group_by(Location, Date) %>% summarise(Price = mean(Price)*10000, Contract = n())

      result$Price <- plot_ly(data = tmp.m.g, x = ~Date, y = ~Price, z = ~Contract, color = ~Location, size = ~Price,
                   #mode = "markers", type = "scatter",
                   text = ~paste("Address: ", Location, "<br>Price: ", round(Price), "<br>Count: ", Contract)) %>%
        layout(title = paste(year, "molit Trade Data(from data.go.kr)"))
    }else{
      tmp.m.g <- tmp.m %>% group_by(Location, Date) %>%
        summarise(rentPrice = mean(rentPrice)*10000, depoPrice = mean(depoPrice)*10000, Contract = n())

      result$Price = plot_ly(data = tmp.m.g, x = ~Date, y = ~rentPrice, z = ~Contract, color = ~Location, size = ~rentPrice,
                      text = ~paste("Address: ", Location, "<br>Rental Price: ", round(rentPrice), "<br>Count: ", Contract)) %>%
        layout(title = paste(year, "molit Rental Price Data(from data.go.kr)"))

      result$Deposit = plot_ly(data = tmp.m.g, x = ~Date, y = ~depoPrice, z = ~Contract, color = ~Location, size = ~depoPrice,
                               text = ~paste("Address: ", Location, "<br>Deposit: ", round(depoPrice), "<br>Count: ", Contract)) %>%
        layout(title = paste(year, "molit Deposit Data(from data.go.kr)"))
    }
  }
  return(result)
  cat("\nJobs Done.\n")
} # end of function.
