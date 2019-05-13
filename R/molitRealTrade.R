#' Ministry of Land Infrastructure and Transport, real estate transaction data.
#'
#' molitRealTrade function import the actual transition data of house. The function also provide simple visualization using plotly.
#'
#' @param key character value. API key issued from <www.data.go.kr>
#' @param year numeric value. the year of real trade
#' @param month numeric value. the month of real trade. default is NULL
#' @param localeCode numeric value. SiGunGu code which means legal area.
#' @param localeName character value. SiGunGu name wich means legal area. It should be Korean.
#' @param houseType character value. decide the type of house. it should be one of "apart", "multi", or "detached".
#' @param tradeType character value. decide the type of trade. it should be one of "trade" or "rent".
#' @param slow logical value. if TRUE, give sleep inbetween importing. default is FALSE
#' @param viz logical value. if TRUE, provide simple 3d visualization result. x: date, y: price, z: the number of contract.
#'
#' @return data.frame and visualization.
#'
#' @details If month value is NULL, all data of the year will imported.\cr
#'    Between localeCode and localeName, one of these parameters should be inserted. \cr
#'    The localeCode parameter recommended five numeric value.
#'    houseType parameter means the type of house. \cr
#'    "apart" means Apartment, "multi" means Multiplex house, and "detached" means detached house.
#'
#' @examples
#'  # example 1 searching by localeCode.
#'  data <- molitRealTrade(key = "my_key", year = 2018, month = 1, localeCode = 11110,
#'                         houseType = "apart", tradeType = "trade", slow = T, viz = F)
#'
#'  # example 2 searching by localeName
#'  data <- molitRealTrade(key = "my_key", year = 2018, month = 1:6, localeName = "서울",
#'                         houseType = "apart", tradeType = "rent", slow = F, viz = T)
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr as.tbl
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr if_else
#' @importFrom dplyr inner_join
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr n
#' @importFrom dplyr right_join
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#' @importFrom utils globalVariables
#' @importFrom utils data
#' @importFrom stats runif
#' @importFrom rlang .data
#'
#' @importFrom plotly plot_ly
#' @importFrom plotly layout
#'
#' @export
utils::globalVariables(c("molit_locale_code", ".data"), add = F)
molitRealTrade <- function(key, year, month = NULL, localeCode = NULL, localeName = NULL,
                           houseType, tradeType, slow = F, viz = F){
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
  datelst <- seq.Date(from = as.Date(paste(year, '-01-01', sep = "")),
                      to = as.Date(paste(year, '-12-01', sep = "")),
                      by = 'month')
  datelst <- as.character(datelst)
  datelst <- substr(gsub("-", "", datelst), start = 1, stop = 6)

  if(!is.null(month)){
    datelst <- datelst[gsub(year, "", datelst) %in% sprintf("%02d", month)]
  }

  ## locale
  if(is.null(localeCode) & !is.null(localeName)){
    localeName <- gsub("시\\b|도\\b", "", localeName) %>% paste(collapse = "|")
    localeCode <- datagokR::molit_locale_code[grepl(localeName, datagokR::molit_locale_code$name),]
    localeCode <- localeCode[localeCode$exist == "존재",] %>% select("code") %>% unlist %>% as.numeric
  }

  ## generate list of urls.
  urls <- lapply(datelst, function(x) paste(url, "serviceKey=", key, "&DEAL_YMD=", x, sep = "")) %>%
    lapply(function(x) paste(x, "&LAWD_CD=", localeCode, sep = "")) %>% unlist

  ### 3. urls's xml parsing.
  all.data <- list(); length(all.data) <- length(urls)
  pb <- txtProgressBar(min = 1, length(urls), style = 3)

  ## xml data parsing as list form.
  for(i in 1:length(urls)){
    tmp.xml <- httr::GET(urls[[i]]) %>% httr::content(as = "parsed", encoding = 'UTF-8')
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

    tmp.data <- data.frame(
      Code = character(Count),
      Dong = character(Count),
      Trade_year = numeric(Count),
      Trade_month = numeric(Count),
      Trade_day = character(Count),
      stringsAsFactors = F
    )

    # common variables(5).
    tmp.data$Code <- unlist( lapply(location, function(x) ifelse(is.null(x$"지역코드"), NA, x$"지역코드")) )
    tmp.data$Dong <- unlist( lapply(location, function(x) ifelse(is.null(x$"법정동"), NA, trimws(x$"지역코드"))) )
    tmp.data$Trade_year <- unlist( lapply(location, function(x) ifelse(is.null(x$"년"), NA, x$"년")) )
    tmp.data$Trade_month <- unlist( lapply(location, function(x) ifelse(is.null(x$"월"), NA, x$"월")) )
    tmp.data$Trade_day <- unlist( lapply(location, function(x) ifelse(is.null(x$"일"), NA, x$"일")) )

    if(tradeType == "trade"){
      # sub-common variables by trade type(2).
      tmp.data$consYear <- unlist( lapply(location, function(x) ifelse(is.null(x$"건축년도"), NA, x$"건축년도")) )
      tmp.data$Price <- unlist( lapply(location, function(x)
        ifelse(is.null(x$"거래금액"), NA, as.numeric(trimws(gsub(",", "", x$"거래금액"))))) )

      # particular variables by house type.
      if(houseType == 'apart'){ # (11)
        tmp.data$addCode <- unlist( lapply(location, function(x) ifelse(is.null(x$"지번"), NA, trimws(x$"지번"))) )
        tmp.data$Name <- unlist( lapply(location, function(x) ifelse(is.null(x$"아파트"), NA, trimws(x$"아파트"))) )
        tmp.data$excArea <- unlist( lapply(location, function(x) ifelse(is.null(x$"전용면적"), NA, trimws(x$"전용면적"))) )
        tmp.data$Floor <- unlist( lapply(location, function(x) ifelse(is.null(x$"층"), NA, trimws(x$"층"))) )
      }else if(houseType == 'multi'){ # (12)
        tmp.data$addCode <- unlist( lapply(location, function(x) ifelse(is.null(x$"지번"), NA, trimws(x$"지번"))) )
        tmp.data$Name <- unlist( lapply(location, function(x) ifelse(is.null(x$"연립다세대"), NA, trimws(x$"연립다세대"))) )
        tmp.data$excArea <- unlist( lapply(location, function(x) ifelse(is.null(x$"전용면적"), NA, trimws(x$"전용면적"))) )
        tmp.data$grdArea <- unlist( lapply(location, function(x) ifelse(is.null(x$"대지권면적"), NA, trimws(x$"대지권면적"))) )
        tmp.data$Floor <- unlist( lapply(location, function(x) ifelse(is.null(x$"층"), NA, trimws(x$"층"))) )
      }else if(houseType == 'detached'){ # (10)
        tmp.data$Type <- unlist( lapply(location, function(x) ifelse(is.null(x$"주택유형"), NA, trimws(x$"주택유형"))) )
        tmp.data$totArea <- unlist( lapply(location, function(x) ifelse(is.null(x$"연면적"), NA, trimws(x$"연면적"))) )
        tmp.data$plottage <- unlist( lapply(location, function(x) ifelse(is.null(x$"대지면적"), NA, trimws(x$"대지면적"))) )
      } # end of if statement.
    }else{
      # sub-common variables by trade type(2).
      tmp.data$rentPrice <- unlist( lapply(location, function(x)
        ifelse(is.null(x$"월세금액"), NA, as.numeric(trimws(gsub(",", "", x$"월세금액"))))) )
      tmp.data$depoPrice <- unlist( lapply(location, function(x)
        ifelse(is.null(x$"보증금액"), NA, as.numeric(trimws(gsub(",", "", x$"보증금액"))))) )

      if(houseType == 'apart'){
        tmp.data$consYear <- unlist( lapply(location, function(x) ifelse(is.null(x$"건축년도"), NA, x$"건축년도")) )
        tmp.data$addCode <- unlist( lapply(location, function(x) ifelse(is.null(x$"지번"), NA, trimws(x$"지번"))) )
        tmp.data$Name <- unlist( lapply(location, function(x) ifelse(is.null(x$"아파트"), NA, trimws(x$"아파트"))) )
        tmp.data$excArea <- unlist( lapply(location, function(x) ifelse(is.null(x$"전용면적"), NA, trimws(x$"전용면적"))) )
        tmp.data$Floor <- unlist( lapply(location, function(x) ifelse(is.null(x$"층"), NA, trimws(x$"층"))) )
      }else if(houseType == 'multi'){
        tmp.data$consYear <- unlist( lapply(location, function(x) ifelse(is.null(x$"건축년도"), NA, x$"건축년도")) )
        tmp.data$addCode <- unlist( lapply(location, function(x) ifelse(is.null(x$"지번"), NA, trimws(x$"지번"))) )
        tmp.data$Name <- unlist( lapply(location, function(x) ifelse(is.null(x$"연립다세대"), NA, trimws(x$"연립다세대"))) )
        tmp.data$excArea <- unlist( lapply(location, function(x) ifelse(is.null(x$"전용면적"), NA, trimws(x$"전용면적"))) )
        tmp.data$Floor <- unlist( lapply(location, function(x) ifelse(is.null(x$"층"), NA, trimws(x$"층"))) )
      }else if(houseType == 'detached'){
        tmp.data$contArea <- unlist( lapply(location, function(x) ifelse(is.null(x$"계약면적"), NA, trimws(x$"계약면적"))) )
      } # end of if statement.
    }

    if(is.null(all.data[[i]])){
      all.data[[i]] <- tmp.data
    }else{
      all.data[[i]] <- bind_rows(all.data[[i]], tmp.data)
    } # end of if statement.

    setTxtProgressBar(pb, value = i)
  } # end of loop i.

  re.da <- bind_rows(all.data) %>% as.tbl
  re.da <- re.da %>% mutate("Code" = as.integer(re.da$Code)) %>%
    left_join(y = molit_locale_code[,c('code', 'name')], by = c("Code" = "code"))

  result <- list(
    data = re.da,
    plot = NULL
  )

  if(viz == T){
    tmp.m <- result$data[,grepl("name|Dong|Price|Trade", colnames(result$data))]

    tmp.m$Trade_day <- tmp.m$Trade_day %>% strsplit(split = "~") %>%
      lapply(function(x) as.numeric(x) %>% mean %>% round) %>% unlist
    tmp.m$Date <- paste(tmp.m$Trade_year, tmp.m$Trade_month, tmp.m$Trade_day, sep = "-") %>% as.Date
    tmp.m$Location <- as.factor(tmp.m$name)

    tmp.m <- tmp.m[,grepl("Dong|Price|Date|Location", colnames(tmp.m))]

    if(tradeType == "trade"){
      tmp.m.g <- tmp.m %>% group_by(.data$Location, .data$Date) %>%
        summarise("Price" = mean(.data$Price)*10000, "Contract" = n()) %>% ungroup

      result$plot <- list(
        price = plot_ly(data = tmp.m.g, x = ~tmp.m.g$Date, y = ~tmp.m.g$Price, z = ~tmp.m.g$Contract,
                        color = ~tmp.m.g$Location, size = ~tmp.m.g$Price,
                        text = ~paste("Address: ", tmp.m.g$Location, "<br>Price: ",
                                      round(tmp.m.g$Price), "<br>Count: ", tmp.m.g$Contract)) %>%
          layout(title = paste(year, "molit Trade Data(from data.go.kr)"),
                 scene = list(xaxis = list(title = "Date"),
                              yaxis = list(title = "Price"),
                              zaxis = list(title = "N")))
      )
    }else{
      tmp.m.g <- tmp.m %>% group_by(.data$Location, .data$Date) %>%
        summarise("rentPrice" = mean(.data$rentPrice)*10000,
                  "depoPrice" = mean(.data$depoPrice)*10000,
                  "Contract" = n()) %>% ungroup

      result$plot <- list(
        price = plot_ly(data = tmp.m.g, x = ~tmp.m.g$Date, y = ~tmp.m.g$rentPrice, z = ~tmp.m.g$Contract,
                        color = ~tmp.m.g$Location, size = ~tmp.m.g$rentPrice,
                        text = ~paste("Address: ", tmp.m.g$Location, "<br>Rental Price: ",
                                      round(tmp.m.g$rentPrice), "<br>Count: ", tmp.m.g$Contract)) %>%
          layout(title = paste(year, "molit Rental Price Data(from data.go.kr)"),
                 scene = list(xaxis = list(title = "Date"),
                              yaxis = list(title = "Rental Price"),
                              zaxis = list(title = "N"))),

        deposit = plot_ly(data = tmp.m.g, x = ~tmp.m.g$Date, y = ~tmp.m.g$depoPrice, z = ~tmp.m.g$Contract,
                          color = ~tmp.m.g$Location, size = ~tmp.m.g$depoPrice,
                          text = ~paste("Address: ", tmp.m.g$Location, "<br>Deposit: ",
                                        round(tmp.m.g$depoPrice), "<br>Count: ", tmp.m.g$Contract)) %>%
          layout(title = paste(year, "molit Deposit Data(from data.go.kr)"),
                 scene = list(xaxis = list(title = "Date"),
                              yaxis = list(title = "Deposit"),
                              zaxis = list(title = "N")))
      )
    }
  }
  return(result)
  cat("\nJobs Done.\n")
} # end of function.
