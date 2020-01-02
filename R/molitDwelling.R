#' Ministry of Land Infrastructure and Transport, real estate for dwelling transaction data.
#'
#' molitDwelling function import the actual transition data of house. The function also provide simple visualization using plotly.
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
#' @return two data.frame for meta-data and imported data.
#' two vectors for error urls and all urls. visualization.
#'
#' @details If month value is NULL, all data of the year will imported.\cr
#'    Between localeCode and localeName, one of these parameters should be inserted. \cr
#'    The localeCode parameter recommended five numeric value.
#'    houseType parameter means the type of house. \cr
#'    "apart" means Apartment, "multi" means Multiplex house, and "detached" means detached house.
#'
#' @examples
#'  # example 1 searching by localeCode.
#'  data <- molitDwelling(key = "my_key", year = 2018, month = 1, localeCode = 11110,
#'                         houseType = "apart", tradeType = "trade", slow = T, viz = F)
#'
#'  # example 2 searching by localeName
#'  data <- molitDwelling(key = "my_key", year = 2018, month = 1:6, localeName = enc2utf8("서울"),
#'                         houseType = "apart", tradeType = "rent", slow = F, viz = T)
#'
#' @importFrom dplyr %>%
#' @importFrom plotly plot_ly
#' @importFrom plotly layout
#'
#' @export

# utils::globalVariables(c(".data"), add = F)
molitDwelling <- function(key, year, month = NULL, localeCode = NULL, localeName = NULL,
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
  errors <- list(); length(errors) <- length(urls)                     # define vector for error urls.
  meta <- data.frame(url = urls, count = "", stringsAsFactors = F) %>% # define data.frame for meta-data.
    dplyr::as.tbl()

  pb <- utils::txtProgressBar(min = 1, length(urls), style = 3)

  ## xml data parsing as list form.
  for(i in 1:length(urls)){
    tmp.xml <- datagokR:::try_GET_content(urls[i])

    # if tmp.xml is error, go next.
    if(tmp_xml$response$header$resultMsg != "NORMAL SERVICE."){
      errors[[i]] <- urls[[i]]
      meta[i,]$count <- "error"
      next
    }

    Count <- tmp.xml$response$body$totalCount
    meta[i,]$count <- ifelse(is.null(Count)|is.na(Count),"error", Count)

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

    tmp.data <- data.frame(
      Code = character(Count),
      Dong = character(Count),
      Trade_year = numeric(Count),
      Trade_month = numeric(Count),
      Trade_day = character(Count),
      stringsAsFactors = F
    )

    # common variables for all houseType and tradeType(5 variables).
    tmp.data$Code <- datagokR:::find_xmlList(location, '지역코드')
    tmp.data$Dong <- datagokR:::find_xmlList(location, '법정동')
    tmp.data$Trade_year <- datagokR:::find_xmlList(location, '년')
    tmp.data$Trade_month <- datagokR:::find_xmlList(location, '월')
    tmp.data$Trade_day <- datagokR:::find_xmlList(location, '일')
    tmp.data$consYear <- datagokR:::find_xmlList(location, '건축년도')

    if(tradeType == "trade"){
      # sub-common variables by trade type(2).
      tmp.data$Price <- as.numeric(trimws(gsub(",", "", datagokR:::find_xmlList(location, '거래금액'))))

      # particular variables by house type.
      if(houseType == 'apart'){ # (11)
        tmp.data$addCode <- trimws(datagokR:::find_xmlList(location, '지번'))
        tmp.data$Name <- trimws(datagokR:::find_xmlList(location, '아파트'))
        tmp.data$excArea <- trimws(datagokR:::find_xmlList(location, '전용면적'))
        tmp.data$Floor <- trimws(datagokR:::find_xmlList(location, '층'))
      }else if(houseType == 'multi'){ # (12)
        tmp.data$addCode <- trimws(datagokR:::find_xmlList(location, '지번'))
        tmp.data$Name <- trimws(datagokR:::find_xmlList(location, '연립다세대'))
        tmp.data$excArea <- trimws(datagokR:::find_xmlList(location, '전용면적'))
        tmp.data$grdArea <- trimws(datagokR:::find_xmlList(location, '대지권면적'))
        tmp.data$Floor <- trimws(datagokR:::find_xmlList(location, '층'))
      }else if(houseType == 'detached'){ # (10)
        tmp.data$Type <- trimws(datagokR:::find_xmlList(location, '주택유형'))
        tmp.data$totArea <- trimws(datagokR:::find_xmlList(location, '연면적'))
        tmp.data$plottage <- trimws(datagokR:::find_xmlList(location, '대지면적'))
      } # end of if statement.
    }else{
      # sub-common variables by trade type(2).
      tmp.data$rentPrice <- as.numeric(trimws(gsub(",", "", datagokR:::find_xmlList(location, '월세금액'))))
      tmp.data$depoPrice <- as.numeric(trimws(gsub(",", "", datagokR:::find_xmlList(location, '보증금액'))))

      if(houseType == 'apart'){
        tmp.data$addCode <- trimws(datagokR:::find_xmlList(location, '지번'))
        tmp.data$Name <- trimws(datagokR:::find_xmlList(location, '아파트'))
        tmp.data$excArea <- trimws(datagokR:::find_xmlList(location, '전용면적'))
        tmp.data$Floor <- trimws(datagokR:::find_xmlList(location, '층'))
      }else if(houseType == 'multi'){
        tmp.data$addCode <- trimws(datagokR:::find_xmlList(location, '지번'))
        tmp.data$Name <- trimws(datagokR:::find_xmlList(location, '연립다세대'))
        tmp.data$excArea <- trimws(datagokR:::find_xmlList(location, '전용면적'))
        tmp.data$Floor <- trimws(datagokR:::find_xmlList(location, '층'))
      }else if(houseType == 'detached'){
        tmp.data$contArea <- trimws(datagokR:::find_xmlList(location, '계약면적'))
      } # end of if statement.
    }

    if(is.null(all.data[[i]])){
      all.data[[i]] <- tmp.data
    }else{
      all.data[[i]] <- dplyr::bind_rows(all.data[[i]], tmp.data)
    } # end of if statement.

    utils::setTxtProgressBar(pb, value = i)
  } # end of loop i.

  result <- list(
    meta = meta,
    data = NULL,
    plot = NULL,
    errors = NULL,
    urls = urls
  )

  re.da <- dplyr::bind_rows(all.data) %>% dplyr::as.tbl()

  if(nrow(re.da) != 0){
    re.da <- re.da %>% dplyr::mutate("Code" = as.integer(re.da$Code)) %>%
      dplyr::left_join(y = datagokR::molit_locale_code[,c('code', 'name')], by = c("Code" = "code"))

    result$data <- re.da
    result$errors <- unlist(errors)

    if(viz == T){
      tmp.m <- result$data[,grepl("name|Dong|Price|Trade", colnames(result$data))]

      tmp.m$Trade_day <- tmp.m$Trade_day %>% strsplit(split = "~") %>%
        lapply(function(x) as.numeric(x) %>% mean %>% round) %>% unlist
      tmp.m$Date <- paste(tmp.m$Trade_year, tmp.m$Trade_month, tmp.m$Trade_day, sep = "-") %>% as.Date
      tmp.m$Location <- as.factor(tmp.m$name)

      tmp.m <- tmp.m[,grepl("Dong|Price|Date|Location", colnames(tmp.m))]

      if(tradeType == "trade"){
        tmp.m.g <- tmp.m %>% dplyr::group_by(.data$Location, .data$Date) %>%
          dplyr::summarise("Price" = mean(.data$Price)*10000, "Contract" = dplyr::n()) %>% dplyr::ungroup

        result$plot <- list(
          price = plotly::plot_ly(data = tmp.m.g, x = ~tmp.m.g$Date, y = ~tmp.m.g$Price, z = ~tmp.m.g$Contract,
                          color = ~tmp.m.g$Location, size = ~tmp.m.g$Price,
                          text = ~paste("Address: ", tmp.m.g$Location, "<br>Price: ",
                                        round(tmp.m.g$Price), "<br>Count: ", tmp.m.g$Contract)) %>%
            plotly::layout(title = paste(year, "molit Trade Data(from data.go.kr)"),
                          scene = list(xaxis = list(title = "Date"),
                                       yaxis = list(title = "Price"),
                                        zaxis = list(title = "N")))
        )
      }else{
        tmp.m.g <- tmp.m %>% dplyr::group_by(.data$Location, .data$Date) %>%
          dplyr::summarise("rentPrice" = mean(.data$rentPrice)*10000,
                           "depoPrice" = mean(.data$depoPrice)*10000,
                           "Contract" = n()) %>% dplyr::ungroup

        result$plot <- list(
          price = plotly::plot_ly(data = tmp.m.g, x = ~tmp.m.g$Date, y = ~tmp.m.g$rentPrice, z = ~tmp.m.g$Contract,
                          color = ~tmp.m.g$Location, size = ~tmp.m.g$rentPrice,
                          text = ~paste("Address: ", tmp.m.g$Location, "<br>Rental Price: ",
                                        round(tmp.m.g$rentPrice), "<br>Count: ", tmp.m.g$Contract)) %>%
            plotly::layout(title = paste(year, "molit Rental Price Data(from data.go.kr)"),
                           scene = list(xaxis = list(title = "Date"),
                                        yaxis = list(title = "Rental Price"),
                                        zaxis = list(title = "N"))),

          deposit = plotly::plot_ly(data = tmp.m.g, x = ~tmp.m.g$Date, y = ~tmp.m.g$depoPrice, z = ~tmp.m.g$Contract,
                            color = ~tmp.m.g$Location, size = ~tmp.m.g$depoPrice,
                            text = ~paste("Address: ", tmp.m.g$Location, "<br>Deposit: ",
                                          round(tmp.m.g$depoPrice), "<br>Count: ", tmp.m.g$Contract)) %>%
            plotly::layout(title = paste(year, "molit Deposit Data(from data.go.kr)"),
                           scene = list(xaxis = list(title = "Date"),
                                       yaxis = list(title = "Deposit"),
                                       zaxis = list(title = "N")))
        )
      } # if tradeType == "trade
    } # if viz == T
  } # if nrow(re.na) != 0

  return(result)
  cat("\nJobs Done.\n")
} # end of function.
