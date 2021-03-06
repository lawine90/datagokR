#' Korea Meteorological Administration, The health index.
#'
#' kmaLifeIndex function import the health index data last 2 days. The function also provide simple visualization using plotly.
#'
#' @param key character value. API key issued from <www.data.go.kr>. no default.
#' @param localeCode numeric value. SiGunGu code which means legal area. one of localeCode or localeName should be inserted.
#' @param localeName character value. SiGunGu name wich means legal area. one of localeCode or localeName should be inserted. It should be Korean.
#' @param type character value. decide the type of index. it should be one of
#'             "Asthma", "Brain", "Skin", "FlowerWoody", "FlowerPine", "FlowerWeeds", "Infl" or "possible". see details.
#' @param slow logical value. if TRUE, give sleep inbetween importing. default is TRUE.
#' @param viz logical value. if TRUE, provide simple 2d visualization result. x: date, y: mean index.
#' @param verbose logical value. if TRUE, provide process bar. Default value set as false.
#'
#' @return data.frame and visualization.
#'
#' @details kmaLifeIndex function import eight index value which calculated by Korea Meteorological Administration and related to public's life.\cr
#'  Explanation about "type" as follow.\cr
#'  "Asthma" = Possibility index of asthma or lung disease. It is povided the whole year.\cr
#'  "Brain" = Possibility index of stroke. It is povided the whole year.\cr
#'  "Skin" = Possibility index of skin disease. It is povided the whole year.\cr
#'  "FlowerWoody" = Risk index of oak tree's pollen. It is povided from Apr to May.\cr
#'  "FlowerPine" = Risk index of pine tree's pollen. It is povided from Apr to May.\cr
#'  "FlowerWeeds" = Risk index of weed's pollen. It is povided from Sep to Oct.\cr
#'  "Infl" = Possibility index of influenza. It is povided from Sep to Apr.\cr
#'  "possible" = All index which affordable now.\cr
#'  See more details about each index from <http://www.weather.go.kr/weather/lifenindustry/life_jisu.jsp#> \cr\cr
#'  Importing data from KMA is so unstable that it is recommended to checking error urls in result.
#'
#' @examples
#'  # example 1 searching by localeCode
#'  key <- 'your key issued from data.go.kr'
#'  data <- kmaHealthIndex(key, localeCode = c(4111100000, 4111156600),
#'                         type = "Asthma", slow = TRUE)
#'
#'  # example 2 searching by localeName
#'  data <- kmaHealthIndex(key, localeName = c(enc2utf8("수원")), type = "possible", slow = TRUE)
#'
#' @importFrom dplyr %>%
#' @export

kmaHealthIndex <- function(key, localeCode = NULL, localeName = NULL, type, slow = T, viz = F, verbose = F){
  ### 1. parameter checking and processing.
  ## key
  if(is.null(key)){ stop("Invalid key. \n Please issue API key first and insert it to \"key\" param.") }

  ## localeCode, localeName
  if(is.null(localeCode) & is.null(localeName)){
    stop("Invalid locale. \n Please insert at least one params between \"localeCode\" and \"localeName\".")
  }
  if(!is.null(localeCode) & (mean(nchar(format(localeCode, scientific = F))) != 10)){
    stop("Invalid localeCode. \n Please insert right \"localeCode\". It should be 10-digit numeric values.")
  }

  ## type
  # data("data_kma_lifeIndex")
  month <- as.numeric(strsplit(as.character(Sys.Date()), "-") %>% unlist)[2]
  if(!(type %in% c(rownames(datagokR::kma_healthIndex_type_check), "possible")) ){
    stop('Invalid type. \n \"type\" param should be one of ',
         paste('"', rownames(datagokR::kma_healthIndex_type_check),
               '"', sep = "", collapse = ", "), ' or "possible"')
  }
  if(type == "possible"){
    type <- datagokR::kma_healthIndex_type_check[,month] %>% which %>% names
  }
  if(datagokR::kma_healthIndex_type_check[type, month] %>% any == F){
    stop("Inappropriate type. you can't import ", type, "-type data at ", month.name[month],
         "\n it can be imported at ", paste(month.name[datagokR::kma_healthIndex_type_check[type,]], collapse = ", "), " only.")
  }

  ## time
  time <- sprintf("%02d", c(6, 18))

  ### 2. REST url
  ## End Point.
  url <- paste("http://newsky2.kma.go.kr/iros/RetrieveWhoIndexService2/get", type, "WhoList?", sep = "")

  ## date time(only yesterday).
  datelst <- c(Sys.Date() - 1) %>% gsub(pattern = "-", replacement = "") %>%
    outer(time, paste, sep = "") %>% as.vector %>% sort

  # remove not comming datelst.
  datelst <- datelst[strptime(datelst,format='%Y%m%d%H') <= Sys.time()]

  ## locale
  if(is.null(localeCode) & !is.null(localeName)){
    localeName <- gsub("시\\b|도\\b|구\\b", "", localeName) %>% paste(collapse = "|")
    localeCode <- datagokR::kma_locale_code[grepl(localeName,
                                                  paste(datagokR::kma_locale_code$name1,
                                                  datagokR::kma_locale_code$name2, sep = " ")),] %>%
      dplyr::select("code") %>% unlist
  }else if(!is.null(localeCode)){
    localeCode <- format(localeCode, scientific = FALSE)
  }

  ## generate list of urls(fxxking so many limitations...).
  # 1st, (url + key)
  # 2nd, (url + key) + datelst. datelst by type condition.
  # 3rd, ((url + key) + datelst) + localeCode.
  urls <- paste(url, "serviceKey=", key, "&time=", sep = "")
  urls <- outer(urls, datelst, paste, sep = "") %>% as.vector %>% paste("&areaNo=", sep = "")
  urls <- outer(urls, localeCode, paste, sep = "") %>% as.vector

  ### 3. urls's xml parsing.
  all.data <- list(); length(all.data) <- length(urls)
  all.error <- list(); length(all.error) <- length(urls)
  errors <- list(); length(errors) <- length(urls)
  suc <- character(length(urls))
  meta <- data.frame(url = urls, success = "", message = "", stringsAsFactors = F) %>% # define data.frame for meta-data.
    dplyr::as.tbl()

  if(verbose == T){pb <- utils::txtProgressBar(min = 1, length(urls), style = 3)}

  ## xml data parsing as list form.
  for(i in 1:length(urls)){
    tmp.xml <- datagokR:::try_xmlToList(urls[i])

    # if tmp.xml is error, go next.
    if(tmp.xml$Header$SuccessYN == 'N') {
      errors[[i]] <- urls[[i]]
      all.error[[i]] <- tmp.xml$Header$ErrMsg
      meta[i,]$success <- "error"
      warning(tmp.xml$Header$ErrMsg)
      if(verbose == T){utils::setTxtProgressBar(pb, value = i)}
      next
    }else if(tmp.xml$Header$SuccessYN == 'Y'){
      suc[i] <- tmp.xml$Header$SuccessYN
      meta[i,]$success <- suc[i]
    }

    if(slow){
      Sys.sleep(stats::runif(1, 0, 1.5))
    }

    # if suc is "N", skip.
    if(suc[i] == "N"){
      next
    }else if(suc[i] =="Y"){
      location <- tmp.xml$Body$IndexModel
      all.data[[i]] <- data.frame(
          idxCode = location$code,
          type = gsub(".*get(.*)WhoList.*", "\\1", urls[i]),
          locale = as.character(location$areaNo),
          time = location$date,
          #datagokR:::find_xmlList(location, 'today')
          d0 = ifelse(is.null(location$today), NA, location$today) %>% as.numeric,
          d1 = ifelse(is.null(location$tomorrow), NA, location$tomorrow) %>% as.numeric,
          d2 = ifelse(is.null(location$theDayAfterTomorrow), NA, location$theDayAfterTomorrow) %>% as.numeric,
          stringsAsFactors = F
      )
    } # if statement regarding to SuccessYN.
    if(verbose == T){utils::setTxtProgressBar(pb, value = i)}
  } # end of loop i.

  ### 4. merge data by index type.
  data <- list(); length(data) <- length(type)
  for(i in 1:length(data)){
    tmp.d <- dplyr::bind_rows(all.data[lapply(all.data, function(x)
      x$type == type[i]) %>% unlist %>% which]) %>% dplyr::as.tbl()

    if(nrow(tmp.d) == 0){
      next
    }

    data[[i]] <- tmp.d %>%
      dplyr::mutate("time" = strptime(.data$time, format='%Y%m%d%H') %>% as.character,
             "locale" = .data$locale)

    data[[i]] <- data[[i]][!duplicated(data[[i]]),] %>% arrange(.data$locale, .data$time)

    # 0: low, 1: normal, 2: high, 3: very high.
    data[[i]]$level <- cut(data[[i]]$d0, breaks = c(0, 1, 2, 3, Inf), right = F,
                           labels = c("low", "normal", "high", "very high"), ordered_result = T)


  }; names(data) <- type

  result <- list(
    data = data,
    plot = NULL,
    errors = unlist(errors),
    urls = urls
  )

  # if(viz){
  #   suppressWarnings(suppressMessages(library(ggplot2)))
  #   suppressWarnings(suppressMessages(library(mapproj)))
  #   data("maps")
  #
  #   plot <- list(); length(plot) <- length(type)
  #
  #   for(i in 1:length(type)){
  #     kst <- unique(data[[i]]$time)[which.min(abs(as.POSIXct(unique(data[[i]]$time)) - Sys.time()))]
  #
  #
  #     # if locale is over 1000, use sgg map.
  #     # else, use emd map.
  #     if(length(localeCode) >= 1200){
  #       data[[i]] <- data[[i]][,1:5] %>% set_colnames(c("idx", "type", "id", "time", "value")) %>%
  #         mutate(id = substr(id, 1, 5)) %>% filter(time == kst) %>% group_by(id) %>%
  #         summarise(value = mean(value)) %>% ungroup %>%
  #         right_join(., sgg) %>% arrange(order)
  #     }else{
  #       data[[i]] <- data[[i]][,1:5] %>% set_colnames(c("idx", "type", "id", "time", "value")) %>%
  #         mutate(id = substr(id, 1, 8)) %>% filter(time == kst) %>% group_by(id) %>%
  #         summarise(value = mean(value)) %>% ungroup %>%
  #         right_join(., emd) %>% arrange(order) %>%
  #         filter(id %in% substr(localeCode, 1, 8))
  #     }
  #
  #     plot[[i]] <- ggplot(data = data[[i]], aes(x = long, y = lat, group = group, fill = value)) +
  #       geom_polygon(color = "gray", size = 0.001) +
  #       scale_fill_gradient(low = "green", high = 'red', guide = F) +
  #       theme_void() +coord_quickmap()
  #   } # for i.
  #   names(plot) <- type
  #   result$plot <- plot
  # } # if viz == T.
  return(result)
}
