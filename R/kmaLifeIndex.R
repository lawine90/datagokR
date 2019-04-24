#' Korea Meteorological Administration, The life weather index.
#'
#' kmaLifeIndex function import the life weather index data last 2 days. The function also provide simple visualization using plotly.
#'
#' @param key character value. API key issued from <www.data.go.kr>. no default.
#' @param time numeric value. The time when the data is generated.
#' @param localeCode numeric value. SiGunGu code which means legal area. one of localeCode or localeName should be inserted.
#' @param localeName character value. SiGunGu name wich means legal area. one of localeCode or localeName should be inserted. It should be Korean.
#' @param type character value. decide the type of index. it should be one of "fp", "st", "hi", "di", "ui", "fb", "ap", "sh" or "possible". see details.
#' @param slow logical value. if TRUE, give sleep inbetween importing. default is TRUE.
#' @param viz logical value. if TRUE, provide simple 2d visualization result. x: date, y: mean index.
#'
#' @return data.frame and visualization.
#'
#' @details kmaLifeIndex function import eight index value which calculated by Korea Meteorological Administration and related to public's life.
#'  Explanation about "type" as follow.
#'  "fp" = Food poison index. It is povided from Jan to Dec.
#'  "st" = Sensory temperature index. It is povided from Nov to Mar.
#'  "hi" = Heat index. It is povided from Jun to Sep.
#'  "di" = Discomport index. It is povided from Jun to Sep.
#'  "ui" = Ultraviolet index. It is povided from Mar to Nov.
#'  "fb" = Freezing burst index. It is povided from Dec to Feb.
#'  "ap" = Atmospheric dispersion index. It is povided from Nov to May.
#'  "sh" = Sensory heat index. It is povided from May to Sep.
#'  "possible" = All index which affordable now.
#'  See more details about each index from <http://www.weather.go.kr/weather/lifenindustry/life_jisu.jsp#>
#'
#' @examples
#'  # example 1 searching by localeCode.
#'  data <- kmaLifeIndex(key, time = seq(0, 21, 3), localeCode = c(4111100000, 4111156600),
#'                       type = "fp", slow = T)
#'
#'  # example 2 searching by localeName
#'  data <- kmaLifeIndex(key, time = seq(0, 21, 3),
#'                       localeName = c("수원"), type = "possible", slow = T)
#'
#' @import dplyr
#' @import bindrcpp
#' @import magrittr
#' @import XML
#' @import utils
#' @import stats
#'
#' @export
kmaLifeIndex <- function(key, time = seq(0, 21, 3), localeCode = NULL, localeName = NULL, type, slow = T, viz = F){
  suppressWarnings(suppressMessages(requireNamespace("dplyr")))
  suppressWarnings(suppressMessages(requireNamespace("magrittr")))
  suppressWarnings(suppressMessages(requireNamespace("bindrcpp")))
  suppressWarnings(suppressMessages(requireNamespace("XML")))

  ### 1. parameter checking and processing.
  ## key
  if(is.null(key)){ stop("Invalid key. \n Please issue API key first and insert it to \"key\" param.") }

  ## localeCode, localeName
  if(is.null(localeCode) & is.null(localeName)){
    stop("Invalid locale. \n Please insert at least one params between \"localeCode\" and \"localeName\".")
  }
  if(!is.null(localeCode) & (mean(nchar(localeCode)) != 10)){
    stop("Invalid localeCode. \n Please insert right \"localeCode\". It should be 10-digit numeric values.")
  }

  ## type
  data("kma_lifeIndex"); month <- as.numeric(strsplit(as.character(Sys.Date()), "-") %>% unlist)[2]
  if(!(type %in% c(rownames(kma_lifeIndex_type_check), "possible")) ){
    stop('Invalid type. \n \"type\" param should be one of ',
         rownames(kma_lifeIndex_type_check) %>% paste('"', ., '"', sep = "", collapse = ", "), ' or "possible"')
  }
  if(type == "possible"){
    type <- kma_lifeIndex_type_check[,month] %>% which %>% names
  }
  if(kma_lifeIndex_type_check[type, month] %>% any == F){
    stop("Inappropriate type. you can't import ", type, "-type data at ", month.name[month],
         "\n it can be imported at ", paste(month.name[kma_lifeIndex_type_check[type,]], collapse = ", "), " only.")
  }

  ## time
  if( any(!(time %in% seq(0, 21, 3))) ){
    warning("Inappropriate time. \n The data of \"fp\", \"ui\", \"sh\" type produced twice per day at 6 and 18 o'clock.",
            "\n Other types data produced 8-times per day at 0, 3, 6, 9, 12, 15, 18, 21 o'clock.")
  }
  if( all(type %in% c("fp", "ui", "sh")) ){
    mt <- outer(c(6,18), time, "-") %>% abs; dimnames(mt) <- list(c(6,18), time)
    time <- names((mt %>% apply(., 1, min) <= 6) %>% which) %>% as.numeric %>% sprintf("%02d", .)
  }else{
    mt <- outer(seq(0, 21, 3), time, "-") %>% abs; dimnames(mt) <- list(seq(0, 21, 3), time)
    time <- names((mt %>% apply(., 1, min) <= 2) %>% which) %>% as.numeric %>% sprintf("%02d", .)
  }


  ### 2. REST url
  ## End Point.
  url <- paste("http://newsky2.kma.go.kr/iros/RetrieveLifeIndexService3/get", kma_lifeIndex_urlType[type], "LifeList?", sep = "")

  ## date time(only last 2 days...).
  datelst <- c(Sys.Date() - 1, Sys.Date()) %>% gsub("-", "", .) %>%
    outer(., time, paste, sep = "") %>% as.vector %>% sort

  # remove not comming datelst.
  datelst <- datelst[strptime(datelst,format='%Y%m%d%H') <= Sys.time()]

  ## locale
  if(is.null(localeCode) & !is.null(localeName)){
    localeName <- gsub("시\\b|도\\b|구\\b", "", localeName) %>% paste(., collapse = "|")
    localeCode <- kma_lifeIndex_locale_code[grepl(localeName, paste(kma_lifeIndex_locale_code$name1,
                                                                    kma_lifeIndex_locale_code$name2, sep = " ")),] %>%
      select(code) %>% unlist %>% as.numeric
  }

  ## generate list of urls(fxxking so many limitations...).
  # 1st, url + key + datelst. datelst by type condition.
  # 2nd, (url + key + datelst) + localeCode.
  urls <- lapply(url, function(x) if(grepl(kma_lifeIndex_urlType[c("fp", "ui", "sh")] %>% paste(collapse = "|"), x)){
    paste(x, "serviceKey=", key, "&time=", datelst[substr(datelst, 9, 10) %in% c("06", "18")], "&areaNo=", sep = "")
  } else{
    paste(x, "serviceKey=", key, "&time=", datelst, "&areaNo=", sep = "")
  }) %>% lapply(., function(x) outer(x, localeCode, paste, sep = "") %>% as.vector) %>% unlist


  ### 3. urls's xml parsing.
  all.data <- list(); length(all.data) <- length(urls)
  all.error <- list(); length(all.error) <- length(urls)
  suc <- character(length(urls))
  pb <- txtProgressBar(min = 1, length(urls), style = 3)

  ## xml data parsing as list form.
  for(i in 1:length(urls)){
    # parsing xml codes with repeat and trycatch.
    repeat{
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
      if(!is.null(tmp.xml)) break
    }

    suc[i] <- tmp.xml$Header$SuccessYN

    if(slow){
      Sys.sleep(runif(1, 0, 1.5))
    }

    # if suc is "N", skip.
    if(suc[i] == "N"){
      all.error[[i]] <- tmp.xml$Header$ErrMsg
      setTxtProgressBar(pb, value = i)
      next
    }else if(suc[i] =="Y"){
      location <- tmp.xml$Body$IndexModel

      if(gsub(".*get(.*)LifeList.*", "\\1", urls[i]) %in% kma_lifeIndex_urlType[c("fp", "ui")]){
        all.data[[i]] <- data.frame(
          idxCode = location$code,
          type = gsub(".*get(.*)LifeList.*", "\\1", urls[i]),
          locale = location$areaNo,
          time = location$date,
          d0 = location$today %>% ifelse(is.null(.), NA, .) %>% as.numeric,
          d1 = location$tomorrow %>% ifelse(is.null(.), NA, .) %>% as.numeric,
          d2 = location$theDayAfterTomorrow %>% ifelse(is.null(.), NA, .) %>% as.numeric,
          stringsAsFactors = F
        )
      }else{
        all.data[[i]] <- data.frame(
          idxCode = location$code,
          type = gsub(".*get(.*)LifeList.*", "\\1", urls[i]),
          locale = location$areaNo,
          time = location$date,
          h3 = location$h3 %>% ifelse(is.null(.), NA, .) %>% as.numeric,
          h6 = location$h6 %>% ifelse(is.null(.), NA, .) %>% as.numeric,
          h9= location$h9 %>% ifelse(is.null(.), NA, .) %>% as.numeric,
          h12 = location$h12 %>% ifelse(is.null(.), NA, .) %>% as.numeric,
          h15 = location$h15 %>% ifelse(is.null(.), NA, .) %>% as.numeric,
          h18 = location$h18 %>% ifelse(is.null(.), NA, .) %>% as.numeric,
          h21 = location$h21 %>% ifelse(is.null(.), NA, .) %>% as.numeric,
          h24 = location$h24 %>% ifelse(is.null(.), NA, .) %>% as.numeric,
          h27 = location$h27 %>% ifelse(is.null(.), NA, .) %>% as.numeric,
          h30 = location$h30 %>% ifelse(is.null(.), NA, .) %>% as.numeric,
          h33 = location$h33 %>% ifelse(is.null(.), NA, .) %>% as.numeric,
          h36 = location$h36 %>% ifelse(is.null(.), NA, .) %>% as.numeric,
          h39 = location$h39 %>% ifelse(is.null(.), NA, .) %>% as.numeric,
          h42 = location$h42 %>% ifelse(is.null(.), NA, .) %>% as.numeric,
          h45 = location$h45 %>% ifelse(is.null(.), NA, .) %>% as.numeric,
          h48 = location$h48 %>% ifelse(is.null(.), NA, .) %>% as.numeric,
          h51 = location$h51 %>% ifelse(is.null(.), NA, .) %>% as.numeric,
          h54 = location$h54 %>% ifelse(is.null(.), NA, .) %>% as.numeric,
          h57 = location$h57 %>% ifelse(is.null(.), NA, .) %>% as.numeric,
          h60 = location$h60 %>% ifelse(is.null(.), NA, .) %>% as.numeric,
          h63 = location$h63 %>% ifelse(is.null(.), NA, .) %>% as.numeric,
          h66 = location$h66 %>% ifelse(is.null(.), NA, .) %>% as.numeric,

          stringsAsFactors = F
        )
      } # if statement regarding to type.
    } # if statement regarding to SuccessYN.
  setTxtProgressBar(pb, value = i)
  } # end of loop i.

  ### 4. merge data by index type.
  data <- list(); length(data) <- length(type)
  for(i in 1:length(data)){
    data[[i]] <- bind_rows(all.data[lapply(all.data, function(x)
      x$type == kma_lifeIndex_urlType[type][i]) %>% unlist %>% which]) %>% as.tbl %>%
      mutate(time = strptime(time, format='%Y%m%d%H') %>% as.character,
             locale = as.numeric(locale))

    data[[i]] <- data[[i]][!duplicated(data[[i]]),]

    if(unique(data[[i]]$type) == "Fsn"){

      # 1. Food Poison.
      data[[i]]$level <- cut(data[[i]]$d0, breaks = c(0, 35, 70, 95, Inf), right = F,
                             labels = c("safe", "care", "warn", "danger"), ordered_result = T)

    }else if(unique(data[[i]]$type) == "Sensorytem"){

      # 2. Sensory temporature.
      data[[i]]$level <- cut(data[[i]]$h3*(-1), breaks = c(-Inf, 10, 25, 45, Inf),
                             labels = c("safe", "care", "warn", "danger"), ordered_result = T)

    }else if(unique(data[[i]]$type) == "Heat"){

      # 3. Heat index.
      data[[i]]$level <- cut(data[[i]]$h3, breaks = c(-Inf, 32, 41, 54, 65, Inf), right = F,
                             labels = c("low", "normal", "high", "very high", "danger"), ordered_result = T)

    }else if(unique(data[[i]]$type) == "Dspl"){

      # 4. Discomport index.
      data[[i]]$level <- cut(data[[i]]$h3, breaks = c(-Inf, 68, 75, 80, Inf), right = F,
                             labels = c("low", "normal", "high", "very high"), ordered_result = T)

    }else if(unique(data[[i]]$type) == "Ultrv"){

      # 5. Ultra-violet index.
      data[[i]]$level <- cut(data[[i]]$d0, breaks = c(0, 3, 6, 8, 11, Inf), right = F,
                             labels = c("low", "normal", "high", "very high", "danger"), ordered_result = T)

    }else if(unique(data[[i]]$type) == "Winter"){

      # 6. Freezing-burst index.
      data[[i]]$level <- cut(data[[i]]$h3, breaks = c(0, 26, 51, 76, 101), right = F,
                             labels = c("low", "normal", "high", "very high"), ordered_result = T)

    }else if(unique(data[[i]]$type) == "Airpollution"){

      # 7. Atmospheric dispersion index.
      data[[i]]$level <- cut(data[[i]]$h3, breaks = c(0, 26, 51, 76, 101), right = F,
                             labels = c("very high", "high", "normal", "low"), ordered_result = T)

    }else{

      # 8. Sensory heat.
      data[[i]]$level <- cut(data[[i]]$h3, breaks = c(-Inf, 21, 25, 28, 31, Inf), right = F,
                             labels = c("safe", "care", "warn", "danger", "very danger"), ordered_result = T)
    }

  }; names(data) <- type

  result <- list(
    data = data,
    plot = NULL
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
