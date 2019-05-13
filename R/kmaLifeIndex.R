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
#'  Explanation about "type" as follow.\cr
#'  "fp" = Food poison index. It is povided from Jan to Dec.\cr
#'  "st" = Sensory temperature index. It is povided from Nov to Mar.\cr
#'  "hi" = Heat index. It is povided from Jun to Sep.\cr
#'  "di" = Discomport index. It is povided from Jun to Sep.\cr
#'  "ui" = Ultraviolet index. It is povided from Mar to Nov.\cr
#'  "fb" = Freezing burst index. It is povided from Dec to Feb.\cr
#'  "ap" = Atmospheric dispersion index. It is povided from Nov to May.\cr
#'  "sh" = Sensory heat index. It is povided from May to Sep.\cr
#'  "possible" = All index which affordable now.\cr
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
utils::globalVariables(c(".data", "code", "kma_lifeIndex_locale_code", "kma_lifeIndex_type_check",
                         "kma_lifeIndex_urlType", "locale"), add = F)
kmaLifeIndex <- function(key, time = seq(0, 21, 3), localeCode = NULL, localeName = NULL, type, slow = T, viz = F){
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
  # data("data_kma_lifeIndex")
  month <- as.numeric(strsplit(as.character(Sys.Date()), "-") %>% unlist)[2]
  if(!(type %in% c(rownames(datagokR::kma_lifeIndex_type_check), "possible")) ){
    stop('Invalid type. \n \"type\" param should be one of ',
         rownames(datagokR::kma_lifeIndex_type_check) %>%
           paste('"', .data, '"', sep = "", collapse = ", "), ' or "possible"')
  }
  if(type == "possible"){
    type <- datagokR::kma_lifeIndex_type_check[,month] %>% which %>% names
  }
  if(datagokR::kma_lifeIndex_type_check[type, month] %>% any == F){
    stop("Inappropriate type. you can't import ", type, "-type data at ", month.name[month],
         "\n it can be imported at ", paste(month.name[datagokR::kma_lifeIndex_type_check[type,]], collapse = ", "), " only.")
  }

  ## time
  if( any(!(time %in% seq(0, 21, 3))) ){
    warning("Inappropriate time. \n The data of \"fp\", \"ui\", \"sh\" type produced twice per day at 6 and 18 o'clock.",
            "\n Other types data produced 8-times per day at 0, 3, 6, 9, 12, 15, 18, 21 o'clock.")
  }
  if( all(type %in% c("fp", "ui", "sh")) ){
    mt <- outer(c(6,18), time, "-") %>% abs; dimnames(mt) <- list(c(6,18), time)
    time <- names((mt %>% apply(MARGIN = 1, FUN = min) <= 6) %>% which) %>% as.character %>% sprintf("%02d")
  }else{
    mt <- outer(seq(0, 21, 3), time, "-") %>% abs; dimnames(mt) <- list(seq(0, 21, 3), time)
    time <- names((mt %>% apply(MARGIN = 1, FUN = min) <= 2) %>% which) %>% as.character %>% sprintf("%02d")
  }


  ### 2. REST url
  ## End Point.
  url <- paste("http://newsky2.kma.go.kr/iros/RetrieveLifeIndexService3/get", datagokR::kma_lifeIndex_urlType[type], "LifeList?", sep = "")

  ## date time(only last 2 days...).
  datelst <- c(Sys.Date() - 1, Sys.Date()) %>% gsub(pattern = "-", replacement = "") %>%
    outer(time, paste, sep = "") %>% as.vector %>% sort

  # remove not comming datelst.
  datelst <- datelst[strptime(datelst,format='%Y%m%d%H') <= Sys.time()]

  ## locale
  if(is.null(localeCode) & !is.null(localeName)){
    localeName <- gsub("시\\b|도\\b|구\\b", "", localeName) %>% paste(collapse = "|")
    localeCode <- datagokR::kma_lifeIndex_locale_code[grepl(localeName,
                                                            paste(datagokR::kma_lifeIndex_locale_code$name1,
                                                            datagokR::kma_lifeIndex_locale_code$name2, sep = " ")),] %>%
      select("code") %>% unlist %>% as.numeric
  }

  ## generate list of urls(fxxking so many limitations...).
  # 1st, url + key + datelst. datelst by type condition.
  # 2nd, (url + key + datelst) + localeCode.
  urls <- lapply(url, function(x) if(grepl(datagokR::kma_lifeIndex_urlType[c("fp", "ui", "sh")] %>% paste(collapse = "|"), x)){
    paste(x, "serviceKey=", key, "&time=", datelst[substr(datelst, 9, 10) %in% c("06", "18")], "&areaNo=", sep = "")
  } else{
    paste(x, "serviceKey=", key, "&time=", datelst, "&areaNo=", sep = "")
  }) %>% lapply(function(x) outer(x, localeCode, paste, sep = "") %>% as.vector) %>% unlist


  ### 3. urls's xml parsing.
  all.data <- list(); length(all.data) <- length(urls)
  all.error <- list(); length(all.error) <- length(urls)
  errors <- list(); length(errors) <- length(urls)
  suc <- character(length(urls))
  pb <- txtProgressBar(min = 1, length(urls), style = 3)

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

    suc[i] <- tmp.xml$Header$SuccessYN

    if(slow){
      Sys.sleep(runif(1, 0, 1.5))
    }

    # if suc is "N", skip.
    if(suc[i] == "N"){
      all.error[[i]] <- tmp.xml$Header$ErrMsg
      errors[[i]] <- urls[[i]]
      setTxtProgressBar(pb, value = i)
      next
    }else if(suc[i] =="Y"){
      location <- tmp.xml$Body$IndexModel

      if(gsub(".*get(.*)LifeList.*", "\\1", urls[i]) %in% datagokR::kma_lifeIndex_urlType[c("fp", "ui")]){
        all.data[[i]] <- data.frame(
          idxCode = location$code,
          type = gsub(".*get(.*)LifeList.*", "\\1", urls[i]),
          locale = location$areaNo,
          time = location$date,
          d0 = ifelse(is.null(location$today), NA, location$today) %>% as.numeric,
          d1 = ifelse(is.null(location$tomorrow), NA, location$tomorrow) %>% as.numeric,
          d2 = ifelse(is.null(location$theDayAfterTomorrow), NA, location$theDayAfterTomorrow) %>% as.numeric,
          stringsAsFactors = F
        )
      }else{
        all.data[[i]] <- data.frame(
          idxCode = location$code,
          type = gsub(".*get(.*)LifeList.*", "\\1", urls[i]),
          locale = location$areaNo,
          time = location$date,
          h3 = ifelse(is.null(location$h3), NA, location$h3) %>% as.numeric,
          h6 = ifelse(is.null(location$h6), NA, location$h6) %>% as.numeric,
          h9 = ifelse(is.null(location$h9), NA, location$h9) %>% as.numeric,
          h12 = ifelse(is.null(location$h12), NA, location$h12) %>% as.numeric,
          h15 = ifelse(is.null(location$h15), NA, location$h15) %>% as.numeric,
          h18 = ifelse(is.null(location$h18), NA, location$h18) %>% as.numeric,
          h21 = ifelse(is.null(location$h21), NA, location$h21) %>% as.numeric,
          h24 = ifelse(is.null(location$h24), NA, location$h24) %>% as.numeric,
          h27 = ifelse(is.null(location$h27), NA, location$h27) %>% as.numeric,
          h30 = ifelse(is.null(location$h30), NA, location$h30) %>% as.numeric,
          h33 = ifelse(is.null(location$h33), NA, location$h33) %>% as.numeric,
          h36 = ifelse(is.null(location$h36), NA, location$h36) %>% as.numeric,
          h39 = ifelse(is.null(location$h39), NA, location$h39) %>% as.numeric,
          h42 = ifelse(is.null(location$h42), NA, location$h42) %>% as.numeric,
          h45 = ifelse(is.null(location$h45), NA, location$h45) %>% as.numeric,
          h48 = ifelse(is.null(location$h48), NA, location$h48) %>% as.numeric,
          h51 = ifelse(is.null(location$h51), NA, location$h51) %>% as.numeric,
          h54 = ifelse(is.null(location$h54), NA, location$h54) %>% as.numeric,
          h57 = ifelse(is.null(location$h57), NA, location$h57) %>% as.numeric,
          h60 = ifelse(is.null(location$h60), NA, location$h60) %>% as.numeric,
          h63 = ifelse(is.null(location$h63), NA, location$h63) %>% as.numeric,
          h66 = ifelse(is.null(location$h66), NA, location$h66) %>% as.numeric,

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
      x$type == datagokR::kma_lifeIndex_urlType[type][i]) %>% unlist %>% which]) %>% as.tbl %>%
      mutate("time" = strptime(.data$time, format='%Y%m%d%H') %>% as.character,
             "locale" = as.numeric(.data$locale))

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
    plot = NULL,
    errors = unlist(errors)
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
