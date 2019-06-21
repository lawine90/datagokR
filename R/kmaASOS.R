#' Korea Meteorological Administration, Automated Synoptic Observing System(ASOS) data of day.
#'
#' kmaASOS function import Korea Moteorological Administration's ASOS data of day.
#'
#' @param key character value. API key issued from <https://data.kma.go.kr>. Note that it's not from <data.go.kr>. no default.
#' @param branchCode numeric value. the codes of branches. If insert "all", import all branches data. See kma_branch data for details.
#' @param fromDate date value. 8-digits date which means starting date.
#' @param toDate date value. 8-digits date which means end date.
#' @param slow logical value. if TRUE, give sleep inbetween importing. default is TRUE.
#' @param viz logical value. if TRUE, provide simple 2d visualization result. x: date, y: mean index.
#'
#' @return data.frame and visualization.
#'
#' @details ASOS is an observation conducted at the same time at all
#'  branches at a fixed time to understand the synoptic weather.
#'  Normaly, it refers to the size of anticyclone and cyclone or daily weather on weather map.\cr
#'  See more details about each data from <https://data.kma.go.kr/data/grnd/selectAsosRltmList.do> \cr
#'  The date argument range is from 1904-01-01 to previous day.\cr
#'  It should be noticed that this function needs the API key issued from https://data.kma.go.kr, not from https://data.go.kr.\cr
#'  Importing data from KMA is very unstable that it is recommended to checking error urls in result.
#'
#' @examples
#'  # example 1 searching by branchCode.
#'  data <- kmaASOS(key, branchCode = c(108), fromDate = as.date("2010-01-01"),
#'                  toDate = as.date("2010-01-10"), slow = T)
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
#' @importFrom httr GET
#' @importFrom httr content
#'
#' @export

kmaASOS <- function(key, branchCode = NULL, fromDate = NULL, toDate = NULL, slow = F, errorCheck = T){
  ### 1. parameter checking
  ## key
  if(is.null(key)){ stop("Invalid key. \n Please issue API key first and insert it to \"key\" param.") }

  ## branchCode
  if(length(branchCode) == 1 & (branchCode == "all")){
    branchCode <- datagokR::kma_branches[datagokR::kma_branches$endDate == "",]
    branchCode <- branchCode[branchCode$code %in% 90:295,]
    branchCode <- branchCode[branchCode$data == "asos",]$code %>% unique
  }
  if(is.null(branchCode)){
    stop("Invalid branchCode. \n Please insert \"branchCode\" argument.")
  }else if(!all(as.numeric(branchCode) %in% 90:295)){
    stop("Invalid branchCode. \n Please insert \"branchCode\" between 90 to 295.")
  }

  ## fromDate and toDate
  if(is.null(fromDate) & is.null(toDate)){
    toDate <- Sys.Date() - 1
    fromDate <- Sys.Date() - 7
  }else if(!is.null(fromDate) & is.null(toDate)){
    toDate <- (as.Date(fromDate) + 7)
    fromDate <- as.Date(fromDate)
  }else if(is.null(fromDate) & !is.null(toDate)){
    fromDate <- (as.Date(toDate) - 7)
    toDate <- as.Date(toDate)
  }else{
    fromDate <- as.Date(fromDate)
    toDate <- as.Date(toDate)
  }
  if(toDate >= Sys.Date()){
    warning("Data offered only to previous day(for now, to ", Sys.Date()-1,
            ").\ntoDate argument forcely replaced to ", Sys.Date()-1)
  }


  ### 2. REST url
  ## End Point.
  url <- paste("http://data.kma.go.kr/apiData/getData?type=json&dataCd=ASOS&dateCd=DAY&",
               "pageIndex=1&apiKey=", key, sep = "")

  ## count.
  listCnt <- ifelse(as.numeric(toDate-fromDate)*2 == 0, 2, as.numeric(toDate-fromDate)*2)
  url <- paste(url, "&schListCnt=", listCnt, sep = "")

  ## from & end date.
  url <- paste(url, "&startDt=", gsub("\\D", "", fromDate),
               "&endDt=", gsub("\\D", "", toDate), sep = "")

  ## branches.
  urls <- paste(url, "&stnIds=", branchCode, sep = "")


  ### 3. urls's xml parsing.
  all.data <- list(); length(all.data) <- length(urls)
  meta <- data.frame(url = urls, success = "", message = "", stringsAsFactors = F) %>% # define data.frame for meta-data.
    as.tbl

  pb <- txtProgressBar(min = 0, length(urls), style = 3)

  ## xml data parsing as list form.
  for(i in 1:length(urls)){
    # parsing xml codes with repeat and trycatch.
    ii <- 0
    repeat{
      ii <- ii + 1
      tmp.xml <- tryCatch(
        {
          httr::GET(urls[[i]]) %>% httr::content(as = "parsed", encoding = 'UTF-8')
          # read_xml(urls[[i]]) %>% xmlTreeParse %>% xmlToList
        }, error = function(e){
          NULL
        }
      )

      if(slow){
        Sys.sleep(runif(1, 0, 2.5))
      }
      if(!is.null(tmp.xml) | ii >= 15) break
    }

    # if tmp.xml is error, go next.
    if(is.null(tmp.xml)){
      meta[i,]$success <- "error"
      next
    }else{
      meta[i,]$success <- tmp.xml[[lapply(tmp.xml, function(x) grepl("msg", names(x))) %>% unlist %>% which]] %>%
        as.character
    }

    if(slow){
      Sys.sleep(runif(1, 0, 1.5))
    }

    # if suc is "N", skip.
    if(meta[i,]$success != "success"){
      meta[i,]$success <- "error"
      setTxtProgressBar(pb, value = i)
      next
    }else if(meta[i,]$success == "success"){
      location <- tmp.xml[[lapply(tmp.xml, function(x) grepl("info", names(x))) %>% unlist %>% which]][[1]]

      all.data[[i]] <- data.frame(
        "date" = lapply(location, function(x) ifelse(is.null(x$TM), "NA", x$TM)) %>% as.character,
        "brch" = lapply(location, function(x) ifelse(is.null(x$STN_ID), "NA", x$STN_ID)) %>% as.numeric,
        "brch_nme" = lapply(location, function(x) ifelse(is.null(x$STN_NM), "NA", x$STN_NM)) %>% as.character,
        "ertTmp_1.5m_avg" = lapply(location, function(x) ifelse(is.null(x$AVG_M1_5_TE), "NA", x$AVG_M1_5_TE)) %>% as.numeric,
        "rain_9_9" = lapply(location, function(x) ifelse(is.null(x$N9_9_RN), "NA", x$N9_9_RN)) %>% as.numeric,
        "seaPrs_min" = lapply(location, function(x) ifelse(is.null(x$MIN_PS), "NA", x$MIN_PS)) %>% as.numeric,
        "hum_avg" = lapply(location, function(x) ifelse(is.null(x$AVG_RHM), "NA", x$AVG_RHM)) %>% as.numeric,
        "insWdSpDrc_max" = lapply(location, function(x) ifelse(is.null(x$MAX_INS_WS_WD), "NA", x$MAX_INS_WS_WD)) %>% as.numeric,
        "surfTmp_avg" = lapply(location, function(x) ifelse(is.null(x$AVG_TS), "NA", x$AVG_TS)) %>% as.numeric,
        "vapPrs_avg" = lapply(location, function(x) ifelse(is.null(x$AVG_PV), "NA", x$AVG_PV)) %>% as.numeric,
        "hum_min" = lapply(location, function(x) ifelse(is.null(x$MIN_RHM), "NA", x$MIN_RHM)) %>% as.numeric,
        "sunDurTm_sum" = lapply(location, function(x) ifelse(is.null(x$SUM_SS_HR), "NA", x$SUM_SS_HR)) %>% as.numeric,
        "sunDur_sum" = lapply(location, function(x) ifelse(is.null(x$SS_DUR), "NA", x$SS_DUR)) %>% as.numeric,
        "seaPrs_avg" = lapply(location, function(x) ifelse(is.null(x$AVG_PS), "NA", x$AVG_PS)) %>% as.numeric,
        "ertTmp_5cm_avg" = lapply(location, function(x) ifelse(is.null(x$AVG_CM5_TE), "NA", x$AVG_CM5_TE)) %>% as.numeric,
        "wdSp_max" = lapply(location, function(x) ifelse(is.null(x$MAX_WS), "NA", x$MAX_WS)) %>% as.numeric,
        "grsTmp_min" = lapply(location, function(x) ifelse(is.null(x$MIN_TG), "NA", x$MIN_TG)) %>% as.numeric,
        "wdSpDrc_max" = lapply(location, function(x) ifelse(is.null(x$MAX_WS_WD), "NA", x$MAX_WS_WD)) %>% as.numeric,
        "smlEpr_sum" = lapply(location, function(x) ifelse(is.null(x$SUM_SML_EV), "NA", x$SUM_SML_EV)) %>% as.numeric,
        "ttlCld_avg" = lapply(location, function(x) ifelse(is.null(x$AVG_TCA), "NA", x$AVG_TCA)) %>% as.numeric,
        "slr_1hr_max" = lapply(location, function(x) ifelse(is.null(x$HR1_MAX_ICSR), "NA", x$HR1_MAX_ICSR)) %>% as.numeric,
        "dewTmp_avg" = lapply(location, function(x) ifelse(is.null(x$AVG_TD), "NA", x$AVG_TD)) %>% as.numeric,
        "seaPrs_max" = lapply(location, function(x) ifelse(is.null(x$MAX_PS), "NA", x$MAX_PS)) %>% as.numeric,
        "ertTmp_20cm_avg" = lapply(location, function(x) ifelse(is.null(x$AVG_CM20_TE), "NA", x$AVG_CM20_TE)) %>% as.numeric,
        "extmSnw_day" = lapply(location, function(x) ifelse(is.null(x$DD_MES), "NA", x$DD_MES)) %>% as.numeric,
        "arTmp_min" = lapply(location, function(x) ifelse(is.null(x$MIN_TA), "NA", x$MIN_TA)) %>% as.numeric,
        "ertTmp_5m_avg" = lapply(location, function(x) ifelse(is.null(x$AVG_M5_0_TE), "NA", x$AVG_M5_0_TE)) %>% as.numeric,
        "arTmp_max" = lapply(location, function(x) ifelse(is.null(x$MAX_TA), "NA", x$MAX_TA)) %>% as.numeric,
        "wdRun_sum" = lapply(location, function(x) ifelse(is.null(x$HR24_SUM_RWS), "NA", x$HR24_SUM_RWS)) %>% as.numeric,
        "ertTmp_3m_avg" = lapply(location, function(x) ifelse(is.null(x$AVG_M3_0_TE), "NA", x$AVG_M3_0_TE)) %>% as.numeric,
        "ertTmp_10cm_avg" = lapply(location, function(x) ifelse(is.null(x$AVG_CM10_TE), "NA", x$AVG_CM10_TE)) %>% as.numeric,
        "ertTmp_0.5m_avg" = lapply(location, function(x) ifelse(is.null(x$AVG_M0_5_TE), "NA", x$AVG_M0_5_TE)) %>% as.numeric,
        "insWdSp_max" = lapply(location, function(x) ifelse(is.null(x$MAX_INS_WS), "NA", x$MAX_INS_WS)) %>% as.numeric,
        "cldAmt_avg" = lapply(location, function(x) ifelse(is.null(x$AVG_TCA), "NA", x$AVG_TCA)) %>% as.numeric,
        "ertTmp_30cm_avg" = lapply(location, function(x) ifelse(is.null(x$AVG_CM30_TE), "NA", x$AVG_CM30_TE)) %>% as.numeric,
        "ertTmp_1m_avg" = lapply(location, function(x) ifelse(is.null(x$AVG_M1_0_TE), "NA", x$AVG_M1_0_TE)) %>% as.numeric,
        "grsSun_sum" = lapply(location, function(x) ifelse(is.null(x$SUM_GSR), "NA", x$SUM_GSR)) %>% as.numeric,
        "araPrs_avg" = lapply(location, function(x) ifelse(is.null(x$AVG_PA), "NA", x$AVG_PA)) %>% as.numeric,
        "wdSp_avg" = lapply(location, function(x) ifelse(is.null(x$AVG_WS), "NA", x$AVG_WS)) %>% as.numeric,
        "fogDur_sum" = lapply(location, function(x) ifelse(is.null(x$SUM_FOG_DUR), "NA", x$SUM_FOG_DUR)) %>% as.numeric,
        "lrgEpr_sum" = lapply(location, function(x) ifelse(is.null(x$SUM_LRG_EV), "NA", x$SUM_LRG_EV)) %>% as.numeric,
        "frsSnw_sum" = lapply(location, function(x) ifelse(is.null(x$SUM_DPTH_FHSC), "NA", x$SUM_DPTH_FHSC)) %>% as.numeric,
        "frsSnw_day" = lapply(location, function(x) ifelse(is.null(x$DD_MEFS), "NA", x$DD_MEFS)) %>% as.numeric,
        "rain_sum" = lapply(location, function(x) ifelse(is.null(x$SUM_RN), "NA", x$SUM_RN)) %>% as.numeric,
        "rain_1hr_max" = lapply(location, function(x) ifelse(is.null(x$HR1_MAX_RN), "NA", x$HR1_MAX_RN)) %>% as.numeric,
        "rain_10mts_max" = lapply(location, function(x) ifelse(is.null(x$mi10_MAX_RN), "NA", x$mi10_MAX_RN)) %>% as.numeric,
        stringsAsFactors = F
      ) %>% as.tbl

      all.data[[i]] <- all.data[[i]] %>% select(c("date", "brch", "brch_nme", sort(colnames(all.data[[i]])[-(1:3)])))

      # Encoding(all.data[[i]]$brch_nme) <- rep("UTF-8", nrow(all.data[[i]]))
      # all.data[[i]]$brch_nme <- enc2utf8(all.data[[i]]$brch_nme)
    } # if statement regarding to SuccessYN.
    setTxtProgressBar(pb, value = i)
  } # end of loop i.

  data <- bind_rows(all.data)


  ### 4. checking error retry.
  if(errorCheck & nrow(meta[meta$success != "success",]) != 0){
    re.data <- list()

    for(i in 1:nrow(meta[meta$success != "success",])){
      # parsing xml codes with repeat and trycatch.
      ii <- 0
      repeat{
        ii <- ii + 1
        tmp.xml <- tryCatch(
          {
            httr::GET(meta[meta$success != "success",]$url[i]) %>%
              httr::content(as = "parsed", encoding = 'UTF-8')
          }, error = function(e){
            NULL
          }
        )

        if(slow){
          Sys.sleep(runif(1, 0, 2.5))
        }
        if(!is.null(tmp.xml) | ii >= 15) break
      }

      if(slow){
        Sys.sleep(runif(1, 0, 1.5))
      }

      # if suc is "N", skip.
      if(is.null(tmp.xml)){
        meta[meta$success != "success",]$success[i] <- "error"
        next
      }else if(!is.null(tmp.xml)){
        meta[meta$success != "success",]$success[i] <- tmp.xml[[lapply(tmp.xml, function(x)
          grepl("msg", names(x))) %>% unlist %>% which]] %>% as.character

        location <- tmp.xml[[lapply(tmp.xml, function(x) grepl("info", names(x))) %>% unlist %>% which]][[1]]

        re.data[[i]] <- data.frame(
          "date" = lapply(location, function(x) ifelse(is.null(x$TM), "NA", x$TM)) %>% as.character,
          "brch" = lapply(location, function(x) ifelse(is.null(x$STN_ID), "NA", x$STN_ID)) %>% as.numeric,
          "brch_nme" = lapply(location, function(x) ifelse(is.null(x$STN_NM), "NA", x$STN_NM)) %>% as.character,
          "ertTmp_1.5m_avg" = lapply(location, function(x) ifelse(is.null(x$AVG_M1_5_TE), "NA", x$AVG_M1_5_TE)) %>% as.numeric,
          "rain_9_9" = lapply(location, function(x) ifelse(is.null(x$N9_9_RN), "NA", x$N9_9_RN)) %>% as.numeric,
          "seaPrs_min" = lapply(location, function(x) ifelse(is.null(x$MIN_PS), "NA", x$MIN_PS)) %>% as.numeric,
          "hum_avg" = lapply(location, function(x) ifelse(is.null(x$AVG_RHM), "NA", x$AVG_RHM)) %>% as.numeric,
          "insWdSpDrc_max" = lapply(location, function(x) ifelse(is.null(x$MAX_INS_WS_WD), "NA", x$MAX_INS_WS_WD)) %>% as.numeric,
          "surfTmp_avg" = lapply(location, function(x) ifelse(is.null(x$AVG_TS), "NA", x$AVG_TS)) %>% as.numeric,
          "vapPrs_avg" = lapply(location, function(x) ifelse(is.null(x$AVG_PV), "NA", x$AVG_PV)) %>% as.numeric,
          "hum_min" = lapply(location, function(x) ifelse(is.null(x$MIN_RHM), "NA", x$MIN_RHM)) %>% as.numeric,
          "sunDurTm_sum" = lapply(location, function(x) ifelse(is.null(x$SUM_SS_HR), "NA", x$SUM_SS_HR)) %>% as.numeric,
          "sunDur_sum" = lapply(location, function(x) ifelse(is.null(x$SS_DUR), "NA", x$SS_DUR)) %>% as.numeric,
          "seaPrs_avg" = lapply(location, function(x) ifelse(is.null(x$AVG_PS), "NA", x$AVG_PS)) %>% as.numeric,
          "ertTmp_5cm_avg" = lapply(location, function(x) ifelse(is.null(x$AVG_CM5_TE), "NA", x$AVG_CM5_TE)) %>% as.numeric,
          "wdSp_max" = lapply(location, function(x) ifelse(is.null(x$MAX_WS), "NA", x$MAX_WS)) %>% as.numeric,
          "grsTmp_min" = lapply(location, function(x) ifelse(is.null(x$MIN_TG), "NA", x$MIN_TG)) %>% as.numeric,
          "wdSpDrc_max" = lapply(location, function(x) ifelse(is.null(x$MAX_WS_WD), "NA", x$MAX_WS_WD)) %>% as.numeric,
          "smlEpr_sum" = lapply(location, function(x) ifelse(is.null(x$SUM_SML_EV), "NA", x$SUM_SML_EV)) %>% as.numeric,
          "ttlCld_avg" = lapply(location, function(x) ifelse(is.null(x$AVG_TCA), "NA", x$AVG_TCA)) %>% as.numeric,
          "slr_1hr_max" = lapply(location, function(x) ifelse(is.null(x$HR1_MAX_ICSR), "NA", x$HR1_MAX_ICSR)) %>% as.numeric,
          "dewTmp_avg" = lapply(location, function(x) ifelse(is.null(x$AVG_TD), "NA", x$AVG_TD)) %>% as.numeric,
          "seaPrs_max" = lapply(location, function(x) ifelse(is.null(x$MAX_PS), "NA", x$MAX_PS)) %>% as.numeric,
          "ertTmp_20cm_avg" = lapply(location, function(x) ifelse(is.null(x$AVG_CM20_TE), "NA", x$AVG_CM20_TE)) %>% as.numeric,
          "extmSnw_day" = lapply(location, function(x) ifelse(is.null(x$DD_MES), "NA", x$DD_MES)) %>% as.numeric,
          "arTmp_min" = lapply(location, function(x) ifelse(is.null(x$MIN_TA), "NA", x$MIN_TA)) %>% as.numeric,
          "ertTmp_5m_avg" = lapply(location, function(x) ifelse(is.null(x$AVG_M5_0_TE), "NA", x$AVG_M5_0_TE)) %>% as.numeric,
          "arTmp_max" = lapply(location, function(x) ifelse(is.null(x$MAX_TA), "NA", x$MAX_TA)) %>% as.numeric,
          "wdRun_sum" = lapply(location, function(x) ifelse(is.null(x$HR24_SUM_RWS), "NA", x$HR24_SUM_RWS)) %>% as.numeric,
          "ertTmp_3m_avg" = lapply(location, function(x) ifelse(is.null(x$AVG_M3_0_TE), "NA", x$AVG_M3_0_TE)) %>% as.numeric,
          "ertTmp_10cm_avg" = lapply(location, function(x) ifelse(is.null(x$AVG_CM10_TE), "NA", x$AVG_CM10_TE)) %>% as.numeric,
          "ertTmp_0.5m_avg" = lapply(location, function(x) ifelse(is.null(x$AVG_M0_5_TE), "NA", x$AVG_M0_5_TE)) %>% as.numeric,
          "insWdSp_max" = lapply(location, function(x) ifelse(is.null(x$MAX_INS_WS), "NA", x$MAX_INS_WS)) %>% as.numeric,
          "cldAmt_avg" = lapply(location, function(x) ifelse(is.null(x$AVG_TCA), "NA", x$AVG_TCA)) %>% as.numeric,
          "ertTmp_30cm_avg" = lapply(location, function(x) ifelse(is.null(x$AVG_CM30_TE), "NA", x$AVG_CM30_TE)) %>% as.numeric,
          "ertTmp_1m_avg" = lapply(location, function(x) ifelse(is.null(x$AVG_M1_0_TE), "NA", x$AVG_M1_0_TE)) %>% as.numeric,
          "grsSun_sum" = lapply(location, function(x) ifelse(is.null(x$SUM_GSR), "NA", x$SUM_GSR)) %>% as.numeric,
          "araPrs_avg" = lapply(location, function(x) ifelse(is.null(x$AVG_PA), "NA", x$AVG_PA)) %>% as.numeric,
          "wdSp_avg" = lapply(location, function(x) ifelse(is.null(x$AVG_WS), "NA", x$AVG_WS)) %>% as.numeric,
          "fogDur_sum" = lapply(location, function(x) ifelse(is.null(x$SUM_FOG_DUR), "NA", x$SUM_FOG_DUR)) %>% as.numeric,
          "lrgEpr_sum" = lapply(location, function(x) ifelse(is.null(x$SUM_LRG_EV), "NA", x$SUM_LRG_EV)) %>% as.numeric,
          "frsSnw_sum" = lapply(location, function(x) ifelse(is.null(x$SUM_DPTH_FHSC), "NA", x$SUM_DPTH_FHSC)) %>% as.numeric,
          "frsSnw_day" = lapply(location, function(x) ifelse(is.null(x$DD_MEFS), "NA", x$DD_MEFS)) %>% as.numeric,
          "rain_sum" = lapply(location, function(x) ifelse(is.null(x$SUM_RN), "NA", x$SUM_RN)) %>% as.numeric,
          "rain_1hr_max" = lapply(location, function(x) ifelse(is.null(x$HR1_MAX_RN), "NA", x$HR1_MAX_RN)) %>% as.numeric,
          "rain_10mts_max" = lapply(location, function(x) ifelse(is.null(x$mi10_MAX_RN), "NA", x$mi10_MAX_RN)) %>% as.numeric,
          stringsAsFactors = F
        ) %>% as.tbl

        re.data[[i]] <- re.data[[i]] %>% select(c("date", "brch", "brch_nme", sort(colnames(re.data[[i]])[-(1:3)])))
      } # if statement regarding to SuccessYN.
    } # end of loop i.

    eror.data <- bind_rows(re.data)
    data <- bind_rows(data, re.data)
  } # end of errorCheck statement.

  result <- list(
    meta = meta,
    data = data,
    plot = NULL
  )
}
