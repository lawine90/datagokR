#' Korea Meteorological Administration, Automated Synoptic Observing System(ASOS) data of day.
#'
#' kmaASOS function import Korea Moteorological Administration's ASOS data of day.
#'
#' @param key character value. API key issued from <https://data.kma.go.kr>. Note that it's not from <data.go.kr>. no default.
#' @param branchCode numeric value. the codes of branches. If insert "all", import all branches data. See kma_branch data for details.
#' @param fromDate date value. 8-digits date which means starting date.
#' @param toDate date value. 8-digits date which means end date.
#' @param slow logical value. if TRUE, give sleep inbetween importing. default is TRUE.
#' @param errorCheck logical value. if TRUE, one more try to get data which occured API error.
#' @param verbose logical value. if TRUE, provide process bar. Default value set as false.
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
#'  key <- 'your key issued from data.kma.go.kr'
#'  data <- kmaASOS(key, branchCode = c(108), fromDate = as.Date("2010-01-01"),
#'                  toDate = as.Date("2010-01-10"), slow = TRUE)
#'
#' @importFrom dplyr %>%
#'
#' @export

kmaASOS <- function(key, branchCode = NULL, fromDate = NULL, toDate = NULL, slow = F, errorCheck = T, verbose = F){
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
  url <- paste("http://data.kma.go.kr/apiData/getData?type=xml&dataCd=ASOS&dateCd=DAY&",
               "pageIndex=1&apiKey=", key, sep = "")

  ## count.
  #listCnt <- ifelse(as.numeric(toDate-fromDate)*2 == 0, 2, as.numeric(toDate-fromDate)*2)
  url <- paste(url, "&schListCnt=", 999, sep = "")

  ## from & end date.
  url <- paste(url, "&startDt=", gsub("\\D", "", fromDate),
               "&endDt=", gsub("\\D", "", toDate), sep = "")

  ## branches.
  urls <- paste(url, "&stnIds=", branchCode, sep = "")


  ### 3. urls's xml parsing.
  all.data <- list(); length(all.data) <- length(urls)
  meta <- data.frame(url = urls, success = "", message = "", stringsAsFactors = F)
  meta <- dplyr::as.tbl(meta)

  if(length(urls) == 1){verbose <- F}
  if(verbose == T){pb <- utils::txtProgressBar(min = 0, length(urls), style = 3)}

  ## xml data parsing as list form.
  for(i in 1:length(urls)){
    # parsing xml codes with repeat and trycatch.
    tmp.xml <- datagokR:::try_read_xml(urls[[i]])
    msg <- xml2::xml_text(xml2::xml_find_all(tmp.xml, '//msg'))

    # if tmp.xml is error, go next.
    if(msg != 'success'){
      if(verbose == T){utils::setTxtProgressBar(pb, value = i)}
      meta[i,]$success <- "error"
      next
    }else{
      meta[i,]$success <- msg
    }

    if(slow){
      Sys.sleep(stats::runif(1, 0, 1.5))
    }; location <- xml2::xml_find_all(tmp.xml, './/info')

    all.data[[i]] <- data.frame(
      "date" = datagokR:::find_xml(location, './TM'),
      "id" = datagokR:::find_xml(location, './STN_ID'),
      "name" = datagokR:::find_xml(location, './STN_NM'),

      # air temperature
      "avg_ta" = datagokR:::find_xml(location, './AVG_TA', 'num'),
      "max_ta" = datagokR:::find_xml(location, './MAX_TA', 'num'),
      "max_ta_hrmt" = datagokR:::find_xml(location, './MAX_TA_HRMT'),
      "min_ta" = datagokR:::find_xml(location, './MIN_TA', 'num'),
      "min_ta_hrmt" = datagokR:::find_xml(location, './MIN_TA_HRMT'),

      # sea-level pressure
      "avg_ps" = datagokR:::find_xml(location, './AVG_PS', 'num'),
      "max_ps" = datagokR:::find_xml(location, './MAX_PS', 'num'),
      "max_ps_hrmt" = datagokR:::find_xml(location, './MAX_PS_HRMT'),
      "min_ps" = datagokR:::find_xml(location, './MIN_PS', 'num'),
      "min_ps_hrmt" = datagokR:::find_xml(location, './MIN_PS_HRMT'),

      # wind speed
      "avg_ws" = datagokR:::find_xml(location, './AVG_WS', 'num'),
      "max_wd" = datagokR:::find_xml(location, './MAX_WD', 'num'),
      "max_ws" = datagokR:::find_xml(location, './MAX_WS', 'num'),
      "max_ws_wd" = datagokR:::find_xml(location, './MAX_WS_WD', 'num'),
      "max_ws_hrmt" = datagokR:::find_xml(location, './MAX_WS_HRMT'),
      "max_ins_ws" = datagokR:::find_xml(location, './MAX_INS_WS', 'num'),
      "max_ins_ws_wd" = datagokR:::find_xml(location, './MAX_INS_WS_WD', 'num'),
      "max_ins_ws_hrmt" = datagokR:::find_xml(location, './MAX_INS_WS_HRMT'),

      # earth temperature
      "avg_cm10_te" = datagokR:::find_xml(location, './AVG_CM10_TE', 'num'),
      "avg_cm20_te" = datagokR:::find_xml(location, './AVG_CM20_TE', 'num'),
      "avg_cm30_te" = datagokR:::find_xml(location, './AVG_CM30_TE', 'num'),
      "avg_cm5_te" = datagokR:::find_xml(location, './AVG_CM5_TE', 'num'),
      "avg_m0_5_te" = datagokR:::find_xml(location, './AVG_M0_5_TE', 'num'),
      "avg_m1_0_te" = datagokR:::find_xml(location, './AVG_M1_0_TE', 'num'),
      "avg_m1_5_te" = datagokR:::find_xml(location, './AVG_M1_5_TE', 'num'),
      "avg_m3_0_te" = datagokR:::find_xml(location, './AVG_M3_0_TE', 'num'),
      "avg_m5_0_te" = datagokR:::find_xml(location, './AVG_M5_0_TE', 'num'),

      # rain
      "n9_9_rn" = datagokR:::find_xml(location, './N9_9_RN', 'num'),
      "mi10_max_rn" = datagokR:::find_xml(location, './MI10_MAX_RN', 'num'),
      "mi10_max_rn_hrmt" = datagokR:::find_xml(location, './MI10_MAX_RN_HRMT', 'num'),
      "hr1_max_rn" = datagokR:::find_xml(location, './HR1_MAX_RN', 'num'),
      "hr1_max_rn_hrmt" = datagokR:::find_xml(location, './HR1_MAX_RN_HRMT', 'num'),
      "sum_rn" = datagokR:::find_xml(location, './SUM_RN', 'num'),
      "sum_rn_dur" = datagokR:::find_xml(location, './SUM_RN_DUR', 'num'),

      # snow
      "dd_mefs" = datagokR:::find_xml(location, './DD_MEFS', 'num'),
      "dd_mefs_hrmt" = datagokR:::find_xml(location, './DD_MEFS_HRMT', 'num'),
      "dd_mes" = datagokR:::find_xml(location, './DD_MES', 'num'),
      "dd_mes_hrmt" = datagokR:::find_xml(location, './DD_MES_HRMT', 'num'),
      "sum_dpth_fhsc" = datagokR:::find_xml(location, './SUM_DPTH_FHSC', 'num'),

      # relative humidity
      "avg_rhm" = datagokR:::find_xml(location, './AVG_RHM', 'num'),
      "min_rhm" = datagokR:::find_xml(location, './MIN_RHM', 'num'),
      "min_rhm_hrmt" = datagokR:::find_xml(location, './MIN_RHM_HRMT'),

      # etc
      "hr1_max_icsr" = datagokR:::find_xml(location, './HR1_MAX_ICSR', 'num'),
      "hr1_max_icsr_hrmt" = datagokR:::find_xml(location, './HR1_MAX_ICSR_HRMT', 'num'),
      "ss_dur" = datagokR:::find_xml(location, './SS_DUR', 'num'),

      "min_tg" = datagokR:::find_xml(location, './MIN_TG', 'num'),
      "avg_td" = datagokR:::find_xml(location, './AVG_TD', 'num'),
      "avg_tca" = datagokR:::find_xml(location, './AVG_TCA', 'num'),
      "avg_lmac" = datagokR:::find_xml(location, './AVG_LMAC', 'num'),
      "avg_pv" = datagokR:::find_xml(location, './AVG_PV', 'num'),
      "avg_ts" = datagokR:::find_xml(location, './AVG_TS', 'num'),
      "avg_pa" = datagokR:::find_xml(location, './AVG_PA', 'num'),

      "sum_lrg_ev" = datagokR:::find_xml(location, './SUM_LRG_EV', 'num'),
      "sum_sml_ev" = datagokR:::find_xml(location, './SUM_SML_EV', 'num'),
      "sum_fog_dur" = datagokR:::find_xml(location, './SUM_FOG_DUR', 'num'),
      "sum_gsr" = datagokR:::find_xml(location, './SUM_GSR', 'num'),
      "sum_ss_hr" = datagokR:::find_xml(location, './SUM_SS_HR', 'num'),
      "sum_rws_hr24" = datagokR:::find_xml(location, './HR24_SUM_RWS', 'num'),
      stringsAsFactors = F
    ) %>% dplyr::as.tbl()
    if(verbose == T){utils::setTxtProgressBar(pb, value = i)}
  } # end of loop i.
  data <- dplyr::bind_rows(all.data)
  ### 4. checking error retry.
  if(errorCheck & nrow(meta[meta$success != "success",]) != 0){
    errors <- meta[meta$success != "success",]
    re.data <- list()

    for(i in 1:nrow(errors)){
      # parsing xml codes with repeat and trycatch.
      tmp.xml <- datagokR:::try_GET_content(errors$url[i])

      if(slow){
        Sys.sleep(stats::runif(1, 0, 1.5))
      }

      # if suc is "N", skip.
      if(is.null(tmp.xml)){
        meta[meta$url %in% errors$url[i],]$success <- "error"
        next
      }else if(!is.null(tmp.xml)){
        meta[meta$url %in% errors$url[i],]$success <- tmp.xml[[lapply(tmp.xml, function(x)
          grepl("msg", names(x))) %>% unlist %>% which]] %>% as.character

        if(meta[meta$url %in% errors$url[i],]$success != 'success'){next}

        location <- tmp.xml[[lapply(tmp.xml, function(x) grepl("info", names(x))) %>% unlist %>% which]][[1]]

        re.data[[i]] <- data.frame(
          "date" = datagokR:::find_xml(location, './TM'),
          "id" = datagokR:::find_xml(location, './STN_ID'),
          "name" = datagokR:::find_xml(location, './STN_NM'),

          # air temperature
          "avg_ta" = datagokR:::find_xml(location, './AVG_TA', 'num'),
          "max_ta" = datagokR:::find_xml(location, './MAX_TA', 'num'),
          "max_ta_hrmt" = datagokR:::find_xml(location, './MAX_TA_HRMT'),
          "min_ta" = datagokR:::find_xml(location, './MIN_TA', 'num'),
          "min_ta_hrmt" = datagokR:::find_xml(location, './MIN_TA_HRMT'),

          # sea-level pressure
          "avg_ps" = datagokR:::find_xml(location, './AVG_PS', 'num'),
          "max_ps" = datagokR:::find_xml(location, './MAX_PS', 'num'),
          "max_ps_hrmt" = datagokR:::find_xml(location, './MAX_PS_HRMT'),
          "min_ps" = datagokR:::find_xml(location, './MIN_PS', 'num'),
          "min_ps_hrmt" = datagokR:::find_xml(location, './MIN_PS_HRMT'),

          # wind speed
          "avg_ws" = datagokR:::find_xml(location, './AVG_WS', 'num'),
          "max_wd" = datagokR:::find_xml(location, './MAX_WD', 'num'),
          "max_ws" = datagokR:::find_xml(location, './MAX_WS', 'num'),
          "max_ws_wd" = datagokR:::find_xml(location, './MAX_WS_WD', 'num'),
          "max_ws_hrmt" = datagokR:::find_xml(location, './MAX_WS_HRMT'),
          "max_ins_ws" = datagokR:::find_xml(location, './MAX_INS_WS', 'num'),
          "max_ins_ws_wd" = datagokR:::find_xml(location, './MAX_INS_WS_WD', 'num'),
          "max_ins_ws_hrmt" = datagokR:::find_xml(location, './MAX_INS_WS_HRMT'),

          # earth temperature
          "avg_cm10_te" = datagokR:::find_xml(location, './AVG_CM10_TE', 'num'),
          "avg_cm20_te" = datagokR:::find_xml(location, './AVG_CM20_TE', 'num'),
          "avg_cm30_te" = datagokR:::find_xml(location, './AVG_CM30_TE', 'num'),
          "avg_cm5_te" = datagokR:::find_xml(location, './AVG_CM5_TE', 'num'),
          "avg_m0_5_te" = datagokR:::find_xml(location, './AVG_M0_5_TE', 'num'),
          "avg_m1_0_te" = datagokR:::find_xml(location, './AVG_M1_0_TE', 'num'),
          "avg_m1_5_te" = datagokR:::find_xml(location, './AVG_M1_5_TE', 'num'),
          "avg_m3_0_te" = datagokR:::find_xml(location, './AVG_M3_0_TE', 'num'),
          "avg_m5_0_te" = datagokR:::find_xml(location, './AVG_M5_0_TE', 'num'),

          # rain
          "n9_9_rn" = datagokR:::find_xml(location, './N9_9_RN', 'num'),
          "mi10_max_rn" = datagokR:::find_xml(location, './MI10_MAX_RN', 'num'),
          "mi10_max_rn_hrmt" = datagokR:::find_xml(location, './MI10_MAX_RN_HRMT', 'num'),
          "hr1_max_rn" = datagokR:::find_xml(location, './HR1_MAX_RN', 'num'),
          "hr1_max_rn_hrmt" = datagokR:::find_xml(location, './HR1_MAX_RN_HRMT', 'num'),
          "sum_rn" = datagokR:::find_xml(location, './SUM_RN', 'num'),
          "sum_rn_dur" = datagokR:::find_xml(location, './SUM_RN_DUR', 'num'),

          # snow
          "dd_mefs" = datagokR:::find_xml(location, './DD_MEFS', 'num'),
          "dd_mefs_hrmt" = datagokR:::find_xml(location, './DD_MEFS_HRMT', 'num'),
          "dd_mes" = datagokR:::find_xml(location, './DD_MES', 'num'),
          "dd_mes_hrmt" = datagokR:::find_xml(location, './DD_MES_HRMT', 'num'),
          "sum_dpth_fhsc" = datagokR:::find_xml(location, './SUM_DPTH_FHSC', 'num'),

          # relative humidity
          "avg_rhm" = datagokR:::find_xml(location, './AVG_RHM', 'num'),
          "min_rhm" = datagokR:::find_xml(location, './MIN_RHM', 'num'),
          "min_rhm_hrmt" = datagokR:::find_xml(location, './MIN_RHM_HRMT'),

          # etc
          "hr1_max_icsr" = datagokR:::find_xml(location, './HR1_MAX_ICSR', 'num'),
          "hr1_max_icsr_hrmt" = datagokR:::find_xml(location, './HR1_MAX_ICSR_HRMT', 'num'),
          "ss_dur" = datagokR:::find_xml(location, './SS_DUR', 'num'),

          "min_tg" = datagokR:::find_xml(location, './MIN_TG', 'num'),
          "avg_td" = datagokR:::find_xml(location, './AVG_TD', 'num'),
          "avg_tca" = datagokR:::find_xml(location, './AVG_TCA', 'num'),
          "avg_lmac" = datagokR:::find_xml(location, './AVG_LMAC', 'num'),
          "avg_pv" = datagokR:::find_xml(location, './AVG_PV', 'num'),
          "avg_ts" = datagokR:::find_xml(location, './AVG_TS', 'num'),
          "avg_pa" = datagokR:::find_xml(location, './AVG_PA', 'num'),

          "sum_lrg_ev" = datagokR:::find_xml(location, './SUM_LRG_EV', 'num'),
          "sum_sml_ev" = datagokR:::find_xml(location, './SUM_SML_EV', 'num'),
          "sum_fog_dur" = datagokR:::find_xml(location, './SUM_FOG_DUR', 'num'),
          "sum_gsr" = datagokR:::find_xml(location, './SUM_GSR', 'num'),
          "sum_ss_hr" = datagokR:::find_xml(location, './SUM_SS_HR', 'num'),
          "sum_rws_hr24" = datagokR:::find_xml(location, './HR24_SUM_RWS', 'num'),
          stringsAsFactors = F
        ) %>% dplyr::as.tbl()
      } # if statement regarding to SuccessYN.
    } # end of loop i.
    eror.data <- dplyr::bind_rows(re.data)
    data <- dplyr::bind_rows(data, eror.data)
  } # end of errorCheck statement.

  result <- list(
    meta = meta,
    data = data,
    plot = NULL
  )
}
