#' Ministry of Food and Drug Safety, Food nutritive components information.
#'
#' nutriComponent function import the food nutritive component information.
#'
#' @param key character value. API key issued from <www.data.go.kr>. no default.
#' @param verbose logical value. if TRUE, provide process bar. Default value set as false.
#'
#' @examples
#'  key <- 'your key issued from data.go.kr'
#'
#'  # example 1 searching by localeCode.
#'  data <- nutriComponent(key, verbose = T)
#'
#' @export

nutriComponent <- function(key, verbose = F){
  ### 1. parameter checking and processing.
  ## key
  if(is.null(key)){ stop("Invalid key. \n Please issue API key first and insert it to \"key\" param.") }

  ### 2. REST url for get n of pages
  ## End Point.
  base <- "http://apis.data.go.kr/1470000/FoodNtrIrdntInfoService/getFoodNtrItdntList?"

  ## generate list of urls(fxxking so many limitations...).
  # 1st, (url + key)
  url <- sprintf("%sserviceKey=%s&numOfRows=100&pageNo=%s", base, key, '1')

  ii <- 0
  repeat{
    ii <- ii + 1
    tmp_xml <- tryCatch({XML::xmlToList(url)}, error = function(e){NULL})
    if(!is.null(tmp_xml) | ii == 15) break
  }

  if(is.null(tmp_xml)){
    stop('XML parsing fail.Please try again.')
  }

  # if(!is.null(tmp_xml$cmmMsgHeader)){
  #   stop(paste(tmp_xml$cmmMsgHeader$returnAuthMsg, ".\nError Code: ",
  #              tmp_xml$cmmMsgHeader$returnReasonCode, sep = ""))
  # }

  # the number of pages.
  nofpage <- (as.numeric(tmp_xml$body$totalCount)/as.numeric(tmp_xml$body$numOfRows))
  nofpage <- ceiling(nofpage)

  urls <- lapply(1:nofpage, function(x) sprintf("%sserviceKey=%s&numOfRows=100&pageNo=%s", base, key, x))
  urls <- unlist(urls)

  ### 3. first urls's xml parsing.
  # parsing xml codes with repeat and trycatch.
  if(verbose == T){pb <- txtProgressBar(min = 1, length(urls), style = 3)}

  all_data <- list()
  for(i in 1:length(urls)){
    ii <- 0
    repeat{
      ii <- ii + 1
      tmp_xml <- tryCatch({XML::xmlToList(urls[i])}, error = function(e){NULL})
      if(!is.null(tmp_xml) | ii == 15) break
    }

    if(is.null(tmp_xml)){
      stop('XML parsing fail.Please try again.')
    }

    all_data[[i]] <- data.frame(
      name_kor = unlist( lapply(tmp_xml$body$items, function(x) ifelse(is.null(x$"DESC_KOR"), '', x$"DESC_KOR")) ),
      serving_wt = unlist( lapply(tmp_xml$body$items, function(x) ifelse(is.null(x$"SERVING_WT"), '', x$"SERVING_WT")) ),
      kcal = unlist( lapply(tmp_xml$body$items, function(x) ifelse(is.null(x$"NUTR_CONT1"), '', x$"NUTR_CONT1")) ),
      carbohydrate = unlist( lapply(tmp_xml$body$items, function(x) ifelse(is.null(x$"NUTR_CONT2"), '', x$"NUTR_CONT2")) ),
      protein = unlist( lapply(tmp_xml$body$items, function(x) ifelse(is.null(x$"NUTR_CONT3"), '', x$"NUTR_CONT3")) ),
      fat = unlist( lapply(tmp_xml$body$items, function(x) ifelse(is.null(x$"NUTR_CONT4"), '', x$"NUTR_CONT4")) ),
      sugar = unlist( lapply(tmp_xml$body$items, function(x) ifelse(is.null(x$"NUTR_CONT5"), '', x$"NUTR_CONT5")) ),
      sodium = unlist( lapply(tmp_xml$body$items, function(x) ifelse(is.null(x$"NUTR_CONT6"), '', x$"NUTR_CONT6")) ),
      cholesterol = unlist( lapply(tmp_xml$body$items, function(x) ifelse(is.null(x$"NUTR_CONT7"), '', x$"NUTR_CONT7")) ),
      saturated_fatty_acid = unlist( lapply(tmp_xml$body$items, function(x) ifelse(is.null(x$"NUTR_CONT8"), '', x$"NUTR_CONT8")) ),
      trans_fatty_acid = unlist( lapply(tmp_xml$body$items, function(x) ifelse(is.null(x$"NUTR_CONT9"), '', x$"NUTR_CONT9")) ),
      year = unlist( lapply(tmp_xml$body$items, function(x) ifelse(is.null(x$"BGN_YEAR"), '', x$"BGN_YEAR")) ),
      factory = unlist( lapply(tmp_xml$body$items, function(x) ifelse(is.null(x$"ANIMAL_PLANT"), '', x$"ANIMAL_PLANT")) ),
      stringsAsFactors = F
    )
    if(verbose == T){setTxtProgressBar(pb, value = i)}
  }

  merged <- bind_rows(all_data)
  for(col in colnames(merged)){
    Encoding(merged[[col]]) <- 'UTF-8'
    merged[[col]][merged[[col]] == 'N/A'] <- NA
    try(
      merged[[col]] <- as.numeric(merged[[col]]),
      silent = T
    )
  }

  return(dplyr::as.tbl(merged))
}
