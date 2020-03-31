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
#'  # example.
#'  data <- nutriComponent(key, verbose = T)
#'
#' @export

nutriComponent <- function(key, verbose = F){
  ### 1. parameter checking and processing.
  ## key
  if(is.null(key)){ stop("Invalid key. \n Please issue API key first and insert it to \"key\" param.") }

  ### 2. REST url for get n of pages
  ## End Point.
  url <- sprintf("http://apis.data.go.kr/1470000/%s/%s?serviceKey=%s&numOfRows=100&pageNo=%s",
                 'FoodNtrIrdntInfoService', 'getFoodNtrItdntList', key, '1')

  tmp_xml <- datagokR:::try_xmlToList(url)
  nofpage <- (as.numeric(tmp_xml$body$totalCount)/as.numeric(tmp_xml$body$numOfRows))
  nofpage <- ceiling(nofpage)

  if(length(nofpage) == 0){
    warning(tmp_xml$cmmMsgHeader$returnAuthMsg)
  }

  # the number of pages.
  urls <- unlist(lapply(1:nofpage, function(x) sprintf("http://apis.data.go.kr/1470000/%s/%s?serviceKey=%s&numOfRows=100&pageNo=%s",
                                                       'FoodNtrIrdntInfoService', 'getFoodNtrItdntList', key, x)))

  ### 3. first urls's xml parsing.
  # parsing xml codes with repeat and trycatch.
  if(length(urls) == 1){verbose <- FALSE}
  if(verbose == T){pb <- txtProgressBar(min = 1, length(urls), style = 3)}

  all_data <- list()
  for(i in 1:length(urls)){
    tmp_xml <- datagokR:::try_xmlToList(urls[i])
    if(is.null(tmp_xml$body$totalCount)){
      warning(tmp_xml$cmmMsgHeader$returnAuthMsg)
      next()
    }

    all_data[[i]] <- data.frame(
      name_kor = datagokR:::find_xmlList(tmp_xml$body$items, "DESC_KOR"),
      serving_wt = datagokR:::find_xmlList(tmp_xml$body$items, "SERVING_WT"),
      kcal = datagokR:::find_xmlList(tmp_xml$body$items, "NUTR_CONT1"),
      carbohydrate = datagokR:::find_xmlList(tmp_xml$body$items, "NUTR_CONT2"),
      protein = datagokR:::find_xmlList(tmp_xml$body$items, "NUTR_CONT3"),
      fat = datagokR:::find_xmlList(tmp_xml$body$items, "NUTR_CONT4"),
      sugar = datagokR:::find_xmlList(tmp_xml$body$items, "NUTR_CONT5"),
      sodium = datagokR:::find_xmlList(tmp_xml$body$items, "NUTR_CONT6"),
      cholesterol = datagokR:::find_xmlList(tmp_xml$body$items, "NUTR_CONT7"),
      saturated_fatty_acid = datagokR:::find_xmlList(tmp_xml$body$items, "NUTR_CONT8"),
      trans_fatty_acid = datagokR:::find_xmlList(tmp_xml$body$items, "NUTR_CONT9"),
      year = datagokR:::find_xmlList(tmp_xml$body$items, "BGN_YEAR"),
      factory = datagokR:::find_xmlList(tmp_xml$body$items, "ANIMAL_PLANT"),
      stringsAsFactors = F
    )
    if(verbose == T){setTxtProgressBar(pb, value = i)}
  }

  merged <- dplyr::bind_rows(all_data)
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
