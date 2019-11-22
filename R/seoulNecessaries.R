#' data.seoul.go.kr, daily necessaries price.
#'
#' seoulNecessaries function import daily necessaries's price data from traditional market and large retailer. The data updated twice a week(tuesday and friday)
#'
#' @param key character value. API key issued from <www.data.go.kr>. no default.
#' @param recent logical value. if TRUE, download only recent 2,000 rows. Default value set as True.
#' @param verbose logical value. if TRUE, provide process bar. Default value set as false.
#'
#' @examples
#'  key <- 'your key issued from data.seoul.go.kr'
#'
#'  # example.
#'  data <- seoulNecessaries(key, recent = T, verbose = T)
#'
#' @export

seoulNecessaries <- function(key, recent = T, verbose = F){
  ### 1. parameter checking and processing.
  ## key
  if(is.null(key)){ stop("Invalid key. \n Please issue API key first and insert it to \"key\" param.") }

  ### 2. REST url for get n of pages
  url <- sprintf("http://openapi.seoul.go.kr:8088/%s/%s/%s/%s/%s/",
                 key, 'json', 'ListNecessariesPricesService', 1, 2)

  ii <- 0
  repeat{
    ii <- ii + 1
    tmp_xml <- tryCatch({jsonlite::read_json(url)}, error = function(e){NULL})
    if(!is.null(tmp_xml) | ii == 15) break
  }

  if(is.null(tmp_xml)){
    stop('XML parsing fail.Please try again.')
  }else{
    total <- tmp_xml$ListNecessariesPricesService$list_total_count
  }

  all_data <- list()
  urls <- sprintf("http://openapi.seoul.go.kr:8088/%s/%s/%s/%s/%s/",
                  key, 'json', 'ListNecessariesPricesService',
                  trimws(format(seq(1, total, by = 1000), scientific = F)),
                  trimws(format(seq(1, total, by = 1000)+999, scientific = F)))

  if(recent == T){urls = urls[1:7]}
  if(verbose == T){pb <- txtProgressBar(min = 1, length(urls), style = 3)}
  for(i in 1:length(urls)){
    ii <- 0
    repeat{
      ii <- ii + 1
      tmp_xml <- tryCatch({jsonlite::read_json(urls[i])}, error = function(e){NULL})
      if(!is.null(tmp_xml) | ii == 15) break
    }

    if(is.null(tmp_xml)){
      stop('XML parsing fail.Please try again.')
    }else{
      loc <- tmp_xml$ListNecessariesPricesService$row
    }

    all_data[[i]] <- data.frame(
      id = as.numeric( lapply(loc, function(x) ifelse(is.null(x$"P_SEQ"), NA, x$"P_SEQ")) ),
      market_id = as.numeric( lapply(loc, function(x) ifelse(is.null(x$"M_SEQ"), NA, x$"M_SEQ")) ),
      market_name = unlist( lapply(loc, function(x) ifelse(is.null(x$"M_NAME"), NA, x$"M_NAME")) ),
      market_type = unlist( lapply(loc, function(x) ifelse(is.null(x$"M_TYPE_NAME"), NA, x$"M_TYPE_NAME")) ),
      market_dir = unlist( lapply(loc, function(x) ifelse(is.null(x$"M_GU_NAME"), NA, x$"M_GU_NAME")) ),
      ness_id = unlist( lapply(loc, function(x) ifelse(is.null(x$"A_SEQ"), NA, x$"A_SEQ")) ),
      ness_name = unlist( lapply(loc, function(x) ifelse(is.null(x$"A_NAME"), NA, x$"A_NAME")) ),
      ness_unit = unlist( lapply(loc, function(x) ifelse(is.null(x$"A_UNIT"), NA, x$"A_UNIT")) ),
      ness_price = unlist( lapply(loc, function(x) ifelse(is.null(x$"A_PRICE"), NA, x$"A_PRICE")) ),
      date = unlist( lapply(loc, function(x) ifelse(is.null(x$"P_DATE"), NA, x$"P_DATE")) ),
      stringsAsFactors = F
    )
    if(verbose == T){setTxtProgressBar(pb, value = i)}
  }

  merged <- dplyr::bind_rows(all_data)
  for(col in colnames(merged)){
    if(class(merged[[col]]) == 'character'){
      Encoding(merged[[col]]) <- 'UTF-8'
    }
  }

  return(dplyr::as.tbl(merged))
}
