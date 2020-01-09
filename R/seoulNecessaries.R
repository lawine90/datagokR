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

  tmp_json <- datagokR:::try_read_json(url)
  if(any(class(tmp_json) %in% 'error')){
    warning('Json parsing fail.\nPlease try again.\nIt maybe caused by api key.')
    return(NULL)
  }else{
    total <- tmp_json$ListNecessariesPricesService$list_total_count
  }

  all_data <- list()
  urls <- sprintf("http://openapi.seoul.go.kr:8088/%s/%s/%s/%s/%s/",
                  key, 'json', 'ListNecessariesPricesService',
                  trimws(format(seq(1, total, by = 1000), scientific = F)),
                  trimws(format(seq(1, total, by = 1000)+999, scientific = F)))

  if(recent == T){urls = urls[1:7]}
  if(verbose == T){pb <- txtProgressBar(min = 1, length(urls), style = 3)}
  for(i in 1:length(urls)){
    tmp_json <- datagokR:::try_read_json(urls[i])
    if(any(class(tmp_json) %in% 'error')){
      warning('Json parsing fail.\nPlease try again.\nIt maybe caused by api key.')
      return(NULL)
    }else{
      loc <- tmp_json$ListNecessariesPricesService$row
    }

    all_data[[i]] <- data.frame(
      id = datagokR:::find_xmlList(loc, 'P_SEQ'),
      market_id = datagokR:::find_xmlList(loc, 'M_SEQ'),
      market_name = datagokR:::find_xmlList(loc, 'M_NAME'),
      market_type = datagokR:::find_xmlList(loc, 'M_TYPE_NAME'),
      market_dir = datagokR:::find_xmlList(loc, 'M_GU_NAME'),
      ness_id = datagokR:::find_xmlList(loc, 'A_SEQ'),
      ness_name = datagokR:::find_xmlList(loc, 'A_NAME'),
      ness_unit = datagokR:::find_xmlList(loc, 'A_UNIT'),
      ness_price = datagokR:::find_xmlList(loc, 'A_PRICE'),
      date = datagokR:::find_xmlList(loc, 'P_DATE'),
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
