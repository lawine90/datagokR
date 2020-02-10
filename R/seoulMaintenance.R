#' data.seoul.go.kr, realtime bike information.
#'
#' seoulMaintenance function import data about apartment maintenance cost.
#'
#' @param key character value. API key issued from <www.data.go.kr>. no default.
#' @param verbose logical value. if TRUE, provide process bar. Default value set as false.
#'
#' @examples
#'  key <- 'your key issued from data.seoul.go.kr'
#'
#'  # example.
#'  data <- seoulMaintenance(key)
#'
#' @export

seoulMaintenance <- function(key, verbose = FALSE){
  ### 1. parameter checking and processing.
  ## key
  if(is.null(key)){ stop("Invalid key. \n Please issue API key first and insert it to \"key\" param.") }

  ### 2. REST url for get n of pages
  url <- sprintf("http://openapi.seoul.go.kr:8088/%s/json/AptView/%s/%s/", key, 1, 2)
  tmp_json <- datagokR:::try_read_json(urls[i])

  if(any(class(tmp_json) %in% 'error')){
    warning('Json parsing fail.\nPlease try again.\nIt maybe caused by api key.')
    return(NULL)
  }else{
    total <- tmp_json$AptView$list_total_count
  }

  all_data <- list()
  urls <- sprintf("http://openapi.seoul.go.kr:8088/%s/%s/%s/%s/%s/", key, 'json',
                  'AptView', seq(1, total, by = 1000), seq(1, total, by = 1000)+999)
  if(verbose == T){pb <- txtProgressBar(min = 1, length(urls), style = 3)}
  for(i in 1:length(urls)){
    tmp_json <- datagokR:::try_read_json(urls[i])
    if(any(class(tmp_json) %in% 'error')){
      warning('Json parsing fail.\nPlease try again.\nIt maybe caused by api key.')
      next()
    }else{
      loc <- tmp_json$AptView$row
    }
    all_data[[i]] <- dplyr::bind_rows(loc)
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
