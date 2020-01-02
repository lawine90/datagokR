#' Try httr::GET and httr::content function at least 5 times If got errors, save error message.
#'
#' @param url url want to read.
#' @param times trying times.
#'
try_GET_content <- function(url, times = 5){
  ii <- 0
  repeat{
    ii <- ii + 1
    xml <- tryCatch(
      {httr::GET(url) %>% httr::content(as = "parsed", encoding = 'UTF-8')},
      error = function(e){e}
    )
    if(!any(class(xml) == 'error') | ii == times) break
  }; closeAllConnections(); gc(); return(xml)
}
