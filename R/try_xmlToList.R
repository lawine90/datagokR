#' Try XML::xmlToList function at least 5 times If got errors, save error message.
#'
#' @param url url want to read.
#' @param times trying times.
#'
try_xmlToList <- function(url, times = 5){
  ii <- 0
  repeat{
    ii <- ii + 1
    xml <- tryCatch(
      {XML::xmlToList(url, encoding = 'UTF-8')},
      error = function(e){e}
    )
    if(!any(class(xml) == 'error') | ii == times) break
  }; closeAllConnections(); gc(); return(xml)
}
