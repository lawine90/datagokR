#' Try xml2::read_xml function at least 5 times If got errors, save error message.
#'
#' @param url url want to read.
#' @param times trying times.
#'
try_read_xml <- function(url, times = 5){
  ii <- 0
  repeat{
    ii <- ii + 1
    xml <- tryCatch(
      {xml2::read_xml(url, encoding = 'UTF-8')},
      error = function(e){e}
    )
    if(!any(class(xml) == 'error') | ii == times) break
  }; closeAllConnections(); gc(); return(xml)
}
