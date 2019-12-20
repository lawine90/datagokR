#' Find text from XML::xmlToList object and if not exist, return NA.
#'
#' @param xml result of xml2::read_xml function.
#' @param tag tag which you want to find.
#' @param type decide class of result between character(char) and numeric(num).
#'
find_xmlList <- function(xml, tag, type = 'char'){
  if(type == 'char'){
    return(unname(unlist(lapply(xml, function(x) ifelse(is.null(x[[tag]]), NA, x[[tag]])))))
  }else if(type == 'num'){
    return(unname(as.numeric(lapply(xml, function(x) ifelse(is.null(x[[tag]]), NA, x[[tag]])))))
  }
}
