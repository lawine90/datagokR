#' Find text from xml2::read_xml object and if not exist, return NA.
#'
#' @param xml result of xml2::read_xml function.
#' @param tag tag which you want to find.
#'
find_xml <- function(xml, tag){
  if(length(xml2::xml_text(xml2::xml_find_all(xml, tag))) != 0){
    return(xml2::xml_text(xml2::xml_find_all(xml, tag)))
  }else{
    return(NA)
  }
}
