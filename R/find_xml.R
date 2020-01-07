#' Find text from xml2::read_xml object and if not exist, return NA.
#'
#' @param xml result of xml2::read_xml function.
#' @param tag tag which you want to find.
#'
find_xml <- function(xml, tag, type = 'char'){
  if(class(xml) != 'xml_nodeset'){
    stop('Input xml should be "xml_nodeset" class')
  }
  if(!grepl('./', tag)){
    warning('Input tag should be start with "./****"')
  }
  if(type == 'char'){
    return(xml2::xml_text(xml2::xml_find_first(xml, tag)))
  }else if(type == 'num'){
    return(as.numeric(xml2::xml_text(xml2::xml_find_first(xml, tag))))
  }
}
