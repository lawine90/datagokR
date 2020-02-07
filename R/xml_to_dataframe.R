#' convert xml_nodeset to data.frame, attributes as column names.
#'
#' @param nodeset result of xml2::xml_find_all function.
#'
xml_to_dataframe <- function(nodeset){
  if(class(nodeset) != 'xml_nodeset'){
    stop('Input should be "xml_nodeset" class')
  }
  lst <- lapply(nodeset, function(x){
    tmp <- xml2::xml_text(xml2::xml_children(x))
    names(tmp) <- xml2::xml_name(xml2::xml_children(x))
    return(as.list(tmp))
  })
  result <- do.call(plyr::rbind.fill, lapply(lst, function(x)
    as.data.frame(x, stringsAsFactors = F)))
  return(dplyr::as.tbl(result))
}
