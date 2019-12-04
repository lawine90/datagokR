#' Data for episPrice function.
#'
#' A dataset contain items, and species codes and names.
#'  It is tbl_df type object, so "dplyr" library is recommended.
#'  The variables are as follow.
#'
#'  \itemize{
#'   \item itemCode. three-digit item codes. it used for searching episPrice function's code argument
#'   \item itemName. korean item names.
#'   \item speciesCode. one or two digit species codes.
#'   \item speciesName. korean species names.
#'  }
#'
#'  @docType data
#'
#'  @usage data(epis_items)
#'
#'  @format A tbl_df type data frame with 351 rows and 4 columns
#'
#'  @keywords datasets
#'
#'  @name epis_items
#'
#'  @source http://data.mafra.go.kr/opendata/data/indexOpenDataDetail.do?data_id=20141220000000000380&filter_ty=
#'
#'  @examples
#'  data(epis_items)
"epis_items"
