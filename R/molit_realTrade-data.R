#' Data for molit_realTrade function.
#'
#' A dataset containing the legal location code and the name of locations.
#'  It is tbl_df type object so "dplyr" library is recommended.
#'  The variables are as follow.
#'
#'  \itemize{
#'   \item code. five-digit legal location code.
#'   \item name. korean location name.
#'   \item exist. existance of the code. 277 codes are exist and 184 are expired code.
#'  }
#'
#'  @docType data
#'
#'  @usage data(molit_realTrade)
#'
#'  @format A tbl_df type data frame with 461 rows and 3 columns
#'
#'  @keywords datasets
#'
#'  @name molit_realTrade
#'
#'  @source https://www.code.go.kr/index.do
#'
#'  @examples
#'  data(molit_realTrade)
"molit_realTrade"
