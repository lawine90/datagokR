#' Data for kmaHealthIndex and kmaLifeIndex function.
#'
#' A dataset contain the legal location code and the name of locations.
#'  It is tbl_df type object so "dplyr" library is recommended.
#'  The variables are as follow.
#'
#'  \itemize{
#'   \item code. five-digit legal location code.
#'   \item name1. korean location name for Si/Do.
#'   \item name2. korean location name for Si/Gu.
#'   \item name3. korean location name for Dong.
#'  }
#'
#'  @docType data
#'
#'  @usage data(kma_locale_code)
#'
#'  @format A tbl_df type data frame with 3,772 rows and 4 columns
#'
#'  @keywords datasets
#'
#'  @name kma_locale_code
#'
#'  @source https://www.code.go.kr/index.do
#'
#'  @examples
#'  data(kma_locale_code)
"kma_locale_code"
