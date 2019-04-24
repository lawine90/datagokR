#' Data for kma_lifeIndex function.
#'
#' 3 dataset, a data.frame for administrative location code and the name of locations(kma_lifeIndex_locale_code),
#'  a matrix contains T/F value which means availibility of each life index by month,
#'  and a character vecter for API url.
#'  It is tbl_df type object so "dplyr" library is recommended.
#'  The variables are as follow.
#'
#'  \itemize{
#'   \item code. five-digit administrative location code.
#'   \item name1. korean location name for Si or Do.
#'   \item name2. korean location name for Si or Gu.
#'   \item name3. korean location name for Eup, Myeon or Dong.
#'  }
#'
#'  @docType data
#'  @keywords datasets
#'  @name kma_lifeIndex
#'  @usage data("kma_lifeIndex")
#'  @format A tbl_df type data frame with 3772 rows and 4 columns. A 8 by 12 matrix. A 8 length vector.
#'  @source https://www.data.go.kr/dataset/15000232/openapi.do?mypageFlag=Y
NULL
