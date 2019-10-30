#' Data for kmaASOS function.
#'
#' A dataframe containing information about branches with observe weather(ASOS) data.
#'  It is tbl_df type object, so "dplyr" library is recommended.
#'  The variables are as follow.
#'
#'
#'  \itemize{
#'   \item code. numeric branch code.
#'   \item startDate. date of the branch is opened.
#'   \item endDate. date of the branch is expired. If empty, it means that the brach is still operated.
#'   \item name. location.
#'   \item long. longitude code.
#'   \item lat. latitude code.
#'  }
#'
#'  @docType data
#'
#'  @usage data(kma_branches)
#'
#'  @format A dplyr Dataframe with 7 columns and 4,626 rows
#'
#'  @keywords datasets
#'
#'  @name kma_branches
#'
#'  @examples
#'  data(kma_branches)
"kma_branches"
