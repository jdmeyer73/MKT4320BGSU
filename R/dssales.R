#' Department Store Sales
#'
#' Sales data for 28 department store locations across 47 weeks and 
#' 69 departments.
#'
#' @details
#' Used for simple examples in linear regression with dummy variables.
#'
#' @docType data
#' @format A data frame with 74,463 rows and 14 variables:
#' \describe{
#'   \item{store}{Numeric. Store identification number.}
#'   \item{week}{Numeric. Week number.}
#'   \item{dept}{Numeric. Department number.}
#'   \item{sales}{Numeric. Weekly sales for store, department combination.}
#'   \item{size}{Numeric. Overall store size in square feet.}
#'   \item{clout}{Numeric. Index for store/week promotions: clearance.}
#'   \item{coup}{Numeric. Index for store/week promotions: coupons.}
#'   \item{tpr}{Numeric. Index for store/week promotions: price reductions.}
#'   \item{samp}{Numeric. Index for store/week promotions: sampling.}
#'   \item{flyer}{Numeric. Index for store/week promotions: flyer.}
#'   \item{endcap}{Numeric. Index for store/week promotions: endcap.}
#'   \item{weekdate}{Character. Week in date format "yyyy-mm-dd".}
#'   \item{quarter}{Factor. Quarter based on weekdate: 1, 2, 3, 4.}
#'   \item{q4}{Factor. "Yes" if Quarter = 4, "No" otherwise.}
#' }
#' @source Unknown.
#' @keywords datasets
"dssales"
