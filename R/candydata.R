#' Candy Sales
#'
#' Weekly sales and marketing data for a bag of candy sold at a grocery store,
#' beginning in February 2010. Unit sales are averaged across store locations.
#'
#' @docType data
#' @format A data frame with 135 rows and 7 variables:
#' \describe{
#'   \item{ad}{Factor or character. Indicator for weekly advertising
#'     (\code{Yes}, \code{No}).}
#'   \item{endcap}{Factor or character. Indicator for endcap display
#'     (\code{Yes}, \code{No}).}
#'   \item{unitssold}{Numeric. Weekly units sold, measured in thousands.}
#'   \item{weeknum}{Integer. Sequential week number starting in February 2010.}
#'   \item{price}{Numeric. Weekly price in U.S. dollars.}
#'   \item{date}{Date. Start date of the sales week.}
#'   \item{month}{Factor. Month of the year (12 levels, abbreviated names).}
#' }
#' @source Unknown.
"candydata"
