#' Candy Sales
#'
#' Weekly unit sales for a bag of candy over 135 weeks at a particular grocery
#'     store, starting in February 2010. The unit sales are averaged across all
#'     store locations. \cr
#'     \cr
#'     \strong{Used in Lab Assignment 2}
#'
#' @usage data(candydata)
#' @format
#' A data frame with 135 rows and 7 columns:
#' \describe{
#'   \item{\code{ad}}{Indicator if the candy was in the weekly ad (Yes/No)}
#'   \item{\code{endcap}}{Indicator if the candy was on an endcap (Yes/No)}
#'   \item{\code{unitssold}}{Weekly units sold in 000s}
#'   \item{\code{weeknum}}{Sequential week number from February 2010 on}
#'   \item{\code{price}}{Price for the week, in dollars}
#'   \item{\code{date}}{Date of start of week}
#'   \item{\code{month}}{Factor variable with 12 levels (abbreviated month name)}
#' }
#' @source Unknown
"candydata"
