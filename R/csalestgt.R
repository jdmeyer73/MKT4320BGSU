#' Online Retailer Sales
#'
#' Data on a sample of about 2300 customers that have purchased items in the
#'     past 24 months from an online retailer that sells small household
#'     goods. \cr
#'     \cr
#'     \strong{Used in Lab Assignment 4}
#'
#' @usage data(csalestgt)
#' @format
#' A data frame with 2350 rows and 8 columns:
#' \describe{
#'   \item{\code{own}}{Factor variable for home ownership with two levels
#'       (\emph{Yes} if the customer is a homeowner, \emph{No} otherwise)}
#'   \item{\code{agecat}}{Factor variable with four levels for the customer’s
#'       age category (\emph{18-24}; \emph{25-44}; \emph{45-64};
#'       \emph{65+})}
#'   \item{\code{loyalty}}{Factor variable with two levels (\emph{Yes} if the
#'       customer is a retailer loyalty card member, \emph{No} otherwise)}
#'   \item{\code{carloan}}{Factor variable with two levels: \emph{Yes} if
#'       the customer has a car loan through the bank, \emph{No} otherwise}
#'   \item{\code{credcat}}{Factor variable with three levels for the customer’s
#'       credit score classification (\emph{High}, \emph{Medium}, \emph{Low})}
#'   \item{\code{kids}}{Factor variable with two levels (\emph{Yes} if
#'       household contains children under 18, \emph{No} otherwise)}
#'   \item{\code{freq}}{Number of transactions over the past 24 months}
#'   \item{\code{spend}}{Dollar amount spend over the past 24 months}
#'   \item{\code{rec}}{Months since last purchase}
#' }
#' @source Unknown
"csalestgt"
