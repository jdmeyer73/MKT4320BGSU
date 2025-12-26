#' Insurance Sales Calls
#'
#' Customer-level data from outbound insurance sales calls made by a bank that
#' also offers car insurance.
#'
#' @docType data
#' @format A data frame with 3,831 rows and 9 variables:
#' \describe{
#'   \item{id}{Integer. Observation identifier.}
#'   \item{age}{Numeric. Customer age in years.}
#'   \item{balance}{Numeric. Average bank balance over the previous month.}
#'   \item{carloan}{Factor. Has a car loan with the bank (\code{Yes}, \code{No}).}
#'   \item{buy}{Factor. Purchased car insurance (\code{Yes}, \code{No}).}
#'   \item{educ}{Factor. Education level
#'     (\code{LT_HS}, \code{HS}, \code{GT_HS}).}
#'   \item{marital}{Factor. Marital status
#'     (\code{Divorced}, \code{Married}, \code{Single}).}
#'   \item{homeins}{Factor. Has homeowner's insurance with the bank
#'     (\code{Yes}, \code{No}).}
#'   \item{preout}{Factor. Outcome of previous non-car insurance call
#'     (\code{No Previous}, \code{Failure}, \code{Success}).}
#' }
#' @source Adapted from the UCI Machine Learning Repository (Bank Marketing).
"insalescall"
