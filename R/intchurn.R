#' Internet Service Provider (ISP) Churn
#'
#' Customer data from an internet service provider indicating whether customers
#' churned in the most recent month.
#'
#' @docType data
#' @format A data frame with 4,835 rows and 13 variables:
#' \describe{
#'   \item{churn}{Factor. Customer churned in the last month (\code{Yes}, \code{No}).}
#'   \item{tenure}{Integer. Length of customer relationship in months.}
#'   \item{bill}{Numeric. Billed amount in the previous month (U.S. dollars).}
#'   \item{numsvcs}{Integer. Number of additional services subscribed.}
#'   \item{senior}{Factor. Senior citizen indicator (\code{Yes}, \code{No}).}
#'   \item{gender}{Factor. Gender (\code{Male}, \code{Female}).}
#'   \item{married}{Factor. Married indicator (\code{Yes}, \code{No}).}
#'   \item{dep}{Factor. Has dependents (\code{Yes}, \code{No}).}
#'   \item{inttype}{Factor. Internet service type (\code{DSL}, \code{Fiber}).}
#'   \item{contract}{Factor. Contract type
#'     (\code{Monthly}, \code{One year}, \code{Two year}).}
#'   \item{ebill}{Factor. Uses electronic billing (\code{Yes}, \code{No}).}
#'   \item{payment}{Factor. Payment method
#'     (\code{Bank transfer}, \code{Credit card}, \code{Bill pay}, \code{Check}).}
#' }
#' @source Adapted from a Kaggle Telco Customer Churn dataset.
"intchurn"
