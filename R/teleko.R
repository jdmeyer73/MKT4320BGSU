#' Teleko Mobile A/B Testing Data
#'
#' Results of an A/B test conducted for Teleko Mobile, examining customer renewal
#' behavior and service usage.
#'
#' @docType data
#' @format A data frame with 10,000 rows and 12 variables:
#' \describe{
#'   \item{renew}{Factor or character. Customer renewed service
#'     (\code{Yes}, \code{No}).}
#'   \item{clv}{Numeric. Predicted customer lifetime value (future revenue).}
#'   \item{promotion}{Factor or character. Received promotion
#'     (\code{Yes}, \code{No}).}
#'   \item{tenure}{Integer. Months since contract start.}
#'   \item{past60}{Numeric. Expenditures in the 60 days prior to renewal decision.}
#'   \item{past30}{Numeric. Expenditures in the 30 days prior to renewal decision.}
#'   \item{past7}{Numeric. Expenditures in the 7 days prior to renewal decision.}
#'   \item{data}{Factor or character. Has data package
#'     (\code{Yes}, \code{No}).}
#'   \item{inter}{Factor or character. Has international plan
#'     (\code{Yes}, \code{No}).}
#'   \item{family}{Factor or character. Has family plan
#'     (\code{Yes}, \code{No}).}
#'   \item{csvc}{Integer. Number of customer service calls in prior 30 days.}
#'   \item{os}{Factor or character. Phone operating system
#'     (\code{Android}, \code{iOS}, \code{Other}).}
#' }
#' @source Harvard Business School Publishing, Case 523-005.
"teleko"
