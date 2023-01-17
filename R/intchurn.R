#' ISP Churn
#'
#' Data on a sample of about 4800 customers that were customers of an
#'     internet service provider (ISP) in the previous month. However, some
#'     of those customers churned within the last month. \cr
#'     \cr
#'     \strong{Used in Lab Assignment 4}
#'
#' @usage data(intchurn)
#' @format
#' A data frame with 4835 rows and 13 columns:
#' \describe{
#'   \item{\code{churn}}{Factor variable for home ownership with two levels
#'       (\emph{Yes} if the customer churned, \emph{No} otherwise)}
#'   \item{\code{tenure}}{Length of time customer has been with the ISP,
#'       in months}
#'   \item{\code{bill}}{Billed amount in the previous month, in dollars}
#'   \item{\code{numsvcs}}{Number of additional services the customer
#'       subscribes to from the ISP}
#'   \item{\code{senior}}{Factor variable with two levels (\emph{Yes} if the
#'       customer is a senior citizen, \emph{No} otherwise)}
#'   \item{\code{gender}}{Factor variable with two levels (\emph{Male} and
#'       \emph{Female})}
#'   \item{\code{married}}{Factor variable with two levels (\emph{Yes} if the
#'       customer is married, \emph{No} otherwise)}
#'   \item{\code{dep}}{Factor variable with two levels (\emph{Yes} if the
#'       customer has dependents, \emph{No} otherwise)}
#'   \item{\code{inttype}}{Factor variable with two levels indicating the
#'       customer's type of internet service (\emph{DSL} and \emph{Fiber})}
#'   \item{\code{contract}}{Factor variable with three levels indicating the
#'       contract type (\emph{Monthly}, \emph{One year}, and \emph{Two year})}
#'   \item{\code{ebill}}{Factor variable with two levels (\emph{Yes} if the
#'       customer uses electronic billing, \emph{No} otherwise)}
#'   \item{\code{payment}}{Factor variable with four levels indicating how
#'       payments are made (\emph{Bank transfer}, \emph{Credit card},
#'       \emph{Bill pay}, and \emph{Check})}
#' }
#' @source Adapted from \url{https://www.kaggle.com/datasets/blastchar/telco-customer-churn}
"intchurn"
