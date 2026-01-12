#' Telecom Customer Data
#'
#' Customer-level data from telecom company.
#'
#' @docType data
#' @format A data frame with 2,382 rows and 7 variables:
#' \describe{
#'   \item{avg6rev}{Numberic. Average monthly revenue (previous 6 months).}
#'   \item{avg6mou}{Numeric. Average monthly minutes used (previous 6 months).}
#'   \item{cc}{Numeric. Average monthly customer care calls (lifetime).}
#'   \item{da}{Numeric. Average monthly directory assistance calls (lifetime).}
#'   \item{ovrmou}{Numeric. Average monthly overage minutes (lifetime).}
#'   \item{income}{Numeric. Household income (in 000s).}
#'   \item{own}{Factor. Home owner (\code{Yes}, \code{No}).}
#' }
#' @source Adapted from Kaggle dataset (Telecom Company Churn).
"telecom"
