#' Bank Marketing
#'
#' The data is related with direct marketing campaigns (phone calls) of a
#' Portuguese banking institution attempting to get people to open a 
#' term deposit account.
#'
#' @docType data
#' @format A data frame with 3,536 rows and 8 variables:
#' \describe{
#'   \item{age}{Numeric. Customer age in years.}
#'   \item{balance}{Numeric. Average bank balance over the previous month.}
#'   \item{educ}{Factor. Education level
#'     (\code{Primary}, \code{Secondary}, \code{Tertiary}).}
#'   \item{default}{Factor. Has credit in default (\code{Yes}, \code{No}).}
#'   \item{housing}{Factor. Has housing loan (\code{Yes}, \code{No}).}
#'   \item{loan}{Factor. Has personal loan (\code{Yes}, \code{No}).}
#'   \item{response}{Factor. Opened term deposit (\code{Yes}, \code{No}).}
#'   \item{married}{Factor. Marital status (\code{Yes}, \code{No}).}
#' }
#' @source Adapted from the UCI Machine Learning Repository (Bank Marketing).
"bankmktg"
