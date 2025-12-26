#' Headphones Conjoint Analysis Data
#'
#' Ratings-based conjoint analysis data for headphones, where respondents
#' evaluated multiple product profiles defined by brand and features.
#'
#' @docType data
#' @format A data frame with 1,539 rows and 9 variables:
#' \describe{
#'   \item{id}{Integer. Respondent identifier.}
#'   \item{profile}{Integer. Profile identifier.}
#'   \item{rating}{Numeric. Profile rating on a 0--100 scale
#'     (\code{0 = low}, \code{100 = high}).}
#'   \item{brand}{Factor or character. Brand attribute
#'     (\code{Sony}, \code{Beats}, \code{Bose}).}
#'   \item{bass}{Factor or character. Bass level attribute
#'     (\code{Low}, \code{Medium}, \code{High}).}
#'   \item{price}{Factor or character. Price attribute
#'     (\code{99}, \code{149}, \code{199}).}
#'   \item{income}{Numeric. Respondent income in thousands of dollars.}
#'   \item{age}{Numeric. Respondent age in years.}
#'   \item{gender}{Factor or character. Respondent gender.}
#' }
#' @source Unknown.
"hphones"
