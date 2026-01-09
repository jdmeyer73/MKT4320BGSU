#' Airline Conjoint Analysis Data
#'
#' Data from a ratings-based conjoint analysis in which respondents evaluated
#' airline profiles defined by multiple attributes.
#'
#' @details
#' Each respondent evaluated 9 airline profiles.
#'
#' @docType data
#' @format A data frame with 360 rows and 9 variables:
#' \describe{
#'   \item{caseid}{Integer. Respondent identifier.}
#'   \item{bundle}{Integer. Profile (stimulus) identifier.}
#'   \item{value}{Numeric. Profile rating on a 0--100 scale (\code{0 = Low}, \code{100 = High}).}
#'   \item{airline}{Factor or character. Airline brand (\code{SW}, \code{Delta}, \code{Spirit}).}
#'   \item{connect}{Factor or character. Number of connections (\code{None}, \code{One}).}
#'   \item{price}{Factor or character. Ticket price (\code{300}, \code{450}, \code{600}).}
#'   \item{inc}{Numeric. Respondent income in thousands of U.S. dollars.}
#'   \item{type}{Factor or character. Flight purpose (\code{Business}, \code{Pleasure}).}
#'   \item{age}{Numeric. Respondent age in years.}
#' }
#' @source Collected from MKT 3200 students for instructional purposes.
#' @keywords internal
"airlineca"
