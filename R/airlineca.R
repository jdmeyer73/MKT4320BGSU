#' Airline Conjoint Analysis Data
#'
#' Data from a traditional conjoint analysis where respondents rated 9 profiles 
#'     on three attributes.
#'
#' @usage data(airlineca)
#' @format
#' A data frame with 360 rows and 9 columns:
#' \describe{
#'   \item{\code{caseid}}{Identification number for each case}
#'   \item{\code{bundle}}{Identification number for each profile}
#'   \item{\code{value}}{Profile rating (0:Low to 100:High)}
#'   \item{\code{airline}}{Airline brand attribute (SW, Delta, Spirit)}
#'   \item{\code{connect}}{Number of connections attribute (None, One)}
#'   \item{\code{price}}{Ticket price attribute ($300, $450, $600)}
#'   \item{\code{inc}}{Respondent income in 000s}
#'   \item{\code{type}}{Respondent flight type (Business, Pleasure)}
#'   \item{\code{age}}{Respondent age in years}
#' }
#' @source Unknown
"airlineca"
