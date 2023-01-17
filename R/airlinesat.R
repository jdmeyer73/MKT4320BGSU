#' Airline Satisfaction Data
#'
#' Dataset adapted from "A concise guide to market research" by Erik Mooi and
#' Marko Sarstedt. Contains passenger information, expectations, and
#' satisfaction ratings for a fictional airline, Oddjob Airways.
#'
#' @usage data(airlinesat)
#' @format
#' A data frame with 1,065 rows and 70 columns:
#' \describe{
#'   \item{age}{Age}
#'   \item{country}{Home Country}
#'   \item{flight_class}{Class of flight (Economy or Business)}
#'   \item{flight_latest}{Lastest flight with airline}
#'   \item{flight_purpose}{Purpose of flight (Business or Leisure)}
#'   \item{flight_type}{Type of flight (Domestic or International)}
#'   \item{gender}{Gender}
#'   \item{language}{Primary language}
#'   \item{nflights}{Flights in past 12 months}
#'   \item{status}{Loyalty program status}
#'   \item{e1 to e23}{Traveler expectations - scale 1(-) to 100(+)}
#'   \item{s1 to s23}{Traveler satisfaction with Oddjob - scale 1(-) to 100(+)}
#'   \item{nps}{Net Promoter Score}
#'   \item{sat1}{General satisfaction - scale 1(-) to 7(+)}
#'   \item{sat2}{Close to ideal airline - scale 1(-) to 7(+)}
#'   \item{sat3}{Meets expectations - scale 1(-) to 7(+)}
#'   \item{loy1}{Positive WOM - scale 1(-) to 7(+)}
#'   \item{loy2}{Recommend - scale 1(-) to 7(+)}
#'   \item{loy3}{Encourage - scale 1(-) to 7(+)}
#'   \item{loy4}{First choice - scale 1(-) to 7(+)}
#'   \item{loy5}{Intend to stay - scale 1(-) to 7(+)}
#'   \item{com1}{Committed - scale 1(-) to 7(+)}
#'   \item{com2}{Relationship meaningful - scale 1(-) to 7(+)}
#'   \item{com3}{Hard loss if out of business - scale 1(-) to 7(+)}
#'   \item{overall_sat}{Overall satisfaction - scale 1(-) to 7(+)}
#'   \item{reputation}{Overall reputation - scale 1(-) to 7(+)}
#' }
#' @source \url{https://www.guide-market-research.com/spss/downloads/}
"airlinesat"
