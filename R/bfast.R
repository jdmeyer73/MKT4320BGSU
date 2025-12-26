#' Breakfast Food Preference Data
#'
#' Poll-style data on breakfast food preferences with basic demographics and
#' lifestyle measures.
#'
#' @details
#' Commonly used for teaching multinomial logistic regression.
#'
#' @docType data
#' @format A data frame with 880 rows and 6 variables:
#' \describe{
#'   \item{agecat}{Factor. Age category with levels \code{Under 31}, \code{31-45},
#'     \code{46-60}, \code{Over 60}.}
#'   \item{gender}{Factor. Gender with levels \code{Female}, \code{Male}.}
#'   \item{marital}{Factor. Marital status with levels \code{Married}, \code{Unmarried}.}
#'   \item{bfast}{Factor. Breakfast preference with levels \code{Cereal}, \code{Bar}, \code{Oatmeal}.}
#'   \item{lifestyle}{Factor. Lifestyle with levels \code{Active}, \code{Inactive}.}
#'   \item{age}{Integer. Age in years.}
#' }
#' @source Adapted from IBM SPSS Statistics Sample Files.
"bfast"
