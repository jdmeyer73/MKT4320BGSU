#' Yogurt Choice Data
#'
#' Long-format choice data from a yogurt brand choice experiment involving
#' four competing brands.
#'
#' @details
#' The dataset contains 2,412 choice occasions, each with four alternatives,
#' resulting in 9,648 rows in long format.
#'
#' @docType data
#' @format A data frame with 9,648 rows and 6 variables:
#' \describe{
#'   \item{id}{Integer. Consumer identifier for each choice occasion.}
#'   \item{feat}{Integer. Feature advertising indicator
#'     (\code{1 = on feature}, \code{0 = not on feature}).}
#'   \item{price}{Numeric. Price per ounce.}
#'   \item{choice}{Factor or character. Choice indicator
#'     (\code{Yes} = brand chosen, \code{No} = not chosen).}
#'   \item{brand}{Factor or character. Brand identifier.}
#'   \item{income}{Numeric. Household income in thousands of dollars.}
#' }
#' @source Adapted from the \code{mlogit} package dataset.
"yogurt"
