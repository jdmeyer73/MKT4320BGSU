#' Cracker Choice Data (Test)
#'
#' Two data frames taken from 784 choice observations of three cracker brands.
#'     Original data split into training data (crk.train) and test/holdout
#'     data (crk.test)
#'
#' @usage data(crk.test)
#' @format
#' \itemize{
#'    \item \code{crk.train} contains data for 590 choice occasions in long 
#'        format (1770 rows and 8 columns)
#'    \item \code{test.yog} contains data for 194 choice occasions in long
#'        format (582 rows and 8 columns)
#' }
#' \describe{
#'   \item{\code{id}}{Unique customer identification number}
#'   \item{\code{choice}}{\code{1} if brand was chosen, \code{0} otherwise}
#'   \item{\code{brand}}{Brand identifier (Keebler, Nabisco, Sunshine)}
#'   \item{\code{proz}}{Price of brand per ounce}
#'   \item{\code{disp}}{\code{1} if brand was on display, \code{0} otherwise}
#'   \item{\code{feat}}{\code{1} if brand was on feature, \code{0} otherwise}
#'   \item{\code{income}}{Consumer income in 000s}
#'   \item{\code{kids}}{\code{1} if household has kids, \code{0} otherwise}
#' }
#' @source Adapted from mlogit (version 0.3-0) data set
"crk.test"