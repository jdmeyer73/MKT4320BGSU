#' Peanut Butter Sales
#'
#' Brand choice across three brands of peanut butter with price and
#' promotion data included.
#'
#' @details
#' Used for standard multinomial logistic regression.
#'
#' @docType data
#' @format A data frame with 5,000 rows and 10 variables:
#' \describe{
#'   \item{extad}{Factor. "Yes" if brand was featured in external advertising; "No" otherwise.}
#'   \item{intad}{Factor. "Yes" if brand was featured in in-store advertising; "No" otherwise.}
#'   \item{display}{Factor. "Yes" if brand was on endcap or in-aisle display; "No" otherwise.}
#'   \item{price}{Numeric. Raw price in dollars.}
#'   \item{weight}{Numeric. Size of container in ounces.}
#'   \item{ppo}{Numeric. 20 ounce container equivalent price in dollars.}
#'   \item{coupon}{Factor. "Yes" if coupon was used for purchase; "No" otherwise.}
#'   \item{spc}{Factor. "Yes" if brand had a price reduction special; "No" otherwise.}
#'   \item{store}{Factor. Grocery store chain; "Eagle", "Jewel Osco", or "Kroger".}
#'   \item{brand}{Factor. Brand chosen; "Peter Pan", "Jif", or "Skippy".}
#' }
#' @source Adapted from ERIM data from the Chicago Booth Kilts Center for Marketing.
#' @keywords datasets
"pb"
