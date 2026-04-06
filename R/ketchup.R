#' Ketchup Sales
#'
#' Brand choice across three brands of ketchup with price and
#' promotion data included.
#'
#' @details
#' Used for standard multinomial logistic regression.
#'
#' @docType data
#' @format A data frame with 6,000 rows and 11 variables:
#' \describe{
#'   \item{extad}{Factor. "Yes" if brand was featured in external advertising; "No" otherwise.}
#'   \item{intad}{Factor. "Yes" if brand was featured in in-store advertising; "No" otherwise.}
#'   \item{display}{Factor. "Yes" if brand was on endcap or in-aisle display; "No" otherwise.}
#'   \item{price}{Numeric. Price for 32 ounce package.}
#'   \item{price10}{Numeric. Retail price in 10-cent units (original price × 10).}
#'   \item{coupon}{Factor. "Yes" if coupon was used for purchase; "No" otherwise.}
#'   \item{spc}{Factor. "Yes" if brand had a price reduction special; "No" otherwise.}
#'   \item{store}{Factor. Grocery store chain; "Eagle", "Jewel Osco", or "Kroger".}
#'   \item{brand}{Factor. Brand chosen; "Peter Pan", "Jif", or "Skippy".}
#'   \item{gender}{Factor. Gender; "Female" or "Male".}
#'   \item{age}{Numeric. Age in years.}
#' }
#' @source Adapted from ERIM data from the Chicago Booth Kilts Center for Marketing. Gender and age are simulated.
#' @keywords datasets
"ketchup"
