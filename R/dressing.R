#' Salad Dressing Sales
#'
#' Weekly sales data for salad dressing across multiple locations and retailers
#' over approximately two years.
#'
#' @docType data
#' @format A data frame with 1,365 rows and 5 variables:
#' \describe{
#'   \item{id}{Integer or character. Unique identifier for each location--week observation.}
#'   \item{vol}{Numeric or integer. Units sold during the week.}
#'   \item{price}{Numeric. Price during the week.}
#'   \item{adv}{Numeric or integer. Advertising level for the week.}
#'   \item{store}{Factor. Retailer identifier
#'     (\code{Alb} = Albertson's; \code{WD} = Winn-Dixie).}
#' }
#' @source Unknown.
"dressing"
