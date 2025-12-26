#' Builder Stores
#'
#' Attribute ratings and preference ratings for four masked home-improvement
#' stores, used for teaching attribute-based preference modeling and perceptual
#' mapping.
#'
#' @docType data
#' @format A data frame with 280 rows and 8 variables:
#' \describe{
#'   \item{brand}{Factor. Store/brand identifier (masked) with 4 levels.}
#'   \item{conv}{Integer. Convenience rating on a 1--6 agreement scale
#'     (\code{1 = Strongly Disagree}, \code{6 = Strongly Agree}).}
#'   \item{choice}{Integer. Perceived selection rating on a 1--6 agreement scale
#'     (\code{1 = Strongly Disagree}, \code{6 = Strongly Agree}).}
#'   \item{price}{Integer. Perceived low-price rating on a 1--6 agreement scale
#'     (\code{1 = Strongly Disagree}, \code{6 = Strongly Agree}).}
#'   \item{product}{Integer. Perceived product quality rating on a 1--6 agreement scale
#'     (\code{1 = Strongly Disagree}, \code{6 = Strongly Agree}).}
#'   \item{service}{Integer. Perceived service quality rating on a 1--6 agreement scale
#'     (\code{1 = Strongly Disagree}, \code{6 = Strongly Agree}).}
#'   \item{avail}{Integer. Perceived in-stock availability rating on a 1--6 agreement scale
#'     (\code{1 = Strongly Disagree}, \code{6 = Strongly Agree}).}
#'   \item{pref}{Integer. Overall preference rating on a 1--5 scale
#'     (\code{1 = Not at all preferred}, \code{5 = Very preferred}).}
#' }
#' @source Unknown.
"builderstores"
