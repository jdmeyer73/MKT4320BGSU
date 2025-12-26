#' @title (Deprecated) Gain and Lift Tables/Charts
#' @description
#' Deprecated. Use \code{\link{gainlift_logistic}} instead.
#'
#' @details
#' \strong{Deprecated.} This function is retained for backward compatibility.
#' It forwards to \code{\link{gainlift_logistic}}.
#'
#' Migration notes:
#' \itemize{
#'   \item \code{MOD}, \code{TRAIN}, \code{TEST}, and \code{POSITIVE} map directly.
#'   \item The new function enforces that the outcome variable is a factor in both
#'     \code{TRAIN} and \code{TEST}.
#'   \item Return elements are preserved (\code{gaintable}, \code{lifttable},
#'     \code{gainplot}, \code{liftplot}).
#' }
#'
#' @param MOD A fitted binary logistic regression \code{glm} object.
#' @param TRAIN Training data.
#' @param TEST Test/holdout data.
#' @param POSITIVE Factor level representing the positive / "success" class.
#'
#' @return A list with \code{gaintable}, \code{lifttable}, \code{gainplot}, and \code{liftplot}.
#'
#' @examples
#' \dontrun{
#' model <- glm(buy ~ age + gender, train, family = "binomial")
#'
#' # Save gain/lift results
#' gl <- gainlift(model, train, test, "Yes")
#' gl$gaintable
#' gl$gainplot
#' gl$lifttable
#' gl$liftplot
#' }
#'
#' @export
gainlift <- function(MOD, TRAIN, TEST, POSITIVE) {
   
   .Deprecated("gainlift_logistic")
   
   gainlift_logistic(
      MOD      = MOD,
      TRAIN    = TRAIN,
      TEST     = TEST,
      POSITIVE = POSITIVE
   )
}
