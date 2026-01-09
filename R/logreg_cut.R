#' @title (Deprecated) Sensitivity/Specificity Plot for Logistic Regression
#' @description
#' Deprecated. Use \code{\link{cutoff_logistic}} instead.
#'
#' @details
#' \strong{Deprecated.} This function is retained for backward compatibility.
#' It forwards to \code{\link{cutoff_logistic}} and reproduces the original
#' sensitivity, specificity, and accuracy plot across classification cutoffs.
#'
#' Migration notes:
#' \itemize{
#'   \item \code{MOD} maps directly to \code{MOD}.
#'   \item \code{DATA} maps directly to \code{DATA}.
#'   \item \code{POS} maps to \code{POSITIVE}.
#'   \item The new function supports optional second data sets and improved
#'     input validation.
#' }
#'
#' @param MOD A fitted binary logistic regression \code{glm} object.
#' @param DATA A data frame for which cutoff diagnostics should be computed.
#' @param POS The level representing the positive outcome.
#'
#' @return
#' Invisibly returns the \code{ggplot} object produced by
#' \code{\link{cutoff_logistic}}.
#'
#' @examples
#' \dontrun{
#' model <- glm(buy ~ age + gender, train, family = "binomial")
#' logreg_cut(model, train, "Yes")
#' }
#'
#' @export
#' @keywords internal
logreg_cut <- function(MOD, DATA, POS) {
   
   .Deprecated("cutoff_logistic")
   
   cutoff_logistic(
      MOD      = MOD,
      DATA     = DATA,
      POSITIVE = POS,
      LABEL1   = "Sample 1"
   )
}
