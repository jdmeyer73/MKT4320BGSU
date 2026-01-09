#' @title (Deprecated) Classification Matrix for Binary Logistic Regression
#' @description
#' Deprecated. Use \code{\link{classify_logistic}} instead.
#'
#' @details
#' \strong{Deprecated.} This function is retained for backward compatibility.
#' It forwards to \code{\link{classify_logistic}} using legacy arguments and
#' prints results to the console, matching prior behavior.
#'
#' Migration notes:
#' \itemize{
#'   \item \code{MOD} maps directly to \code{MOD}.
#'   \item \code{DATA} maps directly to \code{DATA}.
#'   \item \code{POSITIVE} maps directly to \code{POSITIVE}.
#'   \item \code{CUTOFF} maps directly to \code{CUTOFF}.
#'   \item The new function supports optional test data, flextable output,
#'     and additional statistics.
#' }
#'
#' @param MOD A fitted binary logistic regression \code{glm} object.
#' @param DATA A data frame for which the classification matrix should be produced.
#' @param POSITIVE The level representing the positive outcome.
#' @param CUTOFF Probability cutoff for classification (default = 0.5).
#'
#' @return
#' Invisibly returns the result from \code{\link{classify_logistic}}.
#'
#' @examples
#' \dontrun{
#' model <- glm(buy ~ age + gender, train, family = "binomial")
#' logreg_cm(model, train, "Yes")
#' logreg_cm(model, train, "Yes", 0.6)
#' }
#'
#' @export
#' @keywords internal
logreg_cm <- function(MOD, DATA, POSITIVE, CUTOFF = 0.5) {
   
   .Deprecated("classify_logistic")
   
   classify_logistic(
      MOD      = MOD,
      DATA     = DATA,
      POSITIVE = POSITIVE,
      CUTOFF   = CUTOFF,
      ft       = FALSE
   )
}
