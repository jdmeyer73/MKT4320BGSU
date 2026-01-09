#' @title (Deprecated) ROC Curve for Binary Logistic Regression
#' @description
#' Deprecated. Use \code{\link{roc_logistic}} instead.
#'
#' @details
#' \strong{Deprecated.} This function is retained for backward compatibility.
#' It forwards to \code{\link{roc_logistic}} and returns a single ROC curve
#' for the provided data set.
#'
#' Migration notes:
#' \itemize{
#'   \item \code{MOD} maps directly to \code{MOD}.
#'   \item \code{DATA} maps directly to \code{DATA}.
#'   \item The new function supports an optional second data set and custom labels.
#' }
#'
#' @param MOD A fitted binary logistic regression \code{glm} object.
#' @param DATA A data frame for ROC computation (e.g., training data).
#'
#' @return A single ROC \code{ggplot} object (invisibly returned).
#'
#' @examples
#' \dontrun{
#' model <- glm(buy ~ age + gender, train, family = "binomial")
#' logreg_roc(model, train)
#' }
#'
#' @export
#' @keywords internal
logreg_roc <- function(MOD, DATA) {
   
   .Deprecated("roc_logistic")
   
   roc_logistic(
      MOD   = MOD,
      DATA  = DATA,
      LABEL1 = "Sample 1"
   )
}
