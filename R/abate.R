#' @title Average Treatment Effect for A/B Testing (Deprecated)
#' @description
#' Deprecated. Use \code{\link{easy_ab_ate}} instead.
#'
#' @details
#' \code{abate()} is retained for backward compatibility with older course
#' materials. New code should use \code{easy_ab_ate()}.
#'
#' This wrapper forwards arguments to \code{easy_ab_ate()} with \code{ft = TRUE}.
#'
#' @param model A fitted linear regression model of class \code{lm} that includes
#'   the treatment variable and (optionally) covariates.
#' @param treatment Character string. Name of the treatment variable (in quotes).
#'
#' @return A \code{flextable} (same as \code{easy_ab_ate(..., ft = TRUE)}).
#'
#' @examples
#' # m <- lm(y ~ promotion + age + income, data = df)
#' # abate(m, "promotion")            # deprecated
#' # easy_ab_ate(m, "promotion")      # preferred
#'
#' @export
abate <- function(model, treatment) {
   .Deprecated(
      new = "easy_ab_ate",
      package = utils::packageName(),
      msg = "abate() is deprecated; use easy_ab_ate() instead."
   )
   easy_ab_ate(model = model, treatment = treatment, ft = TRUE)
}
