#' @title Easy Margin Plots (Deprecated)
#' @description
#' Deprecated. Use \code{\link{easy_mp}} instead.
#'
#' @details
#' This function is a thin wrapper around \code{\link{easy_mp}} and is kept for
#' backward compatibility. It returns the same list structure as before
#' (\code{plot} and \code{ptable}).
#'
#' Migration:
#' \itemize{
#'   \item \code{mymp(model, focal, int)} \eqn{\rightarrow} \code{easy_mp(model, focal, int)}
#' }
#'
#' @param model A fitted linear regression (\code{lm}) model or binary logistic
#'   regression (\code{glm}, \code{family = "binomial"}) model.
#' @param focal Character string; name of the focal predictor variable.
#' @param int Optional character string; name of the interaction variable.
#'
#' @return A list with elements:
#' \describe{
#'   \item{plot}{A ggplot object.}
#'   \item{ptable}{A \code{ggeffects} table (can be coerced to a data frame).}
#' }
#'
#' @examples
#' \dontrun{
#' # No interaction
#' data(airlinesat)
#' model1 <- lm(nps ~ age + nflights, data = airlinesat)
#' out1 <- mymp(model1, "age")
#' out1$plot
#'
#' # With interaction
#' data(directmktg)
#' model2 <- glm(buy ~ salary + age * gender,
#'               data = directmktg,
#'               family = "binomial")
#' out2 <- mymp(model2, "age", "gender")
#' out2$plot
#' }
#'
#' @export
mymp <- function(model, focal, int = NULL) {
   .Deprecated("easy_mp", package = "MKT4320BGSU")
   easy_mp(model = model, focal = focal, int = int)
}
