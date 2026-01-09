#' @title Quick Marginal Effects Plot
#'
#' @description
#' Convenience wrapper around \code{\link{easy_mp}} that returns only the
#' \code{ggplot} object.
#'
#' @inheritParams easy_mp
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' \dontrun{
#' model1 <- lm(nps ~ age + nflights, data = airlinesat)
#' easy_mp_plot(model1, "age")
#' }
#' 
#' @seealso \code{\link{easy_mp_plot}}
#'
#' @export
#'
#' @importFrom ggplot2 ggplot
#' @keywords internal
easy_mp_plot <- function(model, focal, int = NULL) {
   out <- easy_mp(model, focal, int)
   out$plot
}
