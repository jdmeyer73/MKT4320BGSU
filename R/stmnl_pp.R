#' @title Average Predicted Probabilities for Standard MNL (Deprecated)
#'
#' @description
#' Deprecated. Use \code{\link{pp_std_mnl}} instead.
#'
#' @details
#' This function is a thin wrapper around \code{\link{pp_std_mnl}} kept for
#' backward compatibility.
#'
#' Migration:
#' \itemize{
#'   \item \code{stmnl_pp(model, focal, xlab)} \eqn{\rightarrow}
#'   \code{pp_std_mnl(model, focal = focal, xlab = xlab)}
#' }
#'
#' @param model A fitted \code{nnet::multinom} model.
#' @param focal Character string; name of the focal predictor variable.
#' @param xlab Optional character string; x-axis label used in the plot.
#' @param ft_table Logical; if \code{TRUE}, return the table as a \code{flextable}.
#'
#' @return A list with:
#' \itemize{
#'   \item \code{$table}: data frame or flextable of predicted probabilities
#'   \item \code{$plot}: ggplot object
#' }
#'
#' @examples
#' \dontrun{
#' out <- stmnl_pp(model, focal = "age", xlab = "Age in Years")
#' out$table
#' out$plot
#' }
#'
#' @export
stmnl_pp <- function(model, focal, xlab = NULL, ft_table = FALSE) {
   .Deprecated("pp_std_mnl", package = "MKT4320BGSU")
   pp_std_mnl(model = model, focal = focal, xlab = xlab, ft_table = ft_table)
}
