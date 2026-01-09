#' @title Alternative-Specific MNL Margin Plot (Deprecated)
#' @description
#' Deprecated. Use \code{\link{pp_as_mnl}} instead.
#'
#' @details
#' \code{asmnl_mp()} is retained for backward compatibility with older course
#' materials. New code should call \code{pp_as_mnl()} and use the returned plot:
#' \preformatted{
#' res <- pp_as_mnl(mod, focal_var = "income", focal_type = "case")
#' res$pp_plot
#' }
#'
#' This wrapper returns the \code{$pp_plot} from \code{pp_as_mnl()}.
#'
#'#' @section Migration note:
#' Previously, \code{asmnl_mp()} returned a single margin plot for a
#' case-specific focal variable.
#'
#' Replace:
#' \preformatted{
#' asmnl_mp(mod, "income", "C")
#' }
#'
#' With:
#' \preformatted{
#' res <- pp_as_mnl(mod, focal_var = "income")
#' res$pp_plot
#' }
#' @param mod A fitted \code{mlogit} model.
#' @param focal Character; focal variable name (in quotes).
#' @param type Legacy argument kept for compatibility: \code{"C"} for continuous,
#'   \code{"D"} for binary. This is mapped to \code{pp_as_mnl()} behavior; for
#'   \code{"D"} the two observed values are used automatically.
#'
#' @return A \code{ggplot} object (legacy behavior).
#'
#' @examples
#' # mod <- asmnl_est(...)
#' # p <- asmnl_mp(mod, "income", "C")  # deprecated
#' # p
#'
#' @importFrom utils packageName
#' @export
#' @keywords internal
asmnl_mp <- function(mod, focal, type = c("C", "D")) {
   
   .Deprecated(
      new = "pp_as_mnl",
      package = utils::packageName(),
      msg = "asmnl_mp() is deprecated; use pp_as_mnl(model, focal_var=...) and extract $pp_plot."
   )
   
   type <- match.arg(type)
   
   if (!inherits(mod, "mlogit")) {
      stop("`mod` must be a fitted mlogit model (class 'mlogit').", call. = FALSE)
   }
   if (!is.character(focal) || length(focal) != 1) {
      stop("`focal` must be a single character string.", call. = FALSE)
   }
   
   # Legacy function was explicitly for CASE-specific vars; keep that behavior.
   res <- pp_as_mnl(
      model = mod,
      focal_var = focal,
      focal_type = "case",
      ft = FALSE,
      marginal = FALSE
   )
   
   res$pp_plot
}
