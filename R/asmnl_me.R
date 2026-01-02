#' @title Alternative-Specific MNL Marginal Effects (Deprecated)
#' @description
#' Deprecated. Use \code{\link{pp_as_mnl}} instead.
#'
#' @details
#' \code{asmnl_me()} is retained for backward compatibility with older course
#' materials. The new function \code{pp_as_mnl()} operates on a fitted
#' \code{mlogit} model and computes tables/plots for one focal variable at a time.
#'
#' This deprecated wrapper will compute marginal effects for \emph{each numeric}
#' variable in the training \code{dfidx} data stored in the fitted model and
#' return a named list of \code{pp_as_mnl} results.
#'
#' Recommended new usage:
#' \preformatted{
#' res <- pp_as_mnl(model, focal_var = "income", me_method = "observed")
#' res$pp_table
#' res$pp_plot
#' res$me_table
#' }
#' #' @section Migration note:
#' Previously, \code{asmnl_me()} computed marginal effects for \emph{all}
#' numeric variables at once. The new workflow computes marginal effects for
#' \emph{one focal variable at a time}.
#'
#' Replace:
#' \preformatted{
#' asmnl_me(mod)
#' }
#'
#' With:
#' \preformatted{
#' pp_as_mnl(mod, focal_var = "income", marginal = TRUE)
#' }
#'
#' To reproduce the old "at means" behavior, use:
#' \preformatted{
#' pp_as_mnl(mod, focal_var = "income", marginal = TRUE,
#'           me_method = "means")
#' }
#'
#' @param mod A fitted \code{mlogit} model (legacy input from \code{asmnl_est()}).
#' @param me_method Character; marginal effect method passed to \code{pp_as_mnl()}:
#'   \code{"observed"} (AME) or \code{"means"} (at means). Default \code{"means"}
#'   to mimic the legacy behavior most closely.
#' @param me_step Numeric; step size for \code{me_method="observed"} finite differences.
#' @param ft Logical; if \code{TRUE}, return tables as flextables (default \code{FALSE}).
#' @param digits Integer; rounding for returned tables (default 4).
#'
#' @return A named list of \code{pp_as_mnl} objects (one per focal variable).
#'
#' @examples
#' # mod <- asmnl_est(...)
#' # out <- asmnl_me(mod)  # deprecated
#' # out[["income"]]$me_table
#'
#' @importFrom utils packageName
#' @export
asmnl_me <- function(mod,
                     me_method = c("means", "observed"),
                     me_step = 1,
                     ft = FALSE,
                     digits = 4) {
   
   .Deprecated(
      new = "pp_as_mnl",
      package = utils::packageName(),
      msg = "asmnl_me() is deprecated; use pp_as_mnl(model, focal_var=...) instead."
   )
   
   me_method <- match.arg(me_method)
   
   if (!inherits(mod, "mlogit")) {
      stop("`mod` must be a fitted mlogit model (class 'mlogit').", call. = FALSE)
   }
   if (is.null(mod$model) || !inherits(mod$model, "dfidx")) {
      stop("Training dfidx data not found in `mod$model`.", call. = FALSE)
   }
   
   dd <- as.data.frame(mod$model)
   
   # numeric-only variables (skip indexes + response-ish columns if present)
   is_num <- vapply(dd, function(x) is.numeric(x) || is.integer(x), logical(1))
   vars <- names(dd)[is_num]
   
   # Drop obviously non-covariate numeric columns if they exist
   drop_names <- intersect(vars, c("choice", "chid", "idx", "alt", "id"))
   vars <- setdiff(vars, drop_names)
   
   if (length(vars) == 0) {
      stop("No numeric covariates found in the model data to compute marginal effects for.", call. = FALSE)
   }
   
   out <- setNames(vector("list", length(vars)), vars)
   
   for (v in vars) {
      out[[v]] <- pp_as_mnl(
         model = mod,
         focal_var = v,
         focal_type = "auto",
         digits = digits,
         ft = ft,
         marginal = TRUE,
         me_method = me_method,
         me_step = me_step
      )
   }
   
   out
}
