#' @title Easy Lift Plot for Uplift Modeling (Deprecated)
#'
#' @description
#' Deprecated. Use \code{\link{easy_liftplots}} instead.
#'
#' @details
#' This function is kept for backward compatibility. It returns a single plot:
#' a histogram of predicted lift (when \code{var} is \code{NULL}), a single
#' lift-by-covariate plot (when \code{var} is provided), or a single
#' interaction-style plot (when both \code{var} and \code{byvar} are provided).
#'
#' Migration:
#' \itemize{
#'   \item \code{liftplot(df)} \eqn{\rightarrow} \code{easy_liftplots(x)$hist}
#'   \item \code{liftplot(df, "recency")} \eqn{\rightarrow} \code{easy_liftplots(x, vars="recency")$plots_main[["recency"]]}
#'   \item \code{liftplot(df, "recency", "history")} \eqn{\rightarrow} \code{easy_liftplots(x, pairs=list(c("recency","history")))$plots_pairs[[1]]}
#' }
#'
#' @param data Data frame containing uplift results, including a numeric column
#'   named \code{lift}.
#' @param var Optional character string; covariate name for a single lift plot.
#'   If \code{NULL}, a histogram is returned.
#' @param byvar Optional character string; second covariate name to create a
#'   single interaction-style plot with \code{var}.
#' @param ar Optional aspect ratio passed to \code{theme(aspect.ratio = ar)}.
#' @param ci Error-bar style: use \code{0} for \eqn{\pm 1} SD, or one of
#'   \code{c(0.90, 0.95, 0.975, 0.99)} for normal-approximation confidence
#'   intervals. Default is \code{c(0.90, 0.95, 0.975, 0.99, 0)} (legacy).
#'
#' @return A single \code{ggplot} object.
#'
#' @examples
#' \dontrun{
#' liftplot(results$all)
#' liftplot(results$all, "recency", ar = 1, ci = 0.95)
#' liftplot(results$all, "recency", "history", ar = 1, ci = 0.95)
#' }
#'
#' @export
#' @keywords internal
liftplot <- function(data, var = NULL, byvar = NULL, ar = NULL,
                     ci = c(0.90, 0.95, 0.975, 0.99, 0)) {
   .Deprecated("easy_liftplots", package = "MKT4320BGSU")
   
   if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
   if (!("lift" %in% names(data))) stop("`data` must contain a numeric column named `lift`.", call. = FALSE)
   if (!is.numeric(data$lift)) stop("`data$lift` must be numeric.", call. = FALSE)
   
   # legacy ci could be a vector; new expects length-1
   if (length(ci) != 1) {
      # prefer 0.95 if present; else fall back to first element
      ci <- if (0.95 %in% ci) 0.95 else ci[[1]]
   }
   
   # Build a minimal easy_uplift-like object for easy_liftplots()
   # (easy_liftplots() enforces class and expects x$all + covariate metadata)
   x <- list(
      all = data,
      covariates = setdiff(names(data), "lift")
   )
   class(x) <- "easy_uplift"
   
   if (is.null(var)) {
      out <- easy_liftplots(x, vars = character(0), ar = ar, ci = ci, grid = FALSE, ft = FALSE)
      return(out$hist)
   }
   
   if (!is.character(var) || length(var) != 1) stop("`var` must be a single character string or NULL.", call. = FALSE)
   if (!var %in% names(data)) stop("`var` not found in `data`.", call. = FALSE)
   
   if (is.null(byvar)) {
      out <- easy_liftplots(x, vars = var, ar = ar, ci = ci, grid = FALSE, ft = FALSE)
      return(out$plots_main[[var]])
   }
   
   if (!is.character(byvar) || length(byvar) != 1) stop("`byvar` must be a single character string or NULL.", call. = FALSE)
   if (!byvar %in% names(data)) stop("`byvar` not found in `data`.", call. = FALSE)
   
   out <- easy_liftplots(
      x,
      vars = character(0),
      pairs = list(c(var, byvar)),
      ar = ar,
      ci = ci,
      grid = FALSE,
      ft = FALSE
   )
   
   # return the single pair plot (first/only)
   out$plots_pairs[[1]]
}
