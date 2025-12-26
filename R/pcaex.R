#' @title Easy PCA (Deprecated)
#'
#' @description
#' Deprecated. Use \code{\link{easy_pca_fit}} (diagnostics) and
#' \code{\link{easy_pca_final}} (retained solution) instead.
#'
#' @details
#' This function is a thin wrapper kept for backward compatibility.
#'
#' Migration:
#' \itemize{
#'   \item \code{pcaex(data, group=..., pref=..., comp=NULL)} \eqn{\rightarrow}
#'   \code{easy_pca_fit(data, vars=..., group=...)}
#'   \item \code{pcaex(data, group=..., pref=..., comp=k)} \eqn{\rightarrow}
#'   \code{easy_pca_final(data, vars=..., comp=k, group=...)}
#' }
#'
#' @param data A data frame containing the variables on which to perform PCA.
#' @param group Optional; name of the grouping variable (string). If provided,
#'   PCA is fit on mean-aggregated values by group.
#' @param pref Optional; name of a preference variable (string) to exclude from PCA.
#' @param comp Optional integer; number of components to retain. If missing or
#'   \code{NULL}, returns diagnostics (eigen table + scree plot). If \code{>= 1},
#'   returns retained-solution tables.
#'
#' @return
#' If \code{comp} is missing or \code{NULL}:
#' \describe{
#'   \item{table}{Eigenvalue table (data.frame).}
#'   \item{plot}{Scree plot (ggplot).}
#' }
#' If \code{comp >= 1}:
#' \describe{
#'   \item{table}{Eigenvalue table (data.frame).}
#'   \item{unrotated}{Unrotated loading table + Unexplained (data.frame).}
#'   \item{rotated}{Varimax-rotated loading table + Unexplained (data.frame).}
#'   \item{pcaobj}{The fitted \code{prcomp} object.}
#' }
#'
#' @examples
#' \dontrun{
#' # Diagnostics mode (all components)
#' res1 <- pcaex(pcadata, group = "brand", pref = "pref")
#' res1$table
#' res1$plot
#'
#' # Retained-solution mode (2 components)
#' res2 <- pcaex(pcadata, group = "brand", pref = "pref", comp = 2)
#' res2$table
#' res2$unrotated
#' res2$rotated
#' }
#'
#' @export
pcaex <- function(data, group, pref, comp = NULL) {
   .Deprecated(c("easy_pca_fit", "easy_pca_final"), package = "MKT4320BGSU")
   
   if (!is.data.frame(data)) {
      stop("`data` must be a data frame.", call. = FALSE)
   }
   
   # ---- handle legacy optional args: group/pref may be missing ----
   group_arg <- if (!missing(group)) group else NULL
   pref_arg  <- if (!missing(pref))  pref  else NULL
   
   if (!is.null(group_arg) && (!is.character(group_arg) || length(group_arg) != 1)) {
      stop("`group` must be a single column name string.", call. = FALSE)
   }
   if (!is.null(pref_arg) && (!is.character(pref_arg) || length(pref_arg) != 1)) {
      stop("`pref` must be a single column name string.", call. = FALSE)
   }
   
   # ---- derive vars: all columns except group/pref (if supplied) ----
   drop_cols <- character(0)
   if (!is.null(group_arg)) drop_cols <- c(drop_cols, group_arg)
   if (!is.null(pref_arg))  drop_cols <- c(drop_cols, pref_arg)
   
   vars <- setdiff(names(data), drop_cols)
   
   if (length(vars) < 2) {
      stop("Not enough variables to run PCA after excluding `group`/`pref`.", call. = FALSE)
   }
   
   # Preserve legacy: return plain data.frames (ft = FALSE)
   # Old code also did not expose key/data_used, so we drop those.
   run_fit_mode <- missing(comp) || is.null(comp)
   
   if (run_fit_mode) {
      fit <- easy_pca_fit(data = data, vars = vars, group = group_arg, ft = FALSE)
      return(list(table = fit$table, plot = fit$plot))
   }
   
   # retained-solution mode
   if (!is.numeric(comp) || length(comp) != 1 || is.na(comp) || comp < 1) {
      stop("`comp` must be a single integer >= 1.", call. = FALSE)
   }
   comp <- as.integer(comp)
   
   fin <- easy_pca_final(data = data, vars = vars, comp = comp, group = group_arg, ft = FALSE)
   list(
      table     = fin$table,
      unrotated = fin$unrotated,
      rotated   = fin$rotated,
      pcaobj    = fin$pcaobj
   )
}
