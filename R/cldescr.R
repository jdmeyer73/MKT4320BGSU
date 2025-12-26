#' @title (Deprecated) Describe Clusters
#' @description
#' Deprecated. Use \code{\link{easy_cluster_describe}} instead.
#'
#' @details
#' \strong{Deprecated.} This function is retained for backward compatibility.
#' It forwards to \code{\link{easy_cluster_describe}} with the closest possible
#' mapping of arguments.
#'
#' Migration notes:
#' \itemize{
#'   \item \code{data} is passed unchanged.
#'   \item \code{cvar} maps to \code{cluster_col}.
#'   \item \code{var} maps to \code{vars}.
#'   \item \code{vtype} is ignored. Variable type is auto-detected in
#'     \code{easy_cluster_describe()}.
#'   \item Output structure differs: the legacy \code{means}, \code{aovp},
#'     and \code{tukey} objects are no longer returned. See
#'     \code{\link{easy_cluster_describe}} for the new return format.
#' }
#'
#' @param data A data frame containing the cluster column and variables to describe.
#' @param var Character vector of variables to describe (legacy argument).
#' @param vtype Ignored. Retained for backward compatibility.
#' @param cvar Name of the cluster membership variable.
#'
#' @return
#' Invisibly returns the result from \code{\link{easy_cluster_describe}}.
#'
#' @export
cldescr <- function(data, var, vtype = c("F", "C"), cvar) {
   
   .Deprecated("easy_cluster_describe")
   
   # legacy checks kept minimal on purpose
   if (!is.data.frame(data)) {
      stop("`data` must be a data frame.", call. = FALSE)
   }
   
   if (missing(cvar)) {
      stop("`cvar` (cluster variable) must be supplied.", call. = FALSE)
   }
   
   if (missing(var)) {
      stop("`var` must be supplied.", call. = FALSE)
   }
   
   # coerce legacy inputs
   vars <- as.character(var)
   cluster_col <- as.character(cvar)[1]
   
   easy_cluster_describe(
      data        = data,
      cluster_col = cluster_col,
      vars        = vars,
      auto_print  = TRUE
   )
}
