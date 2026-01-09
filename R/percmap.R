#' @title Perceptual / Joint Space Map from PCA (Deprecated)
#'
#' @description
#' Deprecated. Use \code{\link{easy_pca_maps}} instead.
#'
#' @details
#' This function is kept for backward compatibility. It produces a single
#' perceptual or joint space map using the first two principal components.
#'
#' Migration:
#' \itemize{
#'   \item \code{percmap(data, group)} \eqn{\rightarrow}
#'   \code{easy_pca_maps(data, vars, group, comp = 2)$plots[[1]]}
#'   \item \code{percmap(data, group, pref)} \eqn{\rightarrow}
#'   \code{easy_pca_maps(data, vars, group, comp = 2, pref = pref)$plots[[1]]}
#' }
#'
#' @param data A data frame containing attribute variables and a grouping variable.
#' @param group Character string; name of the grouping variable.
#' @param pref Optional character string; name of a numeric preference variable.
#'
#' @return A \code{ggplot} perceptual (or joint space) map.
#'
#' @examples
#' \dontrun{
#' percmap(mydata, group = "brand")
#' percmap(mydata, group = "brand", pref = "liking")
#' }
#'
#' @export
#' @keywords internal
percmap <- function(data, group, pref) {
   .Deprecated("easy_pca_maps", package = "MKT4320BGSU")
   
   if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
   if (missing(group)) stop("`group` must be supplied.", call. = FALSE)
   
   # legacy behavior: use all numeric vars except group/pref
   drop <- c(group, if (!missing(pref)) pref)
   vars <- names(data)[vapply(data, is.numeric, logical(1))]
   vars <- setdiff(vars, drop)
   
   if (length(vars) < 2) {
      stop("Not enough numeric variables to create a perceptual map.", call. = FALSE)
   }
   
   if (missing(pref)) {
      res <- easy_pca_maps(
         data = data,
         vars = vars,
         group = group,
         comp = 2
      )
   } else {
      res <- easy_pca_maps(
         data = data,
         vars = vars,
         group = group,
         comp = 2,
         pref = pref
      )
   }
   
   # preserve legacy: return single plot
   res$plots[[1]]
}
