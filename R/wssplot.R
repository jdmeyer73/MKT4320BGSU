#' @title Scree Plot Before k-Means Clustering (Deprecated)
#' @description
#' Deprecated. Use \code{\link{easy_km_fit}} instead.
#'
#' @details
#' This function is a thin wrapper around \code{\link{easy_km_fit}}.
#' It returns the same WSS scree plot as \code{easy_km_fit(...)} (element
#' \code{scree_plot}).
#'
#' Migration:
#' \itemize{
#'   \item \code{wssplot(data, nc, seed)} \eqn{\rightarrow} \code{easy_km_fit(data, vars = names(data), k_range = 1:nc, seed = seed)$scree_plot}
#' }
#'
#' @param data A data frame containing only the (numeric) variables used for clustering.
#' @param nc Integer; maximum number of clusters to plot (default = 15).
#' @param seed Integer; random seed for reproducible results (default = 4320).
#'
#' @return A ggplot object (WSS vs. k).
#'
#' @examples
#' \dontrun{
#' # Scree plot with default values
#' wssplot(sc.clvar)
#'
#' # Scree plot up to 10 clusters with seed 1000
#' wssplot(sc.clvar, nc = 10, seed = 1000)
#' }
#'
#' @export
#' @keywords internal
wssplot <- function(data, nc = 15, seed = 4320) {
   .Deprecated("easy_km_fit", package = "MKT4320BGSU")
   
   if (!is.data.frame(data)) {
      stop("`data` must be a data frame.", call. = FALSE)
   }
   if (!is.numeric(nc) || length(nc) != 1 || is.na(nc) || nc < 1) {
      stop("`nc` must be a positive integer.", call. = FALSE)
   }
   
   nc <- as.integer(nc)
   
   res <- easy_km_fit(
      data         = data,
      vars         = names(data),
      k_range      = 1:nc,
      standardize  = FALSE,
      seed         = seed
   )
   
   res$scree_plot
}
