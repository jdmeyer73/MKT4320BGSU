#' @title (Deprecated) Cluster Stopping Indices
#' @description
#' Deprecated. Use \code{\link{easy_hc_fit}} instead.
#'
#' @details
#' \strong{Deprecated.} This function is retained for backward compatibility.
#' It forwards to \code{\link{easy_hc_fit}} and returns the legacy stopping-index
#' data frame (Duda/Hart and pseudo-t^2).
#'
#' @param data A data frame (or numeric matrix) containing only variables used for clustering.
#' @param dist Distance measure: \code{"euc"}, \code{"max"}, \code{"abs"}, \code{"bin"}.
#' @param method Linkage: \code{"ward"}, \code{"single"}, \code{"complete"}, \code{"average"}.
#' @param minclust Integer; minimum number of clusters.
#' @param maxclust Integer; maximum number of clusters.
#'
#' @return A data frame with columns \code{Num.Clusters}, \code{Duda/Hart}, and \code{pseudo-t^2}.
#'
#' @examples
#' \dontrun{
#' clustop(sc.clvar, dist = "euc", method = "ward", minclust = 1, maxclust = 15)
#' }
#'
#' @export
#' @keywords internal
clustop <- function(data,
                    dist = c("euc", "max", "abs", "bin"),
                    method = c("ward", "single", "complete", "average"),
                    minclust = 1,
                    maxclust = 15) {
   
   .Deprecated("easy_hc_fit")
   
   dist <- match.arg(dist)
   method <- match.arg(method)
   
   minclust <- as.integer(minclust)
   maxclust <- as.integer(maxclust)
   if (is.na(minclust) || is.na(maxclust) || minclust < 1 || maxclust < minclust) {
      stop("`minclust` and `maxclust` must be positive integers with maxclust >= minclust.", call. = FALSE)
   }
   
   if (is.matrix(data)) data <- as.data.frame(data)
   if (!is.data.frame(data)) stop("`data` must be a data frame or numeric matrix.", call. = FALSE)
   
   if (is.null(names(data)) || any(!nzchar(names(data)))) {
      names(data) <- paste0("V", seq_len(ncol(data)))
   }
   vars <- names(data)
   
   fit <- easy_hc_fit(
      data = data,
      vars = vars,
      dist = dist,
      method = method,
      k_range = minclust:maxclust,
      standardize = TRUE,
      show_dend = FALSE
   )
   
   data.frame(
      "Num.Clusters" = fit$stop_raw$Num.Clusters,
      "Duda/Hart"    = fit$stop_raw$Duda.Hart,
      "pseudo-t^2"   = fit$stop_raw$pseudo.t2,
      check.names = FALSE
   )
}
