#' @title Cluster Sizes for k-Means Clustering (Deprecated)
#' @description
#' Deprecated. Use \code{\link{easy_km_fit}} instead.
#'
#' @details
#' This function is a thin wrapper around \code{\link{easy_km_fit}}.
#' It fits k-means for the requested \code{centers} values and returns legacy
#' cluster-size tables:
#' \itemize{
#'   \item \code{kcount}: counts by cluster (sorted within each k),
#'   \item \code{kperc}: percentages by cluster (sorted within each k).
#' }
#'
#' Migration:
#' \itemize{
#'   \item \code{ksize(data, centers, nstart, seed)} \eqn{\rightarrow}
#'   \code{easy_km_fit(data, vars = names(data), k_range = centers, nstart = nstart, seed = seed)}
#' }
#'
#' @param data A data frame containing only the (numeric) variables used for clustering.
#' @param centers Integer vector; the k solutions to examine.
#' @param nstart Integer; number of random starts for kmeans (default = 25).
#' @param seed Integer; random seed for reproducible results (default = 4320).
#'
#' @return A list with elements \code{kcount} and \code{kperc}.
#'
#' @examples
#' \dontrun{
#' ks <- ksize(sc.clvar, centers = c(3, 4, 5))
#' ks$kcount
#' ks$kperc
#' }
#'
#' @export
#' @keywords internal
ksize <- function(data, centers, nstart = 25, seed = 4320) {
   .Deprecated("easy_km_fit", package = "MKT4320BGSU")
   
   if (!is.data.frame(data)) {
      stop("`data` must be a data frame.", call. = FALSE)
   }
   if (missing(centers) || length(centers) < 1) {
      stop("`centers` must be provided (one or more integers).", call. = FALSE)
   }
   if (!is.numeric(centers) || any(is.na(centers)) || any(centers < 1)) {
      stop("`centers` must contain positive integers.", call. = FALSE)
   }
   centers <- sort(unique(as.integer(centers)))
   
   if (!is.numeric(nstart) || length(nstart) != 1 || is.na(nstart) || nstart < 1) {
      stop("`nstart` must be a positive integer.", call. = FALSE)
   }
   
   res <- easy_km_fit(
      data         = data,
      vars         = names(data),
      k_range      = centers,
      standardize  = FALSE,
      nstart       = as.integer(nstart),
      seed         = seed
   )
   
   # ---- build legacy tables from km_all ----
   # For each k, tabulate cluster sizes (descending) and percent sizes (descending)
   count_list <- lapply(centers, function(k) {
      km <- res$km_all[[as.character(k)]]
      if (is.null(km)) return(data.frame(Var1 = character(0), Freq = integer(0)))
      tt <- sort(table(km$cluster), decreasing = TRUE)
      data.frame(Var1 = names(tt), Freq = as.integer(tt), stringsAsFactors = FALSE)
   })
   
   perc_list <- lapply(centers, function(k) {
      km <- res$km_all[[as.character(k)]]
      if (is.null(km)) return(data.frame(Var1 = character(0), Freq = numeric(0)))
      tt <- sort(100 * prop.table(table(km$cluster)), decreasing = TRUE)
      data.frame(Var1 = names(tt), Freq = round(as.numeric(tt), 2), stringsAsFactors = FALSE)
   })
   
   merge_all <- function(lst) {
      if (length(lst) == 1) return(lst[[1]])
      Reduce(function(d1, d2) merge(d1, d2, by = "Var1", all = TRUE), lst)
   }
   
   k_count <- suppressWarnings(merge_all(count_list))
   k_perc  <- suppressWarnings(merge_all(perc_list))
   
   # Column names match legacy style: "Num_Clusters", "k_3_Count", ...
   count_cols <- paste0("k_", centers, "_Count")
   perc_cols  <- paste0("k_", centers, "_Percent")
   
   if (ncol(k_count) == 0) {
      k_count <- data.frame(Num_Clusters = character(0))
   } else {
      colnames(k_count) <- c("Num_Clusters", count_cols)
   }
   
   if (ncol(k_perc) == 0) {
      k_perc <- data.frame(Num_Clusters = character(0))
   } else {
      colnames(k_perc) <- c("Num_Clusters", perc_cols)
   }
   
   list(kcount = k_count, kperc = k_perc)
}
