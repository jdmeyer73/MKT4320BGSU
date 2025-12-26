#' @title (Deprecated) Easy Hierarchical Agglomerative Clustering
#' @description
#' Deprecated. Use \code{\link{easy_hc_fit}} (fit & diagnostics) and
#' \code{\link{easy_hc_final}} (final solution + membership) instead.
#'
#' @details
#' \strong{Deprecated.} This function is retained for backward compatibility.
#' It forwards to \code{\link{easy_hc_fit}} for clustering/diagnostics and
#' reconstructs legacy outputs (\code{kcount}, \code{kperc}, \code{hc}, and
#' optionally \code{stop}) when possible.
#'
#' Migration notes:
#' \itemize{
#'   \item \code{cuts} corresponds to selecting \code{k} later via
#'     \code{\link{easy_hc_final}}. This legacy function supports multiple cuts
#'     and returns multi-\code{k} count/percent tables.
#'   \item \code{clustop="Y"} returns legacy Duda/Hart and pseudo-t^2 indices as
#'     a data frame similar to prior behavior.
#' }
#'
#' @param data A data frame (or numeric matrix) containing only variables used for clustering.
#' @param dist Distance measure: \code{"euc"}, \code{"euc2"}, \code{"max"}, \code{"abs"}, \code{"bin"}.
#' @param method Linkage: \code{"ward"}, \code{"single"}, \code{"complete"}, \code{"average"}.
#' @param cuts Optional integer vector of cluster solutions to tabulate (legacy behavior).
#' @param clustop \code{"Y"} to return stopping indices; \code{"N"} otherwise.
#'
#' @return
#' If \code{cuts} is missing:
#' \itemize{
#'   \item returns \code{"No Cuts"} if \code{clustop="N"}
#'   \item returns \code{list(stop = <data.frame>)} if \code{clustop="Y"}
#' }
#'
#' If \code{cuts} is provided:
#' \itemize{
#'   \item returns \code{list(kcount = ..., kperc = ..., hc = ...)} if \code{clustop="N"}
#'   \item returns \code{list(kcount = ..., kperc = ..., hc = ..., stop = ...)} if \code{clustop="Y"}
#' }
#'
#' @examples
#' \dontrun{
#' # No cuts with stopping indices
#' eg1 <- myhc(sc.clvar, "euc", "ward", clustop = "Y")
#' eg1$stop
#'
#' # One cut without stopping indices
#' eg2 <- myhc(sc.clvar, "max", "ward", cuts = 5, clustop = "N")
#' eg2$kcount
#' eg2$kperc
#'
#' # Multiple cuts without stopping indices
#' eg3 <- myhc(sc.clvar, "abs", "ward", cuts = c(2, 3, 4, 6), clustop = "N")
#' eg3$kcount
#' eg3$kperc
#' }
#'
#' @export
myhc <- function(data,
                 dist = c("euc", "euc2", "max", "abs", "bin"),
                 method = c("ward", "single", "complete", "average"),
                 cuts,
                 clustop = c("N", "Y")) {
   
   .Deprecated("easy_hc_fit")
   
   dist <- match.arg(dist)
   method <- match.arg(method)
   clustop <- match.arg(clustop)
   
   # ---- coerce input like legacy code tolerated ----
   if (is.matrix(data)) data <- as.data.frame(data)
   if (!is.data.frame(data)) stop("`data` must be a data frame or numeric matrix.", call. = FALSE)
   
   # ensure column names for vars
   if (is.null(names(data)) || any(!nzchar(names(data)))) {
      names(data) <- paste0("V", seq_len(ncol(data)))
   }
   vars <- names(data)
   
   # legacy stopping indices were 1:10
   k_range <- 1:10
   
   # ---- fit using new function (handles plotting, diagnostics, NA drops, etc.) ----
   fit <- easy_hc_fit(
      data = data,
      vars = vars,
      dist = dist,
      method = method,
      k_range = k_range,
      standardize = TRUE,
      show_dend = TRUE
   )
   
   # helper: legacy stop table
   legacy_stop <- function(stop_raw) {
      data.frame(
         "Num.Clusters" = stop_raw$Num.Clusters,
         "Duda/Hart"    = stop_raw$Duda.Hart,
         "pseudo-t^2"   = stop_raw$pseudo.t2,
         check.names = FALSE
      )
   }
   
   # ---- no cuts (legacy behavior) ----
   if (missing(cuts)) {
      if (clustop == "N") {
         return("No Cuts")
      } else {
         return(list(stop = legacy_stop(fit$stop_raw)))
      }
   }
   
   # ---- cuts provided: reconstruct kcount / kperc like legacy ----
   cuts <- as.integer(cuts)
   cuts <- sort(unique(cuts[!is.na(cuts) & cuts >= 1]))
   if (length(cuts) < 1) stop("`cuts` must contain at least one positive integer.", call. = FALSE)
   
   # membership for each k based on USED rows (legacy used all rows; new drops NAs,
   # but in this wrapper we're passing only clustering vars, so this is typically identical)
   memb_list <- lapply(cuts, function(k) stats::cutree(fit$hc, k = k))
   
   # counts table merged by cluster id
   kc_list <- lapply(seq_along(cuts), function(i) {
      tab <- table(memb_list[[i]])
      data.frame(Cluster = as.integer(names(tab)), Count = as.integer(tab))
   })
   
   kcount <- Reduce(function(d1, d2) merge(d1, d2, by = "Cluster", all = TRUE), kc_list)
   count_names <- paste0("k_", cuts, "_Count")
   names(kcount) <- c("Cluster", count_names)
   
   # perc table merged by cluster id
   kp_list <- lapply(seq_along(cuts), function(i) {
      tab <- table(memb_list[[i]])
      pct <- round(100 * as.numeric(tab) / sum(tab), 2)
      data.frame(Cluster = as.integer(names(tab)), Percent = pct)
   })
   
   kperc <- Reduce(function(d1, d2) merge(d1, d2, by = "Cluster", all = TRUE), kp_list)
   perc_names <- paste0("k_", cuts, "_Percent")
   names(kperc) <- c("Cluster", perc_names)
   
   if (clustop == "N") {
      return(list(kcount = kcount, kperc = kperc, hc = fit$hc))
   } else {
      return(list(kcount = kcount, kperc = kperc, hc = fit$hc, stop = legacy_stop(fit$stop_raw)))
   }
}
