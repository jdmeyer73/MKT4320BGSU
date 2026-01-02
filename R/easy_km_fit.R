#' @title Easy k-Means Clustering (Fit & Diagnostics)
#'
#' @description
#' Fit k-means clustering solutions across a range of cluster counts and compute
#' common diagnostics to support segmentation decisions in marketing analytics.
#'
#' @details
#' Rows with missing values in the clustering variables are removed prior to
#' fitting. The row indices used for clustering are stored internally so cluster
#' membership can be reattached to the original dataset using
#' \code{\link{easy_km_final}}.
#'
#' Diagnostic outputs include within-cluster sum of squares (WSS), average
#' silhouette width, the Gap statistic (using the 1-SE rule), and cluster-size
#' balance measures. A table of cluster size proportions is also returned to help
#' assess whether candidate solutions contain very small or dominant clusters.
#'
#' @param data A data frame containing the full dataset.
#' @param vars Character vector of numeric variable names used for clustering.
#' @param k_range Integer vector of cluster counts to evaluate
#'   (default = \code{1:10}; allowed values 1--20).
#' @param standardize Logical; if \code{TRUE} (default), clustering variables are
#'   standardized before fitting k-means.
#' @param nstart Integer; number of random starts for each k-means solution
#'   (default = 25).
#' @param iter.max Integer; maximum number of iterations allowed for each k-means
#'   run (default = 100).
#' @param B Integer; number of Monte Carlo bootstrap samples used to compute the
#'   Gap statistic (default = 20).
#' @param seed Integer; random seed for reproducible results
#'   (default = 4320).
#'
#' @return
#' A list with the following components:
#' \describe{
#'   \item{km_all}{Named list of fitted \code{\link[stats]{kmeans}} objects, indexed
#'     by the number of clusters.}
#'   \item{diag_raw}{A data frame of diagnostic statistics by number of clusters.}
#'   \item{diag}{A formatted \code{\link[flextable]{flextable}} version of
#'     \code{diag_raw}.}
#'   \item{size_prop_raw}{A data frame of cluster size proportions for each
#'     candidate solution. Rows correspond to cluster solutions and columns
#'     correspond to cluster numbers (up to \code{max(k_range)}).}
#'   \item{size_prop}{A formatted \code{\link[flextable]{flextable}} version of
#'     \code{size_prop_raw}.}
#'   \item{gap}{The full Gap statistic object returned by
#'     \code{\link[cluster]{clusGap}}.}
#'   \item{diss}{The distance matrix used to compute silhouette values.}
#'   \item{scree_df}{Data frame of within-cluster sum of squares (WSS) by number
#'     of clusters.}
#'   \item{scree_plot}{Elbow (WSS) plot as a \code{ggplot} object.}
#'   \item{data_scaled}{The standardized clustering data (or \code{NULL} if
#'     \code{standardize = FALSE}).}
#'   \item{settings}{A list of settings used to fit the k-means models.}
#'   \item{key}{A list containing row-index information for rows used and dropped
#'     during clustering.}
#' }
#'
#' @examples
#' \dontrun{
#' fit <- easy_km_fit(
#'   data = ffseg,
#'   vars = c("eatin", "hours", "health"),
#'   k_range = 2:6
#' )
#'
#' # Diagnostics table
#' fit$diag
#'
#' # Cluster size proportions by solution
#' fit$size_prop
#' }
#'
#' @importFrom stats kmeans sd
#' @importFrom cluster silhouette clusGap
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs theme_bw
#' @importFrom flextable flextable add_header_lines add_footer_lines
#' @importFrom flextable align bold colformat_double autofit
#' @export
easy_km_fit <- function(data,
                        vars,
                        k_range     = 1:10,
                        standardize = TRUE,
                        nstart      = 25,
                        iter.max    = 100,
                        B           = 20,
                        seed        = 4320) {
   
   # ---- validate inputs ----
   if (!is.data.frame(data)) {
      stop("`data` must be a data frame.", call. = FALSE)
   }
   if (missing(vars) || !is.character(vars) || length(vars) < 1) {
      stop("`vars` must be a non-empty character vector.", call. = FALSE)
   }
   if (!all(vars %in% names(data))) {
      bad <- vars[!vars %in% names(data)]
      stop("These `vars` are not found in `data`: ", paste(bad, collapse = ", "), call. = FALSE)
   }
   
   if (!is.numeric(k_range) || any(k_range < 1) || any(k_range > 20)) {
      stop("`k_range` must contain integers between 1 and 20.", call. = FALSE)
   }
   k_range <- sort(unique(as.integer(k_range)))
   k_min   <- min(k_range)
   k_max   <- max(k_range)
   
   if (!is.numeric(nstart) || length(nstart) != 1 || nstart < 1) {
      stop("`nstart` must be a positive integer.", call. = FALSE)
   }
   if (!is.numeric(iter.max) || length(iter.max) != 1 || iter.max < 1) {
      stop("`iter.max` must be a positive integer.", call. = FALSE)
   }
   if (!is.numeric(B) || length(B) != 1 || B < 1) {
      stop("`B` must be a positive integer.", call. = FALSE)
   }
   
   # ---- row-index key ----
   row_index_all <- seq_len(nrow(data))
   
   # ---- segmentation data ----
   seg <- data[, vars, drop = FALSE]
   
   is_num <- vapply(seg, is.numeric, logical(1))
   if (!all(is_num)) {
      bad <- names(seg)[!is_num]
      stop("All segmentation variables must be numeric. Non-numeric: ",
           paste(bad, collapse = ", "), call. = FALSE)
   }
   
   keep <- stats::complete.cases(seg)
   row_index_used <- row_index_all[keep]
   dropped_na     <- which(!keep)
   
   if (sum(keep) < 2) {
      stop("Fewer than 2 complete cases remain after removing missing values.", call. = FALSE)
   }
   
   seg_used <- seg[keep, , drop = FALSE]
   
   # ---- standardization ----
   data_used   <- seg_used
   data_scaled <- NULL
   if (isTRUE(standardize)) {
      data_scaled <- as.data.frame(scale(seg_used))
      data_used   <- data_scaled
   }
   
   x_mat <- as.matrix(data_used)
   n_obs <- nrow(x_mat)
   
   # ---- seed ----
   if (!is.null(seed)) {
      if (!is.numeric(seed) || length(seed) != 1) stop("`seed` must be a single number.", call. = FALSE)
      set.seed(as.integer(seed))
   }
   
   # ---- fit kmeans across k ----
   km_all <- list()
   for (k in k_range) {
      if (k == 1) {
         km_all[[as.character(k)]] <- stats::kmeans(x_mat, centers = 1, nstart = nstart, iter.max = iter.max)
      } else if (k <= (n_obs - 1)) {
         km_all[[as.character(k)]] <- stats::kmeans(x_mat, centers = k, nstart = nstart, iter.max = iter.max)
      } else {
         km_all[[as.character(k)]] <- NULL
      }
   }
   
   # ---- WSS scree plot (elbow) ----
   WSS <- rep(NA_real_, length(k_range))
   names(WSS) <- as.character(k_range)
   
   for (i in seq_along(k_range)) {
      k  <- k_range[i]
      km <- km_all[[as.character(k)]]
      if (!is.null(km)) {
         WSS[i] <- km$tot.withinss
      }
   }
   
   scree_df <- data.frame(
      k   = k_range,
      WSS = WSS
   )
   
   scree_plot <- ggplot2::ggplot(scree_df, ggplot2::aes(x = k, y = WSS)) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::labs(x = "k", y = "WSS") +
      ggplot2::scale_x_continuous(breaks = k_range, minor_breaks = NULL) +
      ggplot2::theme_bw()
   
   # ---- silhouette (avg width) ----
   diss <- stats::dist(x_mat, method = "euclidean")
   
   Silhouette <- rep(NA_real_, length(k_range))
   names(Silhouette) <- as.character(k_range)
   
   for (i in seq_along(k_range)) {
      k <- k_range[i]
      km <- km_all[[as.character(k)]]
      if (!is.null(km) && k >= 2 && k < n_obs) {
         sil <- cluster::silhouette(km$cluster, diss)
         Silhouette[i] <- mean(sil[, "sil_width"])
      }
   }
   
   # ---- Gap statistic ----
   gap_res <- cluster::clusGap(
      x = x_mat,
      FUNcluster = function(x, k) {
         if (!is.null(seed)) set.seed(as.integer(seed))
         km <- stats::kmeans(x, centers = k, nstart = nstart, iter.max = iter.max)
         list(cluster = km$cluster)
      },
      K.max = k_max,
      B     = B
   )
   
   gap_tab  <- as.data.frame(gap_res$Tab)
   rn_k     <- as.numeric(rownames(gap_tab))
   k_seq    <- k_min:k_max
   gap_vals <- gap_tab$gap[match(k_seq, rn_k)]
   se_vals  <- gap_tab$SE.sim[match(k_seq, rn_k)]
   
   # ---- 1-SE Gap rule ----
   is_gap_1se <- rep(FALSE, length(k_seq))
   valid_idx  <- which(k_seq >= 2 & !is.na(gap_vals) & !is.na(se_vals))
   if (length(valid_idx) > 0) {
      idx_max <- valid_idx[which.max(gap_vals[valid_idx])]
      thresh  <- gap_vals[idx_max] - se_vals[idx_max]
      cand    <- valid_idx[gap_vals[valid_idx] >= thresh]
      if (length(cand) > 0) is_gap_1se[cand[1]] <- TRUE
   }
   
   # ---- cluster size proportions table (by k) ----
   size_prop_mat <- matrix(NA_real_, nrow = length(k_range), ncol = k_max)
   colnames(size_prop_mat) <- paste0("Cluster ", seq_len(k_max))
   rownames(size_prop_mat) <- paste0(k_range, "-cluster solution")
   
   # ---- cluster size diagnostics ----
   Small.Prop <- Large.Prop <- CV <- rep(NA_real_, length(k_range))
   names(Small.Prop) <- names(Large.Prop) <- names(CV) <- as.character(k_range)
   
   for (i in seq_along(k_range)) {
      k  <- k_range[i]
      km <- km_all[[as.character(k)]]
      
      if (!is.null(km)) {
         tab <- table(km$cluster)
         sz  <- as.numeric(tab)
         
         Small.Prop[i] <- min(sz) / n_obs
         Large.Prop[i] <- max(sz) / n_obs
         CV[i] <- if (length(sz) > 1) stats::sd(sz) / mean(sz) else NA_real_
         
         # NEW: proportions for each cluster (1..k) stored in fixed-width matrix (1..k_max)
         props <- as.numeric(tab) / sum(tab)
         cl_ids <- as.integer(names(tab))   # should be 1..k
         size_prop_mat[i, cl_ids] <- props
      }
   }
  
   # ---- size proportion tables ----
   size_prop_raw <- data.frame(
      Solution = rownames(size_prop_mat),
      size_prop_mat,
      row.names = NULL,
      check.names = FALSE
   )
   
   size_prop_ft <- flextable::flextable(size_prop_raw)
   size_prop_ft <- flextable::add_header_lines(size_prop_ft, values = "Cluster Size Proportions by Solution")
   size_prop_ft <- flextable::align(size_prop_ft, align = "center", part = "header")
   size_prop_ft <- flextable::bold(size_prop_ft, part = "header")
   size_prop_ft <- flextable::align(size_prop_ft, align = "right", part = "body")
   size_prop_ft <- flextable::colformat_double(size_prop_ft, j = 2:ncol(size_prop_raw), digits = 4)
   size_prop_ft <- flextable::autofit(size_prop_ft)
   
   
   
   # ---- build diagnostics table (raw) ----
   gap_for_k <- rep(NA_real_, length(k_range))
   se_for_k  <- rep(NA_real_, length(k_range))
   for (i in seq_along(k_range)) {
      k <- k_range[i]
      if (k >= k_min && k <= k_max) {
         idx <- match(k, k_seq)
         gap_for_k[i] <- gap_vals[idx]
         se_for_k[i]  <- se_vals[idx]
      }
   }
   
   diag_raw <- data.frame(
      Clusters     = k_range,
      Silhouette   = Silhouette,
      Gap.Stat     = gap_for_k,
      Gap.SE       = se_for_k,
      Small.Prop   = Small.Prop,
      Large.Prop   = Large.Prop,
      CV           = CV
   )
   
   # ---- formatted symbol columns ----
   gap_char <- ifelse(is.na(diag_raw$Gap.Stat), NA_character_, sprintf("%.4f", diag_raw$Gap.Stat))
   for (i in seq_along(k_range)) {
      k <- k_range[i]
      if (k >= k_min && k <= k_max) {
         idx <- match(k, k_seq)
         if (!is.na(idx) && isTRUE(is_gap_1se[idx]) && !is.na(gap_char[i])) {
            gap_char[i] <- paste0(gap_char[i], "*")
         }
      }
   }
   
   sil_char <- ifelse(is.na(diag_raw$Silhouette), NA_character_, sprintf("%.4f", diag_raw$Silhouette))
   
   small_char <- ifelse(is.na(diag_raw$Small.Prop), NA_character_, sprintf("%.4f", diag_raw$Small.Prop))
   idx_small  <- which(!is.na(diag_raw$Small.Prop) & diag_raw$Small.Prop < 0.05)
   small_char[idx_small] <- paste0(small_char[idx_small], "^")
   
   large_char <- ifelse(is.na(diag_raw$Large.Prop), NA_character_, sprintf("%.4f", diag_raw$Large.Prop))
   idx_large  <- which(!is.na(diag_raw$Large.Prop) & diag_raw$Large.Prop > 0.50)
   large_char[idx_large] <- paste0(large_char[idx_large], " \u25CA")
   
   cv_char <- ifelse(is.na(diag_raw$CV), NA_character_, sprintf("%.3f", diag_raw$CV))
   idx_cv1 <- which(!is.na(diag_raw$CV) & diag_raw$CV < 0.5)
   cv_char[idx_cv1] <- paste0(cv_char[idx_cv1], " \u2022")
   idx_cv2 <- which(!is.na(diag_raw$CV) & diag_raw$CV >= 0.5 & diag_raw$CV < 1)
   cv_char[idx_cv2] <- paste0(cv_char[idx_cv2], " \u2022\u2022")
   
   diag_disp <- data.frame(
      Clusters   = diag_raw$Clusters,
      Silhouette = sil_char,
      Gap.Stat   = gap_char,
      Small.Prop = small_char,
      Large.Prop = large_char,
      CV         = cv_char,
      stringsAsFactors = FALSE
   )
   
   ft <- flextable::flextable(diag_disp)
   
   ft <- flextable::add_header_lines(
      ft,
      values = paste0("K-Means Cluster Diagnostics (", ifelse(standardize, "Standardized", "Unscaled"), ")")
   )
   ft <- flextable::align(ft, align = "center", part = "header")
   ft <- flextable::bold(ft, part = "header")
   
   ft <- flextable::colformat_double(ft, j = "Clusters", digits = 0)
   ft <- flextable::align(ft, align = "right", part = "body")
   
   ft <- flextable::add_footer_lines(
      ft,
      values = c(
         "* 1-SE Gap rule.",
         "^ Smallest cluster < 5% of sample.",
         "\u25CA Largest cluster > 50% of sample.",
         "\u2022 CV < 0.5 well balanced; \u2022\u2022 moderately imbalanced.",
         "Silhouette is average silhouette width (higher is better); defined for k >= 2."
      )
   )
   
   ft <- flextable::autofit(ft)
   
   settings <- list(
      vars         = vars,
      k_range      = k_range,
      standardize  = standardize,
      nstart       = nstart,
      iter.max     = iter.max,
      B            = B,
      seed         = seed
   )
   
   key <- list(
      row_index_all  = row_index_all,
      row_index_used = row_index_used,
      dropped_na     = dropped_na
   )
   
   return(list(
      km_all        = km_all,
      diag_raw      = diag_raw,
      diag          = ft,
      size_prop_raw = size_prop_raw,  # NEW
      size_prop     = size_prop_ft,   # NEW
      gap           = gap_res,
      diss          = diss,
      scree_df      = scree_df,
      scree_plot    = scree_plot,
      data_scaled   = data_scaled,
      settings      = settings,
      key           = key
   ))
   
}
