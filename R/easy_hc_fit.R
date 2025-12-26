#' @title Easy Hierarchical Clustering (Fit & Diagnostics)
#' @description
#' Fit a hierarchical agglomerative clustering solution using selected numeric
#' segmentation variables, compute stopping rules and cluster-balance diagnostics
#' across a range of \code{k}, and (optionally) plot a dendrogram with cluster bars.
#'
#' @details
#' Rows with missing values in the segmentation variables are dropped for
#' clustering. The indices of rows used (and dropped) are stored in \code{key}
#' so that cluster membership can be reattached later via \code{\link{easy_hc_final}}.
#'
#' Stopping/diagnostic outputs include Duda/Hart, pseudo-\eqn{t^2}, Gap statistic
#' (1-SE rule), and cluster size balance flags (smallest cluster proportion,
#' largest cluster proportion, and cluster size coefficient of variation).
#'
#' @param data A data frame containing the full dataset.
#' @param vars Character vector of numeric segmentation variable names.
#' @param dist Distance measure. One of: \code{"euc"}, \code{"euc2"},
#'   \code{"max"}, \code{"abs"}, \code{"bin"}.
#' @param method Linkage method. One of: \code{"ward"}, \code{"single"},
#'   \code{"complete"}, \code{"average"}.
#' @param k_range Integer vector of cluster solutions to evaluate (default \code{1:10};
#'   allowed \code{1}--\code{20}).
#' @param standardize Logical; if \code{TRUE} (default), standardizes segmentation
#'   variables prior to clustering.
#' @param show_dend Logical; if \code{TRUE} (default), plots a dendrogram (skipped if
#'   \code{n > dend_max_n}).
#' @param dend_max_n Integer; maximum sample size for drawing a dendrogram (default 300).
#'
#' @return
#' Invisibly returns a list with:
#' \itemize{
#'   \item \code{hc}: The \code{hclust} object.
#'   \item \code{diss}: The distance object used for clustering.
#'   \item \code{stop_raw}: A data frame of stopping indices and diagnostics by \code{k}.
#'   \item \code{stop}: A formatted \code{flextable} version of \code{stop_raw}.
#'   \item \code{data_scaled}: The standardized segmentation data (or \code{NULL} if not standardized).
#'   \item \code{settings}: A list of settings used to fit the model.
#'   \item \code{key}: A list containing row indices (\code{row_index_all}, \code{row_index_used}, \code{dropped_na}).
#' }
#'
#' @examples
#' \dontrun{
#' # Fit + diagnostics (dendrogram shown if n <= dend_max_n)
#' fit <- easy_hc_fit(
#'   data = ffseg,
#'   vars = c("eatin", "hours", "health"),
#'   dist = "euc",
#'   method = "ward",
#'   k_range = 2:10
#' )
#'
#' # View diagnostics table (flextable) and raw data.frame
#' fit$stop
#' fit$stop_raw
#'
#' # Fit without dendrogram
#' fit2 <- easy_hc_fit(
#'   data = ffseg,
#'   vars = c("eatin", "hours", "health"),
#'   show_dend = FALSE
#' )
#' }
#'
#' @importFrom stats dist hclust as.dendrogram cutree complete.cases sd
#' @importFrom dendextend heights_per_k.dendrogram set colored_bars
#' @importFrom NbClust NbClust
#' @importFrom cluster clusGap
#' @importFrom flextable flextable add_header_lines add_footer_lines
#' @importFrom flextable align bold colformat_double autofit
#' @export
easy_hc_fit <- function(data,
                        vars,
                        dist        = c("euc", "euc2", "max", "abs", "bin"),
                        method      = c("ward", "single", "complete", "average"),
                        k_range     = 1:10,
                        standardize = TRUE,
                        show_dend   = TRUE,
                        dend_max_n  = 300) {
   
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
   
   dist   <- match.arg(dist)
   method <- match.arg(method)
   
   if (!is.numeric(k_range) || any(k_range < 1) || any(k_range > 20)) {
      stop("`k_range` must contain integers between 1 and 20.", call. = FALSE)
   }
   k_range <- sort(unique(as.integer(k_range)))
   k_min   <- min(k_range)
   k_max   <- max(k_range)
   
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
   
   n_obs <- nrow(data_used)
   
   # ---- distance + linkage lookup ----
   distdf <- data.frame(
      inp    = c("euc","euc2","max","abs","bin"),
      outp   = c("euclidean","euclidean","maximum","manhattan","binary"),
      dtitle = c("Euclidean","Euclidean^2","Maximum","Absolute","Binary"),
      stringsAsFactors = FALSE
   )
   rownames(distdf) <- distdf$inp
   dtype  <- distdf[dist, "outp"]
   dtitle <- distdf[dist, "dtitle"]
   pw     <- if (dist == "euc2") 2 else 1
   
   linkdf <- data.frame(
      inp    = c("ward","single","complete","average"),
      outp   = c("ward.D","single","complete","average"),
      ltitle = c("Ward's D","Single","Complete","Average"),
      stringsAsFactors = FALSE
   )
   rownames(linkdf) <- linkdf$inp
   ltype  <- linkdf[method, "outp"]
   ltitle <- linkdf[method, "ltitle"]
   
   # ---- clustering (your distance choice) ----
   diss <- stats::dist(data_used, method = dtype) ^ pw
   hc   <- stats::hclust(diss, method = ltype)
   
   # ---- dendrogram ----
   if (isTRUE(show_dend) && n_obs <= dend_max_n) {
      
      hcd   <- stats::as.dendrogram(hc)
      hcd_h <- dendextend::heights_per_k.dendrogram(hcd)
      
      ylim_min <- hcd_h[k_max]
      ylim_max <- hcd_h[1]
      
      the_bars <- sapply(k_range, function(k) stats::cutree(hc, k = k))
      if (length(k_range) == 1L) the_bars <- matrix(the_bars, ncol = 1)
      colnames(the_bars) <- paste0("k_", k_range)
      
      hcd_col <- dendextend::set(hcd, "branches_k_color", k = k_max)
      hcd_col <- dendextend::set(hcd_col, "branches_lwd", 4)
      hcd_col <- dendextend::set(hcd_col, "labels_colors", "white")
      
      plot(
         hcd_col,
         ylim = c(ylim_min, ylim_max),
         ylab = "Similarity Measure",
         main = paste(dtitle, "Distance /", ltitle, "Linkage")
      )
      dendextend::colored_bars(colors = the_bars, dend = hcd_col,
                               sort_by_labels_order = FALSE)
   } else if (isTRUE(show_dend) && n_obs > dend_max_n) {
      message("Skipping dendrogram (n > dend_max_n).")
   }
   
   # ---- stopping indices (NbClust) ----
   # IMPORTANT FIX:
   # For duda/pseudot2, NbClust requires a data matrix (not diss).
   # NbClust does not support squared Euclidean directly; for dist="euc2"
   # we pass "euclidean" to NbClust.
   nb_distance <- if (dist == "euc2") "euclidean" else dtype
   
   nb_duda <- NbClust::NbClust(
      data     = data_used,
      diss     = NULL,
      distance = nb_distance,
      min.nc   = k_min,
      max.nc   = k_max,
      method   = ltype,
      index    = "duda"
   )$All.index
   
   nb_pseudo <- NbClust::NbClust(
      data     = data_used,
      diss     = NULL,
      distance = nb_distance,
      min.nc   = k_min,
      max.nc   = k_max,
      method   = ltype,
      index    = "pseudot2"
   )$All.index
   
   k_seq <- k_min:k_max
   
   # ---- Gap statistic ----
   gap_res <- cluster::clusGap(
      x = as.matrix(data_used),
      FUNcluster = function(x, k) {
         d_tmp  <- stats::dist(x, method = dtype) ^ pw
         hc_tmp <- stats::hclust(d_tmp, method = ltype)
         list(cluster = stats::cutree(hc_tmp, k = k))
      },
      K.max = k_max,
      B     = 20
   )
   
   gap_tab  <- as.data.frame(gap_res$Tab)
   rn_k     <- as.numeric(rownames(gap_tab))
   gap_vals <- gap_tab$gap[match(k_seq, rn_k)]
   se_vals  <- gap_tab$SE.sim[match(k_seq, rn_k)]
   
   # ---- 1-SE Gap rule ----
   valid_idx  <- which(k_seq >= 2)
   is_gap_1se <- rep(FALSE, length(k_seq))
   if (length(valid_idx) > 0) {
      idx_max <- valid_idx[which.max(gap_vals[valid_idx])]
      thresh  <- gap_vals[idx_max] - se_vals[idx_max]
      cand    <- valid_idx[gap_vals[valid_idx] >= thresh]
      if (length(cand) > 0) is_gap_1se[cand[1]] <- TRUE
   }
   
   # ---- cluster size diagnostics ----
   Small.Prop <- Large.Prop <- CV <- numeric(length(k_seq))
   for (i in seq_along(k_seq)) {
      sz <- as.numeric(table(stats::cutree(hc, k = k_seq[i])))
      Small.Prop[i] <- min(sz) / n_obs
      Large.Prop[i] <- max(sz) / n_obs
      CV[i] <- if (length(sz) > 1) stats::sd(sz) / mean(sz) else NA_real_
   }
   
   stop_raw <- data.frame(
      Num.Clusters = k_seq,
      Duda.Hart    = as.numeric(nb_duda),
      pseudo.t2    = as.numeric(nb_pseudo),
      Gap.Stat     = gap_vals,
      Small.Prop   = Small.Prop,
      Large.Prop   = Large.Prop,
      CV           = CV
   )
   
   # ---- formatted symbol columns ----
   gap_char <- sprintf("%.4f", stop_raw$Gap.Stat)
   gap_char[is_gap_1se] <- paste0(gap_char[is_gap_1se], "*")
   
   small_char <- sprintf("%.4f", stop_raw$Small.Prop)
   idx_small  <- which(!is.na(stop_raw$Small.Prop) & stop_raw$Small.Prop < 0.05)
   small_char[idx_small] <- paste0(small_char[idx_small], "^")
   
   large_char <- sprintf("%.4f", stop_raw$Large.Prop)
   idx_large  <- which(!is.na(stop_raw$Large.Prop) & stop_raw$Large.Prop > 0.50)
   large_char[idx_large] <- paste0(large_char[idx_large], " \u25CA")
   
   cv_char <- sprintf("%.3f", stop_raw$CV)
   idx_cv1 <- which(!is.na(stop_raw$CV) & stop_raw$CV < 0.5)
   cv_char[idx_cv1] <- paste0(cv_char[idx_cv1], " \u2022")
   idx_cv2 <- which(!is.na(stop_raw$CV) & stop_raw$CV >= 0.5 & stop_raw$CV < 1)
   cv_char[idx_cv2] <- paste0(cv_char[idx_cv2], " \u2022\u2022")
   
   stop_disp <- data.frame(
      Clusters   = stop_raw$Num.Clusters,
      Duda.Hart  = stop_raw$Duda.Hart,
      pseudo.t2  = stop_raw$pseudo.t2,
      Gap.Stat   = gap_char,
      Small.Prop = small_char,
      Large.Prop = large_char,
      CV         = cv_char,
      stringsAsFactors = FALSE
   )
   
   ft <- flextable::flextable(stop_disp)
   
   # ---- title: centered + bold ----
   ft <- flextable::add_header_lines(
      ft,
      values = paste0(
         "Cluster Diagnostics (",
         dtitle, " Distance / ",
         ltitle, " Linkage)"
      )
   )
   ft <- flextable::align(ft, align = "center", part = "header")
   ft <- flextable::bold(ft, part = "header")
   
   # numeric formatting for numeric columns
   ft <- flextable::colformat_double(ft, j = "Clusters",  digits = 0)
   ft <- flextable::colformat_double(ft, j = "Duda.Hart", digits = 3)
   ft <- flextable::colformat_double(ft, j = "pseudo.t2", digits = 2)
   
   # keep header centered; right-align body
   ft <- flextable::align(ft, align = "right", part = "body")
   
   ft <- flextable::add_footer_lines(
      ft,
      values = c(
         "* 1-SE Gap rule.",
         "^ Smallest cluster < 5% of sample.",
         "\u25CA Largest cluster > 50% of sample.",
         "\u2022 CV < 0.5 well balanced; \u2022\u2022 moderately imbalanced."
         
      )
   )
   
   ft <- flextable::autofit(ft)
   
   settings <- list(
      vars         = vars,
      dist         = dist,
      dist_label   = dtitle,
      method       = method,
      method_label = ltitle,
      k_range      = k_range,
      standardize  = standardize,
      show_dend    = show_dend,
      dend_max_n   = dend_max_n
   )
   
   key <- list(
      row_index_all  = row_index_all,
      row_index_used = row_index_used,
      dropped_na     = dropped_na
   )
   
   return(list(
      hc          = hc,
      diss        = diss,
      stop_raw    = stop_raw,
      stop        = ft,
      data_scaled = data_scaled,
      settings    = settings,
      key         = key
   ))
}
