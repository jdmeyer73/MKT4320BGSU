#' @title Easy Hierarchical Clustering (Final Solution)
#' @description
#' Choose a final number of clusters (\code{k}) from an \code{\link{easy_hc_fit}}
#' result, reattach membership to the full dataset using the stored row-index key,
#' and produce cluster proportions and a mean profile table.
#'
#' @details
#' Cluster membership is computed on the rows that were actually used in clustering
#' (complete cases on the segmentation variables). Membership is then reattached
#' to the full dataset; rows not used for clustering receive \code{NA} membership.
#'
#' If \code{k < 6}, the function also returns a bar plot of mean profiles by cluster
#' with confidence-interval error bars (default 95%).
#'
#' @param fit An object returned by \code{\link{easy_hc_fit}}.
#' @param data The original full dataset used to create \code{fit}.
#' @param k Integer; number of clusters to extract.
#' @param cluster_col Name of the cluster column to append to \code{data}
#'   (default \code{"cluster"}). Must not already exist in \code{data}.
#' @param conf_level Confidence level for CI error bars when plotting (default 0.95).
#' @param auto_print Logical; if \code{TRUE} (default), prints \code{props} and
#'   \code{profile} and displays the plot (if available).
#'
#' @return
#' Invisibly returns a list with:
#' \itemize{
#'   \item \code{props}: Cluster counts and proportions (computed on used rows only).
#'   \item \code{profile}: Cluster means for the segmentation variables (non-standardized scale).
#'   \item \code{plot}: A ggplot object (only if \code{k < 6}); otherwise \code{NULL}.
#'   \item \code{data}: The full dataset with the appended membership column.
#'   \item \code{membership}: Integer membership vector for the used rows (length = rows used in \code{fit}).
#' }
#'
#' @examples
#' \dontrun{
#' # Fit clustering first
#' fit <- easy_hc_fit(
#'   data = ffseg,
#'   vars = c("eatin", "hours", "health"),
#'   dist = "euc",
#'   method = "ward",
#'   k_range = 2:10
#' )
#'
#' # Choose a final k and attach membership
#' final <- easy_hc_final(fit, data = ffseg, k = 3, cluster_col = "HC3")
#'
#' # Outputs
#' final$props
#' final$profile
#' head(final$data)
#'
#' # Suppress printing
#' final2 <- easy_hc_final(fit, data = ffseg, k = 4, auto_print = FALSE)
#' }
#'
#' @importFrom stats cutree qt sd
#' @importFrom ggplot2 ggplot aes geom_col geom_errorbar facet_wrap
#' @importFrom ggplot2 labs theme_bw theme element_text scale_fill_brewer
#' @export
easy_hc_final <- function(fit,
                          data,
                          k,
                          cluster_col = "cluster",
                          conf_level  = 0.95,
                          auto_print  = TRUE) {
   
   # ---- validation ----
   if (!is.list(fit) || is.null(fit$hc) || is.null(fit$key) || is.null(fit$settings)) {
      stop("`fit` must be an object returned by easy_hc_fit().", call. = FALSE)
   }
   if (!is.data.frame(data)) {
      stop("`data` must be a data frame.", call. = FALSE)
   }
   
   k <- as.integer(k)
   if (is.na(k) || k < 1) stop("`k` must be a positive integer.", call. = FALSE)
   
   if (!is.character(cluster_col) || length(cluster_col) != 1 || nchar(cluster_col) < 1) {
      stop("`cluster_col` must be a single non-empty character string.", call. = FALSE)
   }
   if (cluster_col %in% names(data)) {
      stop("`cluster_col` already exists in `data`.", call. = FALSE)
   }
   
   if (!is.numeric(conf_level) || length(conf_level) != 1 || conf_level <= 0 || conf_level >= 1) {
      stop("`conf_level` must be a single number between 0 and 1 (exclusive).", call. = FALSE)
   }
   
   if (!is.logical(auto_print) || length(auto_print) != 1) {
      stop("`auto_print` must be TRUE or FALSE.", call. = FALSE)
   }
   
   vars <- fit$settings$vars
   if (is.null(vars) || !is.character(vars) || length(vars) < 1) {
      stop("fit$settings$vars is missing; fit does not look like it came from easy_hc_fit().", call. = FALSE)
   }
   if (!all(vars %in% names(data))) {
      bad <- vars[!vars %in% names(data)]
      stop("These segmentation variables are not in `data`: ", paste(bad, collapse = ", "), call. = FALSE)
   }
   
   row_index_used <- fit$key$row_index_used
   if (is.null(row_index_used) || !is.numeric(row_index_used)) {
      stop("fit$key$row_index_used is missing; fit does not look like it came from easy_hc_fit().", call. = FALSE)
   }
   if (max(row_index_used) > nrow(data)) {
      stop("`data` does not match the dataset used to create `fit` (row count mismatch).", call. = FALSE)
   }
   
   # ---- membership for used rows ----
   membership_used <- stats::cutree(fit$hc, k = k)
   if (length(membership_used) != length(row_index_used)) {
      stop("Internal mismatch: membership length does not match rows used in clustering.", call. = FALSE)
   }
   membership_used <- as.integer(membership_used)
   
   # ---- attach membership back to full data ----
   membership_full <- rep(NA_integer_, nrow(data))
   membership_full[row_index_used] <- membership_used
   
   data_out <- data
   data_out[[cluster_col]] <- membership_full
   
   # ---- cluster proportions (used rows only) ----
   tab <- table(membership_used)
   props <- data.frame(
      Cluster = as.integer(names(tab)),
      N       = as.integer(tab),
      Prop    = as.numeric(tab) / sum(tab),
      stringsAsFactors = FALSE
   )
   
   # ---- cluster profile (means, non-standardized vars) ----
   seg_used <- data[row_index_used, vars, drop = FALSE]
   clusters <- sort(unique(membership_used))
   
   profile <- data.frame(Cluster = clusters, stringsAsFactors = FALSE)
   for (v in vars) profile[[v]] <- NA_real_
   
   for (cl in clusters) {
      idx <- which(membership_used == cl)
      for (v in vars) {
         profile[profile$Cluster == cl, v] <- mean(seg_used[idx, v], na.rm = TRUE)
      }
   }
   
   # ---- plot: means with 95% CI (k < 6 only) ----
   plt <- NULL
   if (k < 6) {
      
      alpha <- 1 - conf_level
      out_list <- vector("list", length(vars) * length(clusters))
      ctr <- 1L
      
      for (v in vars) {
         for (cl in clusters) {
            
            idx <- which(membership_used == cl)
            x   <- seg_used[idx, v]
            x   <- x[!is.na(x)]
            n   <- length(x)
            
            m <- if (n > 0) mean(x) else NA_real_
            s <- if (n > 1) stats::sd(x) else NA_real_
            
            tcrit <- if (n > 1) stats::qt(1 - alpha/2, df = n - 1) else NA_real_
            se    <- if (n > 1) s / sqrt(n) else NA_real_
            moe   <- if (n > 1) tcrit * se else NA_real_
            
            out_list[[ctr]] <- data.frame(
               Variable = v,
               Cluster  = factor(cl, levels = clusters),
               Mean     = m,
               CI_Lo    = if (n > 1) (m - moe) else NA_real_,
               CI_Hi    = if (n > 1) (m + moe) else NA_real_,
               stringsAsFactors = FALSE
            )
            ctr <- ctr + 1L
         }
      }
      
      plot_df <- do.call(rbind, out_list)
      plot_df$Variable <- factor(plot_df$Variable, levels = vars)
      
      plt <- ggplot2::ggplot(plot_df, ggplot2::aes(x = Cluster, y = Mean, fill = Cluster)) +
         ggplot2::geom_col(width = 0.7) +
         ggplot2::geom_errorbar(
            ggplot2::aes(ymin = CI_Lo, ymax = CI_Hi),
            width = 0.2,
            color = "black"
         ) +
         ggplot2::facet_wrap(~ Variable, scales = "free_y") +
         ggplot2::scale_fill_brewer(palette = "Pastel1") +
         ggplot2::labs(
            title = paste0("Cluster Profiles (Means with ", round(conf_level * 100), "% CI)"),
            x = "Cluster",
            y = "Mean"
         ) +
         ggplot2::theme_bw() +
         ggplot2::theme(
            legend.position = "none",
            plot.title = ggplot2::element_text(hjust = 0.5)
         )
   }
   
   res <- list(
      props      = props,
      profile    = profile,
      plot       = plt,
      data       = data_out,
      membership = membership_used
   )
   
   # ---- optionally print only the "teaching outputs" ----
   if (isTRUE(auto_print)) {
      print(props)
      cat("\n")
      print(profile)
      if (!is.null(plt)) print(plt)
   }
   
   invisible(res)
}
