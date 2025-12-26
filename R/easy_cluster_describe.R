#' @title Easy Cluster Description
#' @description
#' Describe clusters using non-clustering variables (continuous and categorical).
#' The function automatically detects variable type, runs overall tests across
#' clusters, and reports detailed summaries for statistically significant variables.
#'
#' @details
#' For continuous variables, the function reports ANOVA p-values for all variables.
#' For variables significant at the specified \code{alpha}, cluster means, standard
#' deviations, and Games--Howell post-hoc comparisons are provided.
#'
#' For categorical variables, the function reports overall chi-square (or Fisher)
#' test p-values. For significant variables, a cluster-by-category cross-tabulation
#' (column percentages) and Holm-adjusted Fisher multiple-comparison p-values are
#' shown.
#'
#' @param data A data frame containing the cluster membership column and variables
#'   to describe.
#' @param cluster_col Character string naming the cluster membership column in
#'   \code{data}.
#' @param vars Optional character vector of variables to describe. If \code{NULL}
#'   (default), all columns except \code{cluster_col} are used.
#' @param alpha Significance level for hypothesis tests (default = 0.05).
#' @param drop_missing Logical; if \code{TRUE} (default), rows with missing cluster
#'   membership are dropped prior to analysis.
#' @param auto_print Logical; if \code{TRUE} (default), prints summaries to the console.
#'
#' @return
#' Invisibly returns a list with:
#' \itemize{
#'   \item \code{alpha}: The significance level used.
#'   \item \code{continuous_tests}: Data frame of overall tests for continuous variables.
#'   \item \code{continuous_summaries}: Named list of summaries for significant continuous variables.
#'   \item \code{categorical_tests}: Data frame of overall tests for categorical variables.
#'   \item \code{categorical_summaries}: Named list of summaries for significant categorical variables.
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage: describe all non-cluster variables
#' easy_cluster_describe(data = segdata, cluster_col = "cluster")
#'
#' # Describe a subset of variables
#' easy_cluster_describe(
#'   data = segdata,
#'   cluster_col = "cluster",
#'   vars = c("age", "income", "gender")
#' )
#'
#' # Use a different significance level and suppress printing
#' out <- easy_cluster_describe(
#'   data = segdata,
#'   cluster_col = "cluster",
#'   alpha = 0.10,
#'   auto_print = FALSE
#' )
#' }
#'
#' @importFrom stats aov chisq.test fisher.test as.formula
#' @export

easy_cluster_describe <- function(data,
                                  cluster_col = "cluster",
                                  vars = NULL,
                                  alpha = 0.05,
                                  drop_missing = TRUE,
                                  auto_print = TRUE) {
   
   # ---- validate inputs ----
   if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
   
   if (!is.character(cluster_col) || length(cluster_col) != 1) {
      stop("`cluster_col` must be a single character string.", call. = FALSE)
   }
   if (!cluster_col %in% names(data)) {
      stop("`cluster_col` not found in `data`.", call. = FALSE)
   }
   
   if (!is.numeric(alpha) || length(alpha) != 1 || is.na(alpha) || alpha <= 0 || alpha >= 1) {
      stop("`alpha` must be a single number between 0 and 1 (exclusive).", call. = FALSE)
   }
   
   if (!is.logical(drop_missing) || length(drop_missing) != 1) {
      stop("`drop_missing` must be TRUE/FALSE.", call. = FALSE)
   }
   if (!is.logical(auto_print) || length(auto_print) != 1) {
      stop("`auto_print` must be TRUE/FALSE.", call. = FALSE)
   }
   
   # choose vars
   if (is.null(vars)) {
      vars <- setdiff(names(data), cluster_col)
   } else {
      if (!is.character(vars) || length(vars) < 1) {
         stop("`vars` must be NULL or a non-empty character vector.", call. = FALSE)
      }
      bad <- vars[!vars %in% names(data)]
      if (length(bad) > 0) {
         stop("These `vars` are not in `data`: ", paste(bad, collapse = ", "), call. = FALSE)
      }
      vars <- setdiff(vars, cluster_col)
   }
   
   # drop missing clusters (optional)
   cl <- data[[cluster_col]]
   if (drop_missing) {
      keep_cl <- !is.na(cl)
      data <- data[keep_cl, , drop = FALSE]
      cl <- data[[cluster_col]]
   }
   
   # cluster as factor
   cl <- as.factor(cl)
   data[[cluster_col]] <- cl
   k_levels <- levels(cl)
   
   # split vars into continuous vs categorical (auto-detect)
   is_cont <- vapply(data[, vars, drop = FALSE], function(x) is.numeric(x), logical(1))
   cont_vars <- vars[is_cont]
   cat_vars  <- vars[!is_cont]
   
   # coerce character/logical to factor for categorical
   for (v in cat_vars) {
      if (is.character(data[[v]]) || is.logical(data[[v]])) {
         data[[v]] <- as.factor(data[[v]])
      }
   }
   
   # ------------------------------------------------------------------
   # Continuous: overall p-values for ALL; summaries only for significant;
   # Games-Howell post-hoc + plain-language direction string
   # ------------------------------------------------------------------
   continuous_tests <- data.frame()
   continuous_summaries <- list()
   
   have_rstatix <- requireNamespace("rstatix", quietly = TRUE)
   
   if (length(cont_vars) > 0) {
      for (v in cont_vars) {
         
         dfv <- data[, c(cluster_col, v), drop = FALSE]
         dfv <- dfv[!is.na(dfv[[v]]), , drop = FALSE]
         if (nrow(dfv) == 0) next
         
         p_val <- NA_real_
         if (length(unique(dfv[[cluster_col]])) > 1) {
            fit <- stats::aov(dfv[[v]] ~ dfv[[cluster_col]])
            an  <- summary(fit)[[1]]
            p_val <- as.numeric(an[["Pr(>F)"]][1])
         }
         
         continuous_tests <- rbind(
            continuous_tests,
            data.frame(
               Variable = v,
               Test = "ANOVA",
               p_value = p_val,
               Significant = if (!is.na(p_val)) (p_val < alpha) else FALSE,
               stringsAsFactors = FALSE
            )
         )
         
         if (is.na(p_val) || !(p_val < alpha)) next
         
         if (!have_rstatix) {
            stop(
               "Games-Howell post-hoc requires the `rstatix` package. ",
               "Install it with install.packages('rstatix').",
               call. = FALSE
            )
         }
         
         # per-cluster summary
         summ <- data.frame(
            Cluster = k_levels,
            N = 0L,
            Mean = NA_real_,
            SD = NA_real_,
            stringsAsFactors = FALSE
         )
         
         for (lev in k_levels) {
            x <- dfv[dfv[[cluster_col]] == lev, v]
            x <- x[!is.na(x)]
            n <- length(x)
            summ[summ$Cluster == lev, "N"] <- n
            summ[summ$Cluster == lev, "Mean"] <- if (n > 0) mean(x) else NA_real_
            summ[summ$Cluster == lev, "SD"] <- if (n > 1) stats::sd(x) else NA_real_
         }
         
         summ$Mean <- round(summ$Mean, 4)
         summ$SD   <- round(summ$SD, 4)
         
         gh <- rstatix::games_howell_test(
            data = dfv,
            formula = stats::as.formula(paste0(v, " ~ ", cluster_col))
         )
         
         gh_sig <- gh[!is.na(gh$p.adj) & gh$p.adj < alpha, , drop = FALSE]
         
         diff_str <- "Significant differences: none"
         if (nrow(gh_sig) > 0) {
            mean_map <- setNames(summ$Mean, summ$Cluster)
            
            pairs <- character(0)
            for (i in seq_len(nrow(gh_sig))) {
               g1 <- as.character(gh_sig$group1[i])
               g2 <- as.character(gh_sig$group2[i])
               
               m1 <- mean_map[g1]
               m2 <- mean_map[g2]
               if (is.na(m1) || is.na(m2)) next
               
               if (m1 > m2) {
                  pairs <- c(pairs, paste0(g1, " > ", g2))
               } else if (m2 > m1) {
                  pairs <- c(pairs, paste0(g2, " > ", g1))
               } else {
                  pairs <- c(pairs, paste0(g1, " = ", g2))
               }
            }
            
            pairs <- unique(pairs)
            if (length(pairs) > 0) {
               diff_str <- paste0("Significant differences: ", paste(pairs, collapse = ", "))
            }
         }
         
         continuous_summaries[[v]] <- list(
            summary = summ,
            differences = diff_str,
            posthoc = gh
         )
      }
      
      continuous_tests$p_value <- round(continuous_tests$p_value, 4)
   }
   
   # ------------------------------------------------------------------
   # Categorical: overall tests for ALL; for significant:
   # cross-tab (clusters as columns) with column % only + fisher.multcomp (Holm)
   # but keep ONLY the fisher.multcomp p-value table
   # ------------------------------------------------------------------
   categorical_tests <- data.frame()
   categorical_summaries <- list()
   
   have_rva <- requireNamespace("RVAideMemoire", quietly = TRUE)
   
   if (length(cat_vars) > 0) {
      for (v in cat_vars) {
         
         dfv <- data[, c(cluster_col, v), drop = FALSE]
         dfv <- dfv[!is.na(dfv[[v]]), , drop = FALSE]
         if (nrow(dfv) == 0) next
         
         dfv[[v]] <- as.factor(dfv[[v]])
         
         # Cross-tab: variable rows, cluster columns
         tab <- table(dfv[[v]], dfv[[cluster_col]])
         
         # Overall test: try chi-square; fall back to Fisher
         test_name <- "Chi-square"
         p <- NA_real_
         
         chi <- try(stats::chisq.test(tab, correct = FALSE), silent = TRUE)
         if (!inherits(chi, "try-error")) {
            p <- as.numeric(chi$p.value)
         } else {
            test_name <- "Fisher"
            p <- as.numeric(stats::fisher.test(tab)$p.value)
         }
         
         categorical_tests <- rbind(
            categorical_tests,
            data.frame(
               Variable = v,
               Test = test_name,
               p_value = p,
               Significant = if (!is.na(p)) (p < alpha) else FALSE,
               stringsAsFactors = FALSE
            )
         )
         
         if (is.na(p) || !(p < alpha)) next
         
         # Column percentages only (clusters are columns)
         col_pct <- prop.table(tab, 2) * 100
         col_pct <- round(col_pct, 4)
         
         # fisher.multcomp (Holm) + extract ONLY the p-value table
         if (!have_rva) {
            stop(
               "Post-hoc for categorical variables requires `RVAideMemoire`. ",
               "Install it with install.packages('RVAideMemoire').",
               call. = FALSE
            )
         }
         
         fm <- try(RVAideMemoire::fisher.multcomp(tab, p.method = "holm"), silent = TRUE)
         if (inherits(fm, "try-error")) {
            # fallback: some versions may use a different argument name
            fm <- RVAideMemoire::fisher.multcomp(tab, method = "holm")
         }
         
         fm_tab <- NULL
         if (is.matrix(fm)) {
            fm_tab <- fm
         } else if (is.list(fm)) {
            if (!is.null(fm$p.value) && is.matrix(fm$p.value)) {
               fm_tab <- fm$p.value
            } else if (!is.null(fm$pv) && is.matrix(fm$pv)) {
               fm_tab <- fm$pv
            } else if (!is.null(fm$table) && is.matrix(fm$table)) {
               fm_tab <- fm$table
            } else if (!is.null(fm$result) && is.matrix(fm$result)) {
               fm_tab <- fm$result
            }
         }
         
         if (is.null(fm_tab)) {
            stop("Could not extract a p-value table from RVAideMemoire::fisher.multcomp().", call. = FALSE)
         }
         
         fm_tab <- round(fm_tab, 4)
         
         categorical_summaries[[v]] <- list(
            crosstab_colpct = col_pct,
            posthoc_table = fm_tab
         )
      }
      
      categorical_tests$p_value <- round(categorical_tests$p_value, 4)
   }
   
   res <- list(
      alpha = alpha,
      continuous_tests = continuous_tests,
      continuous_summaries = continuous_summaries,
      categorical_tests = categorical_tests,
      categorical_summaries = categorical_summaries
   )
   
   # ---- printing (teaching outputs only) ----
   if (isTRUE(auto_print)) {
      
      if (nrow(continuous_tests) > 0) {
         cat("\nContinuous variables (p-values):\n")
         print(continuous_tests[, c("Variable", "Test", "p_value", "Significant")], row.names = FALSE)
      }
      
      sig_cont <- names(continuous_summaries)
      if (length(sig_cont) > 0) {
         cat("\nContinuous variables (cluster summaries of significant variables):\n")
         for (v in sig_cont) {
            cat("\n--- ", v, " ---\n", sep = "")
            print(continuous_summaries[[v]]$summary, row.names = FALSE)
            cat(continuous_summaries[[v]]$differences, "\n")
         }
      } else {
         cat("\nContinuous variables (cluster summaries of significant variables): none\n")
      }
      
      if (nrow(categorical_tests) > 0) {
         cat("\nCategorical variables (overall tests):\n")
         print(categorical_tests[, c("Variable", "Test", "p_value", "Significant")], row.names = FALSE)
      }
      
      sig_cat <- names(categorical_summaries)
      if (length(sig_cat) > 0) {
         cat("\nCategorical variables (details for significant variables):\n")
         for (v in sig_cat) {
            cat("\n--- ", v, " ---\n", sep = "")
            cat("Cross-tab (column %; clusters are columns):\n")
            print(categorical_summaries[[v]]$crosstab_colpct)
            cat("\nPost-hoc (Holm-adjusted p-values):\n")
            print(categorical_summaries[[v]]$posthoc_table)
         }
      } else {
         cat("\nCategorical variables (details for significant variables): none\n")
      }
   }
   
   invisible(res)
}
