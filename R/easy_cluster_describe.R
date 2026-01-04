#' Easy Cluster Description
#'
#' Describe cluster differences for a single variable (continuous or categorical).
#' Designed for teaching in R Markdown / R Notebooks: tables are rendered as plain
#' text with headings to reduce RStudio "thumbnail" outputs during interactive runs.
#'
#' @details
#' This function tests whether clusters differ on one variable:
#' \itemize{
#'   \item \strong{Continuous variable:} one-way ANOVA. If significant, prints cluster
#'   means (N, Mean, SD) and Games--Howell post-hoc comparisons.
#'   \item \strong{Categorical variable:} chi-square test (or Fisher's exact test if needed).
#'   If significant, prints a column-percentage crosstab (clusters are columns) and
#'   Holm-adjusted Fisher multiple comparisons.
#' }
#'
#' Use \code{auto_print = FALSE} to suppress printing and instead return a results list
#' invisibly (useful for instructor testing or downstream processing).
#'
#' @param data A data frame containing the cluster membership column and the variable to describe.
#' @param cluster_col Character string naming the cluster membership column in \code{data}.
#' @param var Character string naming the single variable to describe.
#' @param alpha Significance level for hypothesis tests (default = 0.05).
#' @param drop_missing Logical; if \code{TRUE} (default), rows with missing cluster membership are dropped.
#' @param auto_print Logical; if \code{TRUE} (default), prints output and returns \code{NULL} invisibly.
#'   If \code{FALSE}, returns a results list invisibly (and prints nothing).
#' @param digits Integer; number of digits used for rounding/formatting numeric output (default = 4).
#'
#' @return
#' If \code{auto_print = TRUE} (default), returns \code{invisible(NULL)} after printing.
#' If \code{auto_print = FALSE}, returns \code{invisible(res)} where \code{res} is a list with:
#' \describe{
#'   \item{alpha}{The alpha level used.}
#'   \item{variable}{The variable analyzed.}
#'   \item{type}{\code{"continuous"} or \code{"categorical"}.}
#'   \item{overall_test}{A one-row data frame with test name, p-value, and significance.}
#'   \item{details}{For significant tests, additional tables (cluster summaries, post-hoc results, etc.).}
#' }
#'
#' @examples
#' \dontrun{
#' # Continuous variable
#' easy_cluster_describe(hc_final$data, cluster_col = "hc4", var = "online")
#'
#' # Categorical variable
#' easy_cluster_describe(hc_final$data, cluster_col = "hc4", var = "edu")
#'
#' # Instructor/testing mode: return results (no printing)
#' res <- easy_cluster_describe(hc_final$data, cluster_col = "hc4", var = "edu", auto_print = FALSE)
#' names(res)
#' res$overall_test
#' }
#'
#' @importFrom stats aov chisq.test fisher.test as.formula sd
#' @importFrom utils capture.output str
#' @export

easy_cluster_describe <- function(data,
                                  cluster_col = "cluster",
                                  var,
                                  alpha = 0.05,
                                  drop_missing = TRUE,
                                  auto_print = TRUE,
                                  digits = 4) {
   
   # -------------------- helpers --------------------
   .heading <- function(txt) cat("\n", txt, "\n", sep = "")
   
   # Render a data.frame/matrix to plain text WITHOUT calling print()
   .table_to_text <- function(x, row.names = TRUE) {
      if (is.matrix(x)) x <- as.data.frame(x, stringsAsFactors = FALSE)
      
      x <- as.data.frame(x, stringsAsFactors = FALSE, optional = TRUE)
      
      # Add rownames as a real column if requested
      if (isTRUE(row.names)) {
         rn <- rownames(x)
         if (is.null(rn)) rn <- rep("", nrow(x))
         x <- cbind(Row = rn, x, stringsAsFactors = FALSE)
      }
      
      # Convert all columns to character (no printing)
      for (j in seq_along(x)) {
         v <- x[[j]]
         if (is.numeric(v)) {
            x[[j]] <- formatC(v, digits = digits, format = "fg", flag = "#")
         } else {
            if (inherits(v, "factor")) v <- as.character(v)
            v[is.na(v)] <- ""
            x[[j]] <- as.character(v)
         }
      }
      
      # Compute widths
      widths <- integer(ncol(x))
      for (j in seq_len(ncol(x))) {
         widths[j] <- max(nchar(names(x)[j]), nchar(x[[j]]), na.rm = TRUE)
         if (!is.finite(widths[j]) || widths[j] < 1) widths[j] <- 1L
      }
      
      pad <- function(s, w) {
         s <- ifelse(is.na(s), "", s)
         paste0(s, strrep(" ", pmax(0, w - nchar(s))))
      }
      
      header <- paste(mapply(pad, names(x), widths), collapse = "  ")
      sep    <- paste(mapply(function(w) strrep("-", w), widths), collapse = "  ")
      
      rows <- character(nrow(x))
      if (nrow(x) > 0) {
         for (i in seq_len(nrow(x))) {
            rows[i] <- paste(mapply(pad, as.character(x[i, , drop = TRUE]), widths), collapse = "  ")
         }
      }
      
      paste(c(header, sep, rows), collapse = "\n")
   }
   
   .cat_table <- function(x, row.names = TRUE) {
      txt <- local(.table_to_text(x, row.names = row.names))
      cat(txt, "\n", sep = "")
      invisible(NULL)
   }
   
   .block <- function(title, x, row.names = TRUE) {
      .heading(title)
      if (is.data.frame(x) || is.matrix(x)) {
         .cat_table(x, row.names = row.names)
      } else {
         # Safe for non-table objects
         cat(paste(capture.output(str(x)), collapse = "\n"), "\n", sep = "")
      }
      invisible(NULL)
   }
   
   # -------------------- validate inputs --------------------
   if (!is.data.frame(data)) stop("`data` must be a data.frame.", call. = FALSE)
   
   if (!is.character(cluster_col) || length(cluster_col) != 1) {
      stop("`cluster_col` must be a single character string.", call. = FALSE)
   }
   if (!cluster_col %in% names(data)) stop("`cluster_col` not found in `data`.", call. = FALSE)
   
   if (missing(var) || is.null(var)) {
      stop("This function requires `var` (exactly ONE variable name).", call. = FALSE)
   }
   if (!is.character(var) || length(var) != 1) {
      stop("`var` must be a single character string (one variable at a time).", call. = FALSE)
   }
   
   v <- var
   if (!v %in% names(data)) stop("`var` not found in `data`: ", v, call. = FALSE)
   if (identical(v, cluster_col)) stop("`var` cannot be the same as `cluster_col`.", call. = FALSE)
   
   if (!is.numeric(alpha) || length(alpha) != 1 || is.na(alpha) || alpha <= 0 || alpha >= 1) {
      stop("`alpha` must be a single number between 0 and 1 (exclusive).", call. = FALSE)
   }
   if (!is.logical(drop_missing) || length(drop_missing) != 1) stop("`drop_missing` must be TRUE/FALSE.", call. = FALSE)
   if (!is.logical(auto_print) || length(auto_print) != 1) stop("`auto_print` must be TRUE/FALSE.", call. = FALSE)
   if (!is.numeric(digits) || length(digits) != 1 || is.na(digits) || digits < 0) {
      stop("`digits` must be a non-negative integer.", call. = FALSE)
   }
   digits <- as.integer(digits)
   
   # -------------------- prep data --------------------
   cl <- data[[cluster_col]]
   if (drop_missing) {
      keep <- !is.na(cl)
      data <- data[keep, , drop = FALSE]
      cl <- data[[cluster_col]]
   }
   
   cl <- as.factor(cl)
   data[[cluster_col]] <- cl
   k_levels <- levels(cl)
   
   dfv <- data[, c(cluster_col, v), drop = FALSE]
   dfv <- dfv[!is.na(dfv[[v]]), , drop = FALSE]
   
   if (nrow(dfv) == 0) {
      if (isTRUE(auto_print)) {
         .heading(paste0("Variable: ", v))
         cat("No non-missing data available for this variable.\n")
         return(invisible(NULL))
      } else {
         res <- list(
            alpha = alpha,
            variable = v,
            type = NA_character_,
            overall_test = data.frame(
               Variable = v, Test = NA_character_, p_value = NA_real_, Significant = FALSE,
               stringsAsFactors = FALSE
            ),
            details = NULL
         )
         return(invisible(res))
      }
   }
   
   is_cont <- is.numeric(dfv[[v]])
   if (!is_cont) {
      if (is.character(dfv[[v]]) || is.logical(dfv[[v]])) dfv[[v]] <- as.factor(dfv[[v]])
      dfv[[v]] <- as.factor(dfv[[v]])
   }
   
   # -------------------- analysis --------------------
   overall_test <- NULL
   details <- NULL
   
   if (is_cont) {
      # -------- continuous --------
      p_val <- NA_real_
      if (length(unique(dfv[[cluster_col]])) > 1) {
         fit <- stats::aov(dfv[[v]] ~ dfv[[cluster_col]])
         an  <- summary(fit)[[1]]
         p_val <- as.numeric(an[["Pr(>F)"]][1])
      }
      
      sig <- (!is.na(p_val)) && (p_val < alpha)
      
      overall_test <- data.frame(
         Variable = v,
         Test = "ANOVA",
         p_value = round(p_val, digits),
         Significant = sig,
         stringsAsFactors = FALSE
      )
      
      if (sig) {
         if (!requireNamespace("rstatix", quietly = TRUE)) {
            stop("Post-hoc requires `rstatix`. Install: install.packages('rstatix')", call. = FALSE)
         }
         
         summ <- local({
            out <- data.frame(
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
               out[out$Cluster == lev, "N"] <- n
               out[out$Cluster == lev, "Mean"] <- if (n > 0) mean(x) else NA_real_
               out[out$Cluster == lev, "SD"] <- if (n > 1) stats::sd(x) else NA_real_
            }
            out$Mean <- round(out$Mean, digits)
            out$SD   <- round(out$SD, digits)
            out
         })
         
         gh_df <- local({
            gh <- rstatix::games_howell_test(
               data = dfv,
               formula = stats::as.formula(paste0(v, " ~ ", cluster_col))
            )
            as.data.frame(gh)
         })
         
         gh_sig <- gh_df[!is.na(gh_df$p.adj) & gh_df$p.adj < alpha, , drop = FALSE]
         
         diff_str <- "Significant differences: none"
         if (nrow(gh_sig) > 0) {
            mean_map <- setNames(summ$Mean, summ$Cluster)
            pairs <- character(0)
            
            for (i in seq_len(nrow(gh_sig))) {
               g1 <- as.character(gh_sig$group1[i])
               g2 <- as.character(gh_sig$group2[i])
               m1 <- mean_map[g1]; m2 <- mean_map[g2]
               if (is.na(m1) || is.na(m2)) next
               if (m1 > m2) pairs <- c(pairs, paste0(g1, " > ", g2))
               else if (m2 > m1) pairs <- c(pairs, paste0(g2, " > ", g1))
            }
            
            pairs <- unique(pairs)
            if (length(pairs) > 0) diff_str <- paste0("Significant differences: ", paste(pairs, collapse = ", "))
         }
         
         details <- list(
            summary = summ,
            differences = diff_str,
            posthoc = gh_df
         )
      }
      
      res <- list(
         alpha = alpha,
         variable = v,
         type = "continuous",
         overall_test = overall_test,
         details = details
      )
      
      if (isTRUE(auto_print)) {
         .heading(paste0("Variable: ", v, " (continuous)"))
         .block("Overall test", overall_test, row.names = FALSE)
         
         if (isTRUE(overall_test$Significant[1]) && !is.null(details)) {
            .block("Cluster means (N, Mean, SD)", details$summary, row.names = FALSE)
            cat("\n", details$differences, "\n", sep = "")
            .block("Post-hoc comparisons (Games-Howell)", details$posthoc, row.names = FALSE)
         } else {
            cat("\nNot significant at alpha = ", alpha, "; no post-hoc shown.\n", sep = "")
         }
         
         return(invisible(NULL))
      }
      
      return(invisible(res))
      
   } else {
      # -------- categorical --------
      tab <- table(dfv[[v]], dfv[[cluster_col]])
      
      test_name <- "Chi-square"
      chi <- try(stats::chisq.test(tab, correct = FALSE), silent = TRUE)
      if (!inherits(chi, "try-error")) {
         p <- as.numeric(chi$p.value)
      } else {
         test_name <- "Fisher"
         p <- as.numeric(stats::fisher.test(tab)$p.value)
      }
      
      sig <- (!is.na(p)) && (p < alpha)
      
      overall_test <- data.frame(
         Variable = v,
         Test = test_name,
         p_value = round(p, digits),
         Significant = sig,
         stringsAsFactors = FALSE
      )
      
      if (sig) {
         # Column percentages (clusters are columns)
         col_pct <- local({
            m <- prop.table(tab, 2) * 100
            round(m, digits)
         })
         
         # Convert to a true wide crosstab-style data.frame (rows = levels, cols = clusters)
         col_pct_df <- local({
            df <- as.data.frame.matrix(col_pct)
            df <- cbind(Level = rownames(df), df, stringsAsFactors = FALSE)
            rownames(df) <- NULL
            df
         })
         
         if (!requireNamespace("RVAideMemoire", quietly = TRUE)) {
            stop("Post-hoc requires `RVAideMemoire`. Install: install.packages('RVAideMemoire')", call. = FALSE)
         }
         
         fm_tab <- local({
            fm <- RVAideMemoire::fisher.multcomp(tab, p.method = "holm")
            out <- NULL
            if (is.matrix(fm)) out <- fm
            if (is.null(out) && is.list(fm) && !is.null(fm$p.value) && is.matrix(fm$p.value)) out <- fm$p.value
            if (is.null(out)) stop("Could not extract p-value table from fisher.multcomp().", call. = FALSE)
            round(out, digits)
         })
         
         details <- list(
            crosstab_colpct = col_pct_df,
            posthoc_table = fm_tab
         )
      }
      
      res <- list(
         alpha = alpha,
         variable = v,
         type = "categorical",
         overall_test = overall_test,
         details = details
      )
      
      if (isTRUE(auto_print)) {
         .heading(paste0("Variable: ", v, " (categorical)"))
         .block("Overall test", overall_test, row.names = FALSE)
         
         if (isTRUE(overall_test$Significant[1]) && !is.null(details)) {
            .block("Cross-tab (column %; clusters are columns)", details$crosstab_colpct, row.names = FALSE)
            .block("Post-hoc (Holm-adjusted p-values)", as.matrix(details$posthoc_table), row.names = TRUE)
         } else {
            cat("\nNot significant at alpha = ", alpha, "; no post-hoc shown.\n", sep = "")
         }
         
         return(invisible(NULL))
      }
      
      return(invisible(res))
   }
}
