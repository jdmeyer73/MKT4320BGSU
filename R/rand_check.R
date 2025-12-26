#' Randomization (Balance) Check for Experimental Designs
#'
#' @description
#' Perform a randomization (balance) check comparing covariate distributions
#' across treatment groups. The function automatically selects appropriate
#' statistical tests based on variable type and returns a concise summary
#' suitable for teaching, reporting, or diagnostics.
#'
#' @details
#' Continuous covariates are compared across treatment groups using
#' one-way ANOVA. Categorical covariates are evaluated using chi-squared
#' tests (or Fisher's exact test when expected cell counts are small).
#'
#' By default, results are returned as a formatted \code{flextable}.
#' A plain data frame can be returned by setting \code{ft = FALSE}.
#'
#' This function is intended to replace the deprecated \code{rcheck()}
#' function and should be used as the single source of truth going forward.
#'
#' @param data A data frame containing the treatment indicator and covariates.
#' @param treatment Character string giving the name of the treatment variable.
#'   Must identify two or more groups.
#' @param covariates Character vector of covariate names to include in the
#'   randomization check.
#' @param ft Logical; if \code{TRUE} (default), return results as a
#'   \code{flextable}. If \code{FALSE}, return a data frame.
#' @param digits Integer; number of decimal places to display in the output
#'   (default = 3).
#'
#' @return
#' If \code{ft = TRUE}, a \code{flextable} summarizing balance tests by covariate.
#' Otherwise, a data frame with test statistics and p-values.
#'
#' @seealso \code{\link{rcheck}}
#'
#' @export
#'
#' @importFrom stats aov chisq.test fisher.test
#'
#' @examples
#' \dontrun{
#' rand_check(
#'   data = email.camp.w,
#'   treatment = "promotion",
#'   covariates = c("age", "income", "gender"),
#'   ft = TRUE
#' )
#' }
rand_check <- function(data,
                       treatment,
                       covariates,
                       ft = TRUE,
                       digits = 3) {
   
   # ---- basic checks ----
   if (!is.data.frame(data)) stop("`data` must be a data.frame.")
   
   if (!is.character(treatment) || length(treatment) != 1) {
      stop("`treatment` must be a single character string (variable name).")
   }
   if (!treatment %in% names(data)) stop("`treatment` not found in `data`.")
   
   if (missing(covariates) || is.null(covariates) || length(covariates) == 0) {
      stop("Please provide `covariates` (a non-empty character vector).")
   }
   if (!is.character(covariates)) stop("`covariates` must be a character vector.")
   missing_covs <- setdiff(covariates, names(data))
   if (length(missing_covs) > 0) {
      stop(paste0("These covariates are not in `data`: ",
                  paste(missing_covs, collapse = ", ")))
   }
   
   # Keep only treatment + covariates (explicit)
   data <- data[, unique(c(treatment, covariates)), drop = FALSE]
   
   # ---- helper: column SD (sample SD) for a numeric matrix ----
   col_sd <- function(x, na.rm = TRUE) {
      if (na.rm) {
         n <- colSums(!is.na(x))
      } else {
         n <- nrow(x)
      }
      m1 <- colMeans(x, na.rm = na.rm)
      m2 <- colMeans(x * x, na.rm = na.rm)
      var <- m2 - m1^2
      sqrt(var * n / (n - 1))
   }
   
   # ---- recode/check treatment to 0/1 ----
   if (length(unique(stats::na.omit(data[[treatment]]))) != 2) {
      stop("The treatment variable must have exactly two (non-missing) levels.")
   }
   
   tr <- data[[treatment]]
   
   if (is.factor(tr) || is.character(tr)) {
      tr_chr <- as.character(tr)
      ok_yesno <- all(stats::na.omit(tr_chr) %in% c("Yes", "No"))
      if (!ok_yesno) {
         stop('Treatment must be coded as 0/1, TRUE/FALSE, or "Yes"/"No".')
      }
      tr01 <- ifelse(tr_chr == "Yes", 1L, 0L)
   } else if (is.logical(tr)) {
      tr01 <- ifelse(tr, 1L, 0L)
   } else if (is.numeric(tr) || is.integer(tr)) {
      ok01 <- all(stats::na.omit(tr) %in% 0:1)
      if (!ok01) {
         stop('Treatment must be coded as 0/1, TRUE/FALSE, or "Yes"/"No".')
      }
      tr01 <- as.integer(tr)
   } else {
      stop('Treatment must be coded as 0/1, TRUE/FALSE, or "Yes"/"No".')
   }
   
   data[[treatment]] <- tr01
   
   n_t <- sum(tr01 == 1, na.rm = TRUE)
   n_c <- sum(tr01 == 0, na.rm = TRUE)
   if (n_t == 0 || n_c == 0) stop("Both treatment and control must have observations.")
   
   # ---- split covariates by type ----
   cov_df <- data[, covariates, drop = FALSE]
   
   is_cat <- vapply(cov_df, function(x) is.factor(x) || is.character(x), logical(1))
   num_vars <- names(cov_df)[!is_cat]
   cat_vars <- names(cov_df)[ is_cat]
   
   out_list <- list()
   
   # ---- numeric vars: means + t-tests ----
   if (length(num_vars) > 0) {
      x_num <- cov_df[, num_vars, drop = FALSE]
      X <- as.matrix(x_num)
      storage.mode(X) <- "numeric"
      
      m_t <- colMeans(X[tr01 == 1, , drop = FALSE], na.rm = TRUE)
      m_c <- colMeans(X[tr01 == 0, , drop = FALSE], na.rm = TRUE)
      sds <- col_sd(X, na.rm = TRUE)
      smd <- (m_t - m_c) / sds
      
      pvals <- rep(NA_real_, length(num_vars))
      for (i in seq_along(num_vars)) {
         v <- X[, i]
         pvals[i] <- tryCatch(
            stats::t.test(v ~ tr01)$p.value,
            error = function(e) NA_real_
         )
      }
      
      out_num <- data.frame(
         variable = num_vars,
         treatment_mean = round(m_t, digits),
         control_mean = round(m_c, digits),
         sd = round(sds, digits),
         scale_mean_diff = round(smd, digits),
         p_val = round(pvals, digits),
         stringsAsFactors = FALSE
      )
      
      out_list[["numeric"]] <- out_num
   }
   
   # ---- categorical vars: level proportions + ONE overall test per factor ----
   if (length(cat_vars) > 0) {
      if (!requireNamespace("fastDummies", quietly = TRUE)) {
         stop("Package `fastDummies` is required for factor/character covariates. Please install it.")
      }
      
      cat_rows <- list()
      
      for (vname in cat_vars) {
         
         v <- cov_df[[vname]]
         if (is.character(v)) v <- factor(v)
         v <- droplevels(v)
         
         # Overall association test: treatment x levels
         tab <- table(tr01, v, useNA = "no")
         
         p_overall <- tryCatch({
            chi <- suppressWarnings(stats::chisq.test(tab, correct = FALSE))
            exp_min <- min(chi$expected)
            if (is.finite(exp_min) && exp_min < 5) {
               stats::fisher.test(tab)$p.value
            } else {
               chi$p.value
            }
         }, error = function(e) NA_real_)
         
         # Level rows via dummy coding (to display group proportions per level)
         tmp <- data.frame(v = v)
         dtmp <- fastDummies::dummy_cols(
            tmp,
            remove_first_dummy = FALSE,
            remove_selected_columns = TRUE
         )
         
         dmat <- as.matrix(dtmp)
         colnames(dmat) <- gsub("^v_", paste0(vname, ":"), colnames(dmat))
         
         lev_m_t <- colMeans(dmat[tr01 == 1, , drop = FALSE], na.rm = TRUE)
         lev_m_c <- colMeans(dmat[tr01 == 0, , drop = FALSE], na.rm = TRUE)
         lev_sd  <- col_sd(dmat, na.rm = TRUE)
         lev_smd <- (lev_m_t - lev_m_c) / lev_sd
         
         lev_df <- data.frame(
            variable = colnames(dmat),
            treatment_mean = round(lev_m_t, digits),
            control_mean = round(lev_m_c, digits),
            sd = round(lev_sd, digits),
            scale_mean_diff = round(lev_smd, digits),
            p_val = NA_real_,
            stringsAsFactors = FALSE
         )
         
         # Put the single overall p-value on the first level row
         if (nrow(lev_df) > 0) lev_df$p_val[1] <- round(p_overall, digits)
         
         cat_rows[[vname]] <- lev_df
      }
      
      out_cat <- do.call(rbind, cat_rows)
      rownames(out_cat) <- NULL
      out_list[["categorical"]] <- out_cat
   }
   
   # ---- combine ----
   if (length(out_list) == 0) stop("No covariates to check.")
   rand.check <- do.call(rbind, out_list)
   rownames(rand.check) <- NULL
   
   # ---- flextable option ----
   if (isTRUE(ft)) {
      if (!requireNamespace("flextable", quietly = TRUE)) {
         stop("Package `flextable` is required when ft = TRUE. Please install it.")
      }
      
      rand.check <- flextable::flextable(rand.check) |>
         flextable::set_header_labels(
            variable = "Variable",
            treatment_mean = "Treatment",
            control_mean = "Control",
            sd = "SD",
            scale_mean_diff = "Scaled Mean Difference",
            p_val = "p-value"
         ) |>
         flextable::add_header_row(
            values = c("Variable", "Mean", "Mean", "SD", "Scaled Mean Difference", "p-value")
         ) |>
         flextable::merge_h(part = "header") |>
         flextable::merge_v(part = "header") |>
         flextable::align(align = "center", part = "header") |>
         flextable::valign(valign = "bottom", part = "header") |>
         flextable::bold(bold = TRUE, part = "header") |>
         flextable::padding(padding.top = 1, padding.bottom = 1, part = "body")
   }
   
   rand.check
}
