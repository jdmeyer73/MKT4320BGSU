#' @title Easy Average Treatment Effect for A/B Testing (Regression-Based)
#' @description
#' Compute the average treatment effect (ATE) for an A/B test using linear
#' regression. Results are shown for (1) a simple model with only the treatment
#' indicator and (2) an adjusted model that includes covariates (i.e., regression
#' adjustment).
#'
#' @details
#' This function requires an existing \code{lm} object that includes the
#' treatment variable and (optionally) any covariates. The "without covariates"
#' model is re-fit internally using the same outcome and data.
#'
#' If the outcome is binary (0/1), the linear model is a linear probability
#' model (LPM). The estimated treatment coefficient is interpreted as an average
#' difference in probability (in percentage points when multiplied by 100).
#'
#' When \code{ft = TRUE}, results are returned as a formatted \code{flextable}
#' comparing models with and without covariates.
#'
#' @param model A fitted linear regression model of class \code{lm}. This model
#'   should include the treatment variable and (optionally) covariates.
#' @param treatment Character string. Name of the treatment variable (in quotes).
#' @param ft Logical; if \code{TRUE} (default) return a \code{flextable}. If
#'   \code{FALSE}, print full regression results to the console and return an
#'   invisible list with model objects and coefficient tables.
#'
#' @return
#' If \code{ft = TRUE}, returns a \code{flextable}. If \code{ft = FALSE}, returns
#' an invisible list:
#' \code{list(model_simple, model_full, coef_simple, coef_full)}.
#'
#' @examples
#' # m <- lm(y ~ promotion + age + income, data = df)
#' # easy_ab_ate(m, treatment = "promotion")
#' # easy_ab_ate(m, treatment = "promotion", ft = FALSE)
#'
#' @importFrom stats lm reformulate pf nobs
#' @importFrom gtsummary tbl_regression add_glance_table tbl_merge
#' @importFrom flextable bold fontsize height hrule padding
#' @importFrom dplyr arrange
#'
#' @export
easy_ab_ate <- function(model, treatment, ft = TRUE) {
   
   # ---- validation ----
   if (!inherits(model, "lm")) {
      stop("`model` must be a linear regression model of class `lm`.")
   }
   if (!is.character(treatment) || length(treatment) != 1) {
      stop("`treatment` must be a single character string (e.g., \"promotion\").")
   }
   
   f_full <- formula(model)
   outcome <- all.vars(f_full)[1]
   dat <- data.frame(model$model)
   
   if (!(treatment %in% names(dat))) {
      stop("`treatment` was not found in the model data. Check the name and ensure it is in the `lm` model.")
   }
   if (!(treatment %in% all.vars(f_full))) {
      stop("`treatment` is not included in the `model` formula.")
   }
   
   # Fit "without covariates" model using same outcome/data
   f_simple <- stats::reformulate(termlabels = treatment, response = outcome)
   model_simple <- stats::lm(f_simple, data = dat)
   
   # Helper: overall model p-value (F-test)
   model_pvalue <- function(m) {
      s <- summary(m)
      fs <- s$fstatistic
      if (is.null(fs)) return(NA_real_)
      stats::pf(unname(fs[1]), unname(fs[2]), unname(fs[3]), lower.tail = FALSE)
   }
   
   # Helper: full coefficient table (all terms)
   coef_table <- function(m) {
      s <- summary(m)
      co <- as.data.frame(s$coefficients, stringsAsFactors = FALSE)
      co$Term <- rownames(co)
      rownames(co) <- NULL
      names(co) <- c("Estimate", "Std.Error", "t.value", "p.value", "Term")
      co <- co[, c("Term", "Estimate", "Std.Error", "t.value", "p.value")]
      co
   }
   
   # ---- flextable output ----
   if (isTRUE(ft)) {
      if (!requireNamespace("gtsummary", quietly = TRUE) ||
          !requireNamespace("flextable", quietly = TRUE) ||
          !requireNamespace("dplyr", quietly = TRUE)) {
         stop("Packages `gtsummary`, `flextable`, and `dplyr` are required for ft = TRUE.")
      }
      
      nocov <- gtsummary::tbl_regression(
         model_simple,
         intercept = TRUE,
         pvalue_fun   = function(x) gtsummary::style_pvalue(x, digits = 3),
         estimate_fun = function(x) gtsummary::style_number(x, digits = 3),
         conf.int = FALSE
      ) |>
         gtsummary::add_glance_table(
            include = c(p.value, r.squared),
            label = list(p.value ~ "p-value", r.squared = "R\u00B2")
         )
      
      cov <- gtsummary::tbl_regression(
         model,
         intercept = TRUE,
         pvalue_fun   = function(x) gtsummary::style_pvalue(x, digits = 3),
         estimate_fun = function(x) gtsummary::style_number(x, digits = 3),
         conf.int = FALSE
      ) |>
         gtsummary::add_glance_table(
            include = c(p.value, r.squared),
            label = list(p.value ~ "p-value", r.squared = "R\u00B2")
         )
      
      comp <- gtsummary::tbl_merge(
         tbls = list(nocov, cov),
         tab_spanner = c("Without\nCovariates", "With\nCovariates")
      ) |>
         gtsummary::modify_table_body(~ .x |> dplyr::arrange(row_type == "glance_statistic")) |>
         gtsummary::as_flex_table() |>
         flextable::bold(bold = TRUE, part = "header") |>
         flextable::fontsize(i = 1, size = 14, part = "header") |>
         flextable::fontsize(size = 12, part = "body") |>
         flextable::height(part = "body", height = 4, unit = "mm") |>
         flextable::hrule(part = "body", rule = "exact") |>
         flextable::padding(padding.top = 1, padding.bottom = 1, part = "body")
      
      return(comp)
   }
   
   # ---- console output: FULL tables ----
   cs <- coef_table(model_simple)
   cf <- coef_table(model)
   
   fmt_num <- function(x, d = 4) round(x, d)
   fmt_p <- function(x) format.pval(x, digits = 3, eps = 0.001)
   
   format_coef <- function(df) {
      df$Estimate  <- fmt_num(df$Estimate, 4)
      df$Std.Error <- fmt_num(df$Std.Error, 4)
      df$t.value   <- fmt_num(df$t.value, 4)
      df$p.value   <- fmt_p(df$p.value)
      df
   }
   
   cs2 <- format_coef(cs)
   cf2 <- format_coef(cf)
   
   cat("\nA/B Test ATE via Linear Regression (lm)\n")
   cat("Outcome:   ", outcome, "\n", sep = "")
   cat("Treatment: ", treatment, "\n\n", sep = "")
   
   cat("--------------------------------------------------\n")
   cat("Without Covariates: ", deparse(f_simple), "\n", sep = "")
   cat("--------------------------------------------------\n")
   print(cs2, row.names = FALSE)
   cat("\nModel fit: N = ", stats::nobs(model_simple),
       ", R^2 = ", round(summary(model_simple)$r.squared, 4),
       ", Model p-value = ", fmt_p(model_pvalue(model_simple)),
       "\n\n", sep = "")
   
   cat("--------------------------------------------------\n")
   cat("With Covariates:    ", deparse(f_full), "\n", sep = "")
   cat("--------------------------------------------------\n")
   print(cf2, row.names = FALSE)
   cat("\nModel fit: N = ", stats::nobs(model),
       ", R^2 = ", round(summary(model)$r.squared, 4),
       ", Model p-value = ", fmt_p(model_pvalue(model)),
       "\n\n", sep = "")
   
   invisible(list(
      model_simple = model_simple,
      model_full   = model,
      coef_simple  = cs,
      coef_full    = cf
   ))
}
