#' @title Cutoff Diagnostics for Binary Logistic Regression
#' @description
#' Plot sensitivity, specificity, and accuracy across classification cutoffs
#' for a fitted binary logistic regression model. Can be used for a single
#' data set (e.g., training) or for two data sets (e.g., training and test)
#' using the same fitted model.
#'
#' @details
#' The outcome variable MUST be a factor with exactly two levels, and
#' \code{POSITIVE} must match one of those levels.
#'
#' For each cutoff from 0 to 1 in steps of 0.01, the function computes:
#' \itemize{
#'   \item Sensitivity (True Positive Rate)
#'   \item Specificity (True Negative Rate)
#'   \item Accuracy
#' }
#' and produces a ggplot line chart with a consistent visual style
#' (theme_bw, legend at bottom) to help choose a reasonable cutoff.
#'
#' @param MOD Fitted binary logistic regression model (\code{glm} with
#'   \code{family = "binomial"}).
#' @param DATA Data frame for which cutoff diagnostics are computed
#'   (e.g., training data).
#' @param POSITIVE Factor level representing the positive / "success" class.
#' @param LABEL1 Character label for \code{DATA} used in the plot title.
#'   Defaults to \code{"Sample 1"}.
#' @param DATA2 Optional second data frame (e.g., test/holdout data) for
#'   which a second cutoff diagnostics plot is produced. Defaults to
#'   \code{NULL} (no second data set).
#' @param LABEL2 Character label for \code{DATA2} used in the second plot
#'   title. Defaults to \code{"Sample 2"}.
#'
#' @return
#' If only \code{DATA} is provided, returns a single \code{ggplot} object
#' (and prints it).
#'
#' If both \code{DATA} and \code{DATA2} are provided, returns a list with:
#' \itemize{
#'   \item \code{$sample1}: ggplot for \code{DATA} (label \code{LABEL1})
#'   \item \code{$sample2}: ggplot for \code{DATA2} (label \code{LABEL2})
#' }
#' and prints both plots.
#'
#' @importFrom stats predict family formula
#' @importFrom ggplot2 ggplot aes geom_line scale_x_continuous scale_y_continuous
#' @importFrom ggplot2 scale_color_manual labs theme_bw theme element_blank
cutoff_logistic <- function(MOD,
                            DATA,
                            POSITIVE,
                            LABEL1 = "Sample 1",
                            DATA2  = NULL,
                            LABEL2 = "Sample 2") {
   
   # ---- basic checks ----
   
   if (!inherits(MOD, "glm")) {
      stop("MOD must be a fitted glm() object for binary logistic regression.",
           call. = FALSE)
   }
   
   fam <- stats::family(MOD)$family
   if (!identical(fam, "binomial")) {
      stop("MOD must be a glm() with family = 'binomial'.",
           call. = FALSE)
   }
   
   if (!is.data.frame(DATA)) {
      stop("DATA must be a data frame.", call. = FALSE)
   }
   
   if (!is.null(DATA2) && !is.data.frame(DATA2)) {
      stop("If provided, DATA2 must be a data frame.", call. = FALSE)
   }
   
   # Outcome variable name from model formula
   dv_name <- toString(stats::formula(MOD)[[2]])
   
   # ---- internal helper to check factor outcome ----
   
   check_factor_outcome <- function(dat, label) {
      if (!dv_name %in% names(dat)) {
         stop(sprintf("Outcome variable '%s' not found in %s.",
                      dv_name, label),
              call. = FALSE)
      }
      if (!is.factor(dat[[dv_name]])) {
         stop(
            paste0(
               "The outcome variable '", dv_name, "' in ", label,
               " must be a FACTOR.\n",
               "Convert it first, e.g.:\n",
               "  ", label, "$", dv_name,
               " <- factor(", label, "$", dv_name, ", levels = c('no','yes'))\n"
            ),
            call. = FALSE
         )
      }
   }
   
   check_factor_outcome(DATA, "DATA")
   if (!is.null(DATA2)) {
      check_factor_outcome(DATA2, "DATA2")
   }
   
   POSITIVE <- as.character(POSITIVE)
   
   # ---- internal helper: compute metrics over cutoffs ----
   
   compute_curves <- function(dat, label) {
      
      y <- dat[[dv_name]]
      if (!(POSITIVE %in% levels(y))) {
         stop(
            sprintf("POSITIVE = '%s' is not a factor level of '%s' in %s.",
                    POSITIVE, dv_name, label),
            call. = FALSE
         )
      }
      
      true_pos <- (y == POSITIVE)
      n <- length(true_pos)
      
      p_hat <- stats::predict(MOD, newdata = dat, type = "response")
      
      cutoffs <- seq(0, 1, by = 0.01)
      
      out_list <- lapply(cutoffs, function(co) {
         pred_pos <- (p_hat >= co)
         
         TP <- sum(pred_pos & true_pos)
         FN <- sum(!pred_pos & true_pos)
         TN <- sum(!pred_pos & !true_pos)
         FP <- sum(pred_pos & !true_pos)
         
         sens <- if ((TP + FN) > 0) TP / (TP + FN) else NA_real_
         spec <- if ((TN + FP) > 0) TN / (TN + FP) else NA_real_
         acc  <- (TP + TN) / n
         
         data.frame(
            cutoff   = co,
            measure  = c("Accuracy", "Sensitivity", "Specificity"),
            value    = c(acc, sens, spec),
            dataset  = label,
            stringsAsFactors = FALSE
         )
      })
      
      do.call(rbind, out_list)
   }
   
   # ---- compute curves ----
   
   df1 <- compute_curves(DATA, LABEL1)
   
   # ---- plotting helper ----
   
   make_plot <- function(df, label) {
      
      df$measure <- factor(df$measure,
                           levels = c("Accuracy", "Sensitivity", "Specificity"))
      
      ggplot2::ggplot(df, ggplot2::aes(x = cutoff, y = value, color = measure)) +
         ggplot2::geom_line(linewidth = 1) +
         ggplot2::scale_x_continuous(
            "Cutoff Value",
            limits = c(0, 1),
            breaks = seq(0, 1, 0.1)
         ) +
         ggplot2::scale_y_continuous(
            "Measure Value",
            limits = c(0, 1),
            breaks = seq(0, 1, 0.1)
         ) +
         ggplot2::scale_color_manual(
            "",
            values = c(
               "Accuracy"    = "red4",
               "Sensitivity" = "forestgreen",
               "Specificity" = "blue"
            )
         ) +
         ggplot2::labs(
            title = paste0("Cutoff Diagnostics for ", label)
         ) +
         ggplot2::theme_bw() +
         ggplot2::theme(
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.minor   = ggplot2::element_blank(),
            legend.position    = "bottom"
         )
   }
   
   p1 <- make_plot(df1, LABEL1)
   
   if (is.null(DATA2)) {
      print(p1)
      return(p1)
   }
   
   df2 <- compute_curves(DATA2, LABEL2)
   p2  <- make_plot(df2, LABEL2)
   
   print(p1)
   print(p2)
   
   return(list(sample1 = p1, sample2 = p2))
}
