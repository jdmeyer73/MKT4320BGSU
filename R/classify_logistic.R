#' @title Classification Matrix for Binary Logistic Regression
#' @description
#' Create a classification matrix and selected statistics for a fitted
#' binary logistic regression model. Optionally, produce results for a
#' second data set (e.g., test/holdout sample) using the same model and
#' cutoff.
#'
#' @details
#' Predictions are based on a specified cutoff. When \code{ft = FALSE},
#' the function prints a full console report including accuracy, PCC,
#' the classification matrix (with totals), and selected statistics for
#' the positive class. When \code{ft = TRUE}, a single combined flextable
#' is returned with identical content and layout.
#'
#' @param MOD A fitted binary logistic regression \code{glm} object
#'   (i.e., \code{family = "binomial"}).
#' @param DATA A data frame for which the classification matrix should be produced.
#' @param POSITIVE The level representing the positive outcome.
#' @param CUTOFF Probability cutoff for classification (default = 0.5).
#' @param DATA2 Optional second data frame (e.g., test/holdout sample).
#' @param LABEL1 Label for the first data set.
#' @param LABEL2 Label for the second data set (if provided).
#' @param digits Number of decimal places (default = 3).
#' @param ft Logical; if TRUE, return a combined flextable (default = FALSE).
#'
#' @return Invisibly returns a list with \code{sample1} and (optionally) \code{sample2}.
#'
#' @importFrom caret confusionMatrix
#' @importFrom stats predict family formula
#' @importFrom flextable flextable autofit add_header_lines add_header_row
#' @export
classify_logistic <- function(MOD, DATA, POSITIVE, CUTOFF = 0.5,
                              DATA2  = NULL,
                              LABEL1 = "Sample 1",
                              LABEL2 = "Sample 2",
                              digits = 3,
                              ft = FALSE) {
   
   # ---- checks ----
   if (!inherits(MOD, "glm")) {
      stop("MOD must be a fitted glm() object for binary logistic regression.", call. = FALSE)
   }
   if (!identical(stats::family(MOD)$family, "binomial")) {
      stop("MOD must be a glm() with family = 'binomial'.", call. = FALSE)
   }
   if (!is.data.frame(DATA)) stop("DATA must be a data frame.", call. = FALSE)
   if (!is.null(DATA2) && !is.data.frame(DATA2)) stop("If provided, DATA2 must be a data frame.", call. = FALSE)
   
   dv_name <- toString(stats::formula(MOD)[[2]])
   
   .fmt <- function(x) if (is.na(x)) "" else format(round(x, digits), nsmall = digits)
   
   compute_one <- function(dat, label) {
      
      if (!dv_name %in% names(dat)) {
         stop(sprintf("Outcome variable '%s' not found in provided data.", dv_name), call. = FALSE)
      }
      
      true <- dat[[dv_name]]
      
      # Normalize outcome to factor
      if (is.numeric(true)) {
         if (!all(true %in% c(0, 1))) stop("Numeric outcome must be coded 0/1.", call. = FALSE)
         true <- factor(true, levels = c(0, 1))
      } else {
         true <- factor(true)
      }
      
      POS <- if (is.numeric(POSITIVE)) as.character(POSITIVE) else POSITIVE
      
      if (!(POS %in% levels(true))) {
         stop(sprintf("POSITIVE level '%s' not found in outcome levels: %s",
                      POS, paste(levels(true), collapse = ", ")), call. = FALSE)
      }
      
      other_levels <- setdiff(levels(true), POS)
      if (length(other_levels) != 1L) stop("Outcome must be binary (exactly two levels).", call. = FALSE)
      NEG <- other_levels
      
      # Predictions
      prob <- stats::predict(MOD, newdata = dat, type = "response")
      pred_class <- ifelse(prob >= CUTOFF, POS, NEG)
      
      pred <- factor(pred_class, levels = c(NEG, POS))
      true <- factor(true,      levels = c(NEG, POS))
      
      cm_obj <- caret::confusionMatrix(data = pred, reference = true, positive = POS)
      
      # Base 2x2 table: rows = Predicted, cols = Reference
      mat <- as.matrix(cm_obj$table)
      
      # Add totals
      mat_tot <- cbind(mat, Total = rowSums(mat))
      mat_tot <- rbind(mat_tot, Total = c(colSums(mat), sum(mat)))
      
      # Overall stats
      acc <- as.numeric(cm_obj$overall["Accuracy"])
      tab_true <- table(true)
      p_pos <- as.numeric(tab_true[POS]) / length(true)
      pcc <- p_pos^2 + (1 - p_pos)^2
      
      # Positive-class selected stats from caret
      sens <- as.numeric(cm_obj$byClass["Sensitivity"])
      spec <- as.numeric(cm_obj$byClass["Specificity"])
      prec <- as.numeric(cm_obj$byClass["Pos Pred Value"])
      
      res <- list(
         label  = label,
         cutoff = CUTOFF,
         table  = mat_tot,
         overall = c(Accuracy = round(acc, digits), PCC = round(pcc, digits)),
         stats_pos = c(
            Sensitivity = round(sens, digits),
            Specificity = round(spec, digits),
            Precision   = round(prec, digits)
         )
      )
      
      if (!isTRUE(ft)) return(res)
      
      # ---- combined flextable ----
      cm_df <- as.data.frame(mat_tot, stringsAsFactors = FALSE)
      cm_df <- cbind(Predicted = rownames(mat_tot), cm_df)
      rownames(cm_df) <- NULL
      
      stats_df <- data.frame(
         Predicted = c("Selected Statistics (Positive Class):", "Sensitivity", "Specificity", "Precision"),
         stringsAsFactors = FALSE
      )
      # Reference columns are NEG and POS
      stats_df[[NEG]] <- c("", .fmt(sens), .fmt(spec), .fmt(prec))
      stats_df[[POS]] <- c("", "", "", "")  # keep layout; stats are for POS only
      stats_df$Total  <- ""
      
      combined_df <- rbind(cm_df, stats_df)
      
      ft_out <- flextable::flextable(combined_df)
      
      # Spanning "Reference" header over the two reference columns
      ft_out <- flextable::add_header_row(
         ft_out,
         values = c("", "Reference", ""),
         colwidths = c(1, 2, 1)
      )
      
      # Header lines: add in reverse order so Title ends up on top
      ft_out <- flextable::add_header_lines(
         ft_out,
         values = paste0("PCC = ", format(round(pcc, digits), nsmall = digits))
      )
      ft_out <- flextable::add_header_lines(
         ft_out,
         values = paste0("Accuracy = ", format(round(acc, digits), nsmall = digits))
      )
      ft_out <- flextable::add_header_lines(
         ft_out,
         values = paste0("Classification Matrix - ", label, sprintf(" (Cutoff = %.2f)", CUTOFF))
      )
      
      ft_out <- flextable::autofit(ft_out)
      
      res$table <- ft_out
      res
   }
   
   res1 <- compute_one(DATA, LABEL1)
   res2 <- if (!is.null(DATA2)) compute_one(DATA2, LABEL2) else NULL
   
   # ---- printing ----
   print_console <- function(res) {
      cat("\n", "Classification Matrix - ", res$label,
          sprintf(" (Cutoff = %.2f)\n", res$cutoff), sep = "")
      cat("Accuracy = ", format(res$overall["Accuracy"], nsmall = digits),
          "\nPCC = ", format(res$overall["PCC"], nsmall = digits),
          "\n\n", sep = "")
      print(res$table)
      cat("\nSelected Statistics (Positive Class):\n")
      cat(sprintf("  Sensitivity (TPR): %.3f\n", res$stats_pos["Sensitivity"]))
      cat(sprintf("  Specificity (TNR): %.3f\n", res$stats_pos["Specificity"]))
      cat(sprintf("  Precision (PPV):   %.3f\n", res$stats_pos["Precision"]))
      cat("\n")
   }
   
   if (isTRUE(ft)) {
      print(res1$table)
      if (!is.null(res2)) print(res2$table)
   } else {
      print_console(res1)
      if (!is.null(res2)) print_console(res2)
   }
   
   invisible(list(sample1 = res1, sample2 = res2))
}
