#' @title Evaluate a Standard Multinomial Logit Model
#'
#' @description
#' Evaluate a fitted \code{nnet::multinom} model by computing model-fit statistics,
#' coefficient estimates, and classification diagnostics.
#'
#' @details
#' The function computes:
#' \itemize{
#'   \item Likelihood-ratio test versus an intercept-only model
#'   \item McFadden's pseudo R-squared
#'   \item Coefficient table (log-odds or relative risk ratios) with
#'     standard errors, z-statistics, and p-values
#'   \item Classification matrix and diagnostics on the model data
#'   \item Optional classification on \code{newdata}
#' }
#'
#' Model-fit statistics and in-sample classification rely on the model frame
#' stored in the fitted object. For this reason, the multinomial logit should be
#' estimated with \code{model = TRUE}.
#'
#' Classification uses predicted classes from
#' \code{predict(model, type = "class")}. Accuracy, PCC, and one-vs-rest
#' Sensitivity, Specificity, and Precision are reported.
#'
#' @param model A fitted \code{nnet::multinom} model.
#' @param exp Logical; if \code{TRUE}, return relative risk ratios
#'   (\code{exp(beta)}). If \code{FALSE}, return log-odds coefficients
#'   (default = \code{FALSE}).
#' @param digits Integer; number of decimals used to round coefficient and
#'   model-fit results (default = \code{4}).
#' @param ft Logical; if \code{TRUE}, return coefficient and classification
#'   tables as \code{flextable} objects (default = \code{FALSE}).
#' @param newdata Optional data frame for an additional classification matrix
#'   (e.g., a holdout or test set). If \code{NULL}, only the model-data
#'   classification is produced.
#' @param label_model Character string; label for the model-data classification
#'   output (default = \code{"Model data"}).
#' @param label_newdata Character string; label for the \code{newdata}
#'   classification output (default = \code{"New data"}).
#' @param class_digits Integer; number of decimals used to round classification
#'   statistics (default = \code{3}).
#' @param ... Additional arguments passed to the intercept-only
#'   \code{nnet::multinom()} fit used for the likelihood-ratio test.
#'
#' @return
#' An object of class \code{"eval_std_mnl"}, a list containing:
#' \describe{
#'   \item{mnmodel}{The fitted \code{multinom} model.}
#'   \item{fit}{One-row data frame with LR chi-square, degrees of freedom,
#'     p-value, and McFadden's pseudo R-squared.}
#'   \item{coef_table}{Coefficient table (data frame or flextable).}
#'   \item{coef_table_df}{Coefficient table as a plain data frame (always).}
#'   \item{classify}{List containing classification results for the model data
#'     and, if supplied, \code{newdata}.}
#'   \item{ft}{Logical; whether output tables were requested as flextables.}
#' }
#'
#' @examples
#' \dontrun{
#' mnl_fit <- nnet::multinom(
#'   choice ~ price + promo,
#'   data  = mydata,
#'   model = TRUE,
#'   trace = FALSE
#' )
#'
#' res <- eval_std_mnl(mnl_fit)
#' res$fit
#' res$coef_table
#'
#' # With holdout classification
#' eval_std_mnl(mnl_fit, newdata = testdata)
#' }
#'
#' @export
#'
#' @importFrom nnet multinom
#' @importFrom stats pnorm pchisq update formula predict
#' @importFrom caret confusionMatrix
#' @importFrom flextable flextable autofit add_header_lines add_header_row
eval_std_mnl <- function(model,
                         exp = FALSE,
                         digits = 4,
                         ft = FALSE,
                         newdata = NULL,
                         label_model = "Model data",
                         label_newdata = "New data",
                         class_digits = 3,
                         ...) {
   
   # ----------------------------
   # Validate model
   # ----------------------------
   if (!inherits(model, "multinom")) {
      stop("`model` must be a fitted nnet::multinom object.", call. = FALSE)
   }
   
   mf <- model$model
   if (is.null(mf) || !is.data.frame(mf)) {
      stop("Model frame was not stored in the multinom object. Ask students to fit with model=TRUE.",
           call. = FALSE)
   }
   
   # ----------------------------
   # (A) Model fit stats (LR test, McFadden)
   # ----------------------------
   form <- stats::formula(model)
   null_formula <- stats::update(form, . ~ 1)
   
   null <- nnet::multinom(
      formula = null_formula,
      data = mf,
      trace = FALSE,
      model = TRUE,
      ...
   )
   
   lr_stat <- null$deviance - model$deviance
   df_lr   <- model$edf - null$edf
   lr_p    <- stats::pchisq(lr_stat, df = df_lr, lower.tail = FALSE)
   mcf_r2  <- (null$deviance - model$deviance) / null$deviance
   
   fit <- data.frame(
      LR_Chi2 = round(lr_stat, digits),
      df = df_lr,
      p_value = lr_p,
      McFadden_R2 = round(mcf_r2, digits),
      stringsAsFactors = FALSE
   )
   
   # ----------------------------
   # (B) Coefficient table
   # ----------------------------
   sm <- summary(model)
   coefs <- sm$coefficients
   ses   <- sm$standard.errors
   
   # 2-class edge case (vectors)
   if (is.null(dim(coefs))) {
      outcome_name <- if (!is.null(model$lev) && length(model$lev) >= 2) model$lev[2] else "outcome2"
      coefs <- matrix(coefs, nrow = 1, dimnames = list(outcome_name, names(coefs)))
      ses   <- matrix(ses,   nrow = 1, dimnames = dimnames(coefs))
   }
   
   y_levels <- rownames(coefs)
   terms    <- colnames(coefs)
   
   out_list <- lapply(seq_along(y_levels), function(i) {
      beta <- coefs[i, ]
      se   <- ses[i, ]
      z    <- beta / se
      p    <- 2 * (1 - stats::pnorm(abs(z)))
      
      est <- if (isTRUE(exp)) exp(beta) else beta
      
      data.frame(
         y.level   = y_levels[i],
         term      = terms,
         estimate  = est,
         std.error = se,
         statistic = z,
         p.value   = p,
         stringsAsFactors = FALSE
      )
   })
   
   coef_df <- do.call(rbind, out_list)
   
   est_col <- if (isTRUE(exp)) "RRR" else "logodds"
   names(coef_df)[names(coef_df) == "estimate"] <- est_col
   
   coef_df[[est_col]] <- round(coef_df[[est_col]], digits)
   coef_df$std.error  <- round(coef_df$std.error, digits)
   coef_df$statistic  <- round(coef_df$statistic, digits)
   coef_df$p.value    <- round(coef_df$p.value, digits)
   
   coef_out <- coef_df
   if (isTRUE(ft)) {
      
      p_txt <- if (is.na(fit$p_value)) {
         "NA"
      } else if (fit$p_value < 1e-4) {
         "< 0.0001"
      } else {
         format(round(fit$p_value, 4), nsmall = 4)
      }
      
      line1 <- paste0("LR chi2 (", fit$df, ") = ",
                      format(fit$LR_Chi2, nsmall = 4), "; p ", p_txt)
      line2 <- paste0("McFadden's Pseudo R-square = ",
                      format(fit$McFadden_R2, nsmall = 4))
      
      coef_out <- flextable::flextable(coef_df)
      coef_out <- flextable::add_header_lines(coef_out, values = line2)
      coef_out <- flextable::add_header_lines(coef_out, values = line1)
      coef_out <- flextable::autofit(coef_out)
   }
   
   # ----------------------------
   # (C) Classification helper (model data + optional newdata)
   # ----------------------------
   dv_name <- toString(stats::formula(model)[[2]])
   
   .norm_class_names <- function(x) gsub("^Class:\\s*", "", x)
   
   .get_stat <- function(byc_mat, cls, stat) {
      if (is.null(byc_mat) || is.null(dim(byc_mat))) return(NA_real_)
      rn <- rownames(byc_mat); cn <- colnames(byc_mat)
      if (is.null(rn) || is.null(cn)) return(NA_real_)
      if (!(stat %in% cn)) return(NA_real_)
      i <- match(cls, rn)
      if (is.na(i)) return(NA_real_)
      as.numeric(byc_mat[i, stat])
   }
   
   .fmt <- function(x) {
      if (is.na(x)) "" else format(round(x, class_digits), nsmall = class_digits)
   }
   
   compute_classification <- function(dat, label) {
      
      if (!dv_name %in% names(dat)) {
         stop(sprintf("Outcome variable '%s' not found in provided data.", dv_name), call. = FALSE)
      }
      
      true <- factor(dat[[dv_name]])
      pred <- factor(stats::predict(model, newdata = dat, type = "class"))
      
      levs <- union(levels(true), levels(pred))
      true <- factor(true, levels = levs)
      pred <- factor(pred, levels = levs)
      
      cm <- caret::confusionMatrix(data = pred, reference = true)
      
      acc <- as.numeric(cm$overall["Accuracy"])
      p_k <- as.numeric(table(true)) / length(true)
      pcc <- sum(p_k^2)
      
      byc <- cm$byClass
      if (is.null(dim(byc))) {
         byc <- t(byc)
         rownames(byc) <- levs[2]
      }
      rownames(byc) <- .norm_class_names(rownames(byc))
      
      keep <- intersect(colnames(byc), c("Sensitivity", "Specificity", "Pos Pred Value"))
      byc <- byc[, keep, drop = FALSE]
      colnames(byc) <- c("Sensitivity", "Specificity", "Precision")[match(
         colnames(byc), c("Sensitivity", "Specificity", "Pos Pred Value")
      )]
      byc_round <- round(byc, class_digits)
      
      mat <- as.matrix(cm$table)
      ref_levels <- colnames(mat)
      
      mat_tot <- cbind(mat, Total = rowSums(mat))
      mat_tot <- rbind(mat_tot, Total = c(colSums(mat), sum(mat)))
      
      out <- list(
         label = label,
         table = mat_tot,
         overall = c(Accuracy = round(acc, class_digits), PCC = round(pcc, class_digits)),
         by_class = byc_round
      )
      
      if (!isTRUE(ft)) return(out)
      
      # combined flextable (confusion matrix + by-class rows)
      cm_df <- as.data.frame(mat_tot, stringsAsFactors = FALSE)
      cm_df <- cbind(Predicted = rownames(mat_tot), cm_df)
      rownames(cm_df) <- NULL
      
      stats_rows <- c("Sensitivity", "Specificity", "Precision")
      stats_df <- data.frame(
         Predicted = c("Statistics by Class:", stats_rows),
         stringsAsFactors = FALSE
      )
      
      for (cls in ref_levels) {
         stats_df[[cls]] <- c(
            "",
            .fmt(.get_stat(byc, cls, "Sensitivity")),
            .fmt(.get_stat(byc, cls, "Specificity")),
            .fmt(.get_stat(byc, cls, "Precision"))
         )
      }
      stats_df$Total <- ""
      
      combined_df <- rbind(cm_df, stats_df)
      
      ft_out <- flextable::flextable(combined_df)
      ft_out <- flextable::add_header_row(
         ft_out,
         values = c("", "Reference", ""),
         colwidths = c(1, length(ref_levels), 1)
      )
      ft_out <- flextable::add_header_lines(
         ft_out,
         values = paste0("PCC = ", format(round(pcc, class_digits), nsmall = class_digits))
      )
      ft_out <- flextable::add_header_lines(
         ft_out,
         values = paste0("Accuracy = ", format(round(acc, class_digits), nsmall = class_digits))
      )
      ft_out <- flextable::add_header_lines(
         ft_out,
         values = paste0("Classification Matrix - ", label)
      )
      ft_out <- flextable::autofit(ft_out)
      
      out$table <- ft_out
      out
   }
   
   # Always compute classification on model data (mf)
   class_model <- compute_classification(mf, label_model)
   
   # Optional classification on newdata
   class_new <- NULL
   if (!is.null(newdata)) {
      class_new <- compute_classification(newdata, label_newdata)
   }
   
   classify_res <- invisible(list(model = class_model, newdata = class_new))
   
   # ----------------------------
   # Return object
   # ----------------------------
   res <- list(
      mnmodel = model,
      fit = fit,
      coef_table = coef_out,
      coef_table_df = coef_df,
      classify = classify_res,
      ft = isTRUE(ft)
   )
   class(res) <- "eval_std_mnl"
   res
}

#' @export
print.eval_std_mnl <- function(x, ...) {
   
   # ---- Console fit text ONLY when not using flextables ----
   if (!isTRUE(x$ft)) {
      
      p_txt <- if (is.na(x$fit$p_value)) {
         "NA"
      } else if (x$fit$p_value < 1e-4) {
         "< 0.0001"
      } else {
         format(round(x$fit$p_value, 4), nsmall = 4)
      }
      
      cat(
         "LR chi2 (", x$fit$df, ") = ", format(x$fit$LR_Chi2, nsmall = 4),
         "; p ", p_txt, "\n",
         sep = ""
      )
      cat("McFadden's Pseudo R-square = ",
          format(x$fit$McFadden_R2, nsmall = 4),
          "\n\n", sep = "")
   }
   
   # ---- Then coefficients ----
   if (inherits(x$coef_table, "flextable")) {
      print(x$coef_table, ...)
   } else {
      print(x$coef_table, row.names = FALSE)
   }
   
   # ---- Then classification matrices ----
   if (!is.null(x$classify)) {
      
      print_one_console <- function(res) {
         cat("\n", "Classification Matrix - ", res$label, "\n", sep = "")
         cat(
            "Accuracy = ", format(res$overall["Accuracy"], nsmall = 3),
            "\nPCC = ", format(res$overall["PCC"], nsmall = 3),
            "\n\n", sep = ""
         )
         print(res$table)
         cat("\nStatistics by Class:\n")
         print(res$by_class)
         cat("\n")
      }
      
      m  <- x$classify$model
      nd <- x$classify$newdata
      
      if (inherits(m$table, "flextable")) {
         print(m$table, ...)
         if (!is.null(nd)) print(nd$table, ...)
      } else {
         print_one_console(m)
         if (!is.null(nd)) print_one_console(nd)
      }
   }
   
   invisible(x)
}
