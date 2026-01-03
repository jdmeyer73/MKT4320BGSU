#' @title Evaluate a Fitted Alternative-Specific MNL (Fit + Coefs + Classification)
#' @description
#' Given a fitted \code{mlogit} model, compute:
#' (1) model fit stats (LL, AIC, BIC, McFadden R^2, LR test when available);
#' (2) a coefficient table (log-odds + OR) with SE/z/p-values; and
#' (3) classification matrix + diagnostics on the model (training) data automatically,
#' plus optional classification on \code{newdata}.
#'
#' @details
#' Students should fit the model themselves using \code{mlogit::mlogit(..., data = train.mdata)}.
#' The training data used by \code{mlogit} are stored inside the fitted object
#' (as a \code{dfidx} object), so you do not need to pass the original long data,
#' or id/alt/choice variable names, to compute in-sample classification.
#'
#' Classification is computed at the \emph{case} (choice-set) level: for each case,
#' the predicted alternative is the one with the highest predicted probability.
#' One-vs-rest Sensitivity/Specificity/Precision are computed using
#' \code{caret::confusionMatrix()}, matching the approach used in \code{eval_std_mnl()}.
#'
#' For holdout evaluation, pass \code{newdata} as a \code{dfidx} object
#' (recommended: \code{test.mdata} from \code{splitsample()}).
#'
#' @param model A fitted \code{mlogit} model.
#' @param digits Integer; decimals to round coefficient and fit results (default 4).
#' @param ft Logical; if \code{TRUE}, return coefficient and classification tables
#'   as \code{flextable} objects (default \code{FALSE}).
#' @param newdata Optional \code{dfidx} object for an additional classification matrix
#'   (e.g., a test set). If \code{NULL}, only the training-data matrix is produced.
#' @param label_model Character; label for the training-data classification matrix
#'   (default "Model data").
#' @param label_newdata Character; label for the \code{newdata} classification matrix
#'   (default "New data").
#' @param class_digits Integer; decimals to round classification results (default 3).
#'
#' @return An object of class \code{"eval_as_mnl"}: a list with
#' \itemize{
#'   \item \code{mnmodel}: the fitted \code{mlogit} model
#'   \item \code{fit}: one-row data frame with LL, AIC, BIC, McFadden R2, LR chi2, df, p-value (when available)
#'   \item \code{coef_table}: coefficient output (data frame or flextable)
#'   \item \code{coef_table_df}: coefficient table as a plain data frame (always)
#'   \item \code{classify_model}: classification results for model (training) data (matrix/list when ft=FALSE; flextable when ft=TRUE)
#' }
#'
#' @importFrom stats coef vcov pnorm logLik AIC BIC fitted
#' @importFrom caret confusionMatrix
#' @importFrom dfidx idx
#' @importFrom flextable flextable autofit add_header_lines add_header_row
#'
#' @export
eval_as_mnl <- function(model,
                        digits = 4,
                        ft = FALSE,
                        newdata = NULL,
                        label_model = "Model data",
                        label_newdata = "New data",
                        class_digits = 3) {
   
   # ----------------------------
   # Validate model
   # ----------------------------
   if (!inherits(model, "mlogit")) {
      stop("`model` must be a fitted mlogit model (class 'mlogit').", call. = FALSE)
   }
   if (!requireNamespace("dfidx", quietly = TRUE)) {
      stop("Package 'dfidx' is required.", call. = FALSE)
   }
   if (!requireNamespace("mlogit", quietly = TRUE)) {
      stop("Package 'mlogit' is required.", call. = FALSE)
   }
   if (isTRUE(ft) && !requireNamespace("flextable", quietly = TRUE)) {
      stop("Package 'flextable' is required when ft=TRUE.", call. = FALSE)
   }
   if (!requireNamespace("caret", quietly = TRUE)) {
      stop("Package 'caret' is required.", call. = FALSE)
   }
   
   fmt <- function(x, d) format(round(x, d), nsmall = d)
   
   # ----------------------------
   # (A) Model fit stats
   # ----------------------------
   sm <- summary(model)
   
   ll  <- as.numeric(stats::logLik(model))
   aic <- stats::AIC(model)
   bic <- stats::BIC(model)
   
   mfR2 <- tryCatch(sm$mfR2[1], error = function(e) NA_real_)
   lr_stat <- tryCatch(sm$lratio$statistic[1], error = function(e) NA_real_)
   lr_df   <- tryCatch(sm$lratio$parameter[1], error = function(e) NA_real_)
   lr_p    <- tryCatch(sm$lratio$p.value, error = function(e) NA_real_)
   
   fit <- data.frame(
      LogLik = round(ll, digits),
      AIC = round(aic, digits),
      BIC = round(bic, digits),
      McFadden_R2 = round(mfR2, digits),
      LR_Chi2 = round(lr_stat, digits),
      df = lr_df,
      p_value = lr_p,
      stringsAsFactors = FALSE
   )
   
   # ----------------------------
   # (B) Coefficient table (log-odds, OR, SE, z, p)
   # ----------------------------
   beta <- stats::coef(model)
   vc <- tryCatch(stats::vcov(model), error = function(e) NULL)
   
   if (is.null(vc)) {
      se <- rep(NA_real_, length(beta)); names(se) <- names(beta)
   } else {
      se <- sqrt(diag(vc))
      se <- se[names(beta)]  # align
   }
   
   z <- beta / se
   p <- 2 * stats::pnorm(abs(z), lower.tail = FALSE)
   
   coef_df <- data.frame(
      term      = names(beta),
      logodds   = as.numeric(beta),
      OR        = exp(as.numeric(beta)),
      std.error = as.numeric(se),
      statistic = as.numeric(z),
      p.value   = as.numeric(p),
      stringsAsFactors = FALSE
   )
   
   coef_df$logodds   <- round(coef_df$logodds, digits)
   coef_df$OR        <- round(coef_df$OR, digits)
   coef_df$std.error <- round(coef_df$std.error, digits)
   coef_df$statistic <- round(coef_df$statistic, digits)
   coef_df$p.value   <- round(coef_df$p.value, digits)
   
   coef_out <- coef_df
   if (isTRUE(ft)) {
      p_txt <- if (is.na(fit$p_value)) {
         "NA"
      } else if (fit$p_value < 1e-4) {
         "< 0.0001"
      } else {
         format(round(fit$p_value, 4), nsmall = 4)
      }
      
      line1 <- paste0(
         "LR chi2 (", fit$df, ") = ",
         format(fit$LR_Chi2, nsmall = 4), "; p ", p_txt
      )
      line2 <- paste0("McFadden's Pseudo R-square = ",
                      format(fit$McFadden_R2, nsmall = 4))
      
      coef_out <- flextable::flextable(coef_df)
      coef_out <- flextable::add_header_lines(coef_out, values = line2)
      coef_out <- flextable::add_header_lines(coef_out, values = line1)
      coef_out <- flextable::autofit(coef_out)
   }
   
   # ----------------------------
   # (C) Classification helpers (case-level) + by-class stats
   # ----------------------------
   as_chosen_logical <- function(x) {
      if (is.logical(x)) return(x)
      if (is.numeric(x) || is.integer(x)) return(!is.na(x) & x == 1)
      xx <- tolower(trimws(as.character(x)))
      yes_vals <- c("yes","y","true","t","1","chosen","choice","select","selected")
      out <- xx %in% yes_vals
      out[is.na(xx) | xx == ""] <- FALSE
      out
   }
   
   get_choice_colname <- function(dfidx_obj) {
      ch <- attr(dfidx_obj, "choice")
      if (!is.null(ch) && length(ch) == 1 && is.character(ch) && ch %in% names(dfidx_obj)) return(ch)
      if ("choice" %in% names(dfidx_obj)) return("choice")
      names(dfidx_obj)[1]
   }
   
   get_case_alt <- function(dfidx_obj) {
      idx <- dfidx::idx(dfidx_obj)
      if (is.null(idx) || ncol(idx) < 2) stop("Could not read dfidx index (case/alt).", call. = FALSE)
      list(case = as.character(idx[, 1]),
           alt  = as.character(idx[, 2]))
   }
   
   row_prob_from_output <- function(dfidx_obj, probs) {
      ca <- get_case_alt(dfidx_obj)
      n  <- length(ca$case)
      
      # Case-level matrix: rows=cases, cols=alts
      if (is.matrix(probs) && nrow(probs) != n) {
         if (is.null(colnames(probs))) stop("Probability matrix has no column names (alts).", call. = FALSE)
         rn <- rownames(probs)
         if (is.null(rn)) stop("Probability matrix has no row names (cases).", call. = FALSE)
         
         r <- match(ca$case, rn)
         c <- match(ca$alt, colnames(probs))
         if (anyNA(r) || anyNA(c)) stop("Could not align probability matrix to dfidx case/alt.", call. = FALSE)
         return(as.numeric(probs[cbind(r, c)]))
      }
      
      # Long-row matrix
      if (is.matrix(probs) && nrow(probs) == n) {
         if (ncol(probs) == 1) return(as.numeric(probs[, 1]))
         if (!is.null(colnames(probs)) && all(ca$alt %in% colnames(probs))) {
            return(as.numeric(probs[cbind(seq_len(n), match(ca$alt, colnames(probs)))]))
         }
         stop("Unrecognized long-row probability matrix shape.", call. = FALSE)
      }
      
      # Long-row vector
      if (is.vector(probs) && length(probs) == n) {
         return(as.numeric(probs))
      }
      
      stop("Unrecognized probability output from mlogit.", call. = FALSE)
   }
   
   case_level_pred_actual <- function(dfidx_obj, probs) {
      ca <- get_case_alt(dfidx_obj)
      choice_col <- get_choice_colname(dfidx_obj)
      y <- as_chosen_logical(dfidx_obj[[choice_col]])
      p_row <- row_prob_from_output(dfidx_obj, probs)
      
      cases <- unique(ca$case)  # preserve order
      
      actual <- vapply(cases, function(cs) {
         ii <- which(ca$case == cs)
         jj <- ii[which(y[ii])]
         if (length(jj) == 0) return(NA_character_)
         ca$alt[jj[1]]
      }, character(1))
      
      pred <- vapply(cases, function(cs) {
         ii <- which(ca$case == cs)
         ca$alt[ii[which.max(p_row[ii])]]
      }, character(1))
      
      data.frame(case = cases, pred = pred, actual = actual, stringsAsFactors = FALSE)
   }
   
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
   
   .fmt_class <- function(x) {
      if (is.na(x)) "" else format(round(x, class_digits), nsmall = class_digits)
   }
   
   compute_classification_dfidx <- function(dfidx_obj, label) {
      # probabilities for this data
      probs <- tryCatch(
         if (identical(dfidx_obj, model$model)) {
            # training: prefer stored probabilities if present
            if (!is.null(model$probabilities)) model$probabilities else stats::fitted(model, type = "probabilities")
         } else {
            predict(model, newdata = dfidx_obj, type = "probabilities")
         },
         error = function(e) NULL
      )
      if (is.null(probs)) stop("Could not obtain predicted probabilities for classification.", call. = FALSE)
      
      pa <- case_level_pred_actual(dfidx_obj, probs)
      pa <- pa[!is.na(pa$actual), , drop = FALSE]
      
      true <- factor(pa$actual)
      pred <- factor(pa$pred)
      
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
      
      # flextable combined layout (same structure as eval_std_mnl)
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
            .fmt_class(.get_stat(byc, cls, "Sensitivity")),
            .fmt_class(.get_stat(byc, cls, "Specificity")),
            .fmt_class(.get_stat(byc, cls, "Precision"))
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
   
   # Training dfidx stored in model$model
   train_dfidx <- model$model
   if (is.null(train_dfidx) || !inherits(train_dfidx, "dfidx")) {
      stop("Training dfidx data not found in model$model.", call. = FALSE)
   }
   
   class_model <- compute_classification_dfidx(train_dfidx, label_model)
   
   class_new <- NULL
   if (!is.null(newdata)) {
      if (!inherits(newdata, "dfidx")) {
         stop("`newdata` must be a dfidx object (e.g., `test.mdata`).", call. = FALSE)
      }
      class_new <- compute_classification_dfidx(newdata, label_newdata)
   }
   
   classify_res <- invisible(list(model = class_model, newdata = class_new))
   
   # For knitting/bookdown: return flextables directly (not nested inside a list)
   classify_model_out <- if (isTRUE(ft)) class_model$table else class_model
   classify_newdata_out <- NULL
   if (!is.null(class_new)) {
      classify_newdata_out <- if (isTRUE(ft)) class_new$table else class_new
   }
   
   # ----------------------------
   # Return object
   # ----------------------------
   res <- list(
      mnmodel = model,
      fit = fit,
      coef_table = coef_out,
      coef_table_df = coef_df,
      classify_model = classify_model_out,
      classify_newdata = classify_newdata_out
   )
   class(res) <- "eval_as_mnl"
   res
}

#' @export
print.eval_as_mnl <- function(x, ...) {
   
   # If ft=TRUE, LR chi2 + McFadden R2 are already embedded as header lines
   # in the coefficient flextable, so avoid printing them again to console.
   if (!inherits(x$coef_table, "flextable")) {
      
      p_txt <- if (is.na(x$fit$p_value)) {
         "NA"
      } else if (x$fit$p_value < 1e-4) {
         "< 0.0001"
      } else {
         format(round(x$fit$p_value, 4), nsmall = 4)
      }
      
      cat(
         "LR chi2 (", x$fit$df, ") = ",
         format(x$fit$LR_Chi2, nsmall = 4),
         "; p ", p_txt, "\n",
         sep = ""
      )
      cat(
         "McFadden's Pseudo R-square = ",
         format(x$fit$McFadden_R2, nsmall = 4),
         "\n\n",
         sep = ""
      )
   }
   
   # ---- Coefficients ----
   if (inherits(x$coef_table, "flextable")) {
      print(x$coef_table, ...)
   } else {
      print(x$coef_table, row.names = FALSE)
   }
   
   # ---- Classification matrices ----
   # IMPORTANT:
   # Only print classification results to the console when ft = FALSE.
   if (!inherits(x$coef_table, "flextable") && (!is.null(x$classify_model) || !is.null(x$classify_newdata))) {
      
      print_one_console <- function(res) {
         cat("\n", "Classification Matrix - ", res$label, "\n", sep = "")
         cat(
            "Accuracy = ", format(res$overall["Accuracy"], nsmall = 3),
            "\nPCC = ", format(res$overall["PCC"], nsmall = 3),
            "\n\n",
            sep = ""
         )
         print(res$table)
         cat("\nStatistics by Class:\n")
         print(res$by_class)
         cat("\n")
      }
      
      m <- x$classify_model
      nd <- x$classify_newdata
      
      print_one_console(m)
      if (!is.null(nd)) print_one_console(nd)
   }
   
   invisible(x)
}

