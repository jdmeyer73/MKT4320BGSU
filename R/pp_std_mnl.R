#' @title Predicted Probabilities for Standard Multinomial Logit
#'
#' @description
#' Compute and visualize average predicted probabilities from a fitted
#' standard multinomial logit model for a focal predictor variable.
#'
#' @details
#' Predicted probabilities are computed from a fitted
#' \code{nnet::multinom} model. For a continuous focal variable, predictions
#' are evaluated at representative values (e.g., mean and \eqn{\pm} 1 SD).
#' For a factor focal variable, predictions are computed across factor levels.
#'
#' The function returns both a table of predicted probabilities and a
#' corresponding plot for teaching and interpretation.
#'
#' @param model A fitted \code{nnet::multinom} model.
#' @param focal Character string; name of the focal predictor variable.
#' @param xlab Optional character string; label for the x-axis in the plot.
#' @param ft_table Logical; if \code{TRUE}, return the probability table as a
#'   \code{flextable} (default = \code{FALSE}).
#'
#' @return A list with components:
#' \describe{
#'   \item{table}{Data frame or flextable of predicted probabilities.}
#'   \item{plot}{A \code{ggplot} object showing predicted probabilities.}
#' }
#'
#' @examples
#' \dontrun{
#' model <- nnet::multinom(choice ~ price + promo,
#'                          data = mydata,
#'                          model = TRUE,
#'                          trace = FALSE)
#'
#' res <- pp_std_mnl(model, focal = "price")
#' res$table
#' res$plot
#' }
#'
#' @export
#'
#' @importFrom nnet multinom
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs theme_bw
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate select
#' @importFrom flextable flextable autofit
pp_std_mnl <- function(model,
                       focal,
                       xlab = NULL,
                       ft_table = FALSE) {
   
   # Optional: fail fast with a clear message if these aren't installed.
   # (Avoids using require()/library() in package code.)
   for (pkg in c("effects", "dplyr", "tidyr", "ggplot2", "flextable")) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
         stop("Package '", pkg, "' is required for pp_std_mnl(). Please install it.",
              call. = FALSE)
      }
   }
   
   if (!inherits(model, "multinom")) {
      stop("model must be a fitted nnet::multinom object.", call. = FALSE)
   }
   
   mf <- model$model
   if (is.null(mf) || !is.data.frame(mf)) {
      stop("Model frame not found in model$model. Fit with model=TRUE.", call. = FALSE)
   }
   
   if (!focal %in% names(mf)) {
      stop(sprintf("focal variable '%s' was not found in model$model.", focal),
           call. = FALSE)
   }
   
   dv_name <- toString(stats::formula(model)[[2]])
   cls <- class(mf[[focal]])[1]
   
   # ----------------------------
   # Helper: reshape predictorEffect output
   # ----------------------------
   reshape_effect_df <- function(pred_df) {
      
      pred_df |>
      dplyr::select(
         dplyr::all_of(focal),
         dplyr::matches("^(prob|L\\.prob|U\\.prob)\\.")
      ) |>
      tidyr::pivot_longer(
         cols = -dplyr::all_of(focal),
         names_to = c(".value", dv_name),
         names_pattern = "^(prob|L\\.prob|U\\.prob)\\.(.*)$",
         values_drop_na = TRUE
      ) |>
      dplyr::mutate(
         p.prob   = round(prob, 4),
         lower.CI = round(L.prob, 4),
         upper.CI = round(U.prob, 4)
      ) |>
      dplyr::select(-prob, -L.prob, -U.prob) |>
      dplyr::as_tibble()
   }
   
   # ----------------------------
   # Factor focal
   # ----------------------------
   if (identical(cls, "factor") || isTRUE(is.factor(mf[[focal]]))) {
      
      pred_full <- data.frame(effects::predictorEffect(focal, model))
      tab <- reshape_effect_df(pred_full)
      
      plot <- ggplot2::ggplot(tab, ggplot2::aes_string(x = focal, y = "p.prob", group = 1)) +
         ggplot2::geom_point(size = 2) +
         ggplot2::geom_line() +
         ggplot2::geom_errorbar(ggplot2::aes(ymin = lower.CI, ymax = upper.CI), width = 0.1) +
         ggplot2::facet_wrap(stats::as.formula(paste0("~`", dv_name, "`")), ncol = 2) +
         ggplot2::labs(
            y = "Predicted Probability",
            x = ifelse(is.null(xlab), focal, xlab)
         ) +
         ggplot2::theme_bw()
      
      if (isTRUE(ft_table)) {
         tab <- flextable::flextable(tab) |> flextable::autofit()
      }
      
      return(list(table = tab, plot = plot))
   }
   
   # ----------------------------
   # Numeric / integer focal
   # ----------------------------
   if (cls %in% c("numeric", "integer")) {
      
      fmean <- mean(mf[[focal]], na.rm = TRUE)
      fsd   <- stats::sd(mf[[focal]], na.rm = TRUE)
      xvals <- c(round(fmean - fsd, 0),
                 round(fmean, 0),
                 round(fmean + fsd, 0))
      
      # Plot: full curve
      pred_full <- data.frame(effects::predictorEffect(focal, model))
      plong_full <- reshape_effect_df(pred_full)
      
      plot <- ggplot2::ggplot(
         plong_full,
         ggplot2::aes_string(x = focal, y = "p.prob", color = dv_name, fill = dv_name)
      ) +
         ggplot2::geom_line(linewidth = 1) +
         ggplot2::geom_ribbon(ggplot2::aes(ymin = lower.CI, ymax = upper.CI),
                              alpha = 0.2, colour = NA) +
         ggplot2::theme_bw() +
         ggplot2::theme(legend.position = "bottom") +
         ggplot2::labs(
            y = "Predicted Probability",
            x = ifelse(is.null(xlab), focal, xlab)
         )
      
      # Table: -1 SD, Mean, +1 SD
      pred_3 <- data.frame(
         effects::predictorEffect(focal, model, focal.levels = xvals)
      )
      tab <- reshape_effect_df(pred_3)
      
      if (isTRUE(ft_table)) {
         tab <- flextable::flextable(tab) |> flextable::autofit()
      }
      
      return(list(table = tab, plot = plot))
   }
   
   stop("Focal variable must be factor, numeric, or integer.", call. = FALSE)
}
