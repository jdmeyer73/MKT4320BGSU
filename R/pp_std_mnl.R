#' @title Predicted Probabilities for Standard Multinomial Logit
#'
#' @description
#' Compute and visualize average predicted probabilities from a fitted
#' standard multinomial logit model for a focal predictor variable.
#' Optionally, predicted probabilities may also be computed across levels
#' of a factor interaction variable.
#'
#' @details
#' Predicted probabilities are computed from a fitted
#' \code{nnet::multinom} model using the \code{effects} package.
#'
#' For a numeric focal predictor, probabilities are evaluated across the
#' observed range of the focal variable to produce a smooth prediction curve.
#' A compact summary table is also returned using three representative values:
#' mean, one standard deviation below the mean, and one standard deviation
#' above the mean.
#'
#' For a factor focal predictor, probabilities are computed across all levels
#' of the factor.
#'
#' If an interaction variable is supplied, predicted probabilities are
#' evaluated across focal values separately for each interaction level.
#' Currently, interaction variables must be factors, and interaction mode
#' is only supported when the focal predictor is numeric.
#'
#' Confidence intervals shown in the plot are pointwise confidence intervals
#' returned by \code{effects}.
#'
#' The function returns both a probability table and a corresponding plot
#' intended for teaching and interpretation of multinomial logit effects.
#'
#' @param model A fitted \code{nnet::multinom} model.
#' @param focal Character string giving the name of the focal predictor variable.
#' @param interaction Optional character string giving the name of a factor
#'   interaction variable.
#' @param xlab Optional character string used as the x-axis label in the plot.
#'   If omitted, the focal variable name is used.
#' @param ft_table Logical; if \code{TRUE}, return the probability table as a
#'   \code{flextable}. If \code{FALSE}, return a tibble.
#'
#' @return A list with components:
#' \describe{
#'   \item{table}{A tibble or flextable containing predicted probabilities and
#'   confidence intervals.}
#'   \item{plot}{A \code{ggplot} object showing predicted probabilities.}
#' }
#'
#' @examples
#' \dontrun{
#' model <- nnet::multinom(choice ~ price + coupon + price:coupon,
#'                         data = mydata,
#'                         model = TRUE,
#'                         trace = FALSE)
#'
#' # focal predictor only
#' res1 <- pp_std_mnl(model, focal = "price")
#' res1$table
#' res1$plot
#'
#' # focal predictor with interaction
#' res2 <- pp_std_mnl(model, focal = "price", interaction = "coupon")
#' res2$table
#' res2$plot
#' }
#'
#' @export
#'
#' @importFrom nnet multinom
#' @importFrom effects predictorEffect Effect
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_errorbar geom_ribbon
#' @importFrom ggplot2 labs theme_bw theme facet_wrap
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate select all_of matches as_tibble
#' @importFrom flextable flextable autofit

pp_std_mnl <- function(model,
                       focal,
                       interaction = NULL,
                       xlab = NULL,
                       ft_table = TRUE) {
   
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
   
   reshape_effect_df <- function(pred_df, vars) {
      
      pred_df |>
         dplyr::select(
            dplyr::all_of(vars),
            dplyr::matches("^(prob|L\\.prob|U\\.prob)\\.")
         ) |>
         tidyr::pivot_longer(
            cols = -dplyr::all_of(vars),
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
   # INTERACTION CASE
   # ----------------------------
   if (!is.null(interaction)) {
      
      if (!interaction %in% names(mf)) {
         stop(sprintf("interaction variable '%s' was not found in model$model.", interaction),
              call. = FALSE)
      }
      
      if (!is.factor(mf[[interaction]])) {
         stop("interaction variable must currently be a factor.", call. = FALSE)
      }
      
      if (!(cls %in% c("numeric", "integer"))) {
         stop("Interaction mode currently requires a numeric focal variable.",
              call. = FALSE)
      }
      
      fmean <- mean(mf[[focal]], na.rm = TRUE)
      fsd   <- stats::sd(mf[[focal]], na.rm = TRUE)
      xvals <- c(round(fmean - fsd, 2),
                 round(fmean, 2),
                 round(fmean + fsd, 2))
      
      # Full curve for plot
      xseq <- seq(
         min(mf[[focal]], na.rm = TRUE),
         max(mf[[focal]], na.rm = TRUE),
         length.out = 100
      )
      
      pred_full <- data.frame(
         effects::Effect(
            focal.predictors = c(focal, interaction),
            mod = model,
            xlevels = stats::setNames(list(xseq), focal)
         )
      )
      
      plot_tab <- reshape_effect_df(pred_full, c(focal, interaction))
      
      plot <- ggplot2::ggplot(
         plot_tab,
         ggplot2::aes(
            x = .data[[focal]],
            y = p.prob,
            color = .data[[interaction]],
            fill = .data[[interaction]]
         )
      ) +
         ggplot2::geom_line(linewidth = 1) +
         ggplot2::geom_ribbon(
            ggplot2::aes(ymin = lower.CI, ymax = upper.CI),
            alpha = 0.15,
            colour = NA
         ) +
         ggplot2::facet_wrap(
            stats::as.formula(paste0("~`", dv_name, "`"))
         ) +
         ggplot2::theme_bw() +
         ggplot2::theme(legend.position = "bottom") +
         ggplot2::labs(
            y = "Predicted Probability",
            x = ifelse(is.null(xlab), focal, xlab),
            color = interaction,
            fill = interaction
         )
      
      # Compact table at -1 SD, mean, +1 SD
      pred_3 <- data.frame(
         effects::Effect(
            focal.predictors = c(focal, interaction),
            mod = model,
            xlevels = stats::setNames(list(xvals), focal)
         )
      )
      
      tab <- reshape_effect_df(pred_3, c(focal, interaction))
      
      if (isTRUE(ft_table)) {
         tab <- flextable::flextable(tab) |>
            flextable::autofit()
      }
      
      return(list(table = tab, plot = plot))
   }
   
   # ----------------------------
   # FACTOR FOCAL
   # ----------------------------
   if (identical(cls, "factor") || isTRUE(is.factor(mf[[focal]]))) {
      
      pred_full <- data.frame(effects::predictorEffect(focal, model))
      tab <- reshape_effect_df(pred_full, focal)
      
      plot <- ggplot2::ggplot(
         tab,
         ggplot2::aes(x = .data[[focal]], y = p.prob, group = 1)
      ) +
         ggplot2::geom_point(size = 2) +
         ggplot2::geom_line() +
         ggplot2::geom_errorbar(
            ggplot2::aes(ymin = lower.CI, ymax = upper.CI),
            width = 0.1
         ) +
         ggplot2::facet_wrap(
            stats::as.formula(paste0("~`", dv_name, "`")),
            ncol = 2
         ) +
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
   # NUMERIC / INTEGER FOCAL
   # ----------------------------
   if (cls %in% c("numeric", "integer")) {
      
      fmean <- mean(mf[[focal]], na.rm = TRUE)
      fsd   <- stats::sd(mf[[focal]], na.rm = TRUE)
      xvals <- c(round(fmean - fsd, 2),
                 round(fmean, 2),
                 round(fmean + fsd, 2))
      
      pred_full <- data.frame(effects::predictorEffect(focal, model))
      plong_full <- reshape_effect_df(pred_full, focal)
      
      plot <- ggplot2::ggplot(
         plong_full,
         ggplot2::aes(
            x = .data[[focal]],
            y = p.prob,
            color = .data[[dv_name]],
            fill  = .data[[dv_name]]
         )
      ) +
         ggplot2::geom_line(linewidth = 1) +
         ggplot2::geom_ribbon(
            ggplot2::aes(ymin = lower.CI, ymax = upper.CI),
            alpha = 0.2, colour = NA
         ) +
         ggplot2::theme_bw() +
         ggplot2::theme(legend.position = "bottom") +
         ggplot2::labs(
            y = "Predicted Probability",
            x = ifelse(is.null(xlab), focal, xlab)
         )
      
      pred_3 <- data.frame(
         effects::predictorEffect(focal, model, focal.levels = xvals)
      )
      
      tab <- reshape_effect_df(pred_3, focal)
      
      if (isTRUE(ft_table)) {
         tab <- flextable::flextable(tab) |> flextable::autofit()
      }
      
      return(list(table = tab, plot = plot))
   }
   
   stop("Focal variable must be factor, numeric, or integer.", call. = FALSE)
}
