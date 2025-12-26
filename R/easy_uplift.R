#' Easy Regression Uplift Modeling (Two-Model / Indirect Approach)
#'
#' @description
#' Perform uplift modeling using the two-model (indirect) approach based on a
#' fitted regression model. Supported model types include binary logistic
#' regression estimated via \code{glm(..., family = binomial(link = "logit"))}
#' and linear regression estimated via \code{lm()}.
#'
#' @details
#' The user supplies a fitted regression model that includes a binary treatment
#' variable with exactly two levels. The function fits two separate subgroup
#' models—one for the control group and one for the treated group—using the
#' predictors from the original model, excluding:
#' \itemize{
#'   \item the treatment main effect, and
#'   \item any interaction terms involving the treatment variable.
#' }
#'
#' Predicted uplift (incremental effect) is computed as:
#' \deqn{lift = \hat{y}_{treat} - \hat{y}_{control}}
#'
#' Predictions are generated on the original model data by default, or on
#' \code{newdata} if supplied.
#'
#' This function replaces the deprecated \code{reguplift()} function and is the
#' recommended interface going forward.
#'
#' @param model A fitted regression model of class \code{glm} (binary logit) or
#'   \code{lm}.
#' @param treatment Character string giving the name of the treatment variable.
#'   The variable must have exactly two levels and be coded as (0/1), logical,
#'   or (\code{"Yes"}, \code{"No"}).
#' @param newdata Optional data frame on which to compute uplift (e.g., holdout
#'   or test data). If \code{NULL}, uplift is computed on the model data.
#' @param bins Integer; number of groups used for the uplift tables and plots.
#'   Must be between 5 and 20. Default is 10.
#' @param aspect_ratio Optional numeric aspect ratio applied to all plots.
#'   Default is \code{NULL}.
#' @param ... Backward-compatible alias support. If \code{ar} is supplied, it is
#'   treated as \code{aspect_ratio}.
#'
#' @return
#' A list of class \code{"easy_uplift"} containing:
#' \itemize{
#'   \item \code{$group} Uplift results by ordered group (based on \code{bins})
#'   \item \code{$all} Observation-level data with predicted lift appended
#'   \item \code{$plots} A list of ggplot objects: Qini (\code{$qini}),
#'     mean uplift by group (\code{$uplift}), and cumulative gain
#'     (\code{$c.gain})
#'   \item \code{$models} Fitted subgroup models: \code{$control} and \code{$treat}
#'   \item \code{$covariates} Character vector of covariates used in the subgroup models
#'   \item \code{$treatment_var} Treatment variable name
#'   \item \code{$outcome_var} Outcome variable name
#'   \item \code{$spec} Metadata describing the uplift specification
#' }
#'
#' @seealso \code{\link{reguplift}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Binary outcome (logistic regression)
#' m <- glm(y ~ age + income + promotion,
#'          data = df,
#'          family = binomial(link = "logit"))
#' res <- easy_uplift(m, treatment = "promotion", bins = 10)
#'
#' # Continuous outcome (linear regression)
#' m2 <- lm(y ~ age + income + promotion, data = df)
#' res2 <- easy_uplift(m2, treatment = "promotion", bins = 10)
#'
#' # Holdout uplift
#' res_holdout <- easy_uplift(m, treatment = "promotion",
#'                            newdata = test, bins = 10)
#' }

easy_uplift <- function(model,
                        treatment,
                        newdata = NULL,
                        bins = 10,
                        aspect_ratio = NULL,
                        ...) {
   
   # ---- backward-compatible alias: ar -> aspect_ratio ----
   dots <- list(...)
   if (!is.null(dots$ar) && is.null(aspect_ratio)) {
      aspect_ratio <- dots$ar
   } else if (!is.null(dots$ar) && !is.null(aspect_ratio)) {
      stop("Provide only one of `aspect_ratio` or legacy `ar` (via ...), not both.")
   }
   
   # ---- validations ----
   if (!is.character(treatment) || length(treatment) != 1) {
      stop("`treatment` must be a single character string (e.g., \"promotion\").")
   }
   if (!(bins %in% 5:20)) {
      stop("`bins` must be an integer between 5 and 20.")
   }
   if (!is.null(aspect_ratio) && !is.numeric(aspect_ratio)) {
      stop("`aspect_ratio` must be numeric (or NULL).")
   }
   
   is_glm <- inherits(model, "glm")
   is_lm  <- inherits(model, "lm")
   
   if (!is_glm && !is_lm) {
      stop("`model` must be either a binary logistic glm (logit) or an lm.")
   }
   
   # Determine outcome type
   if (is_glm) {
      if (is.null(model$family) ||
          model$family$family != "binomial" ||
          model$family$link != "logit") {
         stop("For glm, model must be binomial with logit link: family=binomial(link=\"logit\").")
      }
      outcometype <- "bin"
   } else {
      outcometype <- "cont"
   }
   
   # Use model.frame for robustness
   mf <- stats::model.frame(model)
   f_full <- stats::formula(model)
   outcome <- all.vars(f_full)[1]
   
   if (!(treatment %in% names(mf))) {
      stop("`treatment` was not found in the model frame. Check the variable name and model.")
   }
   if (!(outcome %in% names(mf))) {
      stop("Outcome variable was not found in the model frame.")
   }
   
   data <- as.data.frame(mf)
   
   # ---- helper: recode outcome into numeric outcome column ----
   recode_outcome <- function(df) {
      y <- df[[outcome]]
      
      if (outcometype == "bin") {
         if (is.factor(y) || is.character(y)) {
            y <- as.character(y)
            if (all(y %in% c("Yes", "No"))) {
               df$outcome <- ifelse(y == "Yes", 1, 0)
            } else {
               stop('For a binary logistic model, outcome must be (0/1), logical, or ("Yes","No").')
            }
         } else if (is.logical(y)) {
            df$outcome <- ifelse(y, 1, 0)
         } else if (is.numeric(y) || is.integer(y)) {
            if (all(na.omit(y) %in% 0:1)) {
               df$outcome <- as.numeric(y)
            } else {
               stop('For a binary logistic model, outcome must be (0/1), logical, or ("Yes","No").')
            }
         } else {
            stop('For a binary logistic model, outcome must be (0/1), logical, or ("Yes","No").')
         }
      } else {
         if (!(is.numeric(y) || is.integer(y))) {
            stop("For a linear regression model, outcome must be numeric/integer.")
         }
         df$outcome <- as.numeric(y)
      }
      
      df
   }
   
   # ---- helper: recode treatment into numeric treat column (0/1) ----
   recode_treatment <- function(df) {
      x <- df[[treatment]]
      
      if (length(unique(na.omit(x))) != 2) {
         stop("The treatment variable must have exactly two levels/values.")
      }
      
      if (is.factor(x) || is.character(x)) {
         x <- as.character(x)
         if (all(x %in% c("Yes", "No"))) {
            df$treat <- ifelse(x == "Yes", 1, 0)
         } else {
            stop('The treatment variable must be (0/1), logical, or ("Yes","No").')
         }
      } else if (is.logical(x)) {
         df$treat <- ifelse(x, 1, 0)
      } else if (is.numeric(x) || is.integer(x)) {
         if (all(na.omit(x) %in% 0:1)) {
            df$treat <- as.numeric(x)
         } else {
            stop('The treatment variable must be (0/1), logical, or ("Yes","No").')
         }
      } else {
         stop('The treatment variable must be (0/1), logical, or ("Yes","No").')
      }
      
      df
   }
   
   data <- recode_outcome(data)
   data <- recode_treatment(data)
   
   # Prep newdata (if provided) using the same recodes
   if (!is.null(newdata)) {
      newdata <- as.data.frame(newdata)
      
      if (!(outcome %in% names(newdata))) {
         stop("`newdata` must include the outcome variable (same name as in the model).")
      }
      if (!(treatment %in% names(newdata))) {
         stop("`newdata` must include the treatment variable (same name passed to `treatment`).")
      }
      
      newdata <- recode_outcome(newdata)
      newdata <- recode_treatment(newdata)
   }
   
   # ---- build subgroup modeling formula (treatment can be anywhere in original model) ----
   rhs_terms <- attr(stats::terms(model), "term.labels")
   
   # Drop treatment main effect and any interactions involving treatment (e.g., treat:x, x:treat)
   keep_terms <- rhs_terms[
      !(rhs_terms == treatment | grepl(paste0("(^|:)", treatment, "(:|$)"), rhs_terms))
   ]
   
   rhs <- if (length(keep_terms) == 0) "1" else paste(keep_terms, collapse = " + ")
   model_formula <- stats::as.formula(paste0("outcome ~ ", rhs))
   
   # ---- NEW: store covariate names used in subgroup models ----
   # all.vars extracts variable names even when transformations are present
   covariates <- if (rhs == "1") character(0) else unique(all.vars(stats::as.formula(paste0("~", rhs))))
   # make sure we don't accidentally include treatment/outcome names here
   covariates <- setdiff(covariates, c("outcome", "treat", treatment, outcome))
   
   # ---- split sample ----
   data.0 <- data[data$treat == 0, , drop = FALSE]
   data.1 <- data[data$treat == 1, , drop = FALSE]
   if (nrow(data.0) == 0 || nrow(data.1) == 0) {
      stop("Both treatment groups must have at least 1 observation in the model data.")
   }
   
   # ---- fit subgroup models ----
   if (outcometype == "bin") {
      model0 <- stats::glm(model_formula, data = data.0, family = stats::binomial(link = "logit"))
      model1 <- stats::glm(model_formula, data = data.1, family = stats::binomial(link = "logit"))
   } else {
      model0 <- stats::lm(model_formula, data = data.0)
      model1 <- stats::lm(model_formula, data = data.1)
   }
   
   # ---- choose prediction data ----
   lift.data <- if (is.null(newdata)) data else newdata
   
   # ---- predicted lift ----
   lift.data$out1 <- stats::predict(model1, lift.data, type = "response")
   lift.data$out0 <- stats::predict(model0, lift.data, type = "response")
   lift.data$lift <- lift.data$out1 - lift.data$out0
   
   # rank by lift and place into bins
   lift.data$rank <- rank(-lift.data$lift, ties.method = "random")
   lift.data$group <- cut(lift.data$rank, bins, labels = FALSE)
   
   # ---- uplift by group table ----
   if (outcometype == "bin") {
      
      dataResults <- data.frame(matrix(rep(0), bins, 10))
      colnames(dataResults) <- c(
         "cum_per", "T_Yn", "T_n", "C_Yn", "C_n",
         "inc_Yn", "uplift", "cm_Yn", "cm_uplift", "cm_gain"
      )
      
      for (i in 1:bins) {
         subset <- lift.data[lift.data$group == i, , drop = FALSE]
         dataResults[i, 1] <- i / bins
         dataResults[i, 2] <- sum(subset$treat == 1 & subset$outcome == 1)
         dataResults[i, 3] <- sum(subset$treat == 1)
         dataResults[i, 4] <- sum(subset$treat == 0 & subset$outcome == 1)
         dataResults[i, 5] <- sum(subset$treat == 0)
         
         dataResults[i, 6] <- dataResults[i, 2] - dataResults[i, 4] * dataResults[i, 3] / dataResults[i, 5]
         dataResults[i, 7] <- (dataResults[i, 2] / dataResults[i, 3] - dataResults[i, 4] / dataResults[i, 5]) * 100
      }
      
      dataResults[, 8]  <- (cumsum(dataResults[, 2]) - cumsum(dataResults[, 4]) * cumsum(dataResults[, 3]) / cumsum(dataResults[, 5]))
      dataResults[, 9]  <- (cumsum(dataResults[, 2]) / cumsum(dataResults[, 3]) - cumsum(dataResults[, 4]) / cumsum(dataResults[, 5])) * 100
      dataResults[, 10] <- 100 * (cumsum(dataResults[, 2] - dataResults[, 4] * sum(dataResults[, 3]) / sum(dataResults[, 5])) / sum(dataResults[, 3]))
      
   } else {
      
      dataResults <- data.frame(matrix(rep(0), bins, 10))
      colnames(dataResults) <- c(
         "cum_per", "Tm_o", "T_n", "Cm_o", "C_n",
         "inc_Yn", "uplift", "cm_Yn", "cm_uplift", "cm_gain"
      )
      
      for (i in 1:bins) {
         subset1 <- lift.data[lift.data$group == i & lift.data$treat == 1, , drop = FALSE]
         subset0 <- lift.data[lift.data$group == i & lift.data$treat == 0, , drop = FALSE]
         
         dataResults[i, 1] <- i / bins
         dataResults[i, 2] <- mean(subset1$outcome)
         dataResults[i, 3] <- nrow(subset1)
         dataResults[i, 4] <- mean(subset0$outcome)
         dataResults[i, 5] <- nrow(subset0)
         
         dataResults[i, 6] <- sum(subset1$outcome) - sum(subset0$outcome) * nrow(subset1) / nrow(subset0)
         dataResults[i, 7] <- (dataResults[i, 2] - dataResults[i, 4])
      }
      
      dataResults[, 8]  <- cumsum(dataResults[, 2] * dataResults[, 3]) -
         cumsum(dataResults[, 4] * dataResults[, 5]) * cumsum(dataResults[, 3]) / cumsum(dataResults[, 5])
      
      dataResults[, 9]  <- (cumsum(dataResults[, 2] * dataResults[, 3]) / cumsum(dataResults[, 3]) -
                               cumsum(dataResults[, 4] * dataResults[, 5]) / cumsum(dataResults[, 5]))
      
      dataResults[, 10] <- (cumsum(dataResults[, 2] * dataResults[, 3]) -
                               cumsum(dataResults[, 4] * dataResults[, 5]) * sum(dataResults[, 3]) / sum(dataResults[, 5])) / sum(dataResults[, 3])
   }
   
   # ---- Kendall's tau ----
   ken.tau <- stats::cor.test(seq(nrow(dataResults), 1), dataResults[, 7], method = "kendall")
   kt.est <- paste0(round(ken.tau$estimate, 3))
   kt.p <- ifelse(ken.tau$p.value < 0.001, "< 0.001", paste0("= ", round(ken.tau$p.value, 3)))
   
   # ---- Qini coefficient ----
   sub <- lift.data[order(lift.data$rank), c("outcome", "treat", "rank")]
   N <- nrow(sub)
   
   sub$C_Ot <- cumsum(ifelse(sub$treat == 1, sub$outcome, 0))
   sub$C_Oc <- cumsum(ifelse(sub$treat == 0, sub$outcome, 0))
   sub$C_Tt <- cumsum(ifelse(sub$treat == 1, 1, 0))
   sub$C_Tc <- cumsum(ifelse(sub$treat == 0, 1, 0))
   
   sum_Tt <- sub[N, "C_Tt"]
   sum_Tc <- sub[N, "C_Tc"]
   
   if (outcometype == "bin") {
      sub$cum.gain <- 100 * (sub$C_Ot - sub$C_Oc * sum_Tt / sum_Tc) / sum_Tt
   } else {
      sub$cum.gain <- (sub$C_Ot - sub$C_Oc * sum_Tt / sum_Tc) / sum_Tt
   }
   
   x <- seq(1 / N, 1, 1 / N)
   y <- sub$cum.gain
   
   # AUC (trapezoid)
   auc <- 0
   for (i in 2:length(x)) {
      auc <- auc + 0.5 * (x[i] - x[i - 1]) * (y[i] + y[i - 1])
   }
   
   ovr.cum.gain <- sub[N, "cum.gain"]
   y.rand <- cumsum(rep(ovr.cum.gain / N, N))
   yanno <- mean(y.rand)
   
   auc.rand <- 0
   for (i in 2:length(x)) {
      auc.rand <- auc.rand + 0.5 * (x[i] - x[i - 1]) * (y.rand[i] + y.rand[i - 1])
   }
   
   Qini <- auc - auc.rand
   
   # -------------------------
   # Create Plots (match original look, but remove discrete-size warning)
   # -------------------------
   
   uplift <- ggplot2::ggplot(ggplot2::aes(x = factor(cum_per), y = uplift), data = dataResults) +
      ggplot2::geom_col(fill = "darkorange") +
      ggplot2::labs(
         x = "Percentile",
         y = "Mean Uplift",
         caption = paste0("Kendall's tau = ", kt.est, "; p ", kt.p)
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
         aspect.ratio = aspect_ratio,
         panel.grid.major.x = ggplot2::element_blank(),
         panel.grid.minor = ggplot2::element_blank(),
         panel.background = ggplot2::element_rect(fill = "white"),
         plot.background  = ggplot2::element_rect(fill = "transparent", color = NA_character_)
      )
   
   c.gain <- ggplot2::ggplot(ggplot2::aes(x = factor(cum_per), y = cm_gain), data = dataResults) +
      ggplot2::geom_col(fill = "darkorange") +
      ggplot2::labs(x = "Percentile", y = "Mean Cumulative Incremental Gain") +
      ggplot2::theme_bw() +
      ggplot2::theme(
         aspect.ratio = aspect_ratio,
         panel.grid.major.x = ggplot2::element_blank(),
         panel.grid.minor = ggplot2::element_blank(),
         panel.background = ggplot2::element_rect(fill = "white"),
         plot.background  = ggplot2::element_rect(fill = "transparent", color = NA_character_)
      )
   
   qdf <- rbind(
      data.frame(x = x, y = y,      type = "Model"),
      data.frame(x = x, y = y.rand, type = "Random")
   )
   
   qini <- ggplot2::ggplot(ggplot2::aes(x = x, y = y, color = type), data = qdf) +
      ggplot2::geom_line(
         data = qdf[qdf$type == "Model", , drop = FALSE],
         linewidth = 1
      ) +
      ggplot2::geom_line(
         data = qdf[qdf$type == "Random", , drop = FALSE],
         linewidth = 0.5
      ) +
      ggplot2::scale_color_manual(values = c("darkorange", "#4f2c1d")) +
      ggplot2::scale_x_continuous(
         breaks = seq(0, 1, 0.1),
         labels = scales::percent_format()
      ) +
      ggplot2::labs(
         x = "Percent of population targeted",
         y = "Cumulative Incremental Gains"
      ) +
      ggplot2::annotate(
         "text",
         label = paste0("Q = ", sprintf("%.2f", Qini)),
         x = 0.75, y = yanno, size = 5, color = "red"
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
         panel.grid.minor = ggplot2::element_blank(),
         legend.position = c(0.01, 0.90),
         legend.justification = "left",
         legend.title = ggplot2::element_blank(),
         legend.margin = ggplot2::margin(0, 0, 0, 0),
         legend.background = ggplot2::element_blank(),
         aspect.ratio = aspect_ratio,
         panel.grid.major.x = ggplot2::element_blank(),
         panel.background = ggplot2::element_rect(fill = "white"),
         plot.background  = ggplot2::element_rect(fill = "transparent", color = NA_character_)
      ) +
      ggplot2::guides(
         color = ggplot2::guide_legend(
            override.aes = list(linewidth = c(1, 0.5))
         )
      )
   
   plots <- list(qini = qini, uplift = uplift, c.gain = c.gain)
   models <- list(control = model0, treat = model1)
   
   # ---- NEW: attach metadata + class for downstream functions like liftplot() ----
   spec <- list(
      treatment_var = treatment,
      outcome_var = outcome,
      covariates = covariates,
      bins = bins,
      outcometype = outcometype,
      aspect_ratio = aspect_ratio
   )
   
   res <- list(
      group = dataResults,
      all = lift.data,
      plots = plots,
      models = models,
      covariates = covariates,
      treatment_var = treatment,
      outcome_var = outcome,
      spec = spec
   )
   class(res) <- c("easy_uplift", class(res))
   
   res
}
