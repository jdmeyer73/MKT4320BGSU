#' @title Gain and Lift Tables and Charts for Binary Logistic Regression
#' @description
#' Compute cumulative gain and lift tables and corresponding plots for a fitted
#' binary logistic regression model, evaluated on training and test/holdout data.
#'
#' @details
#' The outcome variable in both \code{TRAIN} and \code{TEST} must be a factor with
#' exactly two levels, and \code{POSITIVE} must match one of those levels.
#'
#' Observations are ranked by predicted probability of the positive class and
#' grouped into 20 equal-sized bins (i.e., demi-deciles). Cumulative gain and lift
#' are computed relative to a random model baseline.
#'
#' @param MOD Fitted binary logistic regression model (\code{glm} with
#'   \code{family = "binomial"}).
#' @param TRAIN Training data used to estimate the model.
#' @param TEST Test or holdout data used for out-of-sample evaluation.
#' @param POSITIVE Factor level representing the positive / "success" class.
#'
#' @return
#' Invisibly returns a list with:
#' \itemize{
#'   \item \code{gaintable}: Data frame of cumulative gain by group.
#'   \item \code{lifttable}: Data frame of cumulative lift by group.
#'   \item \code{gainplot}: \code{ggplot} object showing the cumulative gain curve.
#'   \item \code{liftplot}: \code{ggplot} object showing the cumulative lift curve.
#' }
#'
#' @examples
#' \dontrun{
#' # Fit a binary logistic regression
#' model <- glm(buy ~ age + gender + income,
#'              data = train,
#'              family = "binomial")
#'
#' # Compute gain and lift diagnostics
#' gl <- gainlift_logistic(
#'   MOD      = model,
#'   TRAIN    = train,
#'   TEST     = test,
#'   POSITIVE = "Yes"
#' )
#'
#' # Inspect outputs
#' gl$gaintable
#' gl$lifttable
#'
#' # Display plots
#' gl$gainplot
#' gl$liftplot
#' }
#'
#' @importFrom stats predict family
#' @importFrom ggplot2 ggplot aes geom_line geom_point
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous
#' @importFrom ggplot2 scale_color_manual labs theme_bw theme element_blank
#' @export

gainlift_logistic <- function(MOD, TRAIN, TEST, POSITIVE) {
   
   # ---- model checks ----
   if (!inherits(MOD, "glm"))
      stop("MOD must be a glm() model.", call. = FALSE)
   
   if (!identical(stats::family(MOD)$family, "binomial"))
      stop("MOD must be logistic regression (family = 'binomial').", call. = FALSE)
   
   if (!is.data.frame(TRAIN))
      stop("TRAIN must be a data frame.", call. = FALSE)
   
   if (!is.data.frame(TEST))
      stop("TEST must be a data frame.", call. = FALSE)
   
   # Identify DV name
   dv_name <- toString(stats::formula(MOD)[[2]])
   
   if (!dv_name %in% names(TRAIN))
      stop(sprintf("Outcome '%s' not found in TRAIN.", dv_name), call. = FALSE)
   
   if (!dv_name %in% names(TEST))
      stop(sprintf("Outcome '%s' not found in TEST.", dv_name), call. = FALSE)
   
   # ---- enforce factor outcome ----
   if (!is.factor(TRAIN[[dv_name]]) || !is.factor(TEST[[dv_name]])) {
      stop(
         paste0(
            "The outcome variable '", dv_name, "' must be a FACTOR.\n",
            "Convert it first, for example:\n",
            "  TRAIN$", dv_name, " <- factor(TRAIN$", dv_name, ", levels = c('no','yes'))\n",
            "  TEST$",  dv_name, " <- factor(TEST$",  dv_name, ", levels = c('no','yes'))\n"
         ),
         call. = FALSE
      )
   }
   
   POSITIVE <- as.character(POSITIVE)
   
   if (!(POSITIVE %in% levels(TRAIN[[dv_name]])))
      stop(sprintf("POSITIVE = '%s' is not a factor level in TRAIN.", POSITIVE))
   
   if (!(POSITIVE %in% levels(TEST[[dv_name]])))
      stop(sprintf("POSITIVE = '%s' is not a factor level in TEST.", POSITIVE))
   
   # ---- helper: gain/lift computation ----
   compute_gainlift <- function(dat, label) {
      
      true <- dat[[dv_name]]
      prob <- stats::predict(MOD, newdata = dat, type = "response")
      
      dplyr::tibble(
         prob = prob,
         pos  = as.integer(true == POSITIVE)
      ) |>
         dplyr::mutate(
            bg = (21 - dplyr::ntile(prob, 20)) / 20   # demi-deciles 1.00, .95, ..., .05
         ) |>
         dplyr::group_by(bg) |>
         dplyr::summarise(
            pos = sum(pos),
            obs = dplyr::n(),
            .groups = "drop"
         ) |>
         dplyr::mutate(
            percpos = cumsum(pos) / max(cumsum(pos)),
            perobs  = cumsum(obs) / max(cumsum(obs)),
            lift    = cumsum(pos) /
               (cumsum(obs) * (max(cumsum(pos)) / max(cumsum(obs)))),
            sample = label
         ) |>
         dplyr::select(bg, percpos, lift, sample)
   }
   
   # ---- compute training + holdout ----
   pr_train <- compute_gainlift(TRAIN, "Training")
   pr_test  <- compute_gainlift(TEST,  "Holdout")
   
   # ---- baseline (random model) ----
   base <- data.frame(
      bg      = seq(0.05, 1, 0.05),
      percpos = seq(0.05, 1, 0.05),
      lift    = 1,
      sample  = "Baseline"
   )
   
   pr <- dplyr::bind_rows(pr_train, pr_test, base)
   
   # ---- gain plot (UPDATED scale_y_continuous to limits = c(0, 1)) ----
   gainplot <- ggplot2::ggplot(pr, ggplot2::aes(x = bg, y = percpos, color = sample)) +
      ggplot2::geom_point(size = 1.5) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::scale_x_continuous("Proportion Customers Contacted",
                                  limits = c(0.05, 1),
                                  breaks = seq(0.1, 1, 0.1)) +
      ggplot2::scale_y_continuous("Proportion Customers Positive",
                                  limits = c(0, 1),             # <-- UPDATED
                                  breaks = seq(0, 1, 0.1)) +    # <-- UPDATED
      ggplot2::scale_color_manual(
         "",
         breaks = c("Training", "Holdout", "Baseline"),
         values = c("Training" = "forestgreen",
                    "Holdout"  = "navy",
                    "Baseline" = "red4")
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
         panel.grid.major.x = ggplot2::element_blank(),
         panel.grid.minor   = ggplot2::element_blank(),
         legend.position    = "bottom"
      )
   
   # ---- lift plot ----
   liftplot <- ggplot2::ggplot(pr, ggplot2::aes(x = bg, y = lift, color = sample)) +
      ggplot2::geom_point(size = 1.5) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::scale_x_continuous("Proportion Customers Contacted",
                                  limits = c(0.05, 1),
                                  breaks = seq(0.1, 1, 0.1)) +
      ggplot2::scale_y_continuous("Lift") +
      ggplot2::scale_color_manual(
         "",
         breaks = c("Training", "Holdout", "Baseline"),
         values = c("Training" = "forestgreen",
                    "Holdout"  = "navy",
                    "Baseline" = "red4")
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
         panel.grid.major.x = ggplot2::element_blank(),
         panel.grid.minor   = ggplot2::element_blank(),
         legend.position    = "bottom"
      )
   
   # ---- tables ----
   gaintable <- pr |>
      dplyr::filter(sample != "Baseline") |>
      dplyr::select(bg, sample, percpos) |>
      tidyr::pivot_wider(
         id_cols = bg,
         names_from = sample,
         values_from = percpos
      ) |>
      dplyr::rename(`% Sample` = bg)
   
   lifttable <- pr |>
      dplyr::filter(sample != "Baseline") |>
      dplyr::select(bg, sample, lift) |>
      tidyr::pivot_wider(
         id_cols = bg,
         names_from = sample,
         values_from = lift
      ) |>
      dplyr::rename(`% Sample` = bg)
   
   list(
      gaintable = gaintable,
      lifttable = lifttable,
      gainplot  = gainplot,
      liftplot  = liftplot
   )
}
