#' @title Easy Marginal Effects Plots
#'
#' @description
#' Create marginal effects plots for a focal predictor (with or without an
#' interaction) from a linear regression (\code{lm}) or binary logistic
#' regression (\code{glm} with \code{family = "binomial"}).
#'
#' @details
#' Predicted values (and confidence intervals) are computed using
#' \pkg{ggeffects}. The returned object includes both the plot and the
#' underlying prediction table.
#'
#' This function uses internal label helpers \code{get_x_title()} and (when an
#' interaction is supplied) \code{get_legend_title()} to produce cleaner axis
#' and legend labels.
#'
#' @param model A fitted \code{lm} model or binary logistic \code{glm} model
#'   (\code{family = "binomial"}).
#' @param focal Character string; name of the focal predictor variable.
#' @param int Optional character string; name of the interaction variable.
#' @param focal_range Optional numeric override for the focal predictor values
#'   (only used when the focal predictor is continuous). Provide either a
#'   length-2 numeric vector \code{c(min, max)} to generate a sequence of
#'   values, or a numeric vector of explicit values to use.
#' @param ci Logical; include confidence intervals (bands/error bars)?
#'   Default is \code{TRUE}.
#'
#' @return A list with components:
#' \describe{
#'   \item{plot}{A \code{ggplot} object.}
#'   \item{ptable}{A \code{ggeffects} prediction table (coercible to a data frame).}
#' }
#'
#' @seealso \code{\link{easy_mp_plot}}
#'
#' @examples
#' \dontrun{
#' data(airlinesat)
#' model1 <- lm(nps ~ age + nflights, data = airlinesat)
#'
#' # Default focal range
#' out1 <- easy_mp(model1, "age")
#' out1$plot
#'
#' # Override focal range (continuous focal)
#' out1b <- easy_mp(model1, "age", focal_range = c(20, 70), ci = FALSE)
#' out1b$plot
#' }
#'
#' @export
#'
#' @importFrom ggeffects ggeffect
#' @importFrom insight find_parameters find_terms find_interactions
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_ribbon geom_errorbar
#'   facet_wrap labs theme_bw theme element_blank element_text as_labeller
easy_mp <- function(model, focal, int = NULL, focal_range = NULL, ci = TRUE) {
   
   # ---- small internal helpers ----
   
   # Value sequence for continuous interaction variable
   myval_i <- function(x, n_levels = 4) {
      quants <- stats::quantile(x, c(0.01, 0.5, 0.99), na.rm = TRUE)
      step   <- (quants[3] - quants[1]) / (n_levels - 1)
      
      if (step <= 0) return(rep(quants[2], n_levels))
      
      if (step > 1) {
         vals <- seq(quants[1], quants[3], by = step)
         round(vals)
      } else {
         digits <- abs(floor(log10(abs(step))))
         vals   <- seq(quants[1], quants[3], by = step)
         round(vals, digits)
      }
   }
   
   # Value sequence for continuous focal variable (default behavior)
   myval_f <- function(x) {
      rng  <- range(x, na.rm = TRUE)
      step <- (rng[2] - rng[1]) / 100
      
      if (step <= 0) return(rng[1])
      
      if (step > 1) {
         vals <- seq(rng[1], rng[2], by = step)
         round(vals)
      } else {
         digits <- abs(floor(log10(abs(step))))
         vals   <- seq(rng[1], rng[2], by = step)
         round(vals, digits)
      }
   }
   
   # Override focal sequence (only for continuous focal)
   build_focal_vals <- function(x, focal_range = NULL) {
      if (is.null(focal_range)) return(myval_f(x))
      
      if (!is.numeric(focal_range)) {
         stop("`focal_range` must be numeric when provided.", call. = FALSE)
      }
      
      fr <- focal_range[is.finite(focal_range)]
      if (length(fr) < 2) {
         stop("`focal_range` must have at least 2 finite numeric values.", call. = FALSE)
      }
      
      # If user supplies a length-2 range, generate ~101 points (matching default density)
      if (length(fr) == 2) {
         lo <- min(fr); hi <- max(fr)
         if (!is.finite(lo) || !is.finite(hi) || lo == hi) return(lo)
         
         step <- (hi - lo) / 100
         if (step > 1) {
            vals <- seq(lo, hi, by = step)
            round(vals)
         } else {
            digits <- abs(floor(log10(abs(step))))
            vals   <- seq(lo, hi, by = step)
            round(vals, digits)
         }
      } else {
         # Otherwise, treat as explicit values
         sort(unique(fr))
      }
   }
   
   # Color palettes (kept internal to avoid namespace clutter)
   cvi_colors <- list(
      bgsu = c("#F5C163", "#F7A44A", "#F98831", "#FB6C18",
               "#FD5000", "#DA4805", "#B7410B", "#943A11",
               "#713317", "#4F2C1D"),
      r4   = c("#000000", "#DF536B", "#61D04F", "#2297E6",
               "#28E2E5", "#CD0BBC", "#F5C710", "#9E9E9E")
   )
   
   cvi_palettes <- function(name, n,
                            all_palettes = cvi_colors,
                            type = c("discrete", "continuous")) {
      palette <- all_palettes[[name]]
      if (missing(n)) n <- length(palette)
      type <- match.arg(type)
      
      out <- grDevices::colorRampPalette(palette)(n)
      structure(out, name = name, class = "palette")
   }
   
   scale_fill_cvi_d <- function(name, n = NULL) {
      ggplot2::scale_fill_manual(
         values = cvi_palettes(name, type = "discrete", n = n)
      )
   }
   
   scale_color_cvi_d <- function(name, n = NULL) {
      ggplot2::scale_color_manual(
         values = cvi_palettes(name, type = "discrete", n = n)
      )
   }
   
   # ---- basic model checks ----
   if (!inherits(model, c("lm", "glm"))) {
      stop("`model` must be an 'lm' or 'glm' object.", call. = FALSE)
   }
   
   if (inherits(model, "glm")) {
      fam <- stats::family(model)$family
      if (!identical(fam, "binomial")) {
         stop("`glm` models must be binary logistic (family = 'binomial').",
              call. = FALSE)
      }
   }
   
   # Get info from insight
   params_cond <- insight::find_parameters(model)$conditional
   resp_terms  <- insight::find_terms(model)$response
   
   # ---- log-transform detection ----
   log_f  <- any(grepl(sprintf("log\\(%s\\)", focal), params_cond))
   log_i  <- !is.null(int) &&
      any(grepl(sprintf("log\\(%s\\)", int), params_cond))
   log_dv <- any(grepl("log\\(\\D*\\)", resp_terms))
   
   # Model frame (handles NA pattern the same way as model)
   dat <- model$model
   
   # Focal column name in model frame
   focal_col <- if (log_f) sprintf("log(%s)", focal) else focal
   
   if (!focal_col %in% names(dat)) {
      stop("Focal variable '", focal,
           "' not found in the model frame.", call. = FALSE)
   }
   
   f_raw <- dat[[focal_col]]
   f_num <- is.numeric(f_raw)
   
   f_data <- if (log_f) exp(f_raw) else f_raw
   
   if (f_num) {
      f.val <- build_focal_vals(f_data, focal_range = focal_range)
   }
   
   # Interaction variable handling (only if supplied)
   if (!is.null(int)) {
      int_col <- if (log_i) sprintf("log(%s)", int) else int
      
      if (!int_col %in% names(dat)) {
         stop("Interaction variable '", int,
              "' not found in the model frame.", call. = FALSE)
      }
      
      i_raw <- dat[[int_col]]
      i_num <- is.numeric(i_raw)
      
      i_data <- if (log_i) exp(i_raw) else i_raw
      
      if (i_num) {
         i.val <- myval_i(i_data, n_levels = 4)
      }
   }
   
   # ---- determine plot type ----
   if (!is.null(int)) {
      inter_terms <- paste(insight::find_interactions(model)$conditional,
                           collapse = " ")
      
      f_name <- if (log_f) sprintf("log(%s)", focal) else focal
      i_name <- if (log_i) sprintf("log(%s)", int) else int
      
      ic1 <- paste0(f_name, ":", i_name)
      ic2 <- paste0(i_name, ":", f_name)
      
      has_int <- grepl(ic1, inter_terms, fixed = TRUE) |
         grepl(ic2, inter_terms, fixed = TRUE)
      
      if (!has_int) {
         warning("Model does not include an interaction between focal and ",
                 "interaction variables.")
      }
      
      if (f_num && i_num) {
         plot.type <- "cont.cont"
      } else if (f_num && !i_num) {
         plot.type <- "cont.cat"
      } else if (!f_num && i_num) {
         plot.type <- "cat.cont"
      } else {
         plot.type <- "cat.cat"
      }
   } else {
      plot.type <- if (f_num) "cont.null" else "cat.null"
   }
   
   # ---- y-axis label ----
   dv_name <- all.vars(formula(model))[1]
   
   if (inherits(model, "lm")) {
      ylabel <- paste0(dv_name, " (Predicted)")
   } else {
      ylabel <- paste0(dv_name, " (Predicted Prob.)")
   }
   
   # ---- compute effects & build plots ----
   if (plot.type == "cont.null") {
      
      fterm  <- paste0(focal, " [f.val]")
      ptable <- ggeffects::ggeffect(model, terms = fterm)
      
      if (log_dv) {
         ptable$predicted <- exp(ptable$predicted)
         ptable$conf.low  <- exp(ptable$conf.low)
         ptable$conf.high <- exp(ptable$conf.high)
      }
      
      plot <- ptable |>
         ggplot2::ggplot(ggplot2::aes(x = x, y = predicted)) +
         ggplot2::geom_line(linewidth = 1, color = "darkorange") +
         ggplot2::labs(
            y = ylabel,
            x = get_x_title(focal)
         ) +
         ggplot2::theme_bw()
      
      if (isTRUE(ci)) {
         plot <- plot +
            ggplot2::geom_ribbon(
               ggplot2::aes(ymin = conf.low, ymax = conf.high),
               alpha = 0.2,
               fill  = "darkorange"
            )
      }
      
   } else if (plot.type == "cat.null") {
      
      ptable <- ggeffects::ggeffect(model, focal)
      colorn <- length(unique(ptable$x))
      
      if (log_dv) {
         ptable$predicted <- exp(ptable$predicted)
         ptable$conf.low  <- exp(ptable$conf.low)
         ptable$conf.high <- exp(ptable$conf.high)
      }
      
      plot <- ptable |>
         ggplot2::ggplot(ggplot2::aes(x = x, y = predicted, color = x)) +
         ggplot2::geom_point(size = 4) +
         ggplot2::labs(
            y = ylabel,
            x = get_x_title(focal)
         ) +
         scale_color_cvi_d("bgsu", n = colorn) +
         ggplot2::theme_bw() +
         ggplot2::theme(
            panel.grid.major.x = ggplot2::element_blank(),
            legend.position    = "none"
         )
      
      if (isTRUE(ci)) {
         plot <- plot +
            ggplot2::geom_errorbar(
               ggplot2::aes(ymin = conf.low, ymax = conf.high),
               width     = 0.5,
               linewidth = 1
            )
      }
      
   } else if (plot.type == "cont.cont") {
      
      fterm  <- paste0(focal, " [f.val]")
      iterm  <- paste0(int, " [i.val]")
      ptable <- ggeffects::ggeffect(model, terms = c(fterm, iterm))
      
      if (log_dv) {
         ptable$predicted <- exp(ptable$predicted)
         ptable$conf.low  <- exp(ptable$conf.low)
         ptable$conf.high <- exp(ptable$conf.high)
      }
      
      if (isTRUE(ci)) {
         plot <- ptable |>
            ggplot2::ggplot(
               ggplot2::aes(
                  x     = x,
                  y     = predicted,
                  color = group,
                  fill  = group
               )
            ) +
            ggplot2::geom_line(linewidth = 1) +
            ggplot2::geom_ribbon(
               ggplot2::aes(ymin = conf.low, ymax = conf.high),
               alpha = 0.2
            ) +
            scale_fill_cvi_d("bgsu", n = 4) +
            scale_color_cvi_d("bgsu", n = 4) +
            ggplot2::labs(
               y     = ylabel,
               x     = get_x_title(focal),
               color = get_legend_title(int),
               fill  = get_legend_title(int)
            ) +
            ggplot2::theme_bw() +
            ggplot2::theme(
               panel.grid.major.x = ggplot2::element_blank(),
               legend.position    = "bottom"
            )
      } else {
         plot <- ptable |>
            ggplot2::ggplot(
               ggplot2::aes(
                  x     = x,
                  y     = predicted,
                  color = group
               )
            ) +
            ggplot2::geom_line(linewidth = 1) +
            scale_color_cvi_d("bgsu", n = 4) +
            ggplot2::labs(
               y     = ylabel,
               x     = get_x_title(focal),
               color = get_legend_title(int)
            ) +
            ggplot2::theme_bw() +
            ggplot2::theme(
               panel.grid.major.x = ggplot2::element_blank(),
               legend.position    = "bottom"
            )
      }
      
   } else if (plot.type == "cont.cat") {
      
      fterm  <- paste0(focal, " [f.val]")
      iterm  <- int
      ptable <- ggeffects::ggeffect(model, terms = c(fterm, iterm))
      
      if (log_dv) {
         ptable$predicted <- exp(ptable$predicted)
         ptable$conf.low  <- exp(ptable$conf.low)
         ptable$conf.high <- exp(ptable$conf.high)
      }
      
      colorn <- length(unique(ptable$group))
      
      if (isTRUE(ci)) {
         plot <- ptable |>
            ggplot2::ggplot(
               ggplot2::aes(
                  x     = x,
                  y     = predicted,
                  color = group,
                  fill  = group
               )
            ) +
            ggplot2::geom_line(linewidth = 1) +
            ggplot2::geom_ribbon(
               ggplot2::aes(ymin = conf.low, ymax = conf.high),
               alpha = 0.2
            ) +
            scale_fill_cvi_d("bgsu", n = colorn) +
            scale_color_cvi_d("bgsu", n = colorn) +
            ggplot2::labs(
               y     = ylabel,
               x     = get_x_title(focal),
               color = get_legend_title(int),
               fill  = get_legend_title(int)
            ) +
            ggplot2::theme_bw() +
            ggplot2::theme(
               panel.grid.major.x = ggplot2::element_blank(),
               legend.position    = "bottom"
            )
      } else {
         plot <- ptable |>
            ggplot2::ggplot(
               ggplot2::aes(
                  x     = x,
                  y     = predicted,
                  color = group
               )
            ) +
            ggplot2::geom_line(linewidth = 1) +
            scale_color_cvi_d("bgsu", n = colorn) +
            ggplot2::labs(
               y     = ylabel,
               x     = get_x_title(focal),
               color = get_legend_title(int)
            ) +
            ggplot2::theme_bw() +
            ggplot2::theme(
               panel.grid.major.x = ggplot2::element_blank(),
               legend.position    = "bottom"
            )
      }
      
   } else if (plot.type == "cat.cont") {
      
      fterm  <- focal
      iterm  <- paste0(int, " [i.val]")
      ptable <- ggeffects::ggeffect(model, terms = c(fterm, iterm))
      
      glabel   <- get_legend_title(int)
      appender <- function(string, prefix = glabel) paste0(prefix, "=", string)
      colorn   <- length(unique(ptable$x))
      
      plot <- ptable |>
         ggplot2::ggplot(ggplot2::aes(x = x, y = predicted, group = 1)) +
         ggplot2::geom_point(ggplot2::aes(color = x), size = 4) +
         ggplot2::labs(
            y = ylabel,
            x = get_x_title(focal)
         ) +
         ggplot2::facet_wrap(
            ~group,
            labeller = ggplot2::as_labeller(appender)
         ) +
         scale_color_cvi_d("bgsu", n = colorn) +
         ggplot2::theme_bw() +
         ggplot2::theme(
            panel.grid.major.x = ggplot2::element_blank(),
            legend.position    = "none"
         )
      
      if (isTRUE(ci)) {
         plot <- plot +
            ggplot2::geom_errorbar(
               ggplot2::aes(ymin = conf.low, ymax = conf.high, color = x),
               width     = 0.5,
               linewidth = 1
            )
      }
      
   } else if (plot.type == "cat.cat") {
      
      fterm  <- focal
      iterm  <- int
      ptable <- ggeffects::ggeffect(model, terms = c(fterm, iterm))
      
      glabel   <- get_legend_title(int)
      appender <- function(string, prefix = glabel) paste0(prefix, "=", string)
      colorn   <- length(unique(ptable$x))
      
      plot <- ptable |>
         ggplot2::ggplot(ggplot2::aes(x = x, y = predicted, group = 1)) +
         ggplot2::geom_point(ggplot2::aes(color = x), size = 4) +
         ggplot2::labs(
            y = ylabel,
            x = get_x_title(focal)
         ) +
         ggplot2::facet_wrap(
            ~group,
            labeller = ggplot2::as_labeller(appender)
         ) +
         scale_color_cvi_d("bgsu", n = colorn) +
         ggplot2::theme_bw() +
         ggplot2::theme(
            panel.grid.major.x = ggplot2::element_blank(),
            legend.position    = "none"
         )
      
      if (isTRUE(ci)) {
         plot <- plot +
            ggplot2::geom_errorbar(
               ggplot2::aes(ymin = conf.low, ymax = conf.high, color = x),
               width     = 0.5,
               linewidth = 1
            )
      }
   }
   
   list(plot = plot, ptable = ptable)
}
