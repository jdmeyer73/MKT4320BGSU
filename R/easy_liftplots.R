#' @title Lift Diagnostics for easy_uplift Objects
#' @description
#' Create lift diagnostics for an \code{easy_uplift()} object. The function
#' always produces (1) a histogram of predicted lift and (2) lift-by-covariate
#' error-bar plots. Optionally, create interaction-style plots for user-specified
#' pairs.
#'
#' @details
#' \strong{Ranking metrics}
#' For each covariate, observations are grouped by:
#' \itemize{
#'   \item quantile bins (numeric covariates; default \code{numeric_bins = 5})
#'   \item levels (factor/character/logical and 0/1 indicators)
#' }
#' Let \eqn{\mu = E[lift]} be the overall mean lift and \eqn{\mu_g = E[lift | g]}
#' be the mean lift in subgroup \eqn{g}. Define a CATE-like deviation:
#' \deqn{\Delta_g = \mu_g - \mu.}
#' Then:
#' \itemize{
#'   \item \code{score_wmae} = \eqn{\sum_g w_g |\Delta_g|} where \eqn{w_g} is the
#'     subgroup proportion. This is the primary importance measure.
#'   \item \code{score_maxabs} = \eqn{\max_g |\Delta_g|}, the largest absolute
#'     subgroup deviation (can be driven by small subgroups).
#' }
#'
#' When \code{grid = TRUE}, plots are paginated into cowplot grids of at most 6
#' plots per page (2x3). The grid pages use a common y-axis range to help compare
#' effect sizes across covariates.
#'
#' @param x An object returned by \code{easy_uplift()} (must include \code{x$all}
#'   and \code{x$covariates} or \code{x$spec$covariates}).
#' @param vars Character vector of covariate names to plot. Default is
#'   \code{"all"} (uses \code{x$covariates} / \code{x$spec$covariates}).
#' @param pairs Optional list of length-2 character vectors specifying
#'   interaction-style plots to create, e.g.,
#'   \code{list(c("recency","zip"), c("gender","income"))}.
#' @param ar Optional aspect ratio passed to \code{theme(aspect.ratio = ar)}.
#'   Default is \code{NULL}.
#' @param ci Error-bar style. Use \code{0} for \eqn{\pm 1} SD error bars, or one
#'   of \code{c(0.90, 0.95, 0.975, 0.99)} for normal-approximation confidence
#'   intervals. Default is \code{0.95}.
#' @param bins Integer; number of bins for the histogram. Default is \code{30}.
#' @param numeric_bins Integer; number of quantile bins for numeric covariates.
#'   Default is \code{5}.
#' @param by_numeric_bins Integer; number of quantile bins to use for the second
#'   variable in a pair when it is numeric. Default is \code{3}.
#' @param grid Logical; if \code{TRUE}, also return paginated cowplot grids of
#'   plots. Default is \code{TRUE}.
#' @param top Optional integer. If provided, only the top \code{top} covariates
#'   (by \code{score_wmae}) are included in \code{plots_main/pages_main}.
#'   Rankings are still computed for all covariates.
#' @param ft Logical; if \code{TRUE} (default), return ranking tables as
#'   \code{flextable} objects.
#'
#' @return A named list with components:
#' \itemize{
#'   \item \code{hist}: ggplot histogram of \code{lift}.
#'   \item \code{rank_main}: covariate ranking table (flextable if \code{ft = TRUE}, else data.frame).
#'   \item \code{rank_main_df}: covariate ranking data.frame (always).
#'   \item \code{plots_main}: named list of ggplot main-effect lift plots.
#'   \item \code{pages_main}: (if \code{grid = TRUE}) list of cowplot grids for main-effect plots.
#'   \item \code{rank_pairs}: pair ranking table (if \code{pairs}; flextable if \code{ft = TRUE}).
#'   \item \code{rank_pairs_df}: pair ranking data.frame (if \code{pairs}; always).
#'   \item \code{plots_pairs}: named list of ggplot pair plots (if \code{pairs}).
#'   \item \code{pages_pairs}: (if \code{grid = TRUE} and \code{pairs}) list of cowplot grids for pair plots.
#' }
#'
#' @seealso \code{\link{liftplot}} (deprecated)
#'
#' @examples
#' \dontrun{
#' # x is an easy_uplift object created by easy_uplift()
#' # Histogram + main-effect lift plots for all covariates
#' out <- easy_liftplots(x)
#' out$hist
#'
#' # Only selected covariates
#' out2 <- easy_liftplots(x, vars = c("recency", "frequency"), ci = 0.95)
#' out2$plots_main[["recency"]]
#'
#' # Pair plot
#' out3 <- easy_liftplots(x, pairs = list(c("recency", "frequency")), ci = 0.95)
#' out3$plots_pairs[[1]]
#' }
#'
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_histogram geom_errorbar geom_point theme_bw theme
#'   element_blank element_rect labs position_dodge scale_y_continuous after_stat coord_cartesian
#' @importFrom stats qnorm sd
#' @importFrom scales label_percent
#' @importFrom cowplot plot_grid ggdraw draw_label
#' @importFrom dplyr ntile
#' @importFrom flextable flextable set_header_labels colformat_num autofit align
easy_liftplots <- function(x,
                           vars = "all",
                           pairs = NULL,
                           ar = NULL,
                           ci = 0.95,
                           bins = 30,
                           numeric_bins = 5,
                           by_numeric_bins = 3,
                           grid = TRUE,
                           top = NULL,
                           ft = TRUE) {
   
   
   `%||%` <- function(a, b) if (!is.null(a)) a else b
   
   # ---- enforce easy_uplift only ----
   if (!inherits(x, "easy_uplift")) {
      stop("`easy_liftplots()` only accepts an object returned by easy_uplift() (class 'easy_uplift').")
   }
   if (is.null(x$all) || !is.data.frame(x$all)) {
      stop("The easy_uplift object must contain a scored data.frame at `x$all`.")
   }
   
   df <- x$all
   if (!("lift" %in% names(df))) stop("`x$all` must contain a numeric column named `lift`.")
   if (!is.numeric(df$lift)) stop("`x$all$lift` must be numeric.")
   
   # ---- metadata from easy_uplift ----
   meta_treat   <- x$treatment_var %||% (x$spec %||% list())$treatment_var
   meta_outcome <- x$outcome_var   %||% (x$spec %||% list())$outcome_var
   meta_covars  <- x$covariates    %||% (x$spec %||% list())$covariates
   
   # ---- CI handling ----
   if (length(ci) != 1) stop("`ci` must be a single value: 0, 0.90, 0.95, 0.975, or 0.99.")
   if (isTRUE(all.equal(ci, 0))) {
      ci2 <- NA_real_
      caption <- "Error bars represent 1 SD"
   } else if (ci %in% c(0.90, 0.95, 0.975, 0.99)) {
      ci2 <- 0.5 + ci / 2
      caption <- paste0("Error bars represent ", ci * 100, "% CI")
   } else {
      stop("`ci` must be one of: 0, 0.90, 0.95, 0.975, 0.99.")
   }
   
   # ---- helpers ----
   is_binary01 <- function(v) {
      is.numeric(v) && all(stats::na.omit(unique(v)) %in% 0:1)
   }
   
   coerce_binary_to_factor <- function(v) {
      out <- v
      out[out == 0] <- "No"
      out[out == 1] <- "Yes"
      factor(out, levels = c("No", "Yes"))
   }
   
   var_kind <- function(v) {
      if (is_binary01(v)) return("f")
      if (is.numeric(v) || is.integer(v)) return("n")
      if (is.logical(v) || is.factor(v) || is.character(v)) return("f")
      "f"
   }
   
   summarize_main <- function(dat, v) {
      xvar <- dat[[v]]
      kind <- var_kind(xvar)
      
      if (is_binary01(xvar)) {
         dat[[v]] <- coerce_binary_to_factor(xvar)
         kind <- "f"
      }
      
      overall <- mean(dat$lift, na.rm = TRUE)
      
      if (kind == "n") {
         b <- dplyr::ntile(dat[[v]], numeric_bins)
         repx <- stats::aggregate(dat[[v]], list(bin = b), mean)
         repx$x <- round(repx$x, 2)
         names(repx) <- c("bin", "x_rep")
         
         sumdata <- stats::aggregate(dat$lift, list(bin = b), mean)
         names(sumdata) <- c("bin", "mean")
         sumdata$sd <- stats::aggregate(dat$lift, list(bin = b), stats::sd)$x
         sumdata$n  <- stats::aggregate(dat$lift, list(bin = b), length)$x
         
         sumdata <- merge(sumdata, repx, by = "bin", sort = TRUE)
         sumdata$covariate <- factor(sumdata$x_rep)
      } else {
         grp <- dat[[v]]
         sumdata <- stats::aggregate(dat$lift, list(covariate = grp), mean)
         names(sumdata) <- c("covariate", "mean")
         sumdata$sd <- stats::aggregate(dat$lift, list(grp), stats::sd)$x
         sumdata$n  <- stats::aggregate(dat$lift, list(grp), length)$x
         sumdata$covariate <- factor(sumdata$covariate)
      }
      
      if (isTRUE(all.equal(ci, 0))) {
         sumdata$lowerci <- sumdata$mean - sumdata$sd
         sumdata$upperci <- sumdata$mean + sumdata$sd
      } else {
         z <- stats::qnorm(ci2)
         sumdata$lowerci <- sumdata$mean - z * sumdata$sd / sqrt(sumdata$n)
         sumdata$upperci <- sumdata$mean + z * sumdata$sd / sqrt(sumdata$n)
      }
      
      sumdata$cate <- sumdata$mean - overall
      sumdata$w <- sumdata$n / sum(sumdata$n, na.rm = TRUE)
      
      list(
         var = v,
         kind = kind,
         sumdata = sumdata,
         score_wmae = sum(sumdata$w * abs(sumdata$cate), na.rm = TRUE),
         score_maxabs = max(abs(sumdata$cate), na.rm = TRUE),
         n_groups = nrow(sumdata),
         ymin = min(sumdata$lowerci, na.rm = TRUE),
         ymax = max(sumdata$upperci, na.rm = TRUE)
      )
   }
   
   plot_main <- function(obj, ylim = NULL, show_caption = TRUE) {
      sd <- obj$sumdata
      v <- obj$var
      
      p <- ggplot2::ggplot(sd, ggplot2::aes(x = covariate, y = mean)) +
         ggplot2::geom_errorbar(
            ggplot2::aes(ymin = lowerci, ymax = upperci),
            color = "#4f2c1d", width = 0.5, size = 1
         ) +
         ggplot2::geom_point(fill = "#fd5000", color = "#fd5000", size = 3) +
         ggplot2::theme_bw() +
         ggplot2::theme(
            aspect.ratio = ar,
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.minor.x = ggplot2::element_blank(),
            panel.background = ggplot2::element_rect(fill = "white"),
            plot.background = ggplot2::element_rect(fill = "transparent", color = NA_character_)
         ) +
         ggplot2::labs(
            y = "Mean Lift",
            x = v,
            caption = if (isTRUE(show_caption)) caption else NULL
         )
      
      if (!is.null(ylim) && length(ylim) == 2 && all(is.finite(ylim))) {
         p <- p + ggplot2::coord_cartesian(ylim = ylim)
      }
      p
   }
   
   summarize_pair <- function(dat, v1, v2) {
      if (!(v1 %in% names(dat))) stop(paste0("`", v1, "` not found in data."))
      if (!(v2 %in% names(dat))) stop(paste0("`", v2, "` not found in data."))
      
      x1 <- dat[[v1]]; x2 <- dat[[v2]]
      k1 <- var_kind(x1); k2 <- var_kind(x2)
      
      if (is_binary01(x1)) { dat[[v1]] <- coerce_binary_to_factor(x1); k1 <- "f" }
      if (is_binary01(x2)) { dat[[v2]] <- coerce_binary_to_factor(x2); k2 <- "f" }
      
      overall <- mean(dat$lift, na.rm = TRUE)
      
      if (k1 == "n") {
         b1 <- dplyr::ntile(dat[[v1]], numeric_bins)
         rep1 <- stats::aggregate(dat[[v1]], list(bin1 = b1), mean)
         rep1$x <- round(rep1$x, 2)
         names(rep1) <- c("bin1", "x1_rep")
         dat$bin1 <- b1
         dat <- merge(dat, rep1, by = "bin1", sort = TRUE)
         dat$g1 <- factor(dat$x1_rep)
      } else {
         dat$g1 <- factor(dat[[v1]])
      }
      
      if (k2 == "n") {
         b2 <- dplyr::ntile(dat[[v2]], by_numeric_bins)
         rep2 <- stats::aggregate(dat[[v2]], list(bin2 = b2), mean)
         rep2$x <- round(rep2$x, 2)
         names(rep2) <- c("bin2", "x2_rep")
         dat$bin2 <- b2
         dat <- merge(dat, rep2, by = "bin2", sort = TRUE)
         dat$g2 <- factor(dat$x2_rep)
      } else {
         dat$g2 <- factor(dat[[v2]])
      }
      
      sumdata <- stats::aggregate(dat$lift, list(g1 = dat$g1, g2 = dat$g2), mean)
      names(sumdata) <- c("g1", "g2", "mean")
      sumdata$sd <- stats::aggregate(dat$lift, list(dat$g1, dat$g2), stats::sd)$x
      sumdata$n  <- stats::aggregate(dat$lift, list(dat$g1, dat$g2), length)$x
      
      if (isTRUE(all.equal(ci, 0))) {
         sumdata$lowerci <- sumdata$mean - sumdata$sd
         sumdata$upperci <- sumdata$mean + sumdata$sd
      } else {
         z <- stats::qnorm(ci2)
         sumdata$lowerci <- sumdata$mean - z * sumdata$sd / sqrt(sumdata$n)
         sumdata$upperci <- sumdata$mean + z * sumdata$sd / sqrt(sumdata$n)
      }
      
      sumdata$cate <- sumdata$mean - overall
      sumdata$w <- sumdata$n / sum(sumdata$n, na.rm = TRUE)
      
      list(
         v1 = v1, v2 = v2,
         sumdata = sumdata,
         score_wmae = sum(sumdata$w * abs(sumdata$cate), na.rm = TRUE),
         score_maxabs = max(abs(sumdata$cate), na.rm = TRUE),
         n_groups = nrow(sumdata),
         ymin = min(sumdata$lowerci, na.rm = TRUE),
         ymax = max(sumdata$upperci, na.rm = TRUE)
      )
   }
   
   plot_pair <- function(obj, ylim = NULL, show_caption = TRUE) {
      sd <- obj$sumdata
      v1 <- obj$v1
      v2 <- obj$v2
      
      p <- ggplot2::ggplot(sd, ggplot2::aes(x = g1, y = mean, group = g2)) +
         ggplot2::geom_errorbar(
            ggplot2::aes(ymin = lowerci, ymax = upperci, color = g2),
            width = 0.5, size = 1,
            position = ggplot2::position_dodge(width = 0.9)
         ) +
         ggplot2::geom_point(
            ggplot2::aes(fill = g2, color = g2),
            size = 3,
            position = ggplot2::position_dodge(width = 0.9)
         ) +
         ggplot2::theme_bw() +
         ggplot2::theme(
            aspect.ratio = ar,
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.minor.x = ggplot2::element_blank(),
            panel.background = ggplot2::element_rect(fill = "white"),
            plot.background = ggplot2::element_rect(fill = "transparent", color = NA_character_),
            legend.position = "bottom"
         ) +
         ggplot2::labs(
            y = "Mean Lift",
            x = v1,
            color = v2,
            fill = v2,
            caption = if (isTRUE(show_caption)) caption else NULL
         )
      
      if (!is.null(ylim) && length(ylim) == 2 && all(is.finite(ylim))) {
         p <- p + ggplot2::coord_cartesian(ylim = ylim)
      }
      p
   }
   
   paginate_with_caption <- function(plot_list, caption_text, nrow = 2, ncol = 3) {
      if (length(plot_list) == 0) return(list())
      per_page <- min(nrow * ncol, 6L)
      idx <- split(seq_along(plot_list), ceiling(seq_along(plot_list) / per_page))
      
      lapply(idx, function(ii) {
         grid_plot <- cowplot::plot_grid(plotlist = plot_list[ii], nrow = nrow, ncol = ncol)
         
         cap <- cowplot::ggdraw() +
            cowplot::draw_label(
               caption_text,
               x = 0, hjust = 0,
               size = 10
            )
         
         cowplot::plot_grid(grid_plot, cap, ncol = 1, rel_heights = c(1, 0.08))
      })
   }
   
   as_ft_rank_main <- function(rank_df) {
      ftobj <- flextable::flextable(rank_df)
      ftobj <- flextable::set_header_labels(
         ftobj,
         Covariate = "Covariate",
         Type = "Type",
         Importance = "Importance (WMAE)",
         MaxAbsCATE = "Max |CATE|",
         Groups = "# Groups"
      )
      ftobj <- flextable::colformat_num(ftobj, j = c("Importance", "MaxAbsCATE"), digits = 4)
      ftobj <- flextable::align(ftobj, align = "center", part = "all")
      ftobj <- flextable::autofit(ftobj)
      ftobj
   }
   
   as_ft_rank_pairs <- function(rank_df) {
      ftobj <- flextable::flextable(rank_df)
      ftobj <- flextable::set_header_labels(
         ftobj,
         Pair = "Pair",
         Importance = "Importance (WMAE)",
         MaxAbsCATE = "Max |CATE|",
         Groups = "# Cells"
      )
      ftobj <- flextable::colformat_num(ftobj, j = c("Importance", "MaxAbsCATE"), digits = 4)
      ftobj <- flextable::align(ftobj, align = "center", part = "all")
      ftobj <- flextable::autofit(ftobj)
      ftobj
   }
   
   # ---- histogram (always) ----
   p_hist <- ggplot2::ggplot(df, ggplot2::aes(x = lift)) +
      ggplot2::geom_histogram(
         ggplot2::aes(y = ggplot2::after_stat(count / sum(count))),
         fill = "#fd5000", color = "#4f2c1d", bins = bins
      ) +
      ggplot2::scale_y_continuous(labels = scales::label_percent()) +
      ggplot2::labs(y = "% of Customers", x = "Predicted Lift") +
      ggplot2::theme_bw() +
      ggplot2::theme(
         aspect.ratio = ar,
         panel.grid.major.x = ggplot2::element_blank(),
         panel.grid.minor.x = ggplot2::element_blank(),
         panel.background = ggplot2::element_rect(fill = "white"),
         plot.background = ggplot2::element_rect(fill = "transparent", color = NA_character_)
      )
   
   # ---- vars ----
   if (is.null(vars)) {
      vars <- character(0)
   } else if (identical(vars, "all")) {
      if (is.null(meta_covars) || !is.character(meta_covars) || length(meta_covars) == 0) {
         stop("`vars=\"all\"` requires `x$covariates` (or `x$spec$covariates`) to be present on the easy_uplift object.")
      }
      vars <- intersect(meta_covars, names(df))
   } else {
      if (!is.character(vars)) stop("`vars` must be a character vector or \"all\".")
      missing_vars <- setdiff(vars, names(df))
      if (length(missing_vars) > 0) stop("These `vars` are not in `x$all`: ", paste(missing_vars, collapse = ", "))
   }
   
   # safety drop
   non_covariate_cols <- unique(c(
      "lift","out0","out1","out2","rank","group","treat","outcome",
      meta_treat, meta_outcome
   ))
   non_covariate_cols <- non_covariate_cols[!is.na(non_covariate_cols) & nzchar(non_covariate_cols)]
   vars <- setdiff(vars, non_covariate_cols)
   vars <- vars[!grepl("^out\\d+$", vars)]
   
   # ---- main ranking + plots ----
   main_objs <- lapply(vars, function(v) summarize_main(df, v))
   
   rank_main_df <- if (length(main_objs) == 0) {
      data.frame(
         Covariate = character(0),
         Type = character(0),
         Importance = numeric(0),
         MaxAbsCATE = numeric(0),
         Groups = integer(0),
         stringsAsFactors = FALSE
      )
   } else {
      data.frame(
         Covariate = vapply(main_objs, `[[`, character(1), "var"),
         Type = ifelse(vapply(main_objs, `[[`, character(1), "kind") == "n", "Numeric", "Factor"),
         Importance = vapply(main_objs, `[[`, numeric(1), "score_wmae"),
         MaxAbsCATE = vapply(main_objs, `[[`, numeric(1), "score_maxabs"),
         Groups = vapply(main_objs, `[[`, integer(1), "n_groups"),
         stringsAsFactors = FALSE
      )
   }
   
   if (nrow(rank_main_df) > 0) {
      rank_main_df <- rank_main_df[order(rank_main_df$Importance, decreasing = TRUE), , drop = FALSE]
      rownames(rank_main_df) <- NULL
   }
   
   vars_for_plots <- vars
   if (!is.null(top) && is.finite(top) && top > 0 && nrow(rank_main_df) > 0) {
      vars_for_plots <- head(rank_main_df$Covariate, top)
   }
   
   # individual plots (auto y-axis; keep caption)
   plots_main <- list()
   main_map <- list()
   if (length(main_objs) > 0) {
      main_map <- setNames(main_objs, vapply(main_objs, `[[`, character(1), "var"))
   }
   if (length(vars_for_plots) > 0) {
      plots_main <- lapply(vars_for_plots, function(v) plot_main(main_map[[v]], ylim = NULL, show_caption = TRUE))
      names(plots_main) <- vars_for_plots
   }
   
   # page grids (common y-axis; caption only once)
   pages_main <- NULL
   if (isTRUE(grid) && length(vars_for_plots) > 0) {
      ymin <- min(vapply(main_map[vars_for_plots], `[[`, numeric(1), "ymin"), na.rm = TRUE)
      ymax <- max(vapply(main_map[vars_for_plots], `[[`, numeric(1), "ymax"), na.rm = TRUE)
      ylim_common <- c(ymin, ymax)
      
      plots_main_grid <- lapply(vars_for_plots, function(v) plot_main(main_map[[v]], ylim = ylim_common, show_caption = FALSE))
      names(plots_main_grid) <- vars_for_plots
      
      pages_main <- paginate_with_caption(plots_main_grid, caption_text = caption, nrow = 2, ncol = 3)
   }
   
   # ---- pairs ----
   plots_pairs <- list()
   rank_pairs_df <- NULL
   pages_pairs <- NULL
   
   if (!is.null(pairs)) {
      if (!is.list(pairs)) stop("`pairs` must be a list of length-2 character vectors.")
      pairs_clean <- lapply(pairs, function(p) {
         if (!is.character(p) || length(p) != 2) stop("Each element of `pairs` must be a character vector of length 2.")
         p
      })
      
      all_pair_vars <- unique(unlist(pairs_clean))
      missing_pair <- setdiff(all_pair_vars, names(df))
      if (length(missing_pair) > 0) stop("These variables in `pairs` are not in `x$all`: ", paste(missing_pair, collapse = ", "))
      
      bad_pair <- intersect(all_pair_vars, non_covariate_cols)
      if (length(bad_pair) > 0 || any(grepl("^out\\d+$", all_pair_vars))) {
         stop("`pairs` must reference covariates only (not lift/out*/rank/group/treat/outcome).")
      }
      
      pair_objs <- lapply(pairs_clean, function(p) summarize_pair(df, p[[1]], p[[2]]))
      pair_names <- vapply(
         pair_objs,
         function(o) paste0(o$v1, " \u00D7 ", o$v2),
         character(1)
      )
      pair_map <- setNames(pair_objs, pair_names)
      
      rank_pairs_df <- data.frame(
         Pair = pair_names,
         Importance = vapply(pair_objs, `[[`, numeric(1), "score_wmae"),
         MaxAbsCATE = vapply(pair_objs, `[[`, numeric(1), "score_maxabs"),
         Groups = vapply(pair_objs, `[[`, integer(1), "n_groups"),
         stringsAsFactors = FALSE
      )
      rank_pairs_df <- rank_pairs_df[order(rank_pairs_df$Importance, decreasing = TRUE), , drop = FALSE]
      rownames(rank_pairs_df) <- NULL
      
      # individual pair plots (auto y-axis; caption on individual plots)
      plots_pairs <- lapply(pair_names, function(nm) plot_pair(pair_map[[nm]], ylim = NULL, show_caption = TRUE))
      names(plots_pairs) <- pair_names
      
      # pair grids (common y-axis; caption only once)
      if (isTRUE(grid) && length(pair_names) > 0) {
         yminp <- min(vapply(pair_map, `[[`, numeric(1), "ymin"), na.rm = TRUE)
         ymaxp <- max(vapply(pair_map, `[[`, numeric(1), "ymax"), na.rm = TRUE)
         ylim_common_pairs <- c(yminp, ymaxp)
         
         plots_pairs_grid <- lapply(pair_names, function(nm) plot_pair(pair_map[[nm]], ylim = ylim_common_pairs, show_caption = FALSE))
         names(plots_pairs_grid) <- pair_names
         
         pages_pairs <- paginate_with_caption(plots_pairs_grid, caption_text = caption, nrow = 2, ncol = 3)
      }
   }
   
   # ---- flextable outputs ----
   rank_main <- if (isTRUE(ft)) as_ft_rank_main(rank_main_df) else rank_main_df
   rank_pairs <- NULL
   if (!is.null(rank_pairs_df)) {
      rank_pairs <- if (isTRUE(ft)) as_ft_rank_pairs(rank_pairs_df) else rank_pairs_df
   }
   
   list(
      hist = p_hist,
      rank_main = rank_main,
      rank_main_df = rank_main_df,
      plots_main = plots_main,
      pages_main = pages_main,
      rank_pairs = rank_pairs,
      rank_pairs_df = rank_pairs_df,
      plots_pairs = plots_pairs,
      pages_pairs = pages_pairs
   )
}
