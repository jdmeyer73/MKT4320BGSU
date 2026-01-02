#' @title Predicted Probabilities and Marginal Effects for Alternative-Specific MNL
#'
#' @description
#' Compute predicted probability tables, margin plots, and (optionally)
#' marginal effects for a fitted alternative-specific multinomial logit
#' model estimated via \code{mlogit}.
#'
#' This function replaces the legacy functions \code{asmnl_me()} and
#' \code{asmnl_mp()} by providing a single, unified interface.
#'
#' @details
#' The function operates on a fitted \code{mlogit} object and computes results
#' for one focal variable at a time. Depending on arguments, it can return:
#' \itemize{
#'   \item predicted probability tables,
#'   \item margin plots,
#'   \item average marginal effects (AME) or marginal effects at means.
#' }
#'
#' Case-specific and alternative-specific focal variables are handled
#' automatically based on \code{focal_type}.
#'
#' @param model A fitted \code{mlogit} model.
#' @param focal_var Character string. Name of the focal variable.
#' @param focal_type Character; one of \code{"case"}, \code{"alt"}, or
#'   \code{"auto"} (default).
#' @param grid_n Integer; number of points used to construct the grid of focal
#'   values for predicted probability plots when the focal variable is
#'   continuous (default = 25).
#' @param marginal Logical; if \code{TRUE}, compute marginal effects.
#' @param me_method Character; \code{"observed"} (AME, default) or
#'   \code{"means"}.
#' @param me_step Numeric; finite-difference step size for AME.
#' @param ft Logical; if \code{TRUE}, return tables as \code{flextable} objects.
#' @param digits Integer; rounding for numeric output.
#'
#' @return A list with components that may include:
#' \itemize{
#'   \item \code{pp_table}: predicted probability table,
#'   \item \code{pp_plot}: margin plot (\code{ggplot}),
#'   \item \code{me_table}: marginal effects table (if requested),
#'   \item \code{me_table_df}: marginal effects as a plain data frame.
#' }
#'
#' @examples
#' # res <- pp_as_mnl(mod, focal_var = "income")
#' # res$pp_plot
#'
#' # res_me <- pp_as_mnl(mod, focal_var = "income", marginal = TRUE)
#' # res_me$me_table
#'
#' @importFrom stats fitted
#' @importFrom dfidx idx
#' @importFrom ggplot2 ggplot aes geom_line labs theme_bw
#' @importFrom dplyr arrange mutate
#' @importFrom flextable flextable autofit
#'
#' @export

pp_as_mnl <- function(model,
                      focal_var,
                      focal_type = c("auto", "alt", "case"),
                      grid_n = 25,
                      digits = 4,
                      ft = FALSE,
                      marginal = TRUE,
                      me_method = c("observed", "means"),
                      me_step = 1) {
   
   # ----------------------------
   # Validate model + packages
   # ----------------------------
   if (!inherits(model, "mlogit")) stop("`model` must be a fitted mlogit model (class 'mlogit').", call. = FALSE)
   
   if (!requireNamespace("dfidx", quietly = TRUE)) stop("Package 'dfidx' is required.", call. = FALSE)
   if (!requireNamespace("mlogit", quietly = TRUE)) stop("Package 'mlogit' is required.", call. = FALSE)
   if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.", call. = FALSE)
   if (!requireNamespace("tidyr", quietly = TRUE)) stop("Package 'tidyr' is required.", call. = FALSE)
   if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.", call. = FALSE)
   
   if (isTRUE(ft) && !requireNamespace("flextable", quietly = TRUE)) {
      stop("Package 'flextable' is required when ft=TRUE.", call. = FALSE)
   }
   if (isTRUE(ft) && !requireNamespace("officer", quietly = TRUE)) {
      stop("Package 'officer' is required when ft=TRUE (for text styling).", call. = FALSE)
   }
   
   focal_type <- match.arg(focal_type)
   me_method  <- match.arg(me_method)
   # training data only
   df_use <- model$model
   if (is.null(df_use) || !inherits(df_use, "dfidx")) stop("Training dfidx data not found in model$model.", call. = FALSE)
   label_data <- "Model data"
   
   # ----------------------------
   # Helpers
   # ----------------------------
   get_case_alt <- function(dfidx_obj) {
      idxm <- dfidx::idx(dfidx_obj)
      if (is.null(idxm) || ncol(idxm) < 2) stop("Could not read dfidx index (case/alt).", call. = FALSE)
      list(case = as.character(idxm[, 1]), alt = as.character(idxm[, 2]))
   }
   
   row_prob_from_output <- function(dfidx_obj, probs) {
      ca <- get_case_alt(dfidx_obj)
      n  <- length(ca$case)
      
      # case-level matrix: rows=cases, cols=alts
      if (is.matrix(probs) && nrow(probs) != n) {
         if (is.null(colnames(probs))) stop("Probability matrix has no column names (alts).", call. = FALSE)
         rn <- rownames(probs)
         if (is.null(rn)) stop("Probability matrix has no row names (cases).", call. = FALSE)
         
         r <- match(ca$case, rn)
         c <- match(ca$alt, colnames(probs))
         if (anyNA(r) || anyNA(c)) stop("Could not align probability matrix to dfidx case/alt.", call. = FALSE)
         return(as.numeric(probs[cbind(r, c)]))
      }
      
      # long-row matrix
      if (is.matrix(probs) && nrow(probs) == n) {
         if (ncol(probs) == 1) return(as.numeric(probs[, 1]))
         if (!is.null(colnames(probs)) && all(ca$alt %in% colnames(probs))) {
            return(as.numeric(probs[cbind(seq_len(n), match(ca$alt, colnames(probs)))]))
         }
         stop("Unrecognized long-row probability matrix shape.", call. = FALSE)
      }
      
      # long-row vector
      if (is.vector(probs) && length(probs) == n) return(as.numeric(probs))
      
      stop("Unrecognized probability output from predict.mlogit.", call. = FALSE)
   }
   
   probs_to_case_matrix <- function(dfidx_obj, probs) {
      ca <- get_case_alt(dfidx_obj)
      cases <- unique(ca$case)
      alts  <- unique(ca$alt)
      
      p_row <- row_prob_from_output(dfidx_obj, probs)
      
      M <- matrix(NA_real_, nrow = length(cases), ncol = length(alts),
                  dimnames = list(cases, alts))
      r <- match(ca$case, cases)
      c <- match(ca$alt, alts)
      M[cbind(r, c)] <- p_row
      M
   }
   
   case_prob_matrix <- function(df_scn) {
      probs <- tryCatch(
         predict(model, newdata = df_scn, type = "probabilities"),
         error = function(e) stop(paste0("predict.mlogit failed: ", conditionMessage(e)), call. = FALSE)
      )
      probs_to_case_matrix(df_scn, probs)
   }
   
   mean_probs_for_dfidx <- function(df_scn) {
      colMeans(case_prob_matrix(df_scn), na.rm = TRUE)
   }
   
   set_value_for_alt_numeric <- function(df_scn, ii_alt_rows, val) {
      df_scn[[focal_var]][ii_alt_rows] <- as.numeric(val)
      df_scn[[focal_var]] <- as.numeric(df_scn[[focal_var]])
      df_scn
   }
   
   set_value_for_all_rows_numeric <- function(df_scn, val) {
      df_scn[[focal_var]] <- as.numeric(val)
      df_scn
   }
   
   add_step_for_alt_rows <- function(df_scn, ii_alt_rows, step) {
      df_scn[[focal_var]] <- as.numeric(df_scn[[focal_var]])
      df_scn[[focal_var]][ii_alt_rows] <- df_scn[[focal_var]][ii_alt_rows] + as.numeric(step)
      df_scn
   }
   
   add_step_for_all_rows <- function(df_scn, step) {
      df_scn[[focal_var]] <- as.numeric(df_scn[[focal_var]]) + as.numeric(step)
      df_scn
   }
   
   build_zt_means <- function(dfidx_obj, mod_obj) {
      idx_alt <- dfidx::idx(mod_obj, 2)
      dd <- as.data.frame(dfidx_obj)
      
      keep <- vapply(dd, function(x) is.numeric(x) || is.integer(x) || is.logical(x), logical(1))
      dd_num <- dd[, keep, drop = FALSE]
      for (nm in names(dd_num)) if (is.logical(dd_num[[nm]])) dd_num[[nm]] <- as.numeric(dd_num[[nm]])
      
      zt <- sapply(dd_num, function(x) tapply(x, idx_alt, mean, na.rm = TRUE))
      as.data.frame(zt, stringsAsFactors = FALSE)
   }
   
   in_tol <- function(x, vals, tol = 1e-8) {
      x <- as.numeric(x)
      vals <- as.numeric(vals)
      vapply(x, function(xx) any(abs(xx - vals) < tol), logical(1))
   }
   
   detect_case_specific <- function(dfidx_obj, varname) {
      ca <- get_case_alt(dfidx_obj)
      x  <- as.numeric(dfidx_obj[[varname]])
      uniq_by_case <- tapply(x, ca$case, function(v) length(unique(v[is.finite(v)])))
      all(uniq_by_case <= 1, na.rm = TRUE)
   }
   
   # ----------------------------
   # Validate focal input (NUMERIC ONLY)
   # ----------------------------
   if (!is.character(focal_var) || length(focal_var) != 1) {
      stop("`focal_var` must be a single character string.", call. = FALSE)
   }
   if (!(focal_var %in% names(df_use))) {
      stop(paste0("`focal_var` not found in the dfidx data: ", focal_var), call. = FALSE)
   }
   
   x0 <- df_use[[focal_var]]
   
   if (is.factor(x0) || is.character(x0) || is.logical(x0)) {
      stop(
         paste0(
            "All variables must be numeric. `", focal_var, "` is not numeric.\n",
            "Convert factor/character variables into numeric dummy variables before fitting the model (e.g., fastDummies::dummy_cols())."
         ),
         call. = FALSE
      )
   }
   if (!(is.numeric(x0) || is.integer(x0))) {
      stop(paste0("`", focal_var, "` must be numeric/integer."), call. = FALSE)
   }
   if (all(is.na(x0))) stop(paste0("`", focal_var, "` contains only NA values."), call. = FALSE)
   
   ca0 <- get_case_alt(df_use)
   alts_present <- sort(unique(ca0$alt))
   
   # Determine focal type
   focal_is_case <- if (identical(focal_type, "auto")) {
      detect_case_specific(df_use, focal_var)
   } else if (identical(focal_type, "case")) {
      TRUE
   } else {
      FALSE
   }
   focal_type_final <- if (isTRUE(focal_is_case)) "case" else "alt"
   
   # Global binary check
   x_all <- as.numeric(df_use[[focal_var]])
   x_all <- x_all[is.finite(x_all)]
   uniq_all <- sort(unique(x_all))
   is_binary_global <- length(uniq_all) == 2
   
   # ----------------------------
   # Scenario engine for plots/tables
   # ----------------------------
   out_list <- NULL
   at_table_map <- NULL
   at_plot_map <- NULL
   
   # For alt-specific only
   is_binary_alt <- setNames(rep(FALSE, length(alts_present)), alts_present)
   binary_vals_alt <- vector("list", length(alts_present))
   names(binary_vals_alt) <- alts_present
   
   if (identical(focal_type_final, "alt")) {
      
      out_list <- vector("list", length(alts_present))
      names(out_list) <- alts_present
      
      at_table_map <- vector("list", length(alts_present))
      names(at_table_map) <- alts_present
      
      at_plot_map <- vector("list", length(alts_present))
      names(at_plot_map) <- alts_present
      
      for (v_alt in alts_present) {
         
         ii_v_alt <- which(ca0$alt == v_alt)
         
         x_alt_all <- as.numeric(df_use[[focal_var]][ii_v_alt])
         x_alt <- x_alt_all[is.finite(x_alt_all)]
         if (length(x_alt) == 0) stop(paste0("No finite values for `", focal_var, "` within alternative '", v_alt, "'."), call. = FALSE)
         
         uniq_alt <- sort(unique(x_alt))
         
         if (length(uniq_alt) == 2) {
            
            is_binary_alt[[v_alt]] <- TRUE
            binary_vals_alt[[v_alt]] <- uniq_alt
            
            at_plot_alt  <- uniq_alt
            at_table_alt <- uniq_alt
            focal_values_all <- uniq_alt
            
         } else {
            
            rng_alt <- range(x_alt, na.rm = TRUE)
            mu_alt  <- mean(x_alt, na.rm = TRUE)
            sd_alt  <- stats::sd(x_alt, na.rm = TRUE)
            
            if (!is.finite(sd_alt) || sd_alt == 0) {
               x_min <- rng_alt[1]
               x_max <- rng_alt[2]
            } else {
               x_min <- max(mu_alt - 2 * sd_alt, rng_alt[1])
               x_max <- min(mu_alt + 2 * sd_alt, rng_alt[2])
               if (!is.finite(x_min) || !is.finite(x_max) || x_min == x_max) {
                  x_min <- rng_alt[1]
                  x_max <- rng_alt[2]
               }
            }
            at_plot_alt <- seq(from = x_min, to = x_max, length.out = grid_n)
            
            at_table_alt <- mu_alt + c(-2, -1, 0, 1, 2)
            at_table_alt <- pmin(pmax(at_table_alt, rng_alt[1]), rng_alt[2])
            at_table_alt <- sort(unique(as.numeric(at_table_alt)))
            
            focal_values_all <- sort(unique(as.numeric(c(at_plot_alt, at_table_alt))))
         }
         
         at_plot_map[[v_alt]]  <- at_plot_alt
         at_table_map[[v_alt]] <- at_table_alt
         
         mean_probs_list <- vector("list", length(focal_values_all))
         for (i in seq_along(focal_values_all)) {
            val <- focal_values_all[i]
            df_scn <- set_value_for_alt_numeric(df_use, ii_v_alt, val)
            mean_probs_list[[i]] <- list(focal_value = val, mp = mean_probs_for_dfidx(df_scn))
         }
         
         out_list[[v_alt]] <- mean_probs_list
      }
      
   } else {
      
      # Case-specific
      x_cs <- as.numeric(df_use[[focal_var]])
      x_cs <- x_cs[is.finite(x_cs)]
      if (length(x_cs) == 0) stop(paste0("No finite values for `", focal_var, "`."), call. = FALSE)
      
      uniq_cs <- sort(unique(x_cs))
      
      if (length(uniq_cs) == 2) {
         at_plot_cs  <- uniq_cs
         at_table_cs <- uniq_cs
         focal_values_all <- uniq_cs
      } else {
         rng_cs <- range(x_cs, na.rm = TRUE)
         mu_cs  <- mean(x_cs, na.rm = TRUE)
         sd_cs  <- stats::sd(x_cs, na.rm = TRUE)
         
         if (!is.finite(sd_cs) || sd_cs == 0) {
            x_min <- rng_cs[1]
            x_max <- rng_cs[2]
         } else {
            x_min <- max(mu_cs - 2 * sd_cs, rng_cs[1])
            x_max <- min(mu_cs + 2 * sd_cs, rng_cs[2])
            if (!is.finite(x_min) || !is.finite(x_max) || x_min == x_max) {
               x_min <- rng_cs[1]
               x_max <- rng_cs[2]
            }
         }
         at_plot_cs <- seq(from = x_min, to = x_max, length.out = grid_n)
         
         at_table_cs <- mu_cs + c(-2, -1, 0, 1, 2)
         at_table_cs <- pmin(pmax(at_table_cs, rng_cs[1]), rng_cs[2])
         at_table_cs <- sort(unique(as.numeric(at_table_cs)))
         
         focal_values_all <- sort(unique(as.numeric(c(at_plot_cs, at_table_cs))))
      }
      
      at_plot_map  <- list(case = at_plot_cs)
      at_table_map <- list(case = at_table_cs)
      
      mean_probs_list <- vector("list", length(focal_values_all))
      for (i in seq_along(focal_values_all)) {
         val <- focal_values_all[i]
         df_scn <- set_value_for_all_rows_numeric(df_use, val)
         mean_probs_list[[i]] <- list(focal_value = val, mp = mean_probs_for_dfidx(df_scn))
      }
      
      out_list <- list(case = mean_probs_list)
   }
   
   # ----------------------------
   # Build long data (plot/table)
   # ----------------------------
   choice_alts <- names(out_list[[1]][[1]]$mp)
   
   if (identical(focal_type_final, "alt")) {
      
      pp_long <- do.call(
         rbind,
         lapply(names(out_list), function(v_alt) {
            ml <- out_list[[v_alt]]
            do.call(
               rbind,
               lapply(seq_along(ml), function(i) {
                  fv <- ml[[i]]$focal_value
                  mp <- ml[[i]]$mp
                  data.frame(
                     varied_alt  = v_alt,
                     focal_value = as.numeric(fv),
                     choice_alt  = names(mp),
                     mean_prob   = as.numeric(mp),
                     stringsAsFactors = FALSE
                  )
               })
            )
         })
      )
      
      pp_long <- dplyr::arrange(pp_long, varied_alt, choice_alt, focal_value)
      
   } else {
      
      ml <- out_list[[1]]
      pp_long <- do.call(
         rbind,
         lapply(seq_along(ml), function(i) {
            fv <- ml[[i]]$focal_value
            mp <- ml[[i]]$mp
            data.frame(
               focal_value = as.numeric(fv),
               choice_alt  = names(mp),
               mean_prob   = as.numeric(mp),
               stringsAsFactors = FALSE
            )
         })
      )
      pp_long <- dplyr::arrange(pp_long, choice_alt, focal_value)
   }
   
   # ----------------------------
   # Probability table
   # ----------------------------
   if (identical(focal_type_final, "alt")) {
      
      pp_tbl_long <- pp_long
      keep_tbl <- rep(FALSE, nrow(pp_tbl_long))
      
      for (v_alt in alts_present) {
         ii <- which(pp_tbl_long$varied_alt == v_alt)
         tbl_vals <- at_table_map[[v_alt]]
         keep_tbl[ii] <- in_tol(pp_tbl_long$focal_value[ii], tbl_vals, tol = 1e-8)
      }
      
      pp_tbl_long <- pp_tbl_long[keep_tbl, , drop = FALSE]
      
      pp_table_df <- tidyr::pivot_wider(
         pp_tbl_long,
         id_cols = c(varied_alt, focal_value),
         names_from = choice_alt,
         values_from = mean_prob
      )
      
      pp_table_df <- dplyr::arrange(pp_table_df, varied_alt, as.numeric(focal_value))
      
      prob_cols <- setdiff(names(pp_table_df), c("varied_alt", "focal_value"))
      for (cc in prob_cols) pp_table_df[[cc]] <- round(pp_table_df[[cc]], digits)
      
   } else {
      
      pp_tbl_long <- pp_long
      tbl_vals <- at_table_map[[1]]
      keep_tbl <- in_tol(pp_tbl_long$focal_value, tbl_vals, tol = 1e-8)
      pp_tbl_long <- pp_tbl_long[keep_tbl, , drop = FALSE]
      
      pp_table_df <- tidyr::pivot_wider(
         pp_tbl_long,
         id_cols = c(focal_value),
         names_from = choice_alt,
         values_from = mean_prob
      )
      
      pp_table_df <- dplyr::arrange(pp_table_df, as.numeric(focal_value))
      
      prob_cols <- setdiff(names(pp_table_df), c("focal_value"))
      for (cc in prob_cols) pp_table_df[[cc]] <- round(pp_table_df[[cc]], digits)
   }
   
   pp_table <- pp_table_df
   
   if (isTRUE(ft)) {
      ft_tbl <- flextable::flextable(pp_table_df)
      
      ft_tbl <- flextable::add_header_lines(
         ft_tbl,
         values = paste0("Predicted Probability Table (", focal_var, ") - ", label_data)
      )
      
      # Footer: formatted variable name (bold + italic)
      ft_tbl <- flextable::add_footer_lines(ft_tbl, values = " ")  # placeholder for compose()
      
      ft_tbl <- flextable::compose(
         ft_tbl,
         part = "footer",
         i = 1,
         j = 1,
         value = flextable::as_paragraph(
            "Because ",
            flextable::as_chunk(
               focal_var,
               props = officer::fp_text(bold = TRUE, italic = TRUE)
            ),
            if (isTRUE(is_binary_global)) {
               " is binary, only the two observed values are shown."
            } else {
               " is continuous, the values shown include the mean, +/- 1 unit, and +/- 2 units."
            }
         )
      )
      
      ft_tbl <- flextable::autofit(ft_tbl)
      pp_table <- ft_tbl
   }
   
   # ----------------------------
   # Plot data
   # ----------------------------
   if (identical(focal_type_final, "alt")) {
      
      pp_plot_long <- pp_long
      keep_plot <- rep(FALSE, nrow(pp_plot_long))
      for (v_alt in alts_present) {
         ii <- which(pp_plot_long$varied_alt == v_alt)
         plot_vals <- at_plot_map[[v_alt]]
         keep_plot[ii] <- in_tol(pp_plot_long$focal_value[ii], plot_vals, tol = 1e-8)
      }
      pp_plot_long <- pp_plot_long[keep_plot, , drop = FALSE]
      pp_plot_long <- dplyr::arrange(pp_plot_long, varied_alt, choice_alt, focal_value)
      
      vline_df <- NULL
      if (!isTRUE(is_binary_global)) {
         vline_df <- do.call(
            rbind,
            lapply(alts_present, function(v_alt) {
               ii_v_alt <- which(ca0$alt == v_alt)
               x_alt <- as.numeric(df_use[[focal_var]][ii_v_alt])
               x_alt <- x_alt[is.finite(x_alt)]
               data.frame(varied_alt = v_alt, mean_x = mean(x_alt, na.rm = TRUE), stringsAsFactors = FALSE)
            })
         )
      }
      
      pp_plot <- ggplot2::ggplot(
         pp_plot_long,
         ggplot2::aes(x = focal_value, y = mean_prob, group = choice_alt, color = choice_alt)
      ) +
         ggplot2::geom_line()
      
      if (!is.null(vline_df)) {
         pp_plot <- pp_plot +
            ggplot2::geom_vline(data = vline_df, ggplot2::aes(xintercept = mean_x), linetype = "dashed")
      }
      
      pp_plot <- pp_plot +
         ggplot2::facet_wrap(~ varied_alt, scales = "free_x") +
         ggplot2::labs(
            x = focal_var,
            y = "Mean predicted probability",
            title = paste0("Predicted Probabilities - ", label_data),
            subtitle = paste0("Alt-specific focal variable: vary ", focal_var, " for one alternative at a time")
         ) +
         ggplot2::theme_bw()
      
   } else {
      
      pp_plot_long <- pp_long
      plot_vals <- at_plot_map[[1]]
      keep_plot <- in_tol(pp_plot_long$focal_value, plot_vals, tol = 1e-8)
      pp_plot_long <- pp_plot_long[keep_plot, , drop = FALSE]
      pp_plot_long <- dplyr::arrange(pp_plot_long, choice_alt, focal_value)
      
      vline_x <- NULL
      if (!isTRUE(is_binary_global)) {
         x_cs <- as.numeric(df_use[[focal_var]])
         x_cs <- x_cs[is.finite(x_cs)]
         vline_x <- mean(x_cs, na.rm = TRUE)
      }
      
      pp_plot <- ggplot2::ggplot(
         pp_plot_long,
         ggplot2::aes(x = focal_value, y = mean_prob, group = choice_alt, color = choice_alt)
      ) +
         ggplot2::geom_line()
      
      if (!is.null(vline_x) && is.finite(vline_x)) {
         pp_plot <- pp_plot +
            ggplot2::geom_vline(xintercept = vline_x, linetype = "dashed")
      }
      
      pp_plot <- pp_plot +
         ggplot2::labs(
            x = focal_var,
            y = "Mean predicted probability",
            title = paste0("Predicted Probabilities - ", label_data),
            subtitle = paste0("Case-specific focal variable: vary ", focal_var, " consistently across alternatives within each case")
         ) +
         ggplot2::theme_bw()
   }
   
   # Binary padding
   if (isTRUE(is_binary_global)) {
      pp_plot <- pp_plot +
         ggplot2::scale_x_continuous(
            breaks = uniq_all,
            expand = ggplot2::expansion(mult = 0.25)
         )
   }
   
   # ----------------------------
   # Marginal effects
   # ----------------------------
   me_table_df <- NULL
   me_table <- NULL
   
   if (isTRUE(marginal)) {
      
      if (identical(me_method, "means")) {
         
         if (!exists("effects.mlogit", envir = asNamespace("mlogit"), inherits = FALSE)) {
            stop("effects.mlogit() not found in the installed mlogit package.", call. = FALSE)
         }
         
         zt <- build_zt_means(df_use, model)
         
         if (!(focal_var %in% names(zt))) {
            warning(
               paste0("For me_method='means', zt contains only numeric/logical variables. ",
                      focal_var, " is not available there, so marginal effects at means cannot be computed. Returning NULL.")
            )
         } else {
            
            eff_fun <- get("effects.mlogit", envir = asNamespace("mlogit"), inherits = FALSE)
            
            eff <- tryCatch(
               eff_fun(object = model, covariate = focal_var, type = "aa", data = zt),
               error = function(e) stop(paste0("effects.mlogit failed: ", conditionMessage(e)), call. = FALSE)
            )
            
            me_mat <- as.matrix(eff)
            
            if (identical(focal_type_final, "case")) {
               # Case-specific: single row, brand columns (NO "Effect" column)
               ame_means <- as.numeric(me_mat[, 1])
               names(ame_means) <- rownames(me_mat)
               me_df <- as.data.frame(t(round(ame_means, digits)), stringsAsFactors = FALSE)
            } else {
               # Alt-specific: default row-per-varied-alternative format
               me_df <- as.data.frame(round(me_mat, digits))
               row_lab <- rownames(me_df)
               rownames(me_df) <- NULL
               me_df <- cbind(Alternative = row_lab, me_df)
            }
            
            me_table_df <- me_df
            me_table <- me_table_df
            
            if (isTRUE(ft)) {
               ft_me <- flextable::flextable(me_table_df)
               
               ft_me <- flextable::add_header_lines(
                  ft_me,
                  values = paste0("Marginal effects for ", focal_var, " (at means)")
               )
               
               ft_me <- flextable::compose(
                  ft_me,
                  part = "header",
                  i = 1,
                  j = 1,
                  value = flextable::as_paragraph(
                     "Marginal effects for ",
                     flextable::as_chunk(focal_var, props = officer::fp_text(bold = TRUE, italic = TRUE)),
                     " (at means)"
                  )
               )
               
               ft_me <- flextable::autofit(ft_me)
               me_table <- ft_me
            }
         }
         
      } else {
         
         # Observed-values AME
         P0 <- case_prob_matrix(df_use)   # cases x alts
         alt_names <- colnames(P0)
         
         if (identical(focal_type_final, "alt")) {
            
            # AME rows: one row per "varied alternative"
            me_mat <- matrix(NA_real_, nrow = length(alts_present), ncol = length(alt_names),
                             dimnames = list(alts_present, alt_names))
            
            for (v_alt in alts_present) {
               
               ii_v_alt <- which(ca0$alt == v_alt)
               
               x_alt_all <- as.numeric(df_use[[focal_var]][ii_v_alt])
               x_alt <- x_alt_all[is.finite(x_alt_all)]
               uniq_alt <- sort(unique(x_alt))
               
               if (length(uniq_alt) == 2) {
                  lo <- uniq_alt[1]
                  hi <- uniq_alt[2]
                  
                  df_lo <- set_value_for_alt_numeric(df_use, ii_v_alt, lo)
                  df_hi <- set_value_for_alt_numeric(df_use, ii_v_alt, hi)
                  
                  P_lo <- case_prob_matrix(df_lo)
                  P_hi <- case_prob_matrix(df_hi)
                  
                  dP <- P_hi - P_lo
                  
               } else {
                  df_plus <- add_step_for_alt_rows(df_use, ii_v_alt, me_step)
                  P_plus <- case_prob_matrix(df_plus)
                  dP <- P_plus - P0
               }
               
               me_mat[v_alt, ] <- colMeans(dP, na.rm = TRUE)
            }
            
            me_df <- as.data.frame(round(me_mat, digits))
            me_df <- cbind(Varied_Alt = rownames(me_df), me_df)
            rownames(me_df) <- NULL
            
         } else {
            
            # Case-specific AME: single row, brand columns (NO "Effect" column)
            if (isTRUE(is_binary_global)) {
               lo <- uniq_all[1]
               hi <- uniq_all[2]
               
               df_lo <- set_value_for_all_rows_numeric(df_use, lo)
               df_hi <- set_value_for_all_rows_numeric(df_use, hi)
               
               P_lo <- case_prob_matrix(df_lo)
               P_hi <- case_prob_matrix(df_hi)
               
               dP <- P_hi - P_lo
               
            } else {
               df_plus <- add_step_for_all_rows(df_use, me_step)
               P_plus <- case_prob_matrix(df_plus)
               dP <- P_plus - P0
            }
            
            ame_vec <- colMeans(dP, na.rm = TRUE)
            me_df <- as.data.frame(t(round(ame_vec, digits)), stringsAsFactors = FALSE)
         }
         
         me_table_df <- me_df
         me_table <- me_table_df
         
         if (isTRUE(ft)) {
            ft_me <- flextable::flextable(me_table_df)
            
            ft_me <- flextable::add_header_lines(
               ft_me,
               values = paste0("Marginal effects for ", focal_var, " (at observed values)")
            )
            
            ft_me <- flextable::compose(
               ft_me,
               part = "header",
               i = 1,
               j = 1,
               value = flextable::as_paragraph(
                  "Marginal effects for ",
                  flextable::as_chunk(focal_var, props = officer::fp_text(bold = TRUE, italic = TRUE)),
                  " (at observed values)"
               )
            )
            
            ft_me <- flextable::autofit(ft_me)
            me_table <- ft_me
         }
      }
   }
   
   settings <- list(
      focal_var = focal_var,
      focal_type = focal_type_final,
      grid_n = grid_n,
      data_label = label_data,
      varied_alternatives = alts_present,
      marginal = marginal,
      me_method = me_method,
      me_step = me_step,
      is_binary_global = is_binary_global,
      note = "All variables must be numeric; convert factors to dummy variables before fitting the model."
   )
   
   res <- list(
      mnmodel = model,
      settings = settings,
      pp_table = pp_table,
      pp_table_df = pp_table_df,
      pp_long = pp_long,
      pp_plot = pp_plot,
      me_table = me_table,
      me_table_df = me_table_df
   )
   
   class(res) <- "pp_as_mnl"
   res
}

#' @export
print.pp_as_mnl <- function(x, ...) {
   
   cat("Predicted Probability Plots and Tables (MNL)\n")
   cat("Focal variable: ", x$settings$focal_var, "\n", sep = "")
   cat("Focal type: ", x$settings$focal_type, "\n", sep = "")
   cat("Data: ", x$settings$data_label, "\n", sep = "")
   cat("Marginal effects method: ", x$settings$me_method, "\n\n", sep = "")
   
   if (inherits(x$pp_table, "flextable")) {
      print(x$pp_table, ...)
   } else {
      print(x$pp_table, row.names = FALSE)
   }
   
   cat("\n")
   print(x$pp_plot)
   
   if (!is.null(x$me_table)) {
      cat("\n")
      if (inherits(x$me_table, "flextable")) {
         print(x$me_table, ...)
      } else {
         print(x$me_table, row.names = FALSE)
      }
   }
   
   invisible(x)
}
