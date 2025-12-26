#' @title Easy PCA (Final Retained Solution)
#'
#' @description
#' Fit PCA and return retained-component loadings (unrotated and varimax-rotated),
#' plus eigen diagnostics. Supports optional grouping/aggregation.
#'
#' @details
#' PCA is fit using \code{\link[stats:prcomp]{stats::prcomp}} with
#' \code{center = TRUE} and \code{scale. = TRUE}. If \code{group} is supplied,
#' PCA is fit on mean-aggregated values of \code{vars} by group. Rows (or
#' groups) with missing values in \code{vars} are dropped prior to fitting.
#'
#' Loadings are computed from the retained components and a varimax rotation is
#' applied using \code{\link[stats:varimax]{stats::varimax}}. The \code{Unexplained}
#' column is \eqn{1 -} communality for standardized variables.
#'
#' @param data A data frame containing the full dataset.
#' @param vars Character vector of variable names to use in PCA (required).
#'   All variables must be numeric.
#' @param comp Integer; number of components to retain.
#' @param group Optional; single variable name (string) to aggregate by before PCA.
#' @param ft Logical; if \code{TRUE}, return \code{$unrotated} and \code{$rotated}
#'   as flextables (default = \code{TRUE}).
#'
#' @return A list with components:
#' \describe{
#'   \item{table}{Eigenvalue table (data.frame).}
#'   \item{unrotated}{Unrotated loading table + Unexplained (data.frame or flextable).}
#'   \item{rotated}{Varimax-rotated loading table + Unexplained (data.frame or flextable).}
#'   \item{pcaobj}{The fitted \code{prcomp} object.}
#'   \item{key}{Row ids (ungrouped) or group labels (grouped), aligned to \code{data_used}.}
#'   \item{data_used}{Numeric data actually used in PCA (after preprocessing).}
#' }
#'
#' @examples
#' \dontrun{
#' final <- easy_pca_final(
#'   data = mydata,
#'   vars = c("perform", "leader", "fun", "serious", "bargain", "value"),
#'   comp = 2
#' )
#' final$unrotated
#' final$rotated
#' }
#'
#' @export
#'
#' @importFrom stats prcomp complete.cases varimax
#' @importFrom dplyr group_by summarise across all_of select
#' @importFrom flextable flextable autofit add_header_row bold
#' @importFrom rlang .data
easy_pca_final <- function(data, vars, comp, group = NULL, ft = TRUE) {
   
   # ---- checks ----
   if (!is.data.frame(data)) stop("`data` must be a data frame.")
   if (missing(vars) || is.null(vars) || length(vars) == 0) {
      stop("`vars` is required and must be a non-empty character vector.")
   }
   if (!is.character(vars)) stop("`vars` must be a character vector of column names.")
   if (missing(comp) || length(comp) != 1 || !is.numeric(comp) || comp < 1) {
      stop("`comp` must be a single integer >= 1.")
   }
   comp <- as.integer(comp)
   
   if (!is.logical(ft) || length(ft) != 1) {
      stop("`ft` must be a single logical value (TRUE or FALSE).")
   }
   
   missing_vars <- setdiff(vars, names(data))
   if (length(missing_vars) > 0) {
      stop("These `vars` are not in `data`: ", paste(missing_vars, collapse = ", "))
   }
   
   if (!is.null(group)) {
      if (!is.character(group) || length(group) != 1) stop("`group` must be a single column name string.")
      if (!group %in% names(data)) stop("`group` not found in `data`.")
   }
   
   # Ensure vars are numeric
   non_numeric <- vars[!vapply(data[vars], is.numeric, logical(1))]
   if (length(non_numeric) > 0) {
      stop("All PCA `vars` must be numeric. Non-numeric: ", paste(non_numeric, collapse = ", "))
   }
   
   # ---- build X (and key) ----
   if (!is.null(group)) {
      df_agg <- dplyr::group_by(data, .data[[group]])
      df_agg <- dplyr::summarise(
         df_agg,
         dplyr::across(dplyr::all_of(vars), ~ mean(.x, na.rm = TRUE)),
         .groups = "drop"
      )
      
      key <- df_agg[[group]]
      X <- dplyr::select(df_agg, dplyr::all_of(vars))
   } else {
      key <- seq_len(nrow(data))
      X <- dplyr::select(data, dplyr::all_of(vars))
   }
   
   if (ncol(X) < 2) stop("PCA requires at least 2 variables in `vars`.")
   
   keep <- stats::complete.cases(X)
   X_used <- X[keep, , drop = FALSE]
   key_used <- key[keep]
   
   if (nrow(X_used) < 2) stop("Not enough complete rows to run PCA after dropping missing values.")
   
   p <- ncol(X_used)
   
   # ---- PCA ----
   pcaobj <- stats::prcomp(X_used, scale. = TRUE, center = TRUE)
   
   maxcomp <- ncol(pcaobj$rotation)
   if (comp > maxcomp) stop("`comp` exceeds the maximum number of components.")
   
   # ---- eigen table (full PCA) ----
   eig <- (pcaobj$sdev)^2
   len <- length(eig)
   diff_eig <- c(eig[1:(len - 1)] - eig[2:len], NA_real_)
   prop <- eig / p
   cumprop <- cumsum(prop)
   
   eigtable <- data.frame(
      Component  = seq_len(len),
      Eigenvalue = eig,
      Difference = diff_eig,
      Proportion = prop,
      Cumulative = cumprop
   )
   
   eigtable_fmt <- eigtable
   eigtable_fmt[, -1] <- round(eigtable_fmt[, -1], 4)
   
   # ---- loadings (retain comp) ----
   load_unrot <- pcaobj$rotation[, 1:comp, drop = FALSE] %*%
      diag(pcaobj$sdev[1:comp], nrow = comp)
   
   colnames(load_unrot) <- paste0("Comp_", seq_len(ncol(load_unrot)))
   
   communality <- rowSums(load_unrot^2)
   unexplained <- pmax(0, 1 - communality)
   
   unrotated_df <- as.data.frame(round(load_unrot, 4))
   unrotated_df$Unexplained <- round(unexplained, 4)
   
   # ---- varimax rotation ----
   r <- stats::varimax(load_unrot, normalize = FALSE)
   load_rot <- r$loadings[, 1:comp, drop = FALSE]
   
   colnames(load_rot)   <- paste0("Comp_", seq_len(ncol(load_rot)))
   
   rotated_df <- as.data.frame(round(load_rot, 4))
   rotated_df$Unexplained <- round(unexplained, 4)
   
   unrotated_out <- unrotated_df
   rotated_out <- rotated_df
   
   if (ft) {
      # preserve row names in a first column
      unrotated_ft_df <- data.frame(
         Variable = rownames(unrotated_df),
         unrotated_df,
         row.names = NULL,
         check.names = FALSE
      )
      
      rotated_ft_df <- data.frame(
         Variable = rownames(rotated_df),
         rotated_df,
         row.names = NULL,
         check.names = FALSE
      )
      
      unrotated_out <- flextable::flextable(unrotated_ft_df) |>
         flextable::add_header_row(
            values = "Unrotated PCA Loadings",
            colwidths = ncol(unrotated_ft_df)
         ) |>
         flextable::bold(part = "header") |>
         flextable::autofit()
      
      rotated_out <- flextable::flextable(rotated_ft_df) |>
         flextable::add_header_row(
            values = "Varimax-Rotated PCA Loadings",
            colwidths = ncol(rotated_ft_df)
         ) |>
         flextable::bold(part = "header") |>
         flextable::autofit()
   }
   
   list(
      table = eigtable_fmt,
      unrotated = unrotated_out,
      rotated = rotated_out,
      pcaobj = pcaobj,
      key = key_used,
      data_used = X_used
   )
}
