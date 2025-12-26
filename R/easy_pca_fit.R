#' @title Easy PCA (Fit and Diagnostics)
#'
#' @description
#' Fit a principal components analysis (PCA) model and return diagnostics to
#' help choose how many components to retain.
#'
#' @details
#' PCA is fit using \code{\link[stats:prcomp]{stats::prcomp}} with
#' \code{center = TRUE} and \code{scale. = TRUE}. If \code{group} is supplied,
#' PCA is fit on mean-aggregated values of \code{vars} by group. Rows (or
#' groups) with missing values in \code{vars} are dropped prior to fitting.
#'
#' @param data A data frame containing the full dataset.
#' @param vars Character vector of variable names to use in PCA (required).
#'   All variables must be numeric.
#' @param group Optional; single variable name (string) to aggregate by before PCA.
#' @param ft Logical; if \code{TRUE}, return \code{$table} as a flextable
#'   (default = \code{TRUE}).
#'
#' @return A list with components:
#' \describe{
#'   \item{table}{Eigenvalue table (data.frame or flextable).}
#'   \item{plot}{Scree plot (ggplot).}
#'   \item{pcaobj}{The fitted \code{prcomp} object.}
#'   \item{key}{Row ids (ungrouped) or group labels (grouped), aligned to \code{data_used}.}
#'   \item{data_used}{Numeric data actually used in PCA (after preprocessing).}
#' }
#'
#' @examples
#' \dontrun{
#' fit <- easy_pca_fit(
#'   data = mydata,
#'   vars = c("perform", "leader", "fun", "serious", "bargain", "value")
#' )
#' fit$table
#' fit$plot
#'
#' # Grouped PCA
#' fit_g <- easy_pca_fit(mydata, vars = c("x1","x2","x3"), group = "brand")
#' }
#'
#' @export
#'
#' @importFrom stats prcomp complete.cases
#' @importFrom ggplot2 ggplot aes geom_point geom_line scale_x_continuous theme_bw
#' @importFrom dplyr group_by summarise across all_of select
#' @importFrom flextable flextable autofit
#' @importFrom rlang .data
easy_pca_fit <- function(data, vars, group = NULL, ft = TRUE) {
   
   # ---- checks ----
   if (!is.data.frame(data)) stop("`data` must be a data frame.")
   if (missing(vars) || is.null(vars) || length(vars) == 0) {
      stop("`vars` is required and must be a non-empty character vector.")
   }
   if (!is.character(vars)) stop("`vars` must be a character vector of column names.")
   
   missing_vars <- setdiff(vars, names(data))
   if (length(missing_vars) > 0) {
      stop("These `vars` are not in `data`: ", paste(missing_vars, collapse = ", "))
   }
   
   if (!is.null(group)) {
      if (!is.character(group) || length(group) != 1) stop("`group` must be a single column name string.")
      if (!group %in% names(data)) stop("`group` not found in `data`.")
   }
   
   if (!is.logical(ft) || length(ft) != 1) {
      stop("`ft` must be a single logical value (TRUE or FALSE).")
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
   
   scree <- ggplot2::ggplot(eigtable, ggplot2::aes(x = Component, y = Eigenvalue)) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::scale_x_continuous(breaks = seq_len(len), minor_breaks = NULL) +
      ggplot2::theme_bw()
   
   table_out <- eigtable_fmt
   if (ft) {
      table_out <- flextable::autofit(flextable::flextable(eigtable_fmt))
   }
   
   list(
      table = table_out,
      plot = scree,
      pcaobj = pcaobj,
      key = key_used,
      data_used = X_used
   )
}
