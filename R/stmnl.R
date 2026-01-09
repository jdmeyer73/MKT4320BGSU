#' @title Easy Standard Multinomial Logistic Regression Results (Deprecated)
#'
#' @description
#' Deprecated. Use \code{\link{eval_std_mnl}} instead.
#'
#' @details
#' This function is a thin wrapper around \code{\link{eval_std_mnl}} kept for
#' backward compatibility. It prints model-fit statistics and the coefficient
#' table, similar to the legacy output.
#'
#' Migration:
#' \itemize{
#'   \item \code{stmnl(model)} \eqn{\rightarrow} \code{eval_std_mnl(model)}
#' }
#'
#' @param model A fitted \code{nnet::multinom} model.
#'
#' @return Invisibly returns the \code{eval_std_mnl} result object.
#'
#' @examples
#' \dontrun{
#' # model <- nnet::multinom(y ~ x1 + x2, data = mydata, model = TRUE, trace = FALSE)
#' stmnl(model)
#' }
#'
#' @export
#' @keywords internal
stmnl <- function(model) {
   .Deprecated("eval_std_mnl", package = "MKT4320BGSU")
   
   res <- eval_std_mnl(model, exp = TRUE, ft = FALSE)
   
   # Preserve legacy behavior: print fit lines + coefficient table
   # (print method shows classification too; we avoid that here)
   p_txt <- if (is.na(res$fit$p_value)) {
      "NA"
   } else if (res$fit$p_value < 1e-4) {
      "< 0.0001"
   } else {
      format(round(res$fit$p_value, 4), nsmall = 4)
   }
   
   cat(
      "LR chi2 (", res$fit$df, ") = ", format(res$fit$LR_Chi2, nsmall = 4),
      "; p ", p_txt, "\n",
      sep = ""
   )
   cat(
      "McFadden's Pseudo R-square = ",
      format(res$fit$McFadden_R2, nsmall = 4),
      "\n\n",
      sep = ""
   )
   
   # Match old stmnl(): print table only, no row names
   print(res$coef_table_df, row.names = FALSE)
   
   invisible(res)
}
