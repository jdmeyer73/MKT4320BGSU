#' @title Classification Matrix and Statistics for Standard MNL (Deprecated)
#'
#' @description
#' Deprecated. Use \code{\link{eval_std_mnl}} instead.
#'
#' @details
#' This function is a thin wrapper around \code{\link{eval_std_mnl}} kept for
#' backward compatibility. It computes and prints a classification matrix and
#' associated diagnostics for the supplied dataset, similar to the legacy output.
#'
#' Migration:
#' \itemize{
#'   \item \code{stmnl_cm(model, data)} \eqn{\rightarrow} \code{eval_std_mnl(model, newdata = data)}
#' }
#'
#' @param model A fitted \code{nnet::multinom} model.
#' @param data Data frame for which the classification matrix should be produced
#'   (e.g., training data, holdout data).
#'
#' @return A classification matrix (as a numeric data frame with margins),
#'   matching the legacy return type.
#'
#' @examples
#' \dontrun{
#' # stmnl_cm(model, train)
#' # stmnl_cm(model, test)
#' }
#'
#' @export
#' @keywords internal
stmnl_cm <- function(model, data) {
   .Deprecated("eval_std_mnl", package = "MKT4320BGSU")
   
   if (!is.data.frame(data)) {
      stop("`data` must be a data frame.", call. = FALSE)
   }
   
   # We want classification on *data*; eval_std_mnl always computes model-data too.
   # Use labels to identify which is which, then pull newdata results.
   res <- eval_std_mnl(
      model = model,
      exp = FALSE,
      ft = FALSE,
      newdata = data,
      label_model = "Model data",
      label_newdata = "Data"
   )
   
   cls <- res$classify$newdata
   if (is.null(cls)) {
      stop("Classification on `data` could not be computed.", call. = FALSE)
   }
   
   acc <- as.numeric(cls$overall["Accuracy"])
   pcc <- as.numeric(cls$overall["PCC"])
   
   # Legacy print format:
   cat(
      paste0(format(round(acc, 4), nsmall = 4), " = Hit Ratio"), "\n",
      paste0(format(round(pcc, 4), nsmall = 4), " = PCC"), "\n\n",
      sep = ""
   )
   
   # Legacy return: numeric data.frame with margins + a "Level" column
   mat <- cls$table
   if (inherits(mat, "flextable")) {
      stop("Internal error: expected non-flextable classification table.", call. = FALSE)
   }
   
   # Convert matrix to legacy-style data.frame and add "Level" column
   cm <- data.frame(unclass(mat), check.names = FALSE)
   rnames <- rownames(mat)
   cm <- cbind(Level = rnames, cm)
   
   print(cm)
   
   cm
}
