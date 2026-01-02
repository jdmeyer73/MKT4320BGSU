#' @title Easy Alternative-Specific MNL (Deprecated)
#' @description
#' Deprecated. Use \code{\link{eval_as_mnl}} instead.
#'
#' @details
#' \code{asmnl_est()} is retained for backward compatibility with older course
#' materials. New code should:
#' \enumerate{
#'   \item Create a \code{dfidx} object with \code{dfidx::dfidx(...)}.
#'   \item Fit a model with \code{mlogit::mlogit()}.
#'   \item Evaluate it with \code{eval_as_mnl()} (optionally passing \code{newdata}).
#' }
#'
#' This deprecated wrapper fits the model and then calls \code{eval_as_mnl()}
#' to print fit, coefficients (including odds ratios), and classification results.
#'
#' @param formula A model formula for \code{mlogit}, typically of the form
#'   \code{choice ~ alt_specific_vars | case_specific_vars}.
#' @param data Training data in long format (data frame).
#' @param id Character string. Case identifier column name (in quotes).
#' @param alt Character string. Alternative identifier column name (in quotes).
#' @param choice Character string. Choice indicator column name (in quotes).
#' @param testdata Optional holdout data in long format (data frame). If provided,
#'   it is converted to a \code{dfidx} object and passed as \code{newdata} to
#'   \code{eval_as_mnl()}.
#'
#' @return A fitted \code{mlogit} model (legacy behavior). Evaluation output is
#'   printed via \code{eval_as_mnl()}.
#'
#' @examples
#' # myformula <- choice ~ feat + price | income
#' # mod <- asmnl_est(myformula, data=train_long, id="id", alt="brand",
#' #                  choice="choice", testdata=test_long)
#'
#' @importFrom dfidx dfidx
#' @importFrom mlogit mlogit
#' @importFrom utils packageName
#' @export
asmnl_est <- function(formula, data, id, alt, choice, testdata = NULL) {
   
   .Deprecated(
      new = "eval_as_mnl",
      package = utils::packageName(),
      msg = "asmnl_est() is deprecated; fit with mlogit::mlogit() and evaluate with eval_as_mnl()."
   )
   
   # ---- validation ----
   if (!requireNamespace("dfidx", quietly = TRUE)) {
      stop("Package 'dfidx' is required.", call. = FALSE)
   }
   if (!requireNamespace("mlogit", quietly = TRUE)) {
      stop("Package 'mlogit' is required.", call. = FALSE)
   }
   
   if (!inherits(formula, "formula")) stop("`formula` must be a formula.", call. = FALSE)
   if (!is.data.frame(data)) stop("`data` must be a data.frame (long format).", call. = FALSE)
   if (!is.character(id) || length(id) != 1) stop("`id` must be a single character string.", call. = FALSE)
   if (!is.character(alt) || length(alt) != 1) stop("`alt` must be a single character string.", call. = FALSE)
   if (!is.character(choice) || length(choice) != 1) stop("`choice` must be a single character string.", call. = FALSE)
   
   # ---- fit model (legacy behavior) ----
   train_mdata <- dfidx::dfidx(data, idx = c(id, alt), choice = choice)
   mod <- mlogit::mlogit(formula = formula, data = train_mdata)
   
   # ---- optional holdout evaluation ----
   new_mdata <- NULL
   if (!is.null(testdata)) {
      if (!is.data.frame(testdata)) stop("`testdata` must be a data.frame (long format).", call. = FALSE)
      new_mdata <- dfidx::dfidx(testdata, idx = c(id, alt), choice = choice)
   }
   
   # ---- print evaluation using the new evaluator ----
   eval_res <- eval_as_mnl(
      model = mod,
      ft = FALSE,
      newdata = new_mdata,
      label_model = "Training data",
      label_newdata = "Holdout data"
   )
   print(eval_res)
   
   # ---- return legacy object (mlogit model) ----
   mod
}
