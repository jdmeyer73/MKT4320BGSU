#' Randomization check for A/B testing (deprecated)
#'
#' @description
#' `rcheck()` is deprecated and will be removed in a future release.
#' Use [rand_check()] instead.
#'
#' @details
#' This function is a thin wrapper around [rand_check()] for backward
#' compatibility.
#'
#' **Migration notes**
#' - `nice = "ft"`  -> `ft = TRUE`
#' - `nice = "no"`  -> `ft = FALSE`
#' - `nice = "ht"`  -> no longer supported; this wrapper returns the plain data
#'   frame (like `nice="no"`) with a warning.
#' - `outcome` is ignored by [rand_check()], but this wrapper will still remove
#'   `outcome` columns before determining the covariates to check.
#'
#' @param data Data frame containing the treatment variable and covariates.
#' @param treatment Character string; name of the treatment indicator variable.
#' @param outcome Optional character vector of outcome variable names to exclude
#'   from the balance checks (legacy behavior).
#' @param nice Output format: `"no"` (data frame), `"ft"` (flextable),
#'   `"ht"` (deprecated legacy option; no longer supported).
#'
#' @return
#' A data frame (for `nice="no"` or `nice="ht"`) or a `flextable` object (for
#' `nice="ft"`), matching the legacy intent as closely as possible.
#'
#' @seealso [rand_check()]
#' @export
#'
#' @examples
#' \dontrun{
#' rcheck(email.camp.w, "promotion", outcome = "buy", nice = "ft")
#' }
rcheck <- function(data, treatment, outcome = NULL, nice = c("no", "ft", "ht")) {
   
   .Deprecated("rand_check", package = "MKT4320BGSU")
   
   nice <- match.arg(nice)
   
   # Legacy: remove outcome variables (if provided)
   if (!is.null(outcome) && length(outcome) > 0) {
      if (!is.character(outcome)) stop("`outcome` must be a character vector (variable names).")
      keep <- setdiff(names(data), outcome)
      data <- data[, keep, drop = FALSE]
   }
   
   # Legacy behavior: check *all remaining* covariates (excluding treatment itself)
   covariates <- setdiff(names(data), treatment)
   
   if (length(covariates) == 0) {
      stop("No covariates available to check after excluding `treatment` (and `outcome`, if provided).")
   }
   
   if (identical(nice, "ht")) {
      warning('`nice = "ht"` is no longer supported. Returning a plain data frame instead.')
   }
   
   ft <- identical(nice, "ft")
   
   rand_check(
      data = data,
      treatment = treatment,
      covariates = covariates,
      ft = ft,
      digits = 3
   )
}
