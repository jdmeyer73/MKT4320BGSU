#' Regression Uplift Modeling (deprecated)
#'
#' @description
#' `reguplift()` is deprecated and will be removed in a future release.
#' Use [easy_uplift()] instead.
#'
#' @details
#' This is a thin wrapper around [easy_uplift()] provided for backward
#' compatibility.
#'
#' **Migration notes**
#' - `ng`   → `bins`
#' - `pdata` → `newdata`
#' - `ar`   → `aspect_ratio`
#' - `ct` and `int` are no longer supported and are ignored
#'
#' @param model A fitted logistic (`glm`, logit) or linear (`lm`) regression model.
#' @param treatment Character string; name of the treatment variable.
#' @param pdata Optional data frame used to compute uplift (legacy name).
#' @param ng Integer; number of groups (bins). Must be between 5 and 20.
#' @param ar Optional numeric aspect ratio for plots.
#' @param int Legacy interaction option (ignored).
#' @param ct Legacy comparison-table option (ignored).
#'
#' @return
#' A list with uplift results, matching the core structure of
#' [easy_uplift()]: grouped results, observation-level lift, plots,
#' fitted subgroup models, and metadata.
#'
#' @seealso [easy_uplift()]
#' @export
#'
#' @examples
#' \dontrun{
#' res <- reguplift(model.visit.w, "promotion", ng = 10)
#' }
reguplift <- function(model,
                      treatment,
                      pdata = NULL,
                      ng = 10,
                      ar = NULL,
                      int = "N",
                      ct = "N") {
   
   .Deprecated("easy_uplift", package = "MKT4320BGSU")
   
   if (!missing(int) && int == "Y") {
      warning("`int = \"Y\"` is no longer supported and has been ignored.")
   }
   if (!missing(ct) && ct == "Y") {
      warning("`ct = \"Y\"` is no longer supported and has been ignored.")
   }
   
   easy_uplift(
      model = model,
      treatment = treatment,
      newdata = pdata,
      bins = ng,
      aspect_ratio = ar
   )
}
