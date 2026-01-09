#' @title k-Means Cluster Centers (Deprecated)
#' @description
#' Deprecated. Use \code{\link{easy_km_final}} instead.
#'
#' @details
#' This function is kept for backward compatibility. It produces a table of
#' cluster centers and a bar plot of those centers. New workflows should use
#' \code{\link{easy_km_fit}} + \code{\link{easy_km_final}}.
#'
#' Migration:
#' \itemize{
#'   \item If you already have a \code{kmeans} object: use it directly or prefer
#'   \code{easy_km_fit()} / \code{easy_km_final()} for a full workflow.
#'   \item If you have an \code{easy_km_fit()} result: extract a stored solution via
#'   \code{fit$km_all[[as.character(k)]]} and/or use \code{easy_km_final(fit, data, k)}.
#' }
#'
#' @param kobject A \code{kmeans} object, or an \code{easy_km_fit()} result (advanced).
#' @param k Optional integer. If \code{kobject} is an \code{easy_km_fit()} result,
#'   \code{k} selects which stored solution to use.
#'
#' @return A list with components:
#' \describe{
#'   \item{table}{A data frame of cluster center means (with \code{Cluster} column).}
#'   \item{plot}{A ggplot bar chart of cluster centers by variable.}
#' }
#'
#' @examples
#' \dontrun{
#' # Legacy usage (kmeans object)
#' km <- stats::kmeans(scale(iris[, 1:4]), centers = 3, nstart = 25)
#' out <- kcenters(km)
#' out$table
#' out$plot
#'
#' # If you have an easy_km_fit() object:
#' # fit <- easy_km_fit(mydata, vars = c("x1","x2"), k_range = 2:6)
#' # out <- kcenters(fit, k = 4)
#' }
#'
#' @export
#'
#' @importFrom stats reshape
#' @importFrom ggplot2 ggplot aes geom_col theme labs
#' @keywords internal
kcenters <- function(kobject, k = NULL) {
   .Deprecated("easy_km_final", package = "MKT4320BGSU")
   
   # Allow either a kmeans object, or (optionally) an easy_km_fit result + k
   if (is.list(kobject) && !is.null(kobject$km_all) && !is.null(kobject$settings)) {
      if (is.null(k)) {
         stop("If `kobject` is an easy_km_fit() result, you must supply `k`.", call. = FALSE)
      }
      k_chr <- as.character(as.integer(k))
      if (!k_chr %in% names(kobject$km_all) || is.null(kobject$km_all[[k_chr]])) {
         stop("No stored k-means solution found for k = ", k, ".", call. = FALSE)
      }
      kobject <- kobject$km_all[[k_chr]]
   }
   
   if (!is.list(kobject) || is.null(kobject$centers)) {
      stop("`kobject` must be a `kmeans` object (or an easy_km_fit() result + `k`).", call. = FALSE)
   }
   
   centers <- data.frame(kobject$centers, check.names = FALSE)
   len <- ncol(centers)
   
   centers$Cluster <- row.names(centers)
   
   cenlong <- stats::reshape(
      centers,
      direction = "long",
      v.names   = "value",
      varying   = seq_len(len),
      times     = names(centers)[seq_len(len)],
      timevar   = "Variable"
   )
   
   plt <- ggplot2::ggplot(cenlong, ggplot2::aes(x = Variable, y = value, fill = Cluster)) +
      ggplot2::geom_col(position = "dodge") +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::labs(x = "Segmentation Variable", y = "Cluster Center", fill = "Cluster")
   
   list(table = centers, plot = plt)
}
