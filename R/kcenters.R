#' @title k-Means Cluster Centers
#' @description This function take a \emph{k}-means object and creates a table and
#'     plot of the cluster centers.
#' @details
#' For best results, the results of the function should be saved to an object. \cr
#' REQUIRED PACKAGES:
#' \itemize{
#'   \item ggplot2
#' }
#' @param kobject A saved \emph{k}-means object
#' @return A list containing the following objects:
#' \itemize{
#'   \item \code{$table} A table of the cluster center means.
#'   \item \code{$plot} A plot of the cluster center means.
#' }
#' @examples
#' kmout <- kcenters(k4)
#' kmout$table
#' kmout$plot

kcenters <- function(kobject) {
   require(ggplot2)
   centers <- data.frame(kobject$centers)
   len <- length(centers)
   centers$Cluster <- row.names(centers)
   cenlong <- reshape(centers, direction="long",
                      v.names="value",
                      varying=1:len,
                      times=names(centers)[1:len],
                      timevar="Variable")
   plot <- ggplot(aes(x=Variable, y=value, fill=Cluster), data=cenlong) +
      geom_col(position="dodge") +
      theme(legend.position="bottom") +
      labs(x="Segmentation Variable", y="Cluster Center", fill="Cluster")
   return=list("table"=centers, "plot"=plot)
}
