#' @title Scree Plot Before k-Means Clustering
#' @description This function produces a scree plot for 1 to \emph{n} clusters
#' @details
#' REQUIRED PACKAGES:
#' \itemize{
#'   \item ggplot2
#' }
#' @param data A data frame containing only the variables on which to cluster
#' @param nc An integer representing the maximum number of clusters to plot
#'     (default = 15)
#' @param seed A random number seed for reproducible results (default = 4320)
#' @return A ggplot object
#' @examples
#' #Scree plot with default values
#' wssplot(sc.clvar)
#'
#' #Scree plot for up to 10 clusters and 1000 for random number seed
#' wssplot(sc.clvar, 10, 1000)

wssplot <- function(data, nc=15, seed=4320)
{
   require(ggplot2)
   x <- 1:nc
   wss <- rep(length(1:nc))
   for (i in 1:nc)
   {
      set.seed(seed)
      wss[i] <- sum(kmeans(data, centers=i, nstart=25)$withinss)
   }
   wssdata <- data.frame(x,wss)
   ggplot(wssdata, aes(x=x, y=wss)) +
      geom_line() +
      geom_point() +
      labs(x="k", y="WSS") +
      scale_x_continuous(breaks=seq(1:nc), minor_breaks = NULL)
}
