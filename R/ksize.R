#' @title Cluster Sizes for k-Means Clustering
#' @description This function produces a produces a count and proportion tables
#'      for the selected \emph{k} solutions
#' @details
#' For easiest use, the results of the function should be saved to an object.
#' @param data A data frame containing only the variables on which to cluster
#' @param centers One or more integers for the specific \emph{k} cluster
#'     solutions to be examined
#' @param nstart An integer value for the number of random starting points
#'     to try (default = 25)
#' @param seed A random number seed for reproducible results (default = 4320)
#' @return A list containing the following objects:
#' \itemize{
#'   \item \code{$kcount} is a table of cluster sizes
#'   \item \code{$kperc} is a table of cluster size percentages
#' }
#' @examples
#' #Examining three different cluster solutions for default nstart and seed
#' ks <- ksize(sc.clvar, centers=c(3,4,5))

ksize <- function(data, centers, nstart=25, seed=4320) {
   sortfreq <- function(t) {
      for (tnum in 1:length(t)) {
         t[[tnum]]$Freq <- sort(t[[tnum]]$Freq, decreasing=TRUE)
      }
      return(t)
   }
   if (missing(centers)) {
      error <- "No centers selected"
      return(error)
   } else {
      kc_table <- lapply(centers,
                         function(i) {
                            set.seed(seed)
                            data.frame(table(kmeans(data,
                                                    centers=i,
                                                    nstart=25)$cluster))})
      kc_table <- sortfreq(kc_table)
      k_count <- suppressWarnings(Reduce(function(d1, d2) merge(d1, d2,
                                               by="Var1", all=TRUE),
                        kc_table))
      if (length(centers)>1) {
         cnc <- sapply(centers, function(i) paste0("k_", centers, "_Count"))[,1]
      } else {
         cnc <- paste0("k_", centers, "_Count")
      }
      colnames(k_count) <- c("Num_Clusters", cnc)
      kp_table <- lapply(centers,
                         function(i) {
                            set.seed(seed)
                            data.frame(round(100*prop.table(table(kmeans(data,
                                                                         centers=i,
                                                                         nstart=25)$cluster)),2))})
      kp_table <- sortfreq(kp_table)
      k_perc <- suppressWarnings(Reduce(function(d1, d2) merge(d1, d2,
                                              by="Var1", all=TRUE),
                       kp_table))
      if (length(centers)>1) {
         cnp <- sapply(centers, function(i) paste0("k_", centers, "_Percent"))[,1]
      } else {
         cnp <- paste0("k_", centers, "_Percent")
      }
      colnames(k_perc) <- c("Num_Clusters", cnp)
      return(list("kcount"=k_count, "kperc"=k_perc))
   }
}
