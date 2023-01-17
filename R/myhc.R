#' @title Easy Hierarchical Agglomerative Clustering
#' @description This function performs hierarchical agglomerative clustering,
#'     allowing the user to pick a specific number of clusters, get stopping
#'     indices, etc.
#' @details
#' For best results, the results of the function should be saved to an object. \cr
#' REQUIRED PACKAGES:
#' \itemize{
#'   \item dendextend
#'   \item dplyr
#'   \item NbClust
#' }
#' @param data A data frame containing only the variables on which to cluster
#' @param dist Distance measure to be used for the similarity/dissimilarity
#'     matrix. Must be in quotations. Takes one of the following values:
#' \itemize{
#'   \item \code{dist="euc"} provides Euclidean distance
#'   \item \code{dist="euc2"} provides Euclidean Squared distance
#'   \item \code{dist="max"} provides Maximum distance
#'   \item \code{dist="abs"} provides Absolute distance
#'   \item \code{dist="bin"} provides a distance measure for a set of binary-only variables
#' }
#' @param method Indicates the linkage method. Must be in quotations. Takes one
#'     of the following values:
#' \itemize{
#' \item \code{method="single"} provides Single linkage
#' \item \code{method="complete"} provides Complete linkage
#' \item \code{method="average"} provides Average linkage
#' \item \code{method="ward"} provides Ward’s linkage
#' }
#' @param cuts Indicates how many clusters are desired for the solution (optional).
#' @param clustop Indicates if stopping indices are wanted. Must be in quotations.
#'     Takes one of the following values:
#' \itemize{
#' \item \code{clustop="Y"} if indices are wanted
#' \item \code{clustop="N"} if indices are not wanted
#' }
#' @return A list containing the following objects:
#' \itemize{
#'   \item if \code{cuts} are provided:
#'       \itemize{
#'          \item Dendrogram of the top \emph{n} branches, where \emph{n} is
#'              the highest number of clusters provided by \code{cuts}.
#'              Displayed automatically and not saved to the list object.
#'          \item \code{$kcount} is a table of cluster sizes
#'          \item \code{$kperc} is table of cluster size percentages
#'          \item \code{$hc} is the \code{hclust} object created
#'          \item \code{$stop} is a table of stopping indices for 1 to 10 clusters
#'               if \code{clustop="Y"}
#'       }
#'   \item if \code{cuts} are not provided:
#'       \itemize{
#'          \item Dendrogram with all branches.  Displayed automatically and
#'              not saved to the list object.
#'          \item \code{$stop} is a table of stopping indices for 1 to 10 clusters
#'               if \code{clustop="Y"}
#'       }
#' }
#' @examples
#' #No cuts with stopping indices
#' eg1 <- myhc(sc.clvar, "euc", "ward", "Y")
#' eg1$stop
#'
#' #One cut without stopping indices
#' eg2 <- myhc(sc.clvar, "max", "ward", cuts=5, "N")
#' eg2$kcount
#' eg2$kperc
#'
#' #Multiple cuts without stopping indices
#' eg3 <- myhc(sc.clvar, "abs", "ward", cuts=c(2,3,4,6), "N")
#' eg3$kcount
#' eg3$kperc

myhc <- function(data,
                   dist=c("euc", "euc2", "max", "abs", "bin"),
                   method=c("ward", "single", "complete", "average"),
                   cuts,
                   clustop=c("N","Y")) {
   require(dendextend)
   require(dplyr)
   sortfreq <- function(t) {
      for (tnum in 1:length(t)) {
         t[[tnum]]$Freq <- sort(t[[tnum]]$Freq, decreasing=TRUE)
      }
      return(t)
   }
   #
   # Create lookup for distance method and distance title
   #
   distdf <- data.frame(inp=c("euc", "euc2", "max", "abs", "bin"),
                        outp=c("euclidean", "euclidean", "maximum",
                               "manhattan", "binary"),
                        dtitle=c("Euclidean", "Euclidean^2", "Maximum",
                                 "Absolute", "Binary"))
   outdist <- distdf[,2:3]
   rownames(outdist) <- distdf[,1]
   dtype <- outdist[dist,1]
   dtitle <- outdist[dist,2]
   #
   # Create lookup for linkage method and linkage title
   #
   linkdf <- data.frame(inp=c("ward", "single", "complete", "average"),
                        outp=c("ward.D", "single", "complete", "average"),
                        ltitle=c("Ward's D", "Single", "Complete", "Average"))
   outlink <- linkdf[,2:3]
   rownames(outlink) <- linkdf[,1]
   ltype <- outlink[method,1]
   ltitle <- outlink[method,2]
   #
   # Create power for distance (needed for euc2)
   #
   if(dist=="euc2") {
      pw <- 2
   } else {
      pw <- 1
   }
   #
   # Create dissimilarity matrix
   #
   diss <- dist(data, method=dtype)^pw
   #
   # Create base hclust and dendrogram object
   #
   hc <- hclust(diss, method=ltype)
   hcd <- as.dendrogram(hc)
   #
   # IF CUT IS MISSING, CREATE ONLY DENDROGRAM (AND MAYBE clustop INDICES)
   #
   if(missing(cuts)) {
      dend <- as.dendrogram(hc)
      sub <- "All Branches"
      plot(dend, ylab="Similarity Measure",
           main=paste(dtitle,"Distance /",ltitle,"Linkage"),
           sub=sub)
      if (clustop=="N") {
         return("No Cuts")
      } else {
         require(NbClust)
         duda <- NbClust(data, diss=diss, distance=NULL, min.nc=1, max.nc=10, method=ltype, index="duda")$All.index
         pseudot2 <- NbClust(data, diss=diss, distance=NULL, min.nc=1, max.nc=10, method=ltype, index="pseudot2")$All.index
         out <- data.frame("Num.Clusters"=1:10,"Duda/Hart"=duda, "pseudo-t^2"=pseudot2)
         return(list("stop"=out))
      }
   } else {
      hcd_h <- heights_per_k.dendrogram(hcd)
   #
   # IF CUT IS NOT MISSING, CREATE TABLES AND DENDROGRAM
   #
      #
      # Create tables
      #
      kc_table <- lapply(cuts,
                         function(i) data.frame(table(cutree(hcd, k=i))))
      kc_table <- sortfreq(kc_table)
      k_count <- suppressWarnings(Reduce(function(d1, d2) merge(d1, d2,
                                               by="Var1", all=TRUE),
                        kc_table))
      if (length(cuts)>1) {
         cnc <- sapply(cuts, function(i) paste0("k_", cuts,
                                                "_Count"))[,1]
      } else {
         cnc <- paste0("k_", cuts, "_Count")
      }
      colnames(k_count) <- c("Cluster", cnc)
      k_count

      kp_table <- lapply(cuts,
                         function(i) data.frame(round(100*prop.table(table(cutree(hcd, k=i))),2)))
      kp_table <- sortfreq(kp_table)
      k_perc <- suppressWarnings(Reduce(function(d1, d2) merge(d1, d2,
                                              by="Var1", all=TRUE),
                       kp_table))
      if (length(cuts)>1) {
         cnp <- sapply(cuts, function(i) paste0("k_", cuts, "_Percent"))[,1]
      } else {
         cnp <- paste0("k_", cuts, "_Percent")
      }
      colnames(k_perc) <- c("Cluster", cnp)
      k_perc
      #
      # Create dendrogram with bars
      #
      cuts_m <- max(cuts)
      the_bars <- sapply(cuts,
                         function(i) cutree(hcd, k=i,
                                            order_clusters_as_data = FALSE))
      if (length(cuts)>1) {
         cn <- sapply(cuts, function(i) paste0("k_",cuts))[,1]
      } else {
         cn <- paste0("k_", cuts)
      }
      colnames(the_bars) <- cn
      hcd %>%
         set("branches_k_color", k=cuts_m) %>%
         set("branches_lwd", 4) %>%
         set("labels_colors","white") %>%
         plot(ylim=c(hcd_h[cuts_m], hcd_h[1]),
              ylab="Similarity Measure",
              main=paste(dtitle,"Distance /",ltitle,"Linkage"))
      colored_bars(colors=the_bars, dend=hcd, sort_by_labels_order = FALSE)
      if (clustop=="N") {
         results <- list("kcount"=k_count, "kperc"=k_perc, "hc"=hc)
         return(results)
      } else {
         require(NbClust)
         duda <- NbClust(data, diss=diss, distance=NULL, min.nc=1, max.nc=10, method=ltype, index="duda")$All.index
         pseudot2 <- NbClust(data, diss=diss, distance=NULL, min.nc=1, max.nc=10, method=ltype, index="pseudot2")$All.index
         out <- data.frame("Num.Clusters"=1:10,"Duda/Hart"=duda, "pseudo-t^2"=pseudot2)
         results <- list("kcount"=k_count, "kperc"=k_perc, "hc"=hc, "stop"=out)
         return(results)
      }

   }
 }
