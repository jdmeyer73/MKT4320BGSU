#' @title Easy PCA
#' @description This function provides results from an standard principal 
#'     components analysis in preparation for creating PCA perceptual maps.
#'     While it can be used for traditional, it may not perform correctly.
#' @details
#' For best results, the results of the function should be saved to an object. \cr
#' REQUIRED PACKAGES:
#' \itemize{
#'   \item ggplot2
#'   \item dplyr
#' }
#' @param data A data frame containing the variables on which to perform the
#'     principal components analysis. Can include a preference variable.
#' @param group Name of the grouping variable. Must be in quotations. Can be
#'     excluded if no grouping variable exists. When provided, the PCA is 
#'     performed on an aggregatded data frame (i.e., mean values by group)
#' @param pref Name of the preference variable. Must be in quotations. Can be 
#'     excluded if no preference variable exists.
#' @param comp An integer representing the number of components to retain 
#'     (default = NULL, which retains all components).
#' @return A list containing the following objects:
#' \itemize{
#'   \item If \code{comp=NULL}:
#'      \itemize{
#'         \item \code{$plot}: Scree plot
#'         \item \code{$table}: Table of eigenvalues
#'       }
#'   \item If \code{comp >= 1}:
#'      \itemize{
#'         \item \code{$table}: Table of eigenvalues
#'         \item \code{$unrotated}: Unrotated factor loading table
#'         \item \code{$rotated}: Rotated factor loading table
#'         \item \code{$pcaobj}: PCA object from function \code{prcomp}
#'      }
#' }
#' @examples
#' #All components with grouping and preference variables
#' gb.all <- pcaex(pcadata, group="brand", pref="pref")
#' gb.all$table
#' gb.all$plot
#' 
#' #Two components with grouping and preference variables
#' gb.two <- pcaex(pcadata, group="brand", pref="pref", comp=2)
#' gb.two$table
#' gb.two$unrotated
#' gb.two$rotated

pcaex <- function(data, group, pref, comp=NULL) {
   require(dplyr)
   if(missing(group)) {
      if(missing(pref)) {
         pcadata <- data
         denom <- ncol(pcadata)
      } else {
         prefcnum <- grep(pref,colnames(data))
         pcadata <- data[-prefcnum]
         denom <- ncol(pcadata)
      }
   } else {
      grpcnum <- grep(group, colnames(data))
      if(missing(pref)) {
         pcadata <- data %>% 
            group_by(data[grpcnum]) %>% 
            summarise_all(mean) %>%
            select(-1)
         denom <- ncol(pcadata)
      } else {
         prefcnum <- grep(pref,colnames(data))
         pcadata <- data[-prefcnum] %>% 
            group_by(data[grpcnum]) %>% 
            summarise_all(mean) %>%
            select(-1)
         denom <- ncol(pcadata)
      }
   }
   if(missing(comp)) {
      require(ggplot2)
      pcaobj <- prcomp(pcadata,scale=TRUE, tol=0)
      eigtable <- data.frame(Component=integer(),
                             Eigenvalue=double(),
                             Difference=double(),
                             Proporation=double(),
                             Cumulative=double())
      len <- length(pcaobj$sdev)
      for (i in 1:len) {
         eigtable[i,1] <- i
         eigtable[i,2] <- pcaobj$sdev[i]^2
         if(i!=len) {
            eigtable[i,3] <-  pcaobj$sdev[i]^2 - pcaobj$sdev[i+1]^2
         } else {
            eigtable[i,3] <- NA
         }
         eigtable[i,4] <- (pcaobj$sdev[i]^2)/denom
         if(i==1) {
            eigtable[i,5] <- (pcaobj$sdev[i]^2)/denom
         } else {
            eigtable[i,5] <- (pcaobj$sdev[i]^2)/denom + eigtable[i-1,5]
         }
      }
      eigtable <- cbind("Component"=eigtable[,1], round(eigtable[,2:5],4))
      scree <- ggplot(aes(x=Component, y=Eigenvalue), data=eigtable) +
         geom_point() +
         geom_line() +
         scale_x_continuous(breaks=1:len, minor_breaks = NULL)
      return(list("table"=eigtable, "plot"=scree))
   } else {
      pcaobj <- prcomp(pcadata,scale=TRUE, rank=comp)
      eigtable <- data.frame(Component=integer(),
                             Eigenvalue=double(),
                             Difference=double(),
                             Proporation=double(),
                             Cumulative=double())
      len <- length(pcaobj$sdev)
      for (i in 1:len) {
         eigtable[i,1] <- i
         eigtable[i,2] <- pcaobj$sdev[i]^2
         if(i!=len) {
            eigtable[i,3] <-  pcaobj$sdev[i]^2 - pcaobj$sdev[i+1]^2
         } else {
            eigtable[i,3] <- NA
         }
         eigtable[i,4] <- (pcaobj$sdev[i]^2)/denom
         if(i==1) {
            eigtable[i,5] <- (pcaobj$sdev[i]^2)/denom
         } else {
            eigtable[i,5] <- (pcaobj$sdev[i]^2)/denom + eigtable[i-1,5]
         }
      }
      unex <- 1-(pcaobj$rotation^2)%*%(matrix(pcaobj$sdev[1:comp]^2))
      unrotated <- data.frame(round(pcaobj$rotation,4),"Unexplained"=round(unex,4))
      cnames <- colnames(unrotated)
      rvarm <- varimax(pcaobj$rotation, normalize=FALSE)
      rot <- pcaobj$rotation%*%rvarm$rotmat
      rotated <- data.frame(round(rot,4), round(unex,4))
      names(rotated) <- cnames
      eigtable <- cbind("Component"=eigtable[,1], round(eigtable[,2:5],4))
      return(list("table"=eigtable, "unrotated"=unrotated, 
                  "rotated"=rotated, "pcaobj"=pcaobj))   
   }
}

