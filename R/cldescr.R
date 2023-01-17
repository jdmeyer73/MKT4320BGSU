#' @title Easy Describing Clusters
#' @description This function performs describes clusters based on a set of
#'     variables and tests if there are significant differences between
#'     clusters.
#' @details
#' For best results, the results of the function should be saved to an object.
#' @param data A data frame containing only the variables on which to describe
#'     the clusters. At least one column in the data frame must contain the
#'     cluster membership.
#' @param var The name of the variable(s) to describe the clusters by. Must be
#'     in quotations.  For \code{vtype="F"}, can only be one factor variable.
#'     For \code{vtype="C"}, can be one or more continuous variables
#' @param vtype The type of variable(s) to describe the clusters by. Must be in
#'     quotations. Takes one of the following values:
#' \itemize{
#'   \item \code{vtype="F"} if \code{var} is a factor variable
#'   \item \code{vtype="C"} if \code{var} is a continuous variable(s)
#' }
#' @param cvar The name of the cluster membership variable. Must be in
#'     quotations.
#' @return A list containing the following objects:
#' \itemize{
#'   \item \code{$means} contains a table of mean values by cluster. For
#'       \code{vtype="F"}, each factor level has a separate column, and the
#'       mean values represent the proportion of the cluster that for each
#'       factor level.
#'   \item \code{$aovp} contains a table of ANOVA p-values. For
#'       \code{vtype="C"}, the p-value tests if there are differences between
#'       clusters for the continuous variable. For \code{vtype="F"}, the
#'       p-value tests if there are differences between clusters for each
#'       level of the factor variable.
#'    \item \code{$tukey} contains a table of Tukey HSD post-hoc comparison
#'       p-values to test which clusters are significantly different from each
#'       other. For \code{vtype="C"}, a table will be produced for each
#'       \code{var} that has an overall ANOVA p-value < 0.1. For
#'       \code{vtype="F"}, a table will be produced for each level of the
#'       factor variable that has an overall ANOVA p-value < 0.1.
#' }
#' @examples
#' #Describing clusters for a single continuous variable
#' out1 <- cldescr(ffseg, var="eatin", vtype="C", cvar="HC3")
#' out1$means
#' out1$aovp
#' out1$tukey
#'
#' #Describing clusters for a multiple continuous variables
#' out2 <- cldescr(ffseg, var=c("eatin","hours"), vtype="C", cvar="HC3")
#' out2$means
#' out2$aovp
#' out2$tukey
#'
#' #Describing clusters for a factor variable
#' out3 <- cldescr(ffseg, var="usertype", vtype="F", cvar="HC3")
#' out3$means
#' out3$aovp
#' out3$tukey

cldescr <- function(data, var, vtype=c("F", "C"), cvar) {
   if(vtype=="F") {
      var <- grep(var, colnames(data))
      cvar <- grep(cvar, colnames(data))
      fdata <- data.frame(model.matrix(~data[,var]-1, data=data))
      len <- length(fdata)
      names(fdata) <- substring(names(fdata), 12)
      cnames <- colnames(fdata)
      fdata <- cbind(fdata,Cluster=data[,cvar])
      if(len==2) {
         means <- aggregate(.~Cluster, fdata,
                            FUN=function(x) round(mean(x), digits=4))[,1:2]
         fdata <- fdata[,-2]
         aov <- aov(fdata[[1]]~Cluster, data=fdata)
         p <- summary(aov)[[1]][["Pr(>F)"]][1]
         aovp <- data.frame(Variable=colnames(fdata)[1], p.value=round(p,4))
         if(p<.1) {
            tukey <- round(TukeyHSD(aov)$Cluster[,c(1,4)],4)
         } else {
            tukey <- NULL
         }
      } else {
         aovp <- data.frame(Variable=character(), p.value=numeric())
         tukey <- list()
         means <- aggregate(.~Cluster, fdata,
                            FUN=function(x) round(mean(x), digits=4))
         for (i in 1:len) {
            name <- cnames[i]
            aov <- aov(fdata[[i]]~Cluster, data=fdata)
            p <- summary(aov)[[1]][["Pr(>F)"]][1]
            aovp[i,] <- c(colnames(fdata)[[i]],round(p,4))
            if(p<.1) {
               tukey[[name]] <- round(TukeyHSD(aov)$Cluster[,c(1,4)],4)
            } else {
               tukey[[name]] <- NULL
            }
         }
      }
   } else {
      len <- length(var)
      cnames <- var
      if(len==1) {
         cnamesfull <- c(cnames,"Cluster")
         vari <- which(colnames(data)==var)
         cvar <- grep(cvar, colnames(data))
         fdata <- data.frame(data[,vari],Cluster=data[,cvar])
         names(fdata) <- cnamesfull
         means <- aggregate(.~Cluster, fdata,
                            FUN=function(x) round(mean(x), digits=4))
         aov <- aov(fdata[[1]]~Cluster, data=fdata)
         p <- summary(aov)[[1]][["Pr(>F)"]][1]
         aovp <- data.frame(Variable=colnames(fdata)[1], p.value=round(p,4))
         if(p<.1) {
            tukey <- round(TukeyHSD(aov)$Cluster[,c(1,4)],4)
         } else {
            tukey <- NULL
         }
      } else {
         aovp <- data.frame(Variable=character(), p.value=numeric())
         cnamesfull <- c(cnames, "Cluster")
         tukey <- list()
         vari <- which(colnames(data)==var[1])
         cvar <- grep(cvar, colnames(data))
         fdata <- data.frame(Cluster=data[,cvar], data[,vari])
         for (i in 2:len) {
            vari <- which(colnames(data)==var[i])
            fdata <- data.frame(fdata, data[, vari])
         }
         lenfdata <- length(fdata)
         fdata <- fdata[,c(2:lenfdata,1)]
         names(fdata) <- cnamesfull
         means <- aggregate(.~Cluster, fdata,
                            FUN=function(x) round(mean(x), digits=4))
         for (i in 1:len) {
            name <- cnamesfull[i]
            aov <- aov(fdata[[i]]~Cluster, data=fdata)
            p <- summary(aov)[[1]][["Pr(>F)"]][1]
            aovp[i,] <- c(colnames(fdata)[[i]],round(p,4))
            if(p<.1) {
               tukey[[name]] <- round(TukeyHSD(aov)$Cluster[,c(1,4)],4)
            } else {
               tukey[[name]] <- NULL
            }
         }
      }
   }
   return=list("means"=means, "aovp"=aovp, "tukey"=tukey)
}




