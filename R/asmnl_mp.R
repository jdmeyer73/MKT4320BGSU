#' @title Easy Alternative Specific MNL Margin Plots
#' @description This function creates a margin plot for a case-specific 
#'     independent variable from an estimated alternative specific MNL using
#'     the \code{asmnl_est} function.
#' @details
#' REQUIRED PACKAGES:
#' \itemize{
#'   \item mlogit
#'   \item ggplot2
#' }
#' @param mod The object containing the results of the \code{asmnl_est} function
#' @param focal The case-specific IV for which a margin plot is wanted in 
#'     quotations
#' @param type The type of the case-specific IV in quotations ("C" for 
#'     continuous; "D" for dummy variable)
#' @return Margin plot using ggplot
#' @examples
#' altspec_model <- asmnl_est(formula=myformula, data=train.yog, id="id",
#'                            alt="brand", choice="choice", 
#'                            testdata=test.yog)
#' asmnl_mp(altspec_model, "income", "C")

asmnl_mp <- function(mod, focal, type=c("C","D")) {
   require(ggplot2)
   require(mlogit)
   mlen <- length(mod$model) - 3
   mlen2 <- mlen + 1
   mdata <- mod$model[,1:mlen2]
   zt <- 0
   mnames <- names(mod$model)[2:mlen]
   for (i in 2:mlen) {
      x <- data.frame(tapply(mod$model[[i]], idx(mod,2), mean))
      zt <- data.frame(zt,x)
   }
   zt <- zt[,-1]
   nch <- nrow(zt)
   colnames(zt) <- mnames
   fin <- which(colnames(mod$model)==focal)
   fmin <- min(mod$model[[fin]])
   fmax <- max(mod$model[[fin]])
   frange <- fmax-fmin
   
   
   if (type=="D") {
      newdata <- mdata[1:(2*nch),]
      for (i in 1:dim(zt)[1]) {
         for (j in 1:dim(zt)[2]) {
            newdata[idx(newdata,2)==rownames(zt)[i], colnames(zt)[j]] <- zt[i,j]
         }
      }
      newdata[1:nch,fin] <- fmax
      newdata[(nch+1):(nch*2),fin] <- fmin
      ptable <- predict(mod, newdata=newdata)
      ptable <- data.frame(cbind(ptable,focal=c(fmax,fmin)))
      ptable <- reshape(data=ptable, idvar=focal,
                        times=levels(mod$model$idx[[2]]), v.name="prob", direction="long",
                        varying=levels(mod$model$idx[[2]]))
      cnames <- c(focal, colnames(mod$model$idx)[2], "prob")
      colnames(ptable) <- cnames
      plot <- ggplot(aes_string(x=names(ptable)[1], y=names(ptable)[3], 
                        color=names(ptable)[2]), data=ptable) + 
         geom_point(size=4) +
         geom_line(size=2) + 
         scale_x_continuous(n.break=2) +
         theme(legend.position = "bottom") +
         labs(y="Predicted Probability")
      
   } else if(type=="C") {
      fseq <- seq(fmin,fmax,frange/49)
      newdata <- mdata[1:(50*nch),]
      for (i in 1:dim(zt)[1]) {
         for (j in 1:dim(zt)[2]) {
            newdata[idx(newdata,2)==rownames(zt)[i], colnames(zt)[j]] <- zt[i,j]
         }
      }
      for (i in 1:50) {
         a <- i*nch-nch+1
         b <- i*nch
         newdata[a:b,fin] <- fseq[i]
      }
      ptable <- predict(mod, newdata=newdata)
      ptable <- data.frame(cbind(ptable, focal=fseq))
      ptable <- reshape(data=ptable, idvar=focal,
                        times=levels(mod$model$idx[[2]]), v.name="prob", direction="long",
                        varying=levels(mod$model$idx[[2]]))
      cnames <- c(focal, colnames(mod$model$idx)[2], "prob")
      colnames(ptable) <- cnames
      plot <- ggplot(aes_string(x=names(ptable)[1], y=names(ptable)[3], 
                                color=names(ptable)[2]), data=ptable) + 
         geom_line(size=2) + 
         theme(legend.position = "bottom") +
         labs(y="Predicted Probability")
   }
  plot
}