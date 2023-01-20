#' @title Easy Alternative Specific MNL Marginal Effects
#' @description This function provides marginal effects tables for a saved
#'     \code{asmnl_est} object
#' @details
#' REQUIRED PACKAGES:
#' \itemize{
#'   \item mlogit
#' }
#' @param mod The object containing the results of the \code{asmnl_est} function
#' @return Text output of predicted probabilities for each brand and the
#'     marginal effects for each independent variable
#' @examples
#' altspec_model <- asmnl_est(formula=myformula, data=train.yog, id="id",
#'                            alt="brand", choice="choice", 
#'                            testdata=test.yog)
#' asmnl_me(altspec_model)

asmnl_me <- function(mod) {
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
   colnames(zt) <- mnames

   
   for (i in 1:dim(zt)[1]) {
      for (j in 1:dim(zt)[2]) {
         mdata[idx(mdata,2)==rownames(zt)[i], colnames(zt)[j]] <- zt[i,j]
      }
   }
   pprob <- round(apply(predict(mod, newdata=mdata), 2, mean),4)
   outtitle <- paste0("\n",
                        "--------------------------------","\n",
                        "Predicted Probabilities at Means","\n",
                        "--------------------------------","\n")
   cat(outtitle)
   print(pprob)
   
   for (var in mnames) {
      dash <- paste(rep("-",nchar(var)), collapse="")
      sep <- paste0("---------------------",dash)
      outme <- paste0("Marginal effects for ", var)
      cat("\n",sep,"\n",outme,"\n",sep,"\n")
      print(round(effects(mod, covariate=var, type="aa", data=zt),5))
   }
}