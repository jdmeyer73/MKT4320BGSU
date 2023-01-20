#' @title Classification Matrix and Statistics for Standard MNL
#' @description This function accepts a multinomial logistic regression object
#'    from the \code{multinom} function of the \code{nnet} package and returns 
#'    the classification matrix and associated statistics.
#' @param model A \code{multinom} object from the \code{nnet} package
#' @param data The name of the data frame for which the classification matrix
#'    should be produced (i.e., training data, holdout data, etc.)
#' @return Classification matrix and statistics
#' @examples
#' stmnl_cm(model, train)

stmnl_cm <- function(model, data) {
   pred <- predict(model, data, type="class")
   true <- with(data, eval(model$call$formula[[2]]))
   cm <- data.frame(unclass(addmargins(table(pred, true))))
   len <- dim(cm)[2]
   pcc <- 0
   acc <- 0
   cnames <- colnames(cm)
   rnames <- colnames(cm)
   for (i in 1:(len-1)) {
      cnames[i] <- paste0("T.",cnames[i])
      rnames[i] <- paste0("P.",rnames[i])
      acc <- acc + (cm[i,i]/cm[len,len])
      pcc <- pcc + (cm[len,i]/cm[len,len])^2
   }
   cnames[len] <- "Total"
   rnames[len] <- "Total"
   colnames(cm) <- cnames
   #rownames(cm) <- rnames
   rownames(cm) <- 1:len
   cm <- cbind("Level"=rnames,cm)
   out <- paste(paste0(format(round(acc,4), nsmall=4)," = Hit Ratio"),
                paste0(format(round(pcc,4), nsmall=4)," = PCC"), 
                "\n",sep="\n")
   cat(out)
   cm
}

