#' @title Easy Standard Mutlinomial Logistic Regression Results
#' @description This takes a multinomial logistic regression model result from
#'     the \code{multinom} function of the \code{nnet} package and returns
#'     model fit and odds-ratio estimates
#' @details
#' REQUIRED PACKAGES:
#' \itemize{
#'   \item broom
#' }
#' @param model A \code{multinom} object from the \code{nnet} package
#' @examples
#' stmnl(model)

stmnl <- function(model) {
   require(broom)
   null <- multinom(eval(model$call$formula[[2]])~1, data=eval(model$call$data),
                    trace=FALSE)
   modout <- as.data.frame(tidy(model, exponentiate=TRUE))
   modout[,3:6] <- round(modout[,3:6],4)
   lr.stat <- anova(null,model)$`LR stat.`[2]
   lr.pval <- anova(null,model)$`Pr(Chi)`[2]
   if (lr.pval<.00005) {
      likrat <- paste0("LR chi2 (",
                    anova(null,model)$`   Df`[2],
                    ") = ", round(lr.stat,4), "; p < 0.0001")
   } else {
      likrat <- paste0("LR chi2 (",
                       anova(null,model)$`   Df`[2],
                       ") = ", round(lr.stat,4), "; p = ", 
                       format(round(lr.pval,4), nsmall=4))
   }
   mcf.r2 <- (null$deviance-model$deviance)/null$deviance
   mcfad <- paste0("McFadden's Pseudo R-square = ", 
                   format(round(mcf.r2,4),nsmall=4))
   assessout <- paste(likrat,mcfad,"\n",sep="\n")
   cat(assessout)                   
   print(modout, row.names=FALSE)
}