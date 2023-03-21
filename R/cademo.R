#' @title Demographic Analysis of Traditional Conjoint Results
#' @description This function produces correlation matrices and importance
#'     regressions from a data frame of case-level part-worths/importances that
#'     has been appended with demographics.
#' @details
#' To use this function, the \code{$casetable} from the call to the 
#'     \code{tradca()} function must be appended with the demographic variables
#'     for each case.  Refer to the course tutorial for code to accomplish
#'     this.  \cr
#'      \cr
#' REQUIRED PACKAGES:
#' \itemize{
#'   \item Hmisc
#' }
#' @param formula An object with a saved formula used in a previous traditional
#'     conjoint analysis that was analyzed with the \code{tradca()} function
#' @param data The name of the data frame with the case-level part-worths and
#'     importances that has been appended with demographic variables
#' @param vars A concantenated list of the demographics variables to analyze (
#'     in quotations)
#' @return Text correlation matrices and/or regression results
#' @examples
#' caform <- value ~ airline + connect + price
#' results <- tradca(formula = caform, data = airlineca, idvar="caseid")
#' demos <- airlineca %>%   
#'     group_by(caseid) %>% 
#'     summarise(inc=first(inc), type=first(type)) %>% 
#'     bind_cols(.,results$casetable[,2:ncol(results$casetable)])
#'
#' cademo(caform, demos, c("inc", "type"))

cademo <- function(formula, data, vars) {
   # x is a matrix containing the data
   # method : correlation method. "pearson"" or "spearman"" is supported
   # removeTriangle : remove upper or lower triangle
   # results :  if "html" or "latex"
   # the results will be displayed in html or latex format
   corstars <-function(x, numcn){
      #Compute correlation matrix
      require(Hmisc)
      x <- as.matrix(x)
      correlation_matrix<-rcorr(x, type="pearson")
      R <- correlation_matrix$r # Matrix of correlation coefficients
      p <- correlation_matrix$P # Matrix of p-value 
      
      ## Define notions for significance levels; spacing is important.
      mystars <- ifelse(p < .05, "*", "")
      
      ## trunctuate the correlation matrix to two decimal
      R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
      
      ## build a new matrix that includes the correlations with their apropriate stars
      Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
      diag(Rnew) <- paste(diag(R), " ", sep="")
      rownames(Rnew) <- colnames(x)
      colnames(Rnew) <- paste(colnames(x), "", sep="")
      
      Rnew <- as.matrix(Rnew)
      Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
      Rnew <- as.data.frame(Rnew)

      ## remove last column and return the correlation matrix
      Rnew <- cbind(Rnew[1:numcn])
      if (numcn>1) {
         Rnew <- Rnew[(numcn+1):nrow(Rnew),]
      } else {
         Rnew <- Rnew[-1,,drop=FALSE]
      }
      return(Rnew)
   } 
   
   butes <- attr(terms(formula), "term.labels")
   butes <- paste0(butes,"_")
   butes <- c(butes,"Imp")
   lenvars <- length(vars)
   lenbutes <- length(butes)
   lenimp <- lenbutes-1
   contcn <- NULL
   for (i in 1:lenvars) {
      cnum <- grep(vars[i], colnames(data))
      if (is.numeric(data[[cnum]])) {
         contcn[i] <- grep(vars[i], colnames(data))   
      }
   }
   lencont <- length(contcn)
   for (i in 1:lenbutes) {
      butescn <- (grep(butes[i], colnames(data)))
      corrcols <- c(contcn,butescn)
      cat(paste0("\n","Correlation Matrix for ", butes[i],"\n"))
      print.data.frame(corstars(data[,corrcols],lencont))
   }
   ivs <- paste(vars, collapse=" + ")
   for (i in 1:lenimp) {
      impd <- colnames(data[ncol(data)-lenimp+i])
      impdv <- paste(impd,"~")
      impform <- paste(impdv,ivs)
      implm <- lm(impform, data=data)
      cat(paste0("\n","Regression Results for ", impd,"\n"))
      print(round(summary(implm)$coefficients,4))
   }
}