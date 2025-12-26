#' @title Demographic Analysis of Traditional Conjoint Results
#' @description
#' Produce correlation matrices and simple importance regressions from a data
#' frame of case-level part-worths/importances appended with demographics.
#'
#' @details
#' This is a legacy helper used in older course materials. It is retained for
#' backwards compatibility but is not used in current student workflows.
#'
#' Requires the \code{$casetable} from \code{tradca()} to be appended with case-level
#' demographic variables.
#'
#' @param formula A model formula used in a previous traditional conjoint analysis
#'   analyzed with \code{tradca()}.
#' @param data A data frame containing case-level part-worth columns and appended
#'   demographics.
#' @param vars Character vector of demographic variable names to analyze.
#'
#' @return
#' Prints correlation matrices and/or regression coefficients to the console.
#'
#' @examples
#' \dontrun{
#' caform <- value ~ airline + connect + price
#' results <- tradca(formula = caform, data = airlineca, idvar = "caseid")
#'
#' demos <- airlineca %>%
#'   group_by(caseid) %>%
#'   summarise(inc = first(inc), type = first(type)) %>%
#'   bind_cols(., results$casetable[, 2:ncol(results$casetable)])
#'
#' cademo(caform, demos, c("inc", "type"))
#' }
#'
#' @keywords internal

cademo <- function(formula, data, vars) {
   # x is a matrix containing the data
   # method : correlation method. "pearson"" or "spearman"" is supported
   # removeTriangle : remove upper or lower triangle
   # results :  if "html" or "latex"
   # the results will be displayed in html or latex format
   corstars <-function(x, numcn){
      #Compute correlation matrix
      if (!requireNamespace("Hmisc", quietly = TRUE)) {
         stop("Package 'Hmisc' is required for cademo() (correlation matrix). Please install it.",
              call. = FALSE)
      }
      
      x <- as.matrix(x)
      correlation_matrix <- Hmisc::rcorr(x, type = "pearson")
      
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
   
   butes <- attr(stats::terms(formula), "term.labels")
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