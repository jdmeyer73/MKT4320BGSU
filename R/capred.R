#' @title Traditional Conjoint Analysis Profile Comparison
#' @description This function compares two profiles (specified by the user) 
#'     from a traditional conjoint analysis.
#' @details
#' To use this function, it works best to create the profiles by saving them 
#'     to separate objects. The attribute levels for the profiles should be in 
#'     the form of: \code{attribute_levelnumber}. \cr
#'     For example, \emph{Delta} is the first level of the \code{airline} 
#'     attribute, so we would use \code{airline_1} to indicate that level of
#'     that attribute.
#' @param formula An object with a saved formula used in a previous traditional
#'     conjoint analysis that was analyzed with the \code{tradca()} function
#' @param data The name of the data frame with the case-level part-worths and
#'     importances that has been appended with demographic variables
#' @param prof1 The first profile
#' @param prof2 The second profile
#' @return Mean utility and 95 percent confidence intervals for each profile
#' @examples
#' caform <- value ~ airline + connect + price
#' results <- tradca(formula = caform, data = airlineca, idvar="caseid")
#' 
#' prof1 <- c("airline_1", "connect_2", "price_3")
#' prof2 <- c("airline_3", "connect_2", "price_2")
#' 
#' capred(caform, results$casetable, prof1, prof2)

capred <- function(formula, data, prof1, prof2) {
   if (length(prof1)!=length(prof2)) {
      stop("Profile lengths do not match")
   }
   newout <- data
   len <- length(prof1)
   utp1 <- NULL
   utp2 <- NULL
   prof1name <- NULL
   prof2name <- NULL
   for (i in 1:len) {
      cnum1 <- grep(prof1[i], colnames(data))
      cnum2 <- grep(prof2[i], colnames(data))
      utp1 <- append(utp1,cnum1)
      utp2 <- append(utp2,cnum2)
      prof1name <- append(prof1name, colnames(data[cnum1]))
      prof2name <- append(prof2name, colnames(data[cnum2]))
   }
   newout$prof1 <- rowSums(data[utp1])
   newout$prof2 <- rowSums(data[utp2])
   mu1 <- mean(newout$prof1)
   mu2 <- mean(newout$prof2)
   sd1 <- sd(newout$prof1)
   sd2 <- sd(newout$prof2)
   n <- nrow(newout)
   err1 <- qt(0.975, df=n-1)*sd1/sqrt(n)
   err2 <- qt(0.975, df=n-1)*sd2/sqrt(n)
   ci1 <- paste0("(",round(mu1-err1,3),",",round(mu1+err1,3),")")
   ci2 <- paste0("(",round(mu2-err2,3),",",round(mu2+err2,3),")")
   cat(paste0("\n","Profile 1 = ",paste0(prof1name, collapse="  /  "),"\n"))
   cat(paste0("Mean Utility = ",round(mu1,3),"\n"))
   cat(paste0("95% CI = ", ci1,"\n"))
   cat(paste0("\n","Profile 2 = ",paste0(prof2name, collapse="  /  "),"\n"))
   cat(paste0("Mean Utility = ",round(mu2,3),"\n"))
   cat(paste0("95% CI = ", ci2,"\n"))
}