#' @title Odds Ratio Coefficients Table
#' @description This function accepts a binary logistic regression - glm() -
#'    object and returns the odds ratio coefficients, p-values, and confidence
#'    intervals.
#' @param MOD The saved results of a binary logistic regression glm() call
#' @param DIGITS The number of digits to display (default = 4)
#' @param LEVEL Confidence level for CI (default = 95)
#' @return Table of Odds Ratio Coefficients
#' @examples
#' prelim <- glm(buy ~ age + gender, directmktg, family="binomial")
#'
#' or_table(prelim)
#' or_table(prelim, 3, 90)

or_table <- function(MOD, DIGITS=4, LEVEL=95) {
   vals <- round(data.frame(exp(cbind(OR=coef(MOD),
                        confint.default(MOD,
                                level=LEVEL/100))),
                        coef(summary(MOD))[,4]), DIGITS)
   vals$parameter <- rownames(vals)
   vals <- vals[, c(5,1,4,2,3)]
   lwr <- (100-LEVEL)/2
   upr <- (100+LEVEL)/2
   clnames <- c("Parameter", "OR Est", "p", paste0(lwr,"%"),paste0(upr,"%"))
   colnames(vals) <- clnames
   return(vals)
}
