#' @title Standardized Beta Coefficients
#' @description This function accepts a lm() object and returns the
#'    standardized beta coefficients. Code adapted from the lm.beta package
#' @param object The saved results of a lm() call
#' @param digits The number of digits to display (default = 8)
#' @return The standardized beta coefficients
#' @examples
#' model <- lm(nps ~ age + nflights, data=airlinesat)
#'
#' lm_beta(model, 4)

lm_beta <- function (object, digits=8)
   # Function from lm.beta package
{
   if (!"lm" %in% attr(object, "class"))
      stop("object has to be of class lm")
   object$standardized.coefficients <-
      coef(object) *
      apply(as.matrix(model.matrix(object)), 2, function(x)
         sqrt(sum((x - mean(x, na.rm = T) *
                      attr(attr(object$model, "terms"),
                           "intercept"))^2, na.rm = T)))/
      apply(as.matrix(model.frame(object)[, 1]),
            2, function(x) sqrt(sum((x - mean(x, na.rm = T) *
                                        attr(attr(object$model, "terms"),
                                             "intercept"))^2, na.rm = T)))
   Std.Beta <- data.frame("Std.Beta"=round(object$standardized.coefficients, digits))
   return(Std.Beta)
}
