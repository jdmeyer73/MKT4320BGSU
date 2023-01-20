#' @title Average Predicted Probabilities for Standard MNL
#' @description This function accepts a multinomial logistic regression object
#'    from the \code{multinom} function of the \code{nnet} package and returns 
#'    a table and plot of average predicted probabilities.
#' @details  For continuous focal variables, the table produced uses three
#'     levels to calculate predicted probabilities: -1 SD, Mean, +1 SD \cr
#'     \cr
#' REQUIRED PACKAGES:
#' \itemize{
#'   \item tidyr
#'   \item effects
#'   \item dplyr
#'   \item ggplot2
#' }
#' @param model A \code{multinom} object from the \code{nnet} package
#' @param focal The name of the variable (in quotes) for which predicted 
#'     probabilities are wanted
#' @param xlab The desired x-axis label for the plot, in quotes; optional
#' @return Predicted probabilities table ($table) and ggplot ($plot). Can be
#'     saved as an object.
#' @examples
#' stmnl_pp(model, "Age", "Age in Years")


stmnl_pp <- function(model, focal, xlab=NULL) {
   require(tidyr)
   require(effects)
   require(dplyr)
   require(ggplot2)
   cls <- with(eval(model$call$data), class(eval(parse(text=all_of(focal)))))
   if (cls=="factor") {
      pred.d <- data.frame(predictorEffect(focal, model))
      plong <- pred.d %>% 
         select(all_of(focal),
                starts_with("prob") | starts_with("U.prob") | starts_with("L.prob")) %>%
         pivot_longer(!all_of(focal),
                      names_to=c(".value", as.character(model$call$formula[[2]])),
                      names_sep="b.",
                      values_drop_na=TRUE)
      plong <- data.frame(plong)
      plong[,3:5] <- round(plong[,3:5],4)
      colnames(plong)[3:5] <- c("p.prob", "lower.CI", "upper.CI")
      
      plot <- ggplot(aes_string(x=names(plong)[1], y="p.prob", group=1), data=plong) + 
         geom_point(size=2) +
         geom_line() +
         geom_errorbar(aes(ymin=lower.CI, ymax=upper.CI), width=.1) +
         facet_wrap(~eval(model$call$formula[[2]]), ncol=2) +
         labs(y="Predicted Probability", 
              x=ifelse(is.null(xlab),names(plong)[1],xlab))
   } else if (cls=="integer" | cls=="numeric") {
      fmean <- with(eval(model$call$data), mean(eval(parse(text=focal))))
      fsd <- with(eval(model$call$data), sd(eval(parse(text=focal))))
      xvals <- c(round(fmean-fsd,0), round(fmean,0), round(fmean+fsd,0))
      pred.d <- data.frame(predictorEffect(focal, model))
      plong <- pred.d %>% 
         select(all_of(focal),
                starts_with("prob") | starts_with("U.prob") | starts_with("L.prob")) %>%
         pivot_longer(!all_of(focal),
                      names_to=c(".value", as.character(model$call$formula[[2]])),
                      names_sep="b.",
                      values_drop_na=TRUE)
      plong <- data.frame(plong)
      plong[,3:5] <- round(plong[,3:5],4)
      colnames(plong)[3:5] <- c("p.prob", "lower.CI", "upper.CI")
      plot <- ggplot(aes_string(x=names(plong)[1], y="p.prob",
                                color=names(plong)[2],
                                fill=names(plong)[2]), 
                     data=plong) +
         geom_line(size=1) +
         geom_ribbon(aes(ymin=lower.CI, ymax=upper.CI), alpha=0.2) +
         scale_x_continuous(breaks=waiver(), n.breaks=10) +
         theme(legend.position="bottom") + 
         labs(y="Predicted Probability", 
              x=ifelse(is.null(xlab),names(plong)[1],xlab))
      
      pred.d <- data.frame(predictorEffect(focal, model, focal.levels=xvals))
      plong <- pred.d %>% 
         select(all_of(focal),
                starts_with("prob") | starts_with("U.prob") | starts_with("L.prob")) %>%
         pivot_longer(!all_of(focal),
                      names_to=c(".value", as.character(model$call$formula[[2]])),
                      names_sep="b.",
                      values_drop_na=TRUE)
      plong <- data.frame(plong)
      plong[,3:5] <- round(plong[,3:5],4)
      colnames(plong)[3:5] <- c("p.prob", "lower.CI", "upper.CI")
      
   } else {
      error <- "Focal variable is not of class factor, numeric, or integer"
      return(error)
   }
   return(list("table"=plong, "plot"=plot))
}