#' @title Easy Lift Plot for Uplift Modeling
#' @description This function creates a lift plot following uplift modeling. It
#'     can create a histogram (if \code{"var"} is null) or an error-bar plot. 
#'     For continuous variables, it will create an error-bar for the quintile 
#'     values of the variable. For factor variables, it will create an error-bar 
#'     for each level of the factor. It can also create side-by-side error-bar 
#'     plots for two variables simultaneously by using the \code{"byvar"} option.
#' @details
#' REQUIRED PACKAGES:
#' \itemize{
#'   \item \code{ggplot1}
#' }
#' @param data The name of the data frame with the results of an uplift modeling
#'     analysis.
#' @param var The variable name for which the error-bars should be created. Must 
#'     be in quotations. Default is \code{"NULL"} for a histogram.
#' @param byvar The variable that identifies second variable if side-by-side 
#'     error-bar plots are desired. Must be in quotations. Default is
#'      \code{"NULL"}.
#' @param ar The aspect ratio for the plot. Used if a certain aspect ratio is
#'     desired for presentation purposes. Default is \code{"NULL"}.
#' @param ci The type of error-bar desired. Ignored if \code{"var"} is null.
#' \itemize{
#' \item \code{"0"} for error-bars to represent 1 standard deviation
#' \item \code{"c(0.90, 0.95, 0.975, 0.99)"} for error-bars to represent the 
#'     desired confidence interval.
#' }
#' @return A ggplot object
#' @examples
#' liftplot(results.visit.w$all, ar=1)
#' liftplot(results.visit.w$all, "recency", ar=1, ci=0.95)
#' liftplot(results.visit.w$all, "recency", "history", ar=1, ci=0.95)

liftplot <- function(data, var=NULL, byvar=NULL, ar=NULL, 
                     ci=c(0.90, 0.95, 0.975, 0.99, 0)) {
   require(ggplot2)
   
   
   if (is.null(var)) {
      type <- "h"
   }
   else {
      if (ci==0) {
         caption="Error bars represent 1 SD"
      }
      else if (ci %in% c(0.90, 0.95, 0.975, 0.99)) {
         ci2 <- .5 + ci/2
         caption=paste0("Error bars represent ", ci*100, "% CI")
      }
      else {
         error <- "ci must be one of (0, 0.90, 0.95, 0.975, 0.99)"
         stop(error)
      }
      if (class(data[[var]]) %in% c("integer", "numeric")) {
         if (apply(data,2,function(x) {all(x %in% 0:1)})[[var]]) {
            data[[var]][data[[var]]==0] <- "No"
            data[[var]][data[[var]]==1] <- "Yes"
            type <- "f"
         }
         else {
            type <- "n"
         }
      }
      else if (class(data[[var]]) %in% c("character", "factor", "logical")) {
         type <- "f"
      }
      if (!is.null(byvar)) {
         if (class(data[[byvar]]) %in% c("integer", "numeric")) {
            if (apply(data,2,function(x) {all(x %in% 0:1)})[[byvar]]) {
               data[[byvar]][data[[byvar]]==0] <- "No"
               data[[byvar]][data[[byvar]]==1] <- "Yes"
               bytype <- "f"
            }
            else {
               bytype <- "n"
            }
         }
         else if (class(data[[byvar]]) %in% c("character", "factor", "logical")) {
            bytype <- "f"
         }
         type=paste0(type,bytype)
      }
   }
   
   
   if (type=="h") {
      ggplot(data, aes(x=lift)) +
         geom_histogram(aes(y = after_stat(count / sum(count))),
                        fill="#fd5000",color="#4f2c1d", bins=30) +
         scale_y_continuous(labels=scales::label_percent()) +
         labs(y="% of Customers", x="Predicted Lift") +
         theme_bw() +
         theme(aspect.ratio = ar,
               panel.grid.major.x = element_blank(),
               panel.grid.minor.x = element_blank(),
               panel.background = element_rect(fill="white"),
               plot.background = element_rect(fill="transparent", 
                                              color=NA_character_))
   }
   else if (type=="n") {
      sumdata <- aggregate(data[[var]], 
                           list(covariate=ntile(data[[var]],5)), 
                           mean)
      sumdata$x <- round(sumdata$x,2)
      names(sumdata) <- c("covariate",var)
      sumdata$mean <- aggregate(data$lift, 
                                list(ntile(data[[var]],5)), 
                                mean)$x
      sumdata$sd <- aggregate(data$lift, 
                              list(ntile(data[[var]],5)), 
                              sd)$x
      sumdata$n <- aggregate(data$lift, 
                             list(ntile(data[[var]],5)), 
                             length)$x
      if (ci==0) {
         sumdata$lowerci <- sumdata$mean-sumdata$sd
         sumdata$upperci <- sumdata$mean+sumdata$sd
      }
      else {
         sumdata$lowerci <- sumdata$mean-qnorm(ci2)*sumdata$sd/sqrt(sumdata$n)
         sumdata$upperci <- sumdata$mean+qnorm(ci2)*sumdata$sd/sqrt(sumdata$n)
      }
      ggplot(sumdata, aes(x=factor(sumdata[,2]), y=mean)) +
         geom_errorbar(aes(ymin=lowerci, ymax=upperci), 
                       color="#4f2c1d", width=.5, size=1) +
         geom_point(fill="#fd5000",color="#fd5000", size=3) +
         theme_bw() +
         theme(aspect.ratio = ar,
               panel.grid.major.x = element_blank(),
               panel.background = element_rect(fill="white"),
               plot.background = element_rect(fill="transparent", 
                                              color=NA_character_)) +
         labs(y="Mean Lift", x=var, caption=caption)
   }
   else if (type=="f") {
      sumdata <- aggregate(data$lift, 
                           list(data[[var]]), 
                           mean)
      names(sumdata) <- c("covariate","mean")
      sumdata$sd <- aggregate(data$lift, 
                              list(data[[var]]), 
                              sd)$x
      sumdata$n <- aggregate(data$lift, 
                             list(data[[var]]), 
                             length)$x
      if (ci==0) {
         sumdata$lowerci <- sumdata$mean-sumdata$sd
         sumdata$upperci <- sumdata$mean+sumdata$sd
      }
      else {
         sumdata$lowerci <- sumdata$mean-qnorm(ci2)*sumdata$sd/sqrt(sumdata$n)
         sumdata$upperci <- sumdata$mean+qnorm(ci2)*sumdata$sd/sqrt(sumdata$n)
      }
      ggplot(sumdata, aes(x=covariate, y=mean)) +
         geom_errorbar(aes(ymin=lowerci, ymax=upperci), 
                       color="#4f2c1d", width=.5, size=1) +
         geom_point(fill="#fd5000",color="#fd5000", size=3) +
         theme_bw() +
         theme(aspect.ratio = ar,
               panel.grid.major.x = element_blank(),
               panel.background = element_rect(fill="white"),
               plot.background = element_rect(fill="transparent", 
                                              color=NA_character_)) +
         labs(y="Mean Lift", x=var, caption=caption)
   }
   else if(type %in% c("nn", "nf")) {
      var1 <- aggregate(data[[var]], 
                        list(cov1=ntile(data[[var]],5)), 
                        mean)
      var1$x <- round(var1$x,2)
      names(var1) <- c("cov1","var.main")
      data$cov1 <- ntile(data[[var]],5)
      data <- merge(data, var1, by="cov1")
      data$var.main <- factor(data$var.main)
      if (type=="nn") {
         var2 <- aggregate(data[[byvar]], 
                           list(cov1=ntile(data[[byvar]],3)), 
                           mean)
         var2$x <- round(var2$x,2)
         names(var2) <- c("cov2", "var.by")
         data$cov2 <- ntile(data[[byvar]],3)
         data <- merge(data,var2, by="cov2")
         data$var.by <- factor(data$var.by)
         
         sumdata <- aggregate(data$lift, 
                              list(data$var.main,data$var.by), 
                              mean)
         names(sumdata) <- c(var, byvar, "mean")
         sumdata$sd <- aggregate(data$lift, 
                                 list(data$var.main,data$var.by), 
                                 sd)$x
         sumdata$n <- aggregate(data$lift, 
                                list(data$var.main,data$var.by), 
                                length)$x
         
         if (ci==0) {
            sumdata$lowerci <- sumdata$mean-sumdata$sd
            sumdata$upperci <- sumdata$mean+sumdata$sd
         }
         else {
            sumdata$lowerci <- sumdata$mean-qnorm(ci2)*sumdata$sd/sqrt(sumdata$n)
            sumdata$upperci <- sumdata$mean+qnorm(ci2)*sumdata$sd/sqrt(sumdata$n)
         }
         
         ggplot(sumdata, aes(x=sumdata[,1], y=mean, group=sumdata[,2])) +
            geom_errorbar(aes(ymin=lowerci, ymax=upperci, color=sumdata[,2]), 
                          width=.5, size=1,
                          position=position_dodge(width=.9)) +
            geom_point(aes(fill=sumdata[,2],color=sumdata[,2]), 
                       size=3, position = position_dodge(width = .9)) +
            theme_bw() +
            theme(aspect.ratio = ar,
                  panel.grid.major.x = element_blank(),
                  panel.background = element_rect(fill="white"),
                  plot.background = element_rect(fill="transparent", 
                                                 color=NA_character_),
                  legend.position = "bottom") +
            labs(y="Mean Lift", x=var, color=byvar, fill=byvar,
                 caption=caption)
      }
      else {
         sumdata <- aggregate(data$lift, 
                              list(data$var.main,data[[byvar]]), 
                              mean)
         names(sumdata) <- c(var, byvar, "mean")
         sumdata$sd <- aggregate(data$lift, 
                                 list(data$var.main,data[[byvar]]), 
                                 sd)$x
         sumdata$n <- aggregate(data$lift, 
                                list(data$var.main,data[[byvar]]), 
                                length)$x
         if (ci==0) {
            sumdata$lowerci <- sumdata$mean-sumdata$sd
            sumdata$upperci <- sumdata$mean+sumdata$sd
         }
         else {
            sumdata$lowerci <- sumdata$mean-qnorm(ci2)*sumdata$sd/sqrt(sumdata$n)
            sumdata$upperci <- sumdata$mean+qnorm(ci2)*sumdata$sd/sqrt(sumdata$n)
         }
         
         ggplot(sumdata, aes(x=sumdata[,1], y=mean, group=sumdata[,2])) +
            geom_errorbar(aes(ymin=lowerci, ymax=upperci, color=sumdata[,2]), 
                          width=.5, size=1,
                          position=position_dodge(width=.9)) +
            geom_point(aes(fill=sumdata[,2],color=sumdata[,2]), 
                       size=3, position = position_dodge(width = .9)) +
            theme_bw() +
            theme(aspect.ratio = ar,
                  panel.grid.major.x = element_blank(),
                  panel.background = element_rect(fill="white"),
                  plot.background = element_rect(fill="transparent", 
                                                 color=NA_character_),
                  legend.position = "bottom") +
            labs(y="Mean Lift", x=var, color=byvar, fill=byvar,
                 caption=caption)
      }
      
   }
   else if(type == "fn") {
      var2 <- aggregate(data[[byvar]], 
                        list(cov1=ntile(data[[byvar]],3)), 
                        mean)
      var2$x <- round(var2$x,2)
      names(var2) <- c("cov2", "var.by")
      data$cov2 <- ntile(data[[byvar]],3)
      data <- merge(data,var2, by="cov2")
      data$var.by <- factor(data$var.by)
      
      sumdata <- aggregate(data$lift, 
                           list(data[[var]],data$var.by),
                           mean)
      names(sumdata) <- c(var, byvar, "mean")
      sumdata$sd <- aggregate(data$lift,
                              list(data[[var]],data$var.by), 
                              sd)$x
      sumdata$n <- aggregate(data$lift, 
                             list(data[[var]],data$var.by), 
                             length)$x
      if (ci==0) {
         sumdata$lowerci <- sumdata$mean-sumdata$sd
         sumdata$upperci <- sumdata$mean+sumdata$sd
      }
      else {
         sumdata$lowerci <- sumdata$mean-qnorm(ci2)*sumdata$sd/sqrt(sumdata$n)
         sumdata$upperci <- sumdata$mean+qnorm(ci2)*sumdata$sd/sqrt(sumdata$n)
      }
      
      ggplot(sumdata, aes(x=sumdata[,1], y=mean, group=sumdata[,2])) +
         geom_errorbar(aes(ymin=lowerci, ymax=upperci, color=sumdata[,2]), 
                       width=.5, size=1,
                       position=position_dodge(width=.9)) +
         geom_point(aes(fill=sumdata[,2],color=sumdata[,2]), 
                    size=3, position = position_dodge(width = .9)) +
         theme_bw() +
         theme(aspect.ratio = ar,
               panel.grid.major.x = element_blank(),
               panel.background = element_rect(fill="white"),
               plot.background = element_rect(fill="transparent", 
                                              color=NA_character_),
               legend.position = "bottom") +
         labs(y="Mean Lift", x=var, color=byvar, fill=byvar,
              caption=caption)
   }
   else if(type == "ff") {
      sumdata <- aggregate(data$lift, 
                           list(data[[var]],data[[byvar]]),
                           mean)
      names(sumdata) <- c(var, byvar, "mean")
      sumdata$sd <- aggregate(data$lift,
                              list(data[[var]],data[[byvar]]), 
                              sd)$x
      sumdata$n <- aggregate(data$lift, 
                             list(data[[var]],data[[byvar]]), 
                             length)$x
      if (ci==0) {
         sumdata$lowerci <- sumdata$mean-sumdata$sd
         sumdata$upperci <- sumdata$mean+sumdata$sd
      }
      else {
         sumdata$lowerci <- sumdata$mean-qnorm(ci2)*sumdata$sd/sqrt(sumdata$n)
         sumdata$upperci <- sumdata$mean+qnorm(ci2)*sumdata$sd/sqrt(sumdata$n)
      }
      ggplot(sumdata, aes(x=sumdata[,1], y=mean, group=sumdata[,2])) +
         geom_errorbar(aes(ymin=lowerci, ymax=upperci, color=sumdata[,2]), 
                       width=.5, size=1,
                       position=position_dodge(width=.9)) +
         geom_point(aes(fill=sumdata[,2],color=sumdata[,2]), 
                    size=3, position = position_dodge(width = .9)) +
         theme_bw() +
         theme(aspect.ratio = ar,
               panel.grid.major.x = element_blank(),
               panel.background = element_rect(fill="white"),
               plot.background = element_rect(fill="transparent", 
                                              color=NA_character_),
               legend.position = "bottom") +
         labs(y="Mean Lift", x=var, color=byvar, fill=byvar,
              caption=caption)
   }
}