#' @title Easy Margin Plots
#' @description This function produces margin plots for a focal predictor or a
#'    focal predictor interaction with a another predictor from a linear 
#'    or binary logistic regression
#' @details
#' REQUIRED PACKAGES:
#' \itemize{
#'   \item ggplot2
#'   \item dplyr
#'   \item ggeffects
#'   \item insight
#' }
#' @param model A saved linear regression (lm) model or binary logistic
#'    regression (glm, family="binomial") model.
#' @param focal Name of the focal predictor variable. Must be in quotations. 
#' @param int Name of the interaction predictor variable. Must be in quotations. 
#'    Can be excluded if focal variable only wanted (or no interaction exists).
#' @return
#' \itemize{ 
#'   \item \code{$plot} A ggplot object.
#'   \item \code{$ptable} A ggeffects table that can be saved as a data frame
#'    object.
#' }
#' @examples
#' #No interaction
#' data(airlinesat)
#' model1 <- lm(nps ~ age + nflights, data=airlinesat)
#' mymp(model, "age")
#' 
#' #Interaction
#' data(directmktg)
#' model2 <- glm(buy ~ salary + age*gender, data=directmktg, family="binomial")
#' mymp(model2, "age", "gender")

mymp <- function(model, focal, int=NULL)
{
   require(ggeffects)
   require(insight)
   require(ggplot2)
   require(dplyr)
   
   # VAL function for continuous interactions
   
   myval.i <- function(idata, ilevels=4) {
      quants <- quantile(idata, c(0.01, 0.5, 0.99))
      skip <- (quants[3]-quants[1])/(ilevels-1)
      if (skip>1) {
         val <- round(seq(quants[1], quants[3],by=skip))
      }
      else {
         i.rnd <- abs(floor(log10(abs(skip))))
         val <- round(seq(quants[1], quants[3],by=skip),i.rnd)
         return(val)
      }
   }
   
   # VAL function for continuous focal
   
   myval.f <- function(fdata) {
      skip <- (max(fdata)-min(fdata))/100
      if (skip>1) {
         val <- round(seq(min(fdata), max(fdata),by=skip))
      }
      else {
         i.rnd <- abs(floor(log10(abs(skip))))
         val <- round(seq(min(fdata), max(fdata),by=skip),i.rnd)
      }
      return(val)
   }
   
   # Check for log transformations
   
   mod.parem <- paste(insight::find_parameters(model)$conditional,collapse=" ")
   log.f <- grepl(paste0("log\\(",focal,"\\)"),mod.parem)
   log.i <- grepl(paste0("log\\(",int,"\\)"),mod.parem)
   log.dv <- grepl("log\\(\\D*\\)",insight::find_terms(model)$response)
   
   # Get model columns 
   
   ol <- "log("
   cl <- ")"
   
   if (log.f) {
      log.foc <- paste0(ol,focal,cl)
      print(log.foc)
      col.f <- which(colnames(model$model)==log.foc)
   }
   else {
      col.f <- which(colnames(model$model)==focal)
   }
   
   if (log.i) {
      log.int <- paste0(ol,int,cl)
      print(log.int)
      col.i <- which(colnames(model$model)==log.int)
   }
   else {
      col.i <- which(colnames(model$model)==int)
   }
   # Check focal and interaction type numeric
   
   foc.type.num <- class(model$model[,col.f]) %in% c("numeric","integer")
   int.type.num <- class(model$model[,col.i]) %in% c("numeric","integer")
   
   # Extract focal/interaction data for numeric and calculate f.val, i.val
   
   if (foc.type.num) {
      f.data <- model$model[,col.f]
      if (log.f) {
         f.data <- exp(f.data)
      }
      f.val <- myval.f(f.data)
   }
   
   if (int.type.num) {
      i.data <- model$model[,col.i]
      if (log.i) {
         i.data <- exp(i.data)
      }
      i.val <- myval.i(i.data, ilevels=4)
   }
   
   # Color Palette functions
   
   cvi_colors <- list(bgsu=c("#F5C163", "#F7A44A", "#F98831", "#FB6C18",
                             "#FD5000", "#DA4805", "#B7410B", "#943A11", 
                             "#713317", "#4F2C1D"),
                      r4=c("#000000", "#DF536B", "#61D04F", "#2297E6", 
                           "#28E2E5", "#CD0BBC", "#F5C710", "#9E9E9E"))
   cvi_palettes  <-  function(name, n, all_palettes = cvi_colors, 
                              type = c("discrete", "continuous")) {
      palette = all_palettes[[name]]
      if (missing(n)) {
         n = length(palette)
      }
      type = match.arg(type)
      out = switch(type,
                   continuous = grDevices::colorRampPalette(palette)(n),
                   discrete = grDevices::colorRampPalette(palette)(n)
      )
      structure(out, name = name, class = "palette")
   }
   scale_fill_cvi_d <- function(name, n=NULL) {
      ggplot2::scale_fill_manual(values=cvi_palettes(name, type="discrete", n=n))
   }
   scale_color_cvi_d <- function(name, n=NULL) {
      ggplot2::scale_color_manual(values=cvi_palettes(name, type="discrete", n=n))
   }
   
   # Check interaction exists in model (if passed as argument) and set plot type
   
   if (!is.null(int)) {
      ic3 <- paste(insight::find_interactions(model)$conditional, collapse=" ")
      if (log.f) {
         if (log.i) {
            ic1 <- paste0(log.foc,":",log.int)
            ic2 <- paste0(log.int,":",log.foc)
         }
         else {
            ic1 <- paste0(log.foc,":",int)
            ic2 <- paste0(int,":",log.foc)
            
         }
      }
      else {
         if (log.i) {
            ic1 <- paste0(focal,":",log.int)
            ic2 <- paste0(log.int,":",focal)
            
         }
         else {
            ic1 <- paste0(focal,":",int)
            ic2 <- paste0(int,":",focal)
            
         }      
      }
      
      ic3 <- grepl(ic1,ic3, fixed=TRUE)|grepl(ic2,ic3, fixed=TRUE)
      
      if (!ic3) {
         error <- "Model doesn't include interaction between focal and interaction variables."
         warning(error)
      }
      if (foc.type.num) {
         if (int.type.num) {
            plot.type <- "cont.cont"
         }
         else {
            plot.type <- "cont.cat"
         }
      }
      else {
         if (int.type.num) {
            plot.type <- "cat.cont"
         }
         else {
            plot.type <- "cat.cat"
         }
      }
   }
   else {
      if (foc.type.num) {
         plot.type <- "cont.null"
      }
      else {
         plot.type <- "cat.null"
      }
   }
   # Create y label and check model is linear or binary logistic regression
   
   if (model$call[1]=="lm()") {
      ylabel <- paste0(all.vars(model$call$formula)[1]," (Predicted)")
   }
   else if (model$call[1]=="glm()" & model$family[1]=="binomial") {
      ylabel <- paste0(all.vars(model$call$formula)[1]," (Predicted Prob.)")
   }
   else {
      error <- "Model is not linear regression or binary logistic regression."
      stop(error)
   }
   
   # Create plots
   
   ## Continuous - Null
   
   if (plot.type=="cont.null") {
      fterm <- paste0(focal," [f.val]")
      ptable <- ggeffect(model, terms=fterm)
      if (log.dv) {
         ptable$predicted <- exp(ptable$predicted)
         ptable$conf.low <- exp(ptable$conf.low)
         ptable$conf.high <- exp(ptable$conf.high)
      }
      plot <- ptable %>% 
         ggplot(aes(x=x, y=predicted)) +
         geom_line(linewidth=1, color="darkorange") + 
         geom_ribbon(aes(ymin=conf.low, ymax=conf.high), 
                     alpha=0.2, fill="darkorange") +
         labs(y=ylabel, x=get_x_title(ptable)) +
         theme_bw()
   }
   
   ## Categorical - Null
   
   else if (plot.type=="cat.null") {
      ptable <- ggeffect(model, focal)
      # Get constants
      colorn <- length(unique(ptable$x))
      if (log.dv) {
         ptable$predicted <- exp(ptable$predicted)
         ptable$conf.low <- exp(ptable$conf.low)
         ptable$conf.high <- exp(ptable$conf.high)
      }
      plot <- ptable %>%
         ggplot(aes(x=x, y=predicted, color=x)) +
         geom_point(size=4) +
         geom_errorbar(aes(ymin=conf.low, ymax=conf.high),
                       width=0.5, linewidth=1) +
         labs(y=ylabel, x=get_x_title(ptable)) +
         scale_color_cvi_d("bgsu", n=colorn) +
         theme_bw() +
         theme(panel.grid.major.x=element_blank(),
               legend.position="none")
   }
   
   ## Continuous - Continuous
   
   else if (plot.type=="cont.cont") {
      fterm <- paste0(focal," [f.val]")
      #val <- myval(i.data, ilevels=4)
      iterm <- paste0(int, " [i.val]")
      ptable <- ggeffect(model, terms=c(fterm, iterm))
      if (log.dv) {
         ptable$predicted <- exp(ptable$predicted)
         ptable$conf.low <- exp(ptable$conf.low)
         ptable$conf.high <- exp(ptable$conf.high)
      }
      plot <- ptable %>%
         ggplot(aes(x=x, y=predicted, color=group, fill=group)) +
         geom_line(linewidth=1) + 
         geom_ribbon(aes(ymin=conf.low, ymax=conf.high), 
                     alpha=0.2) +
         scale_fill_cvi_d("bgsu", n=4) +
         scale_color_cvi_d("bgsu", n=4) +
         labs(y=ylabel, x=get_x_title(ptable),
              color=get_legend_title(ptable),
              fill=get_legend_title(ptable)) +
         theme_bw() +
         theme(panel.grid.major.x=element_blank(),
               legend.position = "bottom")
   }
   
   ## Continuous - Categorical
   
   else if (plot.type=="cont.cat") {
      fterm <- paste0(focal," [n=50]")
      iterm <- int
      ptable <- ggeffect(model, terms=c(fterm, iterm))
      if (log.dv) {
         ptable$predicted <- exp(ptable$predicted)
         ptable$conf.low <- exp(ptable$conf.low)
         ptable$conf.high <- exp(ptable$conf.high)
      }
      colorn <- length(unique(ptable$group))
      plot <- ptable %>%
         ggplot(aes(x=x, y=predicted, color=group, fill=group)) +
         geom_line(linewidth=1) + 
         geom_ribbon(aes(ymin=conf.low, ymax=conf.high), 
                     alpha=0.2) +
         scale_fill_cvi_d("bgsu", n=colorn) +
         scale_color_cvi_d("bgsu", n=colorn) +
         labs(y=ylabel, x=get_x_title(ptable),
              color=get_legend_title(ptable),
              fill=get_legend_title(ptable)) +
         theme_bw() +
         theme(panel.grid.major.x=element_blank(),
               legend.position = "bottom")
   }  
   
   ## Categorical - Continuous
   
   else if (plot.type=="cat.cont") {
      fterm <- focal
      #val <- myval(i.data, ilevels=4)
      iterm <- paste0(int, " [val]")
      ptable <- ggeffect(model, terms=c(fterm, iterm))
      glabel <- get_legend_title(ptable)
      appender <- function(string, prefix=glabel) paste0(prefix,"=", string)
      colorn <- length(unique(ptable$x))
      plot <- ptable %>%
         ggplot(aes(x=x, y=predicted, group=1)) +
         geom_point(aes(color=x), size=4) +
         geom_errorbar(aes(ymin=conf.low, ymax=conf.high, color=x),
                       width=0.5, linewidth=1) +
         labs(y=ylabel, x=get_x_title(ptable)) +
         facet_wrap(~group, labeller = as_labeller(appender)) +
         scale_color_cvi_d("bgsu", n=colorn) +
         theme_bw() +
         theme(panel.grid.major.x=element_blank(),
               legend.position="none")
   }
   
   ## Categorical - Categorical
   
   else if (plot.type=="cat.cat") {
      fterm <- focal
      iterm <- int
      ptable <- ggeffect(model, terms=c(fterm, iterm))
      glabel <- get_legend_title(ptable)
      appender <- function(string, prefix=glabel) paste0(prefix,"=", string)
      colorn <- length(unique(ptable$x))
      plot <- ptable %>%
         ggplot(aes(x=x, y=predicted, group=1)) +
         geom_point(aes(color=x), size=4) +
         geom_errorbar(aes(ymin=conf.low, ymax=conf.high, color=x),
                       width=0.5, linewidth=1) +
         labs(y=ylabel, x=get_x_title(ptable)) +
         facet_wrap(~group, labeller = as_labeller(appender)) +
         scale_color_cvi_d("bgsu", n=colorn) +
         theme_bw() +
         theme(panel.grid.major.x=element_blank(),
               legend.position="none")
   }
   
   return(list(plot=plot,ptable=ptable))
}