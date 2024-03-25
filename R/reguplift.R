#' @title Regression Uplift Modeling
#' @description This function performs uplift modeling based on either logistic
#'     regression (for binary outcomes) or linear regression (for continuous 
#'     outcomes). The function uses the two-model, indirect modeling approach.
#' @details
#' REQUIRED PACKAGES:
#' \itemize{
#'   \item \code{ggplot2}
#'   \item \code{gtsummary} (if \code{ct="Y"} option used)
#'   \item \code{flextable} (if \code{ct="Y"} or \code{int="Y"} option used)
#' }
#' @param model A logistic or linear regression model saved results. The 
#'     model must have been run where the treatment variable was the first term
#'     in the right-hand side of the model formula, followed by all independent
#'     variables. For option \code{int="Y"}, no interaction terms should have
#'     been included in the original model.
#' @param treatment The variable name identifying the treatment variable. Must 
#'     be in quotations.
#' @param pdata That data upon which to calculate the lift. Default is
#'     \code{NULL}, in which case the lift will be calculated using the original
#'     model data.
#' @param ng The number of groups to split the data for the group output table 
#'     and the plots. Must be an integer between 5 and 20. Default is 10.
#' @param int An indicator if an interaction check between independent variables
#'     is desired (\code{int="Y"}) or not (\code{int="N"}). Default is N.
#' @param ct An indicator if comparison tables between treatment levels is
#'     desired (\code{ct="Y"}) or not (\code{ct="N"}). Default is N.
#' @return A list containing the following objects:
#' \itemize{
#'   \item \code{$group} Lift results by ordered group based on \code{ng}
#'   \item \code{$all} Original data or \code{pdata} with lift values appended
#'   \item \code{$plots} A Qini plot (\code{$qini}), a mean uplift plot by 
#'     ordered group (\code{$uplift}), and a cumulative gain plot by ordered
#'     group (\code{$c.gain})
#'   \item \code{$comp} Comparison table between treatment levels (if requested).
#'   \item \code{$int} Interaction table showing significant potential 
#'     interactions (if requested).
#' }
#' @examples
#' results.visit.w <- reguplift(model.visit.w, "promotion", ng=10)
#' results.visit.w <- reguplift(model.visit.w, "promotion", ng=10, ct="Y", int="Y")

reguplift <- function (model, treatment, pdata=NULL, 
                     ng=10, ar=NULL, int="N", ct="N") {
   require(ggplot2)
   t.comp <- NULL
   sig.ints <- NULL
   if (!(ng %in% 5:20)) {
      error <- "Number of groups (ng) must be an integer between 5 and 20"
      stop(error)
   }
   if (!is.null(ct) & !(ct %in% c("Y","N"))) {
      error <- "Comparison Table (ct) must be 'Y' or 'N'"
      stop(error)
   }
   if (!is.null(int) & !(int %in% c("Y","N"))) {
      error <- "Interactions Table (int) must be 'Y' or 'N'"
      stop(error)
   }
   if (ct=="Y") {
      require(gtsummary)
      require(flextable)
      glance.extra <- function(model, ...) {
         df_glance <- broom::glance(model) 
         nullmodel <- glm(outcome~1, data=model$data, family="binomial")
         df_glance$p <- round(anova(nullmodel,model, test="LR")[["Pr(>Chi)"]][2],3)
         df_glance$mrsq <- round(1-model$deviance/model$null.deviance,3)
         df_glance
      }
   }
   if (int=="Y") {
      require(flextable)
   }
   outcome <- all.vars(model$call$formula)[1]
   if (!is.null(ar) & class(ar)!="numeric") {
      error <- "Aspect Ratio (ar) must be numeric."
      stop(error)  }
   
   if (model$call[1]=="glm()") {
      if (model$family$family=="binomial" & model$family$link=="logit") {
         outcometype <- "bin"
         data <- data.frame(model$data)
      }
      else {
         error <- "Model type must be binary logistic regression (glm with family=binomial(link=\"logit\")) or linear regression (lm)."
         stop(error)
      }
   }
   else if (model$call[1]=="lm()") {
      outcometype <- "cont"
      data <- data.frame(model$model)
   }
   else {
      error <- "Model type must be binary logistic regression (glm with family=binomial(link=\"logit\")) or linear regression (lm)."
      stop(error)
   }
   col.out <- which(colnames(data)==outcome)
   col.pred <- which(colnames(data)==treatment)
   
   # Code outcome variable for logistic regression model
   
   if (outcometype=="bin") {
      if (class(data[[outcome]]) %in% c("factor", "character")) {
         if (apply(data,2,function(x) {all(x %in% c("Yes", "No"))})[[outcome]]) {
            data$outcome <- ifelse(data[[outcome]]=="Yes",1,0)
            if (!is.null(pdata)) {
               pdata$outcome <- ifelse(pdata[[outcome]]=="Yes",1,0)
            }
         }
         else {
            error <- 'For a binary logistic model, the outcome variable must be either:\n  (1) numeric variable coded as (0,1);\n  (2) factor/character variable coded as ("Yes","No"); or\n  (3) logical variable.'
            stop(error)    
         }
      }
      else if (class(data[[outcome]])=="logical") {
         data$outcome <- ifelse(data[[outcome]]==TRUE,1,0)
         if (!is.null(pdata)) {
            pdata$outcome <- ifelse(pdata[[outcome]]==TRUE,1,0)
         }
      }
      else if (class(data[[outcome]]) %in% c("integer", "numeric")) {
         if (apply(data,2,function(x) {all(x %in% 0:1)})[[outcome]]) {
            data$outcome <- data[[outcome]]
            if (!is.null(pdata)) {
               pdata$outcome <- pdata[[outcome]]
            }
         }
         else {
            error <- 'For a binary logistic model, the outcome variable must be either:\n  (1) numeric variable coded as (0,1);\n  (2) factor/character variable coded as ("Yes","No"); or\n  (3) logical variable.'
            stop(error)    
         }
      }
      else {
         error <- 'For a binary logistic model, the outcome variable must be either:\n  (1) numeric variable coded as (0,1);\n  (2) factor/character variable coded as ("Yes","No"); or\n  (3) logical variable.'
         stop(error)    
      }
   }
   else {
      if (class(data[[outcome]]) %in% c("numeric", "integer")) {
         data$outcome <- data[[outcome]]
         if (!is.null(pdata)) {
            pdata$outcome <- pdata[[outcome]]
         }
      }
      else {
         error <- 'For a linear regression model, the outcome variable must be a numeric/integer variable type.'
         stop(error)
      }
   }
   
   # Check treatment variable and recode to 0,1
   
   if (length(table(data[[treatment]]))!=2) {
      error <- "The treatment variable must have two levels only."
      stop(error)
   }
   else {
      if (class(data[[treatment]]) %in% c("factor", "character")) {
         if (apply(data,2,function(x) {all(x %in% c("Yes", "No"))})[[treatment]]) {
            data$treat <- ifelse(data[[treatment]]=="Yes",1,0)
            if (!is.null(pdata)) {
               pdata$treat <- ifelse(pdata[[treatment]]=="Yes",1,0)
            }
         }
         else {
            error <- 'The treatment variable must be either:\n  (1) numeric variable coded as (0,1);\n  (2) factor/character variable coded as ("Yes","No"); or\n  (3) logical variable.'
            stop(error)    
         }
      }
      else if (class(data[[treatment]])=="logical") {
         data$treat <- ifelse(data[[treatment]]==TRUE,1,0)
         if (!is.null(pdata)) {
            pdata$treat <- ifelse(pdata[[treatment]]==TRUE,1,0)
         }
      }
      else if (class(data[[treatment]]) %in% c("integer", "numeric")) {
         if (apply(data,2,function(x) {all(x %in% 0:1)})[[treatment]]) {
            data$treat <- data[[treatment]]
            if (!is.null(pdata)) {
               pdata$treat <- pdata[[treatment]]
            }
         }
         else {
            error <- 'The treatment variable must be either:\n  (1) numeric variable coded as (0,1);\n  (2) factor/character variable coded as ("Yes","No"); or\n  (3) logical variable.'
            stop(error)    
         }
      }
      else {
         error <- 'The treatment variable must be either:\n  (1) numeric variable coded as (0,1);\n  (2) factor/character variable coded as ("Yes","No"); or\n  (3) logical variable.'
         stop(error)    
      }
   }
   
   model.formula <- model$call$formula
   model.formula <- paste(gsub(outcome, "outcome", model.formula[2]), 
                          model.formula[1], 
                          gsub(treatment,"treat",model.formula[3]), sep=" ")
   model.formula <- gsub("treat \\*|\\* treat|treat \\+", "", model.formula)
   model.formula <- gsub(" ","", model.formula)
   
   # Split Sample
   data.0 <- data[data$treat == 0,]
   data.1 <- data[data$treat == 1,]
   
   # Check Interactions
   if (int=="Y") {
      mf1 <- "outcome~"
      mf2 <- strsplit(model.formula, split = "~")[[1]][2]
      params <- strsplit(mf2, split = "\\+")[[1]]
      sig.ints <- NULL
      sig.ints <- data.frame(c1 = double(), c2 = double(), 
                             c3 = double(), c4 = double(), c5 = character(),
                             c6=(character()))
      for (var in params) {
         mf <- paste0(mf1,var,"*(",mf2,")")
         if (model$call[1]=="glm()") {
            m0 <- data.frame(summary(glm(mf, data = data.0, 
                                         family = binomial(link = "logit")))$coefficients)
            m1 <- data.frame(summary(glm(mf, data = data.1, 
                                         family = binomial(link = "logit")))$coefficients)
         }
         else if (model$call[1]=="lm()") {
            m0 <- data.frame(summary(lm(mf, data=data.0))$coefficients)
            m1 <- data.frame(summary(lm(mf, data=data.1))$coefficients)
         }
         m0$model <- "Control"
         m0 <- m0[m0[, 4] < 0.1 & grepl(":", rownames(m0)), ]
         m0$int <- rownames(m0)
         m1$model <- "Treat"
         m1 <- m1[m1[, 4] < 0.1 & grepl(":", rownames(m1)), ]
         m1$int <- rownames(m1)
         sig.ints <- rbind(sig.ints, 
                           m0[m0[, 4] < 0.1 & grepl(":",
                                                    rownames(m0)), ])
         sig.ints <- rbind(sig.ints, 
                           m1[m1[, 4] < 0.1 & grepl(":",
                                                    rownames(m1)), ])
      }
      sig.ints[, 4] <- round(sig.ints[, 4], 3)
      sig.ints <- sig.ints[, 4:6]
      sig.ints <- within(sig.ints, 
                         var1<-data.frame(do.call('rbind', 
                                                  strsplit(as.character(int), ':',
                                                           fixed=TRUE))))
      sig.ints <- within(sig.ints, 
                         int <- ifelse(var1$X1<var1$X2,
                                       paste0(var1$X1,":",var1$X2),
                                       paste0(var1$X2,":",var1$X1)))
      sig.ints <- sig.ints[,1:3]
      sig.ints <- aggregate(sig.ints[,1], 
                            list(sig.ints[,3], sig.ints[,2]), FUN=min)
      colnames(sig.ints) <- c("int", "model", "p")
      sig.ints <- reshape(sig.ints, idvar = "int", timevar = "model", 
                          direction = "wide")
      sig.ints <- sig.ints[order(sig.ints$int),]
      sig.ints <- flextable(sig.ints)
      sig.ints <- set_header_labels(sig.ints, int="Interaction", 
                                    p.Control="Control",
                                    p.Treat="Treat")
      sig.ints <- bold(sig.ints, bold=TRUE, part="header") 
      sig.ints <- fontsize(sig.ints, i=1, size=14, part="header") 
      sig.ints <- fontsize(sig.ints, size=12, part="body")
      sig.ints <- height(sig.ints, part="body", height=4, unit="mm") 
      sig.ints <- hrule(sig.ints, part="body", rule="exact")
      sig.ints <- padding(sig.ints, padding.top=1, padding.bottom = 1, 
                          part="body") 
      footer2line <- paste0("Outcome = ", outcome)
      footer3line <- paste0("Control: ",treatment," = 0")
      footer4line <- paste0("Treat: ", treatment," = 1")
      sig.ints <- add_footer_lines(sig.ints,
                                   as_paragraph(as_sup("1 "),
                                                "Values are p-values for interaction\n",
                                                as_sup("2 "),footer2line,"\n",
                                                as_sup("3 "),footer3line,"\n",
                                                as_sup("4 "),footer4line))
      sig.ints <- padding(sig.ints, padding.top=1, padding.bottom = 1, 
                          part="body") 
   }
   
   
   # Run models for treatment and control
   
   if (model$call[1]=="glm()") {
      model0 <- glm(model.formula, data=data.0, family=binomial(link="logit"))
      model1 <- glm(model.formula, data=data.1, family=binomial(link="logit"))
      t.exp <- TRUE
      t.est <- "**OR**"
   }
   else if (model$call[1]=="lm()") {
      model0 <- lm(model.formula, data=data.0)
      model1 <- lm(model.formula, data=data.1)
      t.exp <- FALSE
      t.est <- "**Coeff**"
      
   }
   
   # Comparison Tables
   if (ct=="Y") {
      t.control <- tbl_regression(model0,
                                  intercept=TRUE, exponentiate = t.exp,
                                  pvalue_fun = function(x) style_pvalue(x, digits=3),
                                  estimate_fun = function(x) style_number(x, digits=3),
                                  conf.int=FALSE)
      t.control <- add_significance_stars(t.control,
                                          pattern="{estimate}{stars}",
                                          hide_se=TRUE)
      t.control <- modify_header(t.control, estimate=t.est,
                                 label='**Variable**')
      t.treat <- tbl_regression(model1,
                                intercept=TRUE, exponentiate = t.exp,
                                pvalue_fun = function(x) style_pvalue(x, digits=3),
                                estimate_fun = function(x) style_number(x, digits=3),
                                conf.int=FALSE)
      t.treat <- add_significance_stars(t.treat,
                                        pattern="{estimate}{stars}",
                                        hide_se=TRUE)
      t.treat <- modify_header(t.treat, estimate=t.est,
                               label='**Variable**')
      if (model$call[1]=="glm()") {
         t.control <- add_glance_table(t.control,
                                       include=c(p,mrsq),
                                       glance_fun=glance.extra,
                                       label=list(p~"p-value",
                                                  mrsq="McFadden's R\u00B2"))
         t.treat <- add_glance_table(t.treat,
                                     include=c(p,mrsq),
                                     glance_fun=glance.extra,
                                     label=list(p~"p-value",
                                                mrsq="McFadden's R\u00B2"))
      }
      else if (model$call[1]=="lm()") {
         t.control <- add_glance_table(t.control,
                                       include=c(p.value, r.squared),
                                       label=list(p.value~"p-value",
                                                  r.squared="R\u00B2"))
         t.treat <- add_glance_table(t.treat,
                                     include=c(p.value, r.squared),
                                     label=list(p.value~"p-value",
                                                r.squared="R\u00B2"))
      }
      
      t.comp <- tbl_merge(list(t.control, t.treat),
                          tab_spanner=c("**Control**","**Treat**"))
      t.comp <- as_flex_table(t.comp)
      t.comp <- bold(t.comp, bold=TRUE, part="header") 
      t.comp <- fontsize(t.comp, i=1, size=14, part="header") 
      t.comp <- fontsize(t.comp, size=12, part="body")
      t.comp <- height(t.comp, part="body", height=4, unit="mm") 
      t.comp <- hrule(t.comp, part="body", rule="exact")
      t.comp <- padding(t.comp, padding.top=1, padding.bottom = 1, part="body") 
      t.comp <- padding(t.comp, padding.top=1, padding.bottom = 1, part="footer")
   }
   # Predicted LIFT
   
   ## Pick data to predict with
   
   if (is.null(pdata)) {
      lift.data <- data
   }
   else {
      lift.data <- pdata
   }
   
   ## Calculate predicted LIFT
   
   lift.data$out1 <- predict(model1, lift.data, type="response")
   lift.data$out0 <- predict(model0, lift.data, type="response")
   lift.data$lift <- lift.data$out1-lift.data$out0
   
   
   # Rank by lift and place into 'ng' groups
   lift.data$rank <- 0
   lift.data$rank <- rank(-lift.data$lift, ties.method="random")
   lift.data$group <- cut(lift.data$rank, ng, labels=FALSE)
   
   # UPLIFT by GROUP
   
   if (outcometype=="bin") {
      ## For Binary Outcome
      ### Initialize UPLIFT RESULTS TABLE
      
      dataResults <- data.frame(matrix(rep(0), ng, 10))
      colnames(dataResults) <- c("cum_per", "T_Yn", "T_n", "C_Yn", 
                                 "C_n", "inc_Yn", "uplift", "cm_Yn", "cm_uplift",
                                 "cm_gain")
      
      for(i in 1:ng){
         subset <- lift.data[lift.data$group == i, ]
         dataResults[i,1] <- i/ng
         dataResults[i,2] <- sum(subset$treat == 1 & subset$outcome == 1)
         dataResults[i,3] <- sum(subset$treat == 1)
         dataResults[i,4] <- sum(subset$treat == 0 & subset$outcome == 1)
         dataResults[i,5] <- sum(subset$treat == 0)
         dataResults[i,6] <- dataResults[i, 2] - dataResults[i, 4]*dataResults[i, 3]/dataResults[i, 5]
         dataResults[i,7] <- (dataResults[i,2]/dataResults[i,3]-dataResults[i,4]/dataResults[i,5])*100
      }
      TT_n <- sum(dataResults$T_n)
      dataResults[,8] <- (cumsum(dataResults[,2])-cumsum(dataResults[,4])*cumsum(dataResults[,3])/cumsum(dataResults[,5]))
      dataResults[,9] <- (cumsum(dataResults[,2])/cumsum(dataResults[,3]) - cumsum(dataResults[,4])/cumsum(dataResults[,5]))*100
      dataResults[,10] <- 100*(cumsum(dataResults[,2] - dataResults[,4] * sum(dataResults[, 3]) / sum(dataResults[, 5])) / sum(dataResults[, 3]))
   }
   
   else {
      ## For Continuous Outcome
      ### Initialize UPLIFT RESULTS TABLE
      
      dataResults <- data.frame(matrix(rep(0), ng, 10))
      colnames(dataResults) <- c("cum_per", "Tm_o", "T_n", "Cm_o", "C_n", 
                                 "inc_Yn", "uplift","cm_Yn","cm_uplift", "cm_gain")
      
      for(i in 1:ng){
         subset1 <- lift.data[lift.data$group == i & lift.data$treat == 1, ]
         subset0 <- lift.data[lift.data$group == i & lift.data$treat == 0, ]
         dataResults[i,1] <- i/ng
         dataResults[i,2] <- mean(subset1$outcome)
         dataResults[i,3] <- nrow(subset1)
         dataResults[i,4] <- mean(subset0$outcome)
         dataResults[i,5] <- nrow(subset0)
         dataResults[i,6] <- sum(subset1$outcome)-sum(subset0$outcome)*nrow(subset1)/nrow(subset0)
         dataResults[i,7] <- (dataResults[i,2]-dataResults[i,4])
      }
      TT_n <- sum(dataResults$T_n)
      dataResults[,8] <- cumsum(dataResults[,2]*dataResults[,3]) - cumsum(dataResults[,4]*dataResults[,5]) * cumsum(dataResults[,3])/cumsum(dataResults[,5])
      dataResults[,9] <- (cumsum(dataResults[,2]*dataResults[,3])/cumsum(dataResults[,3])-cumsum(dataResults[,4]*dataResults[,5])/cumsum(dataResults[,5]))
      dataResults[,10] <- (cumsum(dataResults[,2]*dataResults[,3])-cumsum(dataResults[,4]*dataResults[,5])*sum(dataResults[,3])/sum(dataResults[,5]))/sum(dataResults[,3])
   }
   # Kendalls Tau
   ken.tau <- cor.test(seq(nrow(dataResults),1), 
                       dataResults[,7], method="kendall")
   kt.est <- paste0(round(ken.tau$estimate,3))
   kt.p <- ifelse(ken.tau$p.value<0.001,
                  "< 0.001",
                  paste0("= ", round(ken.tau$p.value,3)))
   # QINI Coefficient Q
   ## Order Lift Table
   sub <- lift.data[order(lift.data$rank),c("outcome", "treat", "rank")]
   
   N <- nrow(sub)
   sub$C_Ot <- cumsum(ifelse(sub$treat==1,sub$outcome,0))
   sub$C_Oc <- cumsum(ifelse(sub$treat==0,sub$outcome,0))
   sub$C_Tt <- cumsum(ifelse(sub$treat==1,1,0))
   sub$C_Tc <- cumsum(ifelse(sub$treat==0,1,0))
   sum_Tt <- sub[N,"C_Tt"]
   sum_Tc <- sub[N,"C_Tc"]
   
   if (outcometype=="bin") {
      sub$cum.gain <- 100*(sub$C_Ot-sub$C_Oc*sum_Tt/sum_Tc)/sum_Tt
   }
   else {
      sub$cum.gain <- (sub$C_Ot-sub$C_Oc*sum_Tt/sum_Tc)/sum_Tt
   }
   
   ## Calculate AUC
   ### Calculate area under cumulative gains curve
   auc <- 0
   x <- seq(1/N,1,1/N)
   y <- sub$cum.gain
   for (i in 2:length(x)) {
      auc <- auc+0.5*(x[i]-x[i-1])*(y[i]+y[i-1])
   }
   ### Calculate area under random cumulative gains curve
   ovr.cum.gain <- sub[N,"cum.gain"]
   random.cum.gain <- cumsum(rep(ovr.cum.gain / N, N))
   yanno <- mean(random.cum.gain)
   y.rand <- random.cum.gain
   auc.rand <- 0
   for (i in 2:length(x)) {
      auc.rand <- auc.rand+0.5*(x[i]-x[i-1])*(y.rand[i]+y.rand[i-1])
   }
   ###  Compute different between areas (Qini Coefficient)
   Qini <- auc-auc.rand
   
   
   
   # Create Plots
   
   ## Uplift
   uplift <- ggplot(aes(x=factor(cum_per), y=uplift), data=dataResults) +
      geom_col(fill="darkorange") + 
      labs(x="Percentile", y="Mean Uplift",
           caption=paste0("Kendall's tau = ", kt.est, "; p ", kt.p)) +
      theme_bw() + 
      theme(aspect.ratio = ar,
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill="white"),
            plot.background = element_rect(fill="transparent", 
                                           color=NA_character_))
   
   # ## Cumulative Uplift
   # c.uplift <- ggplot(aes(x=factor(cum_per), y=cm_uplift), data=dataResults) +
   #   geom_col(fill="darkorange") + 
   #   labs(x="Percentile", y="Mean Cumulative Incremental Uplift") +
   #   theme_bw() + 
   #   theme(aspect.ratio = ar,
   #         panel.grid.major.x = element_blank(),
   #         panel.grid.minor = element_blank(),
   #         panel.background = element_rect(fill="white"),
   #         plot.background = element_rect(fill="transparent", 
   #                                        color=NA_character_))
   
   ## Cumulative Gain
   c.gain <- ggplot(aes(x=factor(cum_per), y=cm_gain), data=dataResults) +
      geom_col(fill="darkorange") + 
      labs(x="Percentile", y="Mean Cumulative Incremental Gain") +
      theme_bw() + 
      theme(aspect.ratio = ar,
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill="white"),
            plot.background = element_rect(fill="transparent", 
                                           color=NA_character_))
   
   
   ## QINI Plot
   data <- rbind(data.frame("x"=x, "y"=y, "type" = "Model"), 
                 data.frame("x"=x, "y"=y.rand, "type" = "Random"))
   qini <- ggplot(aes(x=x, y=y, color=type), data=data) +
      geom_line(aes(size=type)) + 
      scale_color_manual(values=c("darkorange","#4f2c1d")) + 
      scale_size_manual(values=c(1,.5)) +
      scale_x_continuous(breaks=seq(0,1,.1), 
                         labels=scales::percent_format()) +
      labs(x="Percent of population targeted",
           y="Cumulative Incremental Gains") +
      annotate("text", label=paste0("Q = ", sprintf("%.2f", Qini)), 
               x=0.75, y=yanno, size=5, color="red") +
      theme_bw() + 
      theme(panel.grid.minor=element_blank(),
            legend.position = c(0.01,.90),
            legend.justification="left",
            legend.title=element_blank(),
            legend.margin=margin(0,0,0,0),
            legend.background = element_blank(),
            aspect.ratio = ar,
            panel.grid.major.x = element_blank(),
            panel.background = element_rect(fill="white"),
            plot.background = element_rect(fill="transparent", 
                                           color=NA_character_))
   plots=list(qini=qini, uplift=uplift, c.gain=c.gain)
   models=list(control=model0,treat=model1)
   
   return(list(group=dataResults, all=lift.data, 
               plots=plots, comp=t.comp, int=sig.ints))
   
}