#' @title Sensitivity/Specificity Plot
#' @description This function accepts a binary logistic regression - glm() -
#'    object and returns a sensitivity/specificity plot.
#' @details
#' REQUIRED PACKAGES:
#' \itemize{
#'   \item ggplot2
#' }
#' @param MOD The saved results of a binary logistic regression glm() call
#' @param DATA The name of the data frame for which the classification matrix
#'    should be produced (i.e., training data, holdout data, etc.)
#' @param POSITIVE The level of the factor variable that is "true" or a
#'    "success"; must be in quotations
#' @return A ggplot object
#' @examples
#' model <- glm(buy ~ age + gender, train, family="binomial")
#'
#' logreg_cut(model, train, "Yes")

logreg_cut <- function(MOD, DATA, POS) {
   require(ggplot2)
   true <- relevel(DATA[[toString(formula(MOD)[[2]])]], ref=POS)
   true <- as.numeric(DATA[[toString(formula(MOD)[[2]])]])-1
   p.prob <- predict(MOD, DATA, type="response")
   mat <- matrix(nrow=303,
                 ncol=3,
                 dimnames=list(seq(1,303,1),
                               c("co","value","type")))
   for (i in 1:101) {
      mat[i,1] <- (i-1)/100
      mat[i+101,1] <- (i-1)/100
      mat[i+202,1] <- (i-1)/100
      df <- data.frame(cbind(true, pred=ifelse(p.prob>=mat[i,1],1,0)))
      mat[i,2] <- sum(df$pred==1 & df$true==1)/sum(df$true)
      mat[i,3] <- "sens"
      mat[i+101,2] <- sum(df$pred==0 & df$true==0)/(nrow(df)-sum(df$true))
      mat[i+101,3] <- "spec"
      mat[i+202,2] <- sum(df$pred==df$true)/nrow(df)
      mat[i+202,3] <- "acc"

   }
   r1 <- data.frame(mat)
   r2 <- reshape(r1,timevar="type",idvar="co", direction="wide")

   p <- ggplot(aes(x=as.numeric(co),
                   y=as.numeric(value),
                   color=factor(type)), data=r1) +
      geom_line(aes(size=factor(type))) +
      geom_point(aes(shape=factor(type))) +
      scale_shape_manual("Measure", values=c(NA,19,19),
                         labels=c("Accuracy", "Sensitivity", "Specificity")) +
      scale_size_manual("Measure",values=c(1.2,.7,.7),
                        labels=c("Accuracy", "Sensitivity", "Specificity")) +
      scale_color_manual("Measure",values=c("red4", "forestgreen", "blue"),
                         labels=c("Accuracy", "Sensitivity", "Specificity")) +
      scale_x_continuous("Cutoff Value", breaks=seq(0,1,.1)) +
      scale_y_continuous("Measure Value", breaks=seq(0,1,.1)) +
      theme(legend.position = "bottom")
   suppressWarnings(print(p))
}
