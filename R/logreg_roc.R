#' @title ROC Curve
#' @description This function accepts a binary logistic regression - glm() -
#'    object and returns the ROC curve.
#' @details
#' REQUIRED PACKAGES:
#' \itemize{
#'   \item pROC
#'   \item ggplot2
#' }
#' @param MOD The saved results of a binary logistic regression glm() call
#' @param DATA The name of the data frame for which the classification matrix
#'    should be produced (i.e., training data, holdout data, etc.)
#' @return ggplot2 object
#' @examples
#' model <- glm(buy ~ age + gender, train, family="binomial")
#'
#' logreg_roc(model, train)

logreg_roc <- function(MOD, DATA) {
   require(pROC)
   require(ggplot2)
   dvprob <- predict(MOD, DATA, type="response")
   true <- DATA[[toString(formula(MOD)[[2]])]]
   rocobj <- roc(true, dvprob)
   auc <- round(auc(true, dvprob),4)
   ggroc(rocobj, color="midnightblue", size=1.5) +
      annotate("text", label=(paste0("AUC = ", auc)),
               x=.52, y=.35, size=5, color="red") +
      labs(x="Specificity", y="Sensitivity") +
      geom_segment(aes(x=0, y=1, xend=1, yend=0))
}
