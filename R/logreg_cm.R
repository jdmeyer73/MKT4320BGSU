#' @title Classification Matrix and Statistics
#' @description This function accepts a binary logistic regression - glm() -
#'    object and returns the classification matrix and associated statistics.
#' @details
#' REQUIRED PACKAGES:
#' \itemize{
#'   \item caret
#' }
#' @param MOD The saved results of a binary logistic regression glm() call
#' @param DATA The name of the data frame for which the classification matrix
#'    should be produced (i.e., training data, holdout data, etc.)
#' @param POSITIVE The level of the factor variable that is "true" or a
#'    "success"; must be in quotations
#' @param CUTOFF Cutoff threshold for predicted success/failure (default = 0.5)
#' @return Classification matrix and statistics
#' @examples
#' model <- glm(buy ~ age + gender, train, family="binomial")
#'
#' logreg_cm(model, train, "Yes")
#' logreg_cm(model, train, "Yes", 0.6)

logreg_cm <- function(MOD, DATA, POSITIVE, CUTOFF=0.5) {
   require(caret)
   dvprob <- predict(MOD, DATA, type="response")
   dvnum <- ifelse(dvprob<CUTOFF, 1, 2)
   dvfac <- factor(dvnum)
   levels(dvfac) <- levels(MOD$model[,1])
   true <- DATA[[toString(formula(MOD)[[2]])]]
   cm <- confusionMatrix(dvfac, true, POSITIVE)
   print(cm)
   numpos <- table(DATA[[toString(formula(MOD)[[2]])]])[POSITIVE]
   numobs <- nrow(DATA)
   pcc <- paste0("PCC = ",
                 sprintf("%.2f%%",
                         ((numpos/numobs)^2 + (1-(numpos/numobs))^2)*100))
   cat(pcc)
}
