#' @title Split Sample
#' @description This function creates a training and test sample based on the
#'    data and outcome variable provided
#' @details
#' REQUIRED PACKAGES:
#' \itemize{
#'   \item caret
#' }
#' @param data The data to be split.
#' @param outcome Name of the outcome/dependent variable to be modeled. 
#' @param p Percent of data to be in the training sample (remainder is in test).
#'    Defaults to 0.75 if excluded.
#' @param seed Seed value for random number generator.
#'    Defaults to 4320 if excluded.
#' @return
#' \itemize{ 
#'   \item \code{train} The training data; will be assigned to global environment.
#'   \item \code{test} The testing data; will be assigned to the global environment.
#' }
#' @examples
#' splitsample(directmktg, buy, 0.6, 123)
splitsample <- function(data, outcome, p=0.75, seed=4320) {
   require(caret)
   if (p>=1 | p<=0) {
      error <- "p must be between 0 and 1"
      stop(error)
   }
   outcome <- deparse(substitute(outcome))
   out.col <- which(colnames(data)==outcome)
   set.seed(seed)
   inTrain <- createDataPartition(y=data[[out.col]],
                                  p=p,
                                  list=FALSE)
   train <- data[inTrain,]
   test <- data[-inTrain,]
   assign("train", train, envir=.GlobalEnv)
   assign("test", test, envir=.GlobalEnv)
}