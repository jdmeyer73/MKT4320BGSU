#' @title Check Train/Test Split
#' @description
#' Diagnostic summary for a train/test split created by
#' \code{\link{splitsample}}.
#'
#' @details
#' This function summarizes sample sizes and the distribution of the outcome
#' variable across the training and testing datasets.
#'
#' The outcome variable \strong{must be a factor} in both datasets. If the
#' outcome is numeric or character, the function stops with a clear error
#' explaining how to convert it.
#'
#' @param train A data frame containing the training data.
#' @param test A data frame containing the testing data.
#' @param outcome Name of the factor outcome variable (unquoted or quoted).
#'
#' @return
#' An invisible list with:
#' \itemize{
#'   \item \code{n_summary}: Data frame of row counts and proportions for train/test.
#'   \item \code{outcome_summary}: List of class counts and proportions for each split.
#' }
#'
#' The function prints a concise summary to the console.
#'
#' @examples
#' \dontrun{
#' # Correct usage
#' directmktg$buy <- factor(directmktg$buy, levels = c("no", "yes"))
#' check_split(train, test, buy)
#'
#' # Incorrect: numeric outcome triggers an error
#' # check_split(train, test, buy_numeric)
#' }
#'
#' @importFrom rlang ensym as_name
#' @export
check_split <- function(train, test, outcome) {
   
   # ----- basic checks -----
   
   if (!is.data.frame(train))
      stop("`train` must be a data frame.", call. = FALSE)
   
   if (!is.data.frame(test))
      stop("`test` must be a data frame.", call. = FALSE)
   
   # Allow unquoted or quoted outcome
   outcome_name <- rlang::as_name(rlang::ensym(outcome))
   
   if (!outcome_name %in% names(train))
      stop(sprintf("Outcome variable '%s' not found in `train`.", outcome_name),
           call. = FALSE)
   
   if (!outcome_name %in% names(test))
      stop(sprintf("Outcome variable '%s' not found in `test`.", outcome_name),
           call. = FALSE)
   
   # ----- enforce factor outcome -----
   
   if (!is.factor(train[[outcome_name]]) || !is.factor(test[[outcome_name]])) {
      
      stop(
         paste0(
            "The outcome variable '", outcome_name, "' must be a FACTOR.\n",
            "It appears to be numeric or character.\n\n",
            "Convert it first, for example:\n",
            "  data$", outcome_name, " <- factor(data$", outcome_name, ")\n\n",
            "For binary outcomes, specify levels explicitly, e.g.:\n",
            "  data$", outcome_name, " <- factor(data$", outcome_name,
            ", levels = c('no','yes'))\n"
         ),
         call. = FALSE
      )
   }
   
   # ----- check matching columns in train and test -----
   
   if (!identical(sort(names(train)), sort(names(test)))) {
      warning("Train and test do not have identical column names.")
   }
   
   # ----- sample size summary -----
   
   n_train <- nrow(train)
   n_test  <- nrow(test)
   n_total <- n_train + n_test
   
   n_summary <- data.frame(
      split      = c("train", "test"),
      n          = c(n_train, n_test),
      proportion = round(c(n_train, n_test) / n_total, 3)
   )
   
   # ----- outcome distribution summary -----
   
   y_train <- train[[outcome_name]]
   y_test  <- test[[outcome_name]]
   
   counts_train <- table(y_train)
   counts_test  <- table(y_test)
   
   props_train <- prop.table(counts_train)
   props_test  <- prop.table(counts_test)
   
   outcome_summary <- list(
      train_counts = counts_train,
      test_counts  = counts_test,
      train_props  = round(props_train, 3),
      test_props   = round(props_test, 3)
   )
   
   # ----- print summary -----
   
   cat("Train/Test Split Summary\n")
   cat("------------------------\n")
   cat(sprintf("Train: %d rows (%.1f%%)\n", n_train, 100 * n_train / n_total))
   cat(sprintf("Test : %d rows (%.1f%%)\n\n", n_test, 100 * n_test / n_total))
   
   cat("Outcome distribution (proportions):\n")
   tab <- rbind(
      train = as.numeric(props_train),
      test  = as.numeric(props_test)
   )
   colnames(tab) <- levels(y_train)
   print(round(tab, 3))
   
   invisible(
      list(
         n_summary       = n_summary,
         outcome_summary = outcome_summary
      )
   )
}
