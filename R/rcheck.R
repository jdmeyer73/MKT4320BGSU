#' @title Randomization check for A/B Testing
#' @description This function checks if the characteristics/covariates used for
#'     uplift modeling from an A/B Test were randomly assigned to the test and
#'     control groups.
#' @details
#' REQUIRED PACKAGES:
#' \itemize{
#'   \item \code{fastDummies}
#'   \item \code{htmlTable} (if \code{nice="ht"} option used)
#'   \item \code{flextable} (if \code{nice="ft"} option used)
#' }
#' @param data The name of the data frame containing the treatment variable and
#'     the covariates.
#' @param treatment The variable name identifying the treatment variable. Must 
#'     be in quotations.
#' @param outcome The name or names of the variables that identifies the 
#'     outcome variables. Default is \code{NULL}. Must be in quotations.
#' @param nice Format for the output
#' \itemize{
#' \item \code{"no"} for standard output
#' \item \code{"ft"} for output using package \code{flextable}
#' \item \code{"ht"} for output using package \code{htmlTable}
#' }
#' @return A table containing the results of the randomization ehck
#' @examples
#' rcheck(email.camp.w, "promotion", c("visit", "spend"), nice="ft")


rcheck <- function(data, treatment, outcome=NULL, nice=c("no","ft", "ht")) {
   require(fastDummies)
   if (nice=="ht") {
      require(htmlTable)
   }
   else if (nice=="ft") {
      require(flextable)
   }
   
   # Function for column standard deviations
   colSdColMeans <- function(x, na.rm=TRUE) {
      if (na.rm) {
         n <- colSums(!is.na(x)) # thanks @flodel
      } else {
         n <- nrow(x)
      }
      colVar <- colMeans(x*x, na.rm=na.rm) - (colMeans(x, na.rm=na.rm))^2
      return(sqrt(colVar * n/(n-1)))
   }
   
   # Remove outcome variables if present
   
   if (length(outcome)>0) {
      data <- data[,!(colnames(data) %in% outcome)]
   }
   
   # Check treatment variable and recode to 0,1
   if (length(table(data[[treatment]]))!=2) {
      error <- "The treatment variable must have two levels only."
      stop(error)
   }
   else {
      if (class(data[[treatment]]) %in% c("factor", "character")) {
         if (apply(data,2,function(x) {all(x %in% c("Yes", "No"))})[[treatment]]) {
            data[[treatment]] <- ifelse(data[[treatment]]=="Yes",1,0)
         }
         else {
            error <- 'The treatment variable must be either:\n  (1) numeric variable coded as (0,1);\n  (2) factor/character variable coded as ("Yes","No"); or\n  (3) logical variable.'
            stop(error)
         }
      }
      else if (class(data[[treatment]])=="logical") {
         data[[treatment]] <- ifelse(data[[treatment]]==TRUE,1,0)
      }
      else if (class(data[[treatment]]) %in% c("integer", "numeric")) {
         if (apply(data,2,function(x) {all(x %in% 0:1)})[[treatment]]) {
            data[[treatment]] <- data[[treatment]]
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
   
   # Get character columns
   char_cols <- sapply(data, class)
   char_cols <- char_cols[char_cols %in% c("factor", "character")]
   char_cols <- names(char_cols)
   
   # Get number of factor/character levels
   data.colnames <- names(data)
   fact.rows <- 0
   for (i in 1:length(data.colnames)) {
      if (class(data[[i]])=="factor") {
         add <- length(levels(data[[i]]))
         fact.rows <- fact.rows + add
      }
      else if (class(data[[i]])=="character") {
         add <- length(unique(data[[i]]))
         fact.rows <- fact.rows + add
      }
   }
   
   # Create Dummies using fastDummies
   data <- data.frame(dummy_cols(data))
   
   # Remove character/factor columns
   data <- data[,!(colnames(data) %in% char_cols)]
   
   # Extract treatment variable and remove treatment from data
   data.Treat <- data[[treatment]]
   data <- data[,!(colnames(data) == treatment)]
   # Matrix of covariates
   data.X <- model.matrix(~ -1 + ., data)
   all.rows <- length(colnames(data))
   num.rows <- all.rows - fact.rows
   fact.rows <- num.rows + 1
   rand.check <- data.frame(
      variable = colnames(data.X),
      treatment_mean = round(colMeans(data.X[data.Treat==1,]),3),
      control_mean = round(colMeans(data.X[data.Treat==0,]),3),
      sd = round(colSdColMeans(data.X),3),
      scale_mean_diff = round((colMeans(data.X[data.Treat==1,]) - colMeans(data.X[data.Treat==0,]))/colSdColMeans(data.X),3),
      p_val=0
   )
   
   for(i in 1:num.rows) {
      rand.check[i,6] <- round(t.test(data.X[,i] ~ data.Treat)$p.value,3)
   }
   for(i in fact.rows:all.rows) {
      rand.check[i,6] <- round(prop.test(c(sum(data.X[data.Treat==1,i]),
                                           sum(data.X[data.Treat==0,i])),
                                         c(sum(data.Treat==1),
                                           sum(data.Treat==0)))$p.value,3)
   }
   
   if (nice=="ht") {
      header <- c("Variable", "Mean<br>(Treatment)", "Mean<br>(Control)",
                  "SD", "Scaled Mean<br>Difference", "p-value")
      rand.check <- addHtmlTableStyle(rand.check,
                                      css.cell="padding-left: 1em; 
                                    padding-right: 1em;")
      rand.check <- htmlTable(rand.check,
                              rnames=FALSE,
                              header=header)
   }
   else if (nice=="ft") {
      rand.check <- flextable(rand.check) %>%
         set_header_labels(variable="Variable",
                           treatment_mean="Treatment",
                           control_mean="Control",
                           sd="SD",
                           scale_mean_diff="Scaled Mean Difference",
                           p_val="p-value") %>%
         add_header_row(values=c("Variable","Mean", "Mean", "SD",
                                 "Scaled Mean Difference", "p-value")) %>%
         merge_h(part="header") %>%
         merge_v(part="header") %>%
         flextable::align(align="center", part="header") %>%
         flextable::valign(valign="bottom", part="header") %>%
         bold(bold=TRUE, part="header") %>%
         padding(padding.top=1, padding.bottom = 1, part="body")
   }
   
   return(rand.check)
   
}