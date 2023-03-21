#' @title Traditional Conjoint Results
#' @description This function produces overall part-worth plots for each 
#'     attribute in a traditional conjoint analysis, along with an attribute
#'     importance plot and a table with case-level part worths and importances.
#' @details
#' For best results, the results of the function should be saved to an object. \cr
#' REQUIRED PACKAGES:
#' \itemize{
#'   \item broom
#'   \item dplyr
#'   \item stringr
#'   \item ggplot2
#' }
#' @param formula An object with a saved formula with a DV on the left side
#'     and IVs on the right side separated by a tilde (e.g., \code{myform = 
#'         value ~ attr1 + attr2 + attr3})
#' @param data The name of the data frame with the value ratings for each
#'     profile
#' @param idvar The variable that identifies the case (in quotations)
#' @return A list containing the following objects:
#' \itemize{
#'   \item \code{$pwplot} contains a plot with average part-worth values for
#'       each attribute
#'   \item \code{$impplot} contains a plot with the average importances
#'   \item \code{$casetable} contains a data frame with the part-worths and
#'       importances for each case
#' }
#' @examples
#' caform <- value ~ airline + connect + price
#' results <- tradca(formula = caform, data = airlineca, idvar="caseid")
#'
#' results$pwplot
#' results$impplot
#' results$casetable


tradca <- function(formula, data, idvar) {
   require(broom)
   require(dplyr)
   require(stringr)
   require(ggplot2)
   minid <- with(data, min(idvar))
   temp <- data %>% filter(idvar==minid) %>% lm(formula, data=.)
   len <- length(formula)
   idvarnum <- grep(idvar, colnames(data))
   butes <- attr(terms(formula), "term.labels")
   
   caseout <- data %>% 
      group_by(data[idvarnum]) %>%
      do(suppressWarnings(tidy(lm(formula=formula, data=.)))) %>%
      select(1:3) %>%
      filter(term!="(Intercept)") %>% 
      mutate_at(vars(starts_with("estimate")), funs(round(.,2))) %>%
      as.data.frame() 
   
   coutlong <- caseout   
   
   for (i in 1:len) {
      coutlong <- coutlong %>% 
         group_by_at(1) %>%
         summarise(term=paste0(butes[i],(temp$xlevels[[i]])[[1]])) %>%
         mutate(estimate=0) %>%
         bind_rows(coutlong, .) %>%
         arrange(idvar) %>%
         mutate(term=gsub(butes[i],paste0(butes[i],"_"),term)) %>%
         arrange(idvar,term)
   }
  clongtemp <- coutlong
   caseout <- reshape(data=coutlong, idvar=idvar, v.names="estimate", timevar="term", direction="wide") %>%
      rename_at(vars(starts_with("estimate")), funs(str_replace(.,"estimate.", "")))
   
   coutlong[,4:5] <- str_split_fixed(coutlong$term,"_", 2)
   coutlong <- coutlong %>%
      rename(attribute=4,
             level=5)
   cout <- caseout
   varlist <- NULL
   for (i in 1:len) {
      minc <- min(grep(butes[i], colnames(caseout)))
      maxc <- max(grep(butes[i], colnames(caseout)))
      varlist[i] <- paste0("range",i)
      for (j in 1:nrow(caseout)) {
         maxatt.row <- max(caseout[j,minc:maxc])
         minatt.row <- min(caseout[j,minc:maxc])
         range <- maxatt.row-minatt.row
         caseout[j,paste0("range",i)] <- range
         for(k in minc:maxc) {
            if (minatt.row != 0) {
               caseout[j,k] <- caseout[j,k] + range
            }
         }
      }
   }
   
   caseout$totrange <- rowSums(caseout[(ncol(caseout)-(len-1)):ncol(caseout)])
   for (i in 1:len) {
      for (j in 1:nrow(caseout)) {
         imp <- round(100*caseout[j,varlist[i]]/caseout[j,"totrange"],2)
         caseout[j,paste0("Imp_",butes[i])] <- imp
      }
   }
   caseout <- caseout[,-((ncol(caseout)-len*2):(ncol(caseout)-len))]
   
   impdata <- caseout %>%
      select(starts_with("Imp_")) %>%
      summarise_all(~round(mean(.),2))
   colnames(impdata) <- gsub("Imp_","",colnames(impdata))
   impdata <- data.frame(t(impdata))
   names(impdata)[1] <- "importance"
   impdata$attribute <- rownames(impdata)
   impplot <- impdata %>% ggplot(aes(x=attribute, y=importance, fill=attribute)) +
      geom_col(show.legend=FALSE) + 
      labs(x="Attribute", y="Mean Importance") +
      geom_text(aes(label=importance), vjust=.95, fontface="bold", color="white")
   
   pwplot <- coutlong %>% 
      group_by(attribute,level) %>% 
      summarise(pw=mean(estimate), .groups="drop") %>% 
      ggplot(aes(x=level, y=pw, group=1)) + 
         geom_line(aes(color=attribute), show.legend=FALSE, size=2) + 
         geom_point(size=2) +
         facet_wrap(~attribute, scales="free_x") + 
         labs(x="Attribute Level", y="Mean Part-Worth")
   
   return(list("casetable" = caseout, "impplot"=impplot, "pwplot"=pwplot, "coutlong"=coutlong))
}