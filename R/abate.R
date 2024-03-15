#' @title Average Treatment Effect for A/B Testing
#' @description This function uses linear regression to calculate the average
#'      treatment effects both without controls and with controls. The function
#'      returns a \code{flextable} object.
#' @details
#' REQUIRED PACKAGES:
#' \itemize{
#'   \item \code{dplyr}
#'   \item \code{gtsummary}
#'   \item \code{flextable}
#' }
#' @param model An existing linear regression (\code{lm}) object containing all
#'     control variables and the treatment variable.
#' @param treatment The treatment variable name Must be in quotations.
#' @return A \code{flextable} object
#' @examples
#' abate(visit, "promotion")

abate <- function (model, treatment) {
   if (model$call[1] !="lm()") {
      error <- "Model type must be linear regression model (lm)."
      stop(error)
   }
   
   require(gtsummary)
   require(flextable)
   require(dplyr)
   
   outcome <- all.vars(model$call$formula)[1]
   data <- data.frame(model$model)
   
   mf2 <- model$call$formula
   mf1 <- paste0(outcome,"~",treatment)
   
   model1 <- lm(mf1, data=data)
   
   nocontrol <- tbl_regression(model1,
                               intercept=TRUE,
                               pvalue_fun = function(x) style_pvalue(x, digits=3),
                               estimate_fun = function(x) style_number(x, digits=3),
                               conf.int=FALSE) %>% 
      add_glance_table(include=c(p.value, r.squared),
                       label=list(p.value~"p-value",
                                  r.squared="R\u00B2"))
   control <- tbl_regression(model,
                             intercept=TRUE,
                             pvalue_fun = function(x) style_pvalue(x, digits=3),
                             estimate_fun = function(x) style_number(x, digits=3),
                             conf.int=FALSE) %>%
      add_glance_table(include=c(p.value, r.squared),
                       label=list(p.value~"p-value",
                                  r.squared="R\u00B2"))
   
   comp <- tbl_merge(list(nocontrol, control),
                     tab_spanner=c("Without\nControls","With\nControls")) %>%
      modify_table_body(~.x %>% arrange(row_type == "glance_statistic")) %>%
      as_flex_table() %>%
      bold(bold=TRUE, part="header") %>%
      fontsize(i=1, size=14, part="header") %>%
      fontsize(size=12, part="body") %>%
      height(part="body", height=4, unit="mm") %>%
      hrule(part="body", rule="exact") %>%
      padding(padding.top=1, padding.bottom = 1, part="body") 
   
   return(comp)
   
}