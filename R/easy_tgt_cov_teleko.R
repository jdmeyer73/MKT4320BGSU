#' @title Targeting Evaluation Using Covariate Rules for Teleko A/B Test
#'
#' @description
#' Evaluate one or more simple covariate-based targeting rules for the Teleko A/B test.
#'
#' The function applies user-defined logical rules to create treatment targets and
#' calculates weighted expected outcomes under selective treatment assignment.
#'
#' This allows comparison of managerial targeting heuristics such as:
#' \itemize{
#'   \item tenure thresholds
#'   \item recent usage thresholds
#'   \item factor-based targeting rules
#'   \item combinations of multiple customer characteristics
#' }
#'
#' Rules must be supplied as character strings.
#'
#' Numeric variables may be written directly:
#' \code{"tenure < 11 & past30 > 5"}
#'
#' Factor variables must use quotation marks around factor levels:
#' \code{"contract == 'Month-to-month'"}
#'
#' Multiple factor levels may be specified using:
#' \code{"region \%in\% c('North','South')"}
#'
#' Output is returned as a formatted \code{flextable}, with:
#' \itemize{
#'   \item highest renewal rate highlighted in yellow
#'   \item highest CLV highlighted in green
#' }
#'
#' @param combined A dataframe containing Teleko experimental data with required columns:
#' \code{treat}, \code{renew}, \code{clv}, and any covariates referenced in \code{rules}.
#'
#' @param rules Character string or character vector containing one or more logical rules.
#'
#' Single rule example:
#' \code{"tenure < 11 & past30 > 5"}
#'
#' Multiple rule example:
#' \code{c("tenure < 11 & past30 > 5", "tenure < 9 & past30 > 7")}
#'
#' @param title Optional table title.
#' Default is \code{"Targeting Based on Covariate Rules"}.
#'
#' @return A \code{flextable} summarizing:
#' \itemize{
#'   \item rule tested
#'   \item Mean Renew Rate
#'   \item Mean CLV Amount
#'   \item Proportion targeted
#' }
#'
#' @examples
#' # Numeric rule
#' easy_tgt_cov_teleko(
#'   combined,
#'   rules = "tenure < 11 & past30 > 5"
#' )
#'
#' # Multiple numeric rules
#' easy_tgt_cov_teleko(
#'   combined,
#'   rules = c(
#'     "tenure < 11 & past30 > 5",
#'     "tenure < 9 & past30 > 7",
#'     "tenure < 6"
#'   )
#' )
#'
#' # Factor rule
#' easy_tgt_cov_teleko(
#'   combined,
#'   rules = "contract == 'Month-to-month'"
#' )
#'
#' # Mixed numeric + factor rule
#' easy_tgt_cov_teleko(
#'   combined,
#'   rules = "contract == 'Month-to-month' & tenure < 12"
#' )
#'
#' # Multiple factor levels
#' easy_tgt_cov_teleko(
#'   combined,
#'   rules = "region %in% c('North','South')"
#' )
#'
#' @importFrom dplyr %>% group_by summarise mutate filter ungroup bind_rows
#' @importFrom flextable flextable add_header_lines bold align autofit bg
#' @importFrom scales percent dollar
#' @export
easy_tgt_cov_teleko <- function(combined,
                                rules,
                                title = "Targeting Based on Covariate Rules") {
   
   library(dplyr)
   library(flextable)
   library(scales)
   
   sum.treat0 <- nrow(combined[combined$treat == 0, ])
   sum.treat1 <- nrow(combined[combined$treat == 1, ])
   
   # Force single rule into vector automatically
   rules <- as.character(rules)
   
   results <- lapply(rules, function(rule) {
      
      temp <- combined %>%
         mutate(target = ifelse(eval(parse(text = rule)), 1, 0))
      
      temp %>%
         group_by(target, treat) %>%
         summarise(
            mean.renew = mean(renew),
            mean.clv   = mean(clv),
            n          = n(),
            .groups = "drop_last"
         ) %>%
         mutate(
            N    = ifelse(treat == 0, sum.treat0, sum.treat1),
            prop = n / N
         ) %>%
         filter(target == 1 & treat == 1 | target == 0 & treat == 0) %>%
         ungroup() %>%
         summarise(
            Rule = rule,
            `Mean Renew Rate` = percent(weighted.mean(mean.renew, n), accuracy = 0.01),
            `Mean CLV Amount` = dollar(weighted.mean(mean.clv, n), accuracy = 0.01),
            Proportion = percent(last(prop), accuracy = 0.01)
         )
   })
   
   out <- bind_rows(results)
   
   ft <- flextable(out)
   
   # Highlight best renew
   max_renew_row <- which.max(as.numeric(gsub("%", "", out$`Mean Renew Rate`)))
   ft <- bg(ft, i = max_renew_row, j = "Mean Renew Rate", bg = "#FFF2CC")
   
   # Highlight best CLV
   max_clv_row <- which.max(as.numeric(gsub("[$,]", "", out$`Mean CLV Amount`)))
   ft <- bg(ft, i = max_clv_row, j = "Mean CLV Amount", bg = "#D9EAD3")
   
   ft <- add_header_lines(ft, values = title)
   ft <- bold(ft, part = "header")
   ft <- align(ft, align = "center", part = "all")
   ft <- autofit(ft)
   
   return(ft)
}
