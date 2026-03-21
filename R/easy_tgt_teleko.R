#' @title Targeting Evaluation for Teleko A/B Test Strategies
#'
#' @description
#' Evaluate alternative targeting strategies for the Teleko A/B test by comparing
#' expected renewal rates and customer lifetime value (CLV) under different
#' treatment assignment rules.
#'
#' The function supports:
#' \itemize{
#'   \item \code{"none"} = treat nobody
#'   \item \code{"all"} = treat everybody
#'   \item \code{"churn"} = target customers most likely to churn
#'   \item \code{"clv"} = target customers with highest predicted CLV
#'   \item \code{"renew_lift"} = target customers with highest predicted renewal lift
#'   \item \code{"clv_lift"} = target customers with highest predicted CLV lift
#'   \item \code{"combined"} = target customers based on both renewal lift and CLV lift
#' }
#'
#' For methods using ranges:
#' \itemize{
#'   \item \code{"churn"} and \code{"clv"} use 0.05 increments
#'   \item \code{"renew_lift"}, \code{"clv_lift"}, and \code{"combined"} use 500-rank increments
#' }
#'
#' Output is returned as a formatted \code{flextable}, with:
#' \itemize{
#'   \item highest renewal rate highlighted in yellow
#'   \item highest CLV highlighted in green
#' }
#'
#' @param combined A dataframe containing Teleko experimental data with required columns:
#' \code{treat}, \code{renew}, \code{clv}, \code{renew_out0}, \code{clv_out0},
#' \code{renew_rank}, and \code{clv_rank}.
#'
#' @param method Character string indicating targeting method.
#' One of:
#' \code{"none"}, \code{"all"}, \code{"churn"}, \code{"clv"},
#' \code{"renew_lift"}, \code{"clv_lift"}, or \code{"combined"}.
#'
#' @param cutoff1 Optional numeric cutoff.
#' Used for:
#' \itemize{
#'   \item methods \code{"churn"}, \code{"clv"}, \code{"renew_lift"}, \code{"clv_lift"}
#'   \item renew cutoff when \code{method = "combined"}
#' }
#'
#' Can be:
#' \itemize{
#'   \item a single value
#'   \item a two-value range
#' }
#'
#' @param cutoff2 Optional numeric cutoff used only when
#' \code{method = "combined"} for CLV rank cutoffs.
#'
#' @return A \code{flextable} summarizing:
#' \itemize{
#'   \item Mean Renew Rate
#'   \item Mean CLV Amount
#'   \item Proportion targeted
#' }
#'
#' @examples
#' easy_tgt_teleko(combined, method = "none")
#'
#' easy_tgt_teleko(combined, method = "all")
#'
#' easy_tgt_teleko(combined,
#'                 method = "churn",
#'                 cutoff1 = c(.30, .70))
#'
#' easy_tgt_teleko(combined,
#'                 method = "renew_lift",
#'                 cutoff1 = c(4000, 6000))
#'
#' easy_tgt_teleko(combined,
#'                 method = "combined",
#'                 cutoff1 = c(3000, 5000),
#'                 cutoff2 = c(3000, 4000))
#'
#' @importFrom dplyr %>% group_by summarise mutate filter ungroup bind_rows select
#' @importFrom flextable flextable add_header_lines bold align autofit bg
#' @importFrom scales percent dollar
#' @export
#' 
easy_tgt_teleko <- function(combined,
                            method,
                            cutoff1 = NULL,
                            cutoff2 = NULL) {
   
   library(dplyr)
   library(flextable)
   library(scales)
   
   sum.treat0 <- nrow(combined[combined$treat == 0, ])
   sum.treat1 <- nrow(combined[combined$treat == 1, ])
   
   # -----------------------------
   # Helper analysis function
   # -----------------------------
   
   run_analysis <- function(temp, label1 = NA, label2 = NA) {
      
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
            cutoff1 = label1,
            cutoff2 = label2,
            mean.renew = weighted.mean(mean.renew, n),
            mean.clv   = weighted.mean(mean.clv, n),
            proportion = last(prop)
         )
   }
   
   # -----------------------------
   # Titles
   # -----------------------------
   
   title <- switch(method,
                   "none"       = "Treat Nobody",
                   "all"        = "Treat Everybody",
                   "churn"      = "Targeting Options Based on Most Likely to Churn",
                   "clv"        = "Targeting Options Based on Highest Predicted CLV",
                   "renew_lift" = "Targeting Options Based on Highest Renew Lift",
                   "clv_lift"   = "Targeting Options Based on Highest CLV Lift",
                   "combined"   = "Targeting Options Based on Combined Renew and CLV Lift")
   
   # -----------------------------
   # NONE = treat nobody
   # -----------------------------
   
   if(method == "none") {
      
      out <- combined %>%
         filter(treat == 0) %>%
         summarise(
            `Mean Renew Rate` = percent(mean(renew), accuracy = 0.1),
            `Mean CLV Amount` = dollar(mean(clv), accuracy = 0.01),
            Proportion = "0%"
         )
      
      ft <- flextable(out)
      ft <- add_header_lines(ft, values = title)
      ft <- bold(ft, part = "header")
      ft <- align(ft, align = "center", part = "all")
      ft <- autofit(ft)
      
      return(ft)
   }
   
   # -----------------------------
   # ALL = treat everybody
   # -----------------------------
   
   if(method == "all") {
      
      out <- combined %>%
         filter(treat == 1) %>%
         summarise(
            `Mean Renew Rate` = percent(mean(renew), accuracy = 0.1),
            `Mean CLV Amount` = dollar(mean(clv), accuracy = 0.01),
            Proportion = "100%"
         )
      
      ft <- flextable(out)
      ft <- add_header_lines(ft, values = title)
      ft <- bold(ft, part = "header")
      ft <- align(ft, align = "center", part = "all")
      ft <- autofit(ft)
      
      return(ft)
   }
   
   # -----------------------------
   # Methods 1–4
   # -----------------------------
   
   if(method %in% c("churn", "clv", "renew_lift", "clv_lift")) {
      
      if(is.null(cutoff1)) stop("Please provide cutoff1.")
      
      if(length(cutoff1) == 1) {
         cut_seq <- cutoff1
      } else {
         if(method %in% c("churn", "clv")) {
            cut_seq <- seq(cutoff1[1], cutoff1[2], by = 0.05)
         }
         if(method %in% c("renew_lift", "clv_lift")) {
            cut_seq <- seq(cutoff1[1], cutoff1[2], by = 500)
         }
      }
      
      results <- lapply(cut_seq, function(cut) {
         
         temp <- combined
         
         if(method == "churn") {
            temp$target <- ifelse(temp$renew_out0 < quantile(temp$renew_out0, cut), 1, 0)
         }
         
         if(method == "clv") {
            temp$target <- ifelse(temp$clv_out0 > quantile(temp$clv_out0, 1 - cut), 1, 0)
         }
         
         if(method == "renew_lift") {
            temp$target <- ifelse(temp$renew_rank < cut, 1, 0)
         }
         
         if(method == "clv_lift") {
            temp$target <- ifelse(temp$clv_rank < cut, 1, 0)
         }
         
         run_analysis(temp, cut)
      })
      
      out <- bind_rows(results) %>%
         select(-cutoff2)
      
      if(method %in% c("churn", "clv")) {
         out$cutoff1 <- percent(out$cutoff1)
      }
      
      names(out) <- c("Top X", "Mean Renew Rate", "Mean CLV Amount", "Proportion")
      
      out <- out %>%
         mutate(
            `Mean Renew Rate` = percent(`Mean Renew Rate`, accuracy = 0.01),
            `Mean CLV Amount` = dollar(`Mean CLV Amount`, accuracy = 0.01),
            Proportion = percent(Proportion, accuracy = 0.01)
         )
      
      ft <- flextable(out)
      
      max_renew_row <- which.max(as.numeric(gsub("%", "", out$`Mean Renew Rate`)))
      ft <- bg(ft, i = max_renew_row, j = "Mean Renew Rate", bg = "#FFF2CC")
      
      max_clv_row <- which.max(as.numeric(gsub("[$,]", "", out$`Mean CLV Amount`)))
      ft <- bg(ft, i = max_clv_row, j = "Mean CLV Amount", bg = "#D9EAD3")
      
      ft <- add_header_lines(ft, values = title)
      ft <- bold(ft, part = "header")
      ft <- align(ft, align = "center", part = "all")
      ft <- autofit(ft)
      
      return(ft)
   }
   
   # -----------------------------
   # Combined method
   # -----------------------------
   
   if(method == "combined") {
      
      if(is.null(cutoff1) | is.null(cutoff2)) {
         stop("Please provide cutoff1 and cutoff2.")
      }
      
      renew_seq <- if(length(cutoff1) == 1) cutoff1 else seq(cutoff1[1], cutoff1[2], by = 500)
      clv_seq   <- if(length(cutoff2) == 1) cutoff2 else seq(cutoff2[1], cutoff2[2], by = 500)
      
      combos <- expand.grid(renew = renew_seq, clv = clv_seq)
      
      if(nrow(combos) > 50) {
         stop("Too many combinations (>50 rows). Reduce one or both cutoff ranges.")
      }
      
      results <- lapply(1:nrow(combos), function(i) {
         
         r_cut <- combos$renew[i]
         c_cut <- combos$clv[i]
         
         temp <- combined
         temp$target <- ifelse(temp$renew_rank < r_cut & temp$clv_rank < c_cut, 1, 0)
         
         run_analysis(temp, r_cut, c_cut)
      })
      
      out <- bind_rows(results)
      
      names(out) <- c("Top X Renew", "Top X CLV", "Mean Renew Rate", "Mean CLV Amount", "Proportion")
      
      out <- out %>%
         mutate(
            `Mean Renew Rate` = percent(`Mean Renew Rate`, accuracy = 0.1),
            `Mean CLV Amount` = dollar(`Mean CLV Amount`, accuracy = 0.01),
            Proportion = percent(Proportion, accuracy = 0.1)
         )
      
      ft <- flextable(out)
      
      max_renew_row <- which.max(as.numeric(gsub("%", "", out$`Mean Renew Rate`)))
      ft <- bg(ft, i = max_renew_row, j = "Mean Renew Rate", bg = "#FFF2CC")
      
      max_clv_row <- which.max(as.numeric(gsub("[$,]", "", out$`Mean CLV Amount`)))
      ft <- bg(ft, i = max_clv_row, j = "Mean CLV Amount", bg = "#D9EAD3")
      
      ft <- add_header_lines(ft, values = title)
      ft <- bold(ft, part = "header")
      ft <- align(ft, align = "center", part = "all")
      ft <- autofit(ft)
      
      return(ft)
   }
   
   stop("Invalid method.")
}
