#' A/B Testing Data
#'
#' Results of an A/B test an email campaign for women's merchandise.
#'
#' @usage data(email.camp.w)
#' @format
#' A data frame with 10000 rows and 9 columns:
#' \describe{
#'   \item{\code{visit}}{1 if customer visited site within two weeks after 
#'         experiment, 0 otherwise}
#'   \item{\code{spend}}{Amount spent on site within two weeks after experiment}
#'   \item{\code{recency}}{Number of months since last }
#'   \item{\code{history}}{Amount spent on site in past year}
#'   \item{\code{womens}}{1 if customer purchased womens merchandise in the past
#'         year, 0 otherwise}
#'   \item{\code{zip}}{Factor variable indicating if customer lives in a Rural,
#'         Suburban, or Urban area}
#'   \item{\code{newbie}}{1 if customer is new to site in past year, 0 otherwise}
#'   \item{\code{channel}}{Factor variable indicating channel customer purchased
#'         from in past year, Phone, Web, or Multichannel (i.e., both)}
#' }
#' @source https://blog.minethatdata.com/2008/03/minethatdata-e-mail-analytics-and-data.html
"email.camp.w"
