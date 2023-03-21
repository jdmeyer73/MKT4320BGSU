#' @title Easy Time Series Plot
#' @description This function creates a time series plot of the data
#' @details
#' REQUIRED PACKAGES:
#' \itemize{
#'   \item \code{fpp3} (NOTE: This package requires several other packages)
#' }
#' @param data The name of the data frame with the time variable and 
#'     measure variable
#' @param tvar The variable that identifies the time period. Must be in 
#'     quotations.
#' @param obs The variable that identifies the measure. Must be in 
#'     quotations.
#' @param datetype Indicates the time period type. Takes one of the following 
#'     values:
#' \itemize{
#' \item \code{"ym"} if the time period is year-month
#' \item \code{"yq"} if the time period is year-quarter
#' \item \code{"yw"} if the time period is year-week
#' }
#' @param h An integer indicating the number of holdout/forecast periods.
#' @return A time series plot created in \code{ggplot}
#' @examples
#' tsplot(qsales, "t", "sales", "yq", 8)

tsplot <- function(data, tvar, obs, datetype=c("ym", "yq", "yw"), h) {
   require(fpp3)
   fc <- h-1
   if (datetype=="ym") {
      tsdata <- data %>% 
         mutate(Month=yearmonth(ym(.data[[tvar]])),
                Measure=.data[[obs]]) %>% 
         as_tsibble(index=Month)
   } else if (datetype=="yq") {
      tsdata <- data %>%
         mutate(Quarter=yearquarter(.data[[tvar]])) %>%
         as_tsibble(index=Quarter)
   } else if (datetype=="yw") {
      tsdata <- data %>%
         mutate(Week=yearweek(.data[[tvar]])) %>%
         as_tsibble(index=Week)
   } else {
      stop("Time variable not in correct format")
   }
   if (h>0) {
      train <- tsdata[1:(nrow(tsdata)-h), ]
      forecast <- tsdata[(nrow(tsdata)-fc):nrow(tsdata), ]
      autoplot(train, .data[[obs]], size=1) +
         autolayer(forecast, .data[[obs]], color="red", size=1) +
         labs(y=obs) +
         theme_bw() +
         theme(panel.grid.major.x = element_blank(),
               panel.grid.minor.x = element_blank())
   } else {
      autoplot(tsdata, .data[[obs]], size=1) +
         labs(y=obs) +
         theme_bw() +
         theme(panel.grid.major.x = element_blank(),
               panel.grid.minor.x = element_blank())
   }
}