#' @title Easy ACF and PACF Plots
#' @description This function creates ACF and PACF plots needed for ARIMA 
#'     forecasting.
#' @details
#' For best results, the results of the function should be saved to an object. \cr
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
#' @param lags An integer indicating how many lags should be shown on the plot 
#'     (Default: \code{lags=25}).
#' @param d An integer indicating the order of non-seasonal differencing  
#'     (Default: \code{d=0}).
#' @param D An integer indicating the order of seasonal differencing  
#'     (Default: \code{D=0}).
#' @return A list containing the following objects:
#' \itemize{
#'   \item \code{$acf} contains the ACF plot
#'   \item \code{$pacf} contains the PACF plot
#' }
#' @examples
#' out <- acplots(msales, "t", "sales", "ym", 12, lags=25, d=0, D=1)
#' out$acf
#' out$pacf

acplots <- function(data, tvar, obs, datetype=c("ym", "yq", "yw"), h, lags=25, d=0, D=c(0,1)) {
   require(fpp3)
   if (datetype=="ym") {
      slag <- 12
      tsdata <- data %>% 
         mutate(Date=yearmonth(ym(.data[[tvar]])),
                Measure=.data[[obs]]) %>% 
         as_tsibble(index=Date)
   } else if (datetype=="yq") {
      slag <- 4
      tsdata <- data %>%
         mutate(Date=yearquarter(.data[[tvar]]),
                Measure=.data[[obs]]) %>%
         as_tsibble(index=Date)
   } else if (datetype=="yw") {
      slag <- 52
      tsdata <- data %>%
         mutate(Date=yearweek(.data[[tvar]]),
                Measure=.data[[obs]]) %>%
         as_tsibble(index=Date)
   } else {
      stop("Time variable not in correct format")
   }
   pdata <- tsdata[1:(nrow(tsdata)-h), ]
   ci <- 1.96/sqrt(nrow(pdata))
   if (D==0) {
      if (d==0) {
         dlabel <- paste0("")
         acfdata <- pdata %>%
            ACF(Measure, lag_max=lags)
         pacfdata <- pdata %>%
            PACF(Measure, lag_max=lags)
      } else {
         dlabel <- paste0(" (d=",d,")")
         acfdata <- pdata %>%
            ACF(difference(Measure, differences=d), lag_max=lags)
         pacfdata <- pdata %>%
            PACF(difference(Measure, differences=d), lag_max=lags)
      }
   } else if (D==1) {
      if (d==0) {
         dlabel <- paste0(" (D=1)")
         acfdata <- pdata %>%
            ACF(difference(Measure, lag=slag), lag_max=lags)
         pacfdata <- pdata %>%
            PACF(difference(Measure, lag=slag), lag_max=lags)
      } else {
         dlabel <- paste0(" (d=",d,", D=1)")
         acfdata <- pdata %>%
            ACF(difference(difference(Measure, lag=slag), differences=d), 
                lag_max=lags)
         pacfdata <- pdata %>%
            PACF(difference(difference(Measure, lag=slag), differences=d), 
                lag_max=lags)
      }
   }
   aylabel <- paste0("Autocorrelations of ", obs, dlabel)
   pylabel <- paste0("Partial Autocorrelations of ", obs, dlabel)
   acf <- acfdata %>%
      ggplot(aes(x=lag, y=acf)) +
      geom_segment(aes(xend=lag, yend=0), size=1, color="blue") +
      geom_ribbon(aes(ymin=-ci, ymax=ci), alpha=.05, fill="red") +
      geom_point(aes(x=lag, y=acf), color="blue", size=2) +
      theme_bw() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()) +
      labs(x="Lag", y=aylabel)
   pacf <- pacfdata %>%
      ggplot(aes(x=lag, y=pacf)) +
      geom_segment(aes(xend=lag, yend=0), size=1, color="blue") +
      geom_ribbon(aes(ymin=-ci, ymax=ci), alpha=.05, fill="red") +
      geom_point(aes(x=lag, y=pacf), color="blue", size=2) +
      theme_bw() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()) +
      labs(x="Lag", y=pylabel)
   results <- list("acf"=acf, "pacf"=pacf)
   return(results)
}