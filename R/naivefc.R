#' @title Easy Naive Forecasting
#' @description This function analyzed time series data using naive 
#'     forecasting methods.
#' @details
#' For best results, the results of the function should be saved to an object. \cr
#' REQUIRED PACKAGES:
#' \itemize{
#'   \item \code{fpp3} (NOTE: This package requires several other packages)
#' }
#' NOTE:  The model names are:
#' \itemize{
#'   \item \strong{Naive} for the naive model
#'   \item \strong{Naive.Drift} for the naive model with drift
#'   \item \strong{Seas.Naive} for the seasonal naive model
#'   \item \strong{Seas.Naive.Drift} for the seasonal naive model with drift
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
#' @return A list containing the following objects:
#' \itemize{
#'   \item \code{$plot} contains the naive methods plot
#'   \item \code{$acc} contains the accuracy measures
#'   \item \code{$fcresplot} contains a plot of the holdout period residuals
#'   \item \code{$fcresid} contains a dataframe of the holdout period residuals 
#'       (seldom used separately)
#' }
#' @examples
#' naive <- naivefc(msales, "t", "sales", "ym", 12)
#' naive$plot
#' naive$acc
#' naive$fcresplot

naivefc <- function(data, tvar, obs, datetype=c("ym", "yq", "yw"), h) {
   require(fpp3)
   fc <- h-1
   if (datetype=="ym") {
      tsdata <- data %>% 
         mutate(Date=yearmonth(ym(.data[[tvar]])),
                Measure=.data[[obs]]) %>% 
         as_tsibble(index=Date)
   } else if (datetype=="yq") {
      tsdata <- data %>%
         mutate(Date=yearquarter(.data[[tvar]]),
                Measure=.data[[obs]]) %>%
         as_tsibble(index=Date)
   } else if (datetype=="yw") {
      tsdata <- data %>%
         mutate(Date=yearweek(.data[[tvar]]),
                Measure=.data[[obs]]) %>%
         as_tsibble(index=Date)
   } else {
      stop("Time variable not in correct format")
   }
   train <- tsdata[1:(nrow(tsdata)-h), ]
   forecast <- tsdata[(nrow(tsdata)-fc):nrow(tsdata), ]
   train_fit <- train %>% 
      fabletools::model('Naive'=NAIVE(Measure),
            'Seas.Naive'=SNAIVE(Measure),
            'Naive.Drift'=NAIVE(Measure~drift()),
            'Seas.Naive.Drift'=SNAIVE(Measure~drift()))
   train_fc <- train_fit %>% forecast(h=h)
   plot <- train_fc %>% autoplot(train, level=NULL, size=1) + 
      autolayer(forecast, .data[[obs]]) +
      labs(y=obs) +
      guides(color=guide_legend(title="Model")) +
      theme(legend.position = "bottom")
   acc <- accuracy(train_fc, forecast) %>% 
      select(.model, RMSE, MAE, MAPE) %>%
      rename(Model=1) %>% 
      mutate_at(2:4, round, 3) %>% 
      data.frame()
   fcresid <- train_fc %>% 
      mutate(actual=rep(forecast$Measure,4),
             resid=actual-.mean)
   fcresplot <- fcresid %>% ggplot(aes(x=Date, y=resid, color=.model)) +
      geom_line(size=1) +
      geom_hline(yintercept = 0, color="black", linetype="dashed", size=2) +
      guides(color=guide_legend(title="Model")) +
      theme(legend.position = "bottom")
   
   results <- list("plot"=plot, "acc"=acc, "fcresid"=fcresid, "fcresplot"=fcresplot)
   return(results)
}
