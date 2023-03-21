#' @title Easy White Noise Plot and Test
#' @description This function creates a cumulative periodogram and white 
#'     noise test needed for ARIMA forecasting.
#' @details
#' REQUIRED PACKAGES:
#' \itemize{
#'   \item \code{fpp3} (NOTE: This package requires several other packages)
#'   \item \code{ggfortify}
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
#' @param arima Provides the non-seasonal autoregressive (\emph{p}), 
#'     differencing (\emph{d}), and moving average (\emph{q}) parameters, 
#'     in the form of \code{c(p,d,q)}, where the parameters are integers.
#' @param sarima Provides the seasonal autoregressive (\emph{P}), 
#'     differencing (\emph{D}), and moving average (\emph{Q}) parameters, 
#'     in the form of \code{c(P,D,Q)}, where the parameters are integers.
#' @return A cumulative periodogram and results of the Ljung-Box White Noise 
#'     test.
#' @examples
#' tswh.noise(msales, "t", "sales", "ym", 12, arima=c(0,0,0), sarima=c(0,0,0))
#' tswh.noise(msales, "t", "sales", "ym", 12, c(2,1,0), c(2,1,0))

tswh.noise <- function(data, tvar, obs, datetype=c("ym", "yq", "yw"), h=0, arima, sarima) {
   require(fpp3)
   require(ggfortify)
   p <- arima[1]
   d <- arima[2]
   q <- arima[3]
   P <- sarima[1]
   D <- sarima[2]
   Q <- sarima[3]
   
   if (datetype=="ym") {
      slag <- 24
      tsdata <- data %>% 
         mutate(Date=yearmonth(ym(.data[[tvar]])),
                Measure=.data[[obs]]) %>% 
         as_tsibble(index=Date)
   } else if (datetype=="yq") {
      slag <- 12
      tsdata <- data %>%
         mutate(Date=yearquarter(.data[[tvar]]),
                Measure=.data[[obs]]) %>%
         as_tsibble(index=Date)
   } else if (datetype=="yw") {
      slag <- 26
      tsdata <- data %>%
         mutate(Date=yearweek(.data[[tvar]]),
                Measure=.data[[obs]]) %>%
         as_tsibble(index=Date)
   } else {
      stop("Time variable not in correct format")
   }
   tsdata <- tsdata[1:(nrow(tsdata)-h), ]
   ts <- tsdata %>%
      model(model=ARIMA(Measure~pdq(p,d,q)+PDQ(P,D,Q))) %>% residuals()
   bt <- Box.test(ts$.resid, lag=slag, type="Ljung-Box", fitdf=(p+q+P+Q))
   ggcpgram(ts$.resid)+
      theme_bw() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            plot.caption=element_text(hjust=0, size=14)) +
      labs(caption=paste0("Ljung-Box test: p=",round(bt$p.value,4)))
   
}