#' @title Easy Automatic ARIMA Modeling
#' @description This function runs an ARIMA model as specified by the user and 
#'     an automatic search for the “best” model if requested.
#' @details
#' For best results, the results of the function should be saved to an object. \cr
#' REQUIRED PACKAGES:
#' \itemize{
#'   \item \code{fpp3} (NOTE: This package requires several other packages)
#'   \item \code{ggfortify}
#'   \item \code{stringr}
#'   \item \code{cowplot}
#' }
#' NOTE 1:  The model names are:
#' \itemize{
#'   \item \strong{Self} for the model specified by the user
#'   \item \strong{Auto} for the "best" model if requested by the user
#' }
#' NOTE 2:  If \code{auto="Y"} is specified, this function can take several 
#'     minutes to run.
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
#' @param arima Provides the non-seasonal autoregressive (\emph{p}), 
#'     differencing (\emph{d}), and moving average (\emph{q}) parameters, 
#'     in the form of \code{c(p,d,q)}, where the parameters are integers.
#' @param sarima Provides the seasonal autoregressive (\emph{P}), 
#'     differencing (\emph{D}), and moving average (\emph{Q}) parameters, 
#'     in the form of \code{c(P,D,Q)}, where the parameters are integers.
#' @param auto Instructs the function to do automatic searhing or not. Can be
#'     one of two options.
#' \itemize{
#' \item \code{"Y"} An automatic search is performed to find the “best” model
#' \item \code{"N"} An automatic search is not performed
#' }
#' @return A list containing the following objects:
#' \itemize{
#'   \item \code{$plot} contains the ARIMA methods plot
#'   \item \code{$acc} contains the accuracy measures
#'   \item \code{$fcresplot} contains a plot of the holdout period residuals
#'   \item \code{$fcresid} contains a dataframe of the holdout period residuals 
#'       (seldom used separately)
#'   \item \code{$acresid} contains a plot of the ACF/PACF residuals for the 
#'       model(s)
#'   \item \code{$wn} contains cumulative periodogram(s) and results of the 
#'       Ljung-Box White Noise test(s)
#' }
#' @examples
#' # Running only the model specified by the user
#' selfonly <- autoarima(msales, "t", "sales", "ym", 12, 
#'                       arima=c(2,1,0), sarima=c(2,1,0), auto="N")
#' selfonly$plot
#' selfonly$acc
#' selfonly$fcresplot
#' selfonly$ascresid
#' selfonly$wn
#' 
#' # Running the automatic search
#' arima <- autoarima(msales, "t", "sales", "ym", 12, 
#'                    arima=c(2,1,0), sarima=c(2,1,0), auto="Y")
#' arima$plot
#' arima$acc
#' arima$fcresplot
#' arima$ascresid
#' arima$wn

autoarima <- function(data, tvar, obs, datetype=c("ym", "yq", "yw"), h, arima, sarima, auto=c("Y","N")) {
   require(fpp3)
   require(ggfortify)
   require(stringr)
   require(cowplot)
   p <- arima[1]
   d <- arima[2]
   q <- arima[3]
   P <- sarima[1]
   D <- sarima[2]
   Q <- sarima[3]
   fc <- h-1
   SelfDF <- p+q+P+Q
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
   train <- tsdata[1:(nrow(tsdata)-h), ]
   forecast <- tsdata[(nrow(tsdata)-fc):nrow(tsdata), ]
   if (P+D+Q==0) {
      pm2 <- ifelse(p-2<0,0,p-2)
      qm2 <- ifelse(q-2<0,0,q-2)
      pp2 <- p+2
      qp2 <- q+2
      if (auto=="Y") {
      train_fit <- train %>% 
         fabletools::model(Self=ARIMA(Measure~pdq(p,d,q)+PDQ(0,0,0)),
                           Auto=ARIMA(Measure~pdq(p=c(pm2:pp2), 
                                                  q=c(qm2:qp2))+PDQ(0,0,0), 
                                                  stepwise=FALSE, 
                                                  approx=FALSE))
      } else {
         train_fit <- train %>% 
            fabletools::model(Self=ARIMA(Measure~pdq(p,d,q)+PDQ(0,0,0)))
      }
   } else {
      pm2 <- ifelse(p-2<0,0,p-2)
      qm2 <- ifelse(q-2<0,0,q-2)
      pp2 <- p+2
      qp2 <- q+2
      PM2 <- ifelse(P-2<0,0,P-2)
      QM2 <- ifelse(Q-2<0,0,Q-2)
      PP2 <- P+2
      QP2 <- Q+2
      if (auto=="Y") {
         train_fit <- train %>% 
            fabletools::model(Self=ARIMA(Measure~pdq(p,d,q)+PDQ(P,D,Q)),
                              Auto=ARIMA(Measure~
                                            pdq(p=c(pm2:pp2),
                                                q=c(qm2:qp2)) +
                                            PDQ(P=c(PM2:PP2),
                                                Q=c(QM2:QP2)), 
                                         stepwise=FALSE, 
                                         approx=FALSE))
      } else {      
         train_fit <- train %>% 
            fabletools::model(Self=ARIMA(Measure~pdq(p,d,q)+PDQ(P,D,Q)))
      }
   }
   train_fc <- train_fit %>% fabletools::forecast(h=h)
   modnames <- train_fit %>% 
      pivot_longer(everything(), names_to = ".model",
                   values_to="Order") %>% 
      mutate(ARIMA=format(Order)) %>%
      data.frame() %>%
      select(.model,ARIMA) %>%
      mutate(ARIMA=str_replace(ARIMA," .*", ""),
             ARIMA=str_replace(ARIMA,"<",""),
             ARIMA=str_replace(ARIMA,"ARIMA","A"),
             ARIMA=str_replace(ARIMA,"\\)\\(", "\\)S\\("),
             ARIMA=str_replace(ARIMA,">",""))
   acc <- fabletools::accuracy(train_fc, forecast) %>%
      inner_join(glance(train_fit), by=".model") %>%
      inner_join(modnames, by=".model") %>%
      mutate(".model"=paste0(.model, ": ", ARIMA)) %>%
      select(.model, RMSE, MAE, MAPE, AICc) %>%
      rename(Model=1) %>% 
      mutate_at(2:5, round, 3) %>% 
      data.frame()
   
   Self.Resid <- train_fit[1] %>% residuals()
   Self.bt <- Box.test(Self.Resid$.resid, lag=slag, type="Ljung-Box", fitdf=SelfDF)
   Self.cp <- ggcpgram(Self.Resid$.resid)+
      theme_bw() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            plot.caption=element_text(hjust=0, size=14)) +
      labs(caption=paste0("Ljung-Box test: p=",round(Self.bt$p.value,4)),
           title=paste0(modnames[1,1],": ",modnames[1,2]))
   ci <- 1.96/sqrt(nrow(train))
   Self.ACF <- train_fit[1] %>% residuals() %>% ACF(.resid, lag_max=25) %>% 
      ggplot(aes(x=lag, y=acf)) + 
      geom_segment(aes(xend=lag, yend=0), size=1, color="blue") +
      geom_ribbon(aes(ymin=-ci, ymax=ci), alpha=.05, fill="red") +
      geom_point(aes(x=lag, y=acf), color="blue", size=2) +
      theme_bw() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()) +
      labs(title="Autocorrelations of residuals from Self")
   
   Self.PACF <- train_fit[1] %>% residuals() %>% PACF(.resid, lag_max=25) %>% 
      ggplot(aes(x=lag, y=pacf)) + 
      geom_segment(aes(xend=lag, yend=0), size=1, color="blue") +
      geom_ribbon(aes(ymin=-ci, ymax=ci), alpha=.05, fill="red") +
      geom_point(aes(x=lag, y=pacf), color="blue", size=2) +
      theme_bw() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()) +
      labs(title="Partial Autocorrelations of residuals from Self")
   if (auto=="Y") {
      AM <- modnames[2,2]
      if (grepl("S",AM)) {
         AutoDF <- as.integer(substr(AM,3,3)) + as.integer(substr(AM,7,7)) +
            as.integer(substr(AM,11,11)) + as.integer(substr(AM,15,15))
      } else {
         AutoDF <- as.integer(substr(AM,3,3)) + as.integer(substr(AM,7,7))
      }
      Auto.Resid <- train_fit[2] %>% residuals()
      Auto.bt <- Box.test(Auto.Resid$.resid, lag=slag, type="Ljung-Box", 
                          fitdf=AutoDF)
      Auto.cp <- ggcpgram(Auto.Resid$.resid)+
         theme_bw() +
         theme(panel.grid.major.x = element_blank(),
               panel.grid.minor.x = element_blank(),
               plot.caption=element_text(hjust=0, size=14)) +
         labs(caption=paste0("Ljung-Box test: p=",round(Auto.bt$p.value,4)),
              title=paste0(modnames[2,1],": ",modnames[2,2]))
      wn <- plot_grid(Self.cp, Auto.cp, ncol=2)
      Auto.ACF <- train_fit[2] %>% residuals() %>% ACF(.resid, lag_max=25) %>% 
         ggplot(aes(x=lag, y=acf)) + 
         geom_segment(aes(xend=lag, yend=0), size=1, color="blue") +
         geom_ribbon(aes(ymin=-ci, ymax=ci), alpha=.05, fill="red") +
         geom_point(aes(x=lag, y=acf), color="blue", size=2) +
         theme_bw() +
         theme(panel.grid.major.x = element_blank(),
               panel.grid.minor.x = element_blank()) +
         labs(title="Autocorrelations of residuals from Auto")
      Auto.PACF <- train_fit[2] %>% residuals() %>% PACF(.resid, lag_max=25) %>% 
         ggplot(aes(x=lag, y=pacf)) + 
         geom_segment(aes(xend=lag, yend=0), size=1, color="blue") +
         geom_ribbon(aes(ymin=-ci, ymax=ci), alpha=.05, fill="red") +
         geom_point(aes(x=lag, y=pacf), color="blue", size=2) +
         theme_bw() +
         theme(panel.grid.major.x = element_blank(),
               panel.grid.minor.x = element_blank()) +
         labs(title="Partial Autocorrelations of residuals from Auto")
      acresid <- plot_grid(Self.ACF, Auto.ACF, 
                           Self.PACF, Auto.PACF,
                           ncol=2, nrow=2)
   }

   
   if (auto=="N") {
      wn <- Self.cp
      acresid <- plot_grid(Self.ACF, Self.PACF,
                        ncol=2, nrow=1)
   }

   fcresid <- train_fc %>% 
      mutate(actual=rep(forecast$Measure,ifelse(auto=="Y",2,1)),
             resid=actual-.mean) %>% 
      inner_join(modnames, by=".model") %>%
      mutate(.model=paste0(.model, ": ", ARIMA)) %>%
      select(-ARIMA)
   fcresplot <- fcresid %>% ggplot(aes(x=Date, y=resid, color=.model)) +
      geom_line(size=1) +
      geom_hline(yintercept = 0, color="black", linetype="dashed", size=2) +
      guides(color=guide_legend(title="Model")) +
      theme(legend.position = "bottom")
   plot <- train_fc %>% autoplot(train, level=NULL, size=1) + 
      autolayer(forecast, .data[[obs]]) +
      labs(y=obs) +
      guides(color=guide_legend(title="Model")) +
      theme(legend.position = "bottom")
   results <- list("plot"=plot, "acc"=acc, 
                   "fcresid"=fcresid, "fcresplot"=fcresplot,
                   "acresid"=acresid, "wn"=wn)
   return(results)
}