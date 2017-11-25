#Idea of the strategy

#Install the libraries
install.packages("quantmod")
install.packages("zoo")
install.packages("Rcpp")
install.packages("xts", repos="http://cloud.r-project.org")
install.packages("lattice")
install.packages("timeSeries")
install.packages("rugarch")
install.packages("forecast")
install.packages("tseries")

require(tseries)
require(forecast)
require(timeSeries)
require(rugarch)
require(lattice)
require(quantmod)
require(xts)
require(Rcpp)
require(zoo)

library(rugarch)
library(forecast)
library(tseries)

#Set working directory
setwd("~/OneDrive/Документы/ботва/Statistical Prediction/MSBD 5013/R Platform/Week3")

#Load files
load("./Data/data_20170717_20170915.RData")
info = read.csv("./Data/information.csv")

AU = data_format1[['AU.SHF']]
AU_min <- xts(AU[,-1], order.by=AU[,1])
AU_10min <- to.period(AU_min, period = "minutes", k = 10)

# data_list - list containing n sequential elements from data_format2

n <- 13
low_df <- fit_arima_model(return = "low")
high_df <- fit_arima_model()

generate_bar = function(data_list, counter){
  open = data_list[[1]][, "open"]
  bar_len = length(data_list)
  
  #Create rollmean
  df_close <- data.frame(matrix(ncol = bar_len, nrow = nrow(data_list[[1]])))
  rownames(df_close) <- row.names(data_list[[1]])
  for (i in 1:length(data_list)) {
    df_close[i] = data_list[[i]][, "close"]
  }
  
  t_close = t(df_close)
  rm <- rollmeanr((t_close),3,fill=NA)
  
  close_roll_mean = rm[bar_len,]

  close = data_list[[length(data_list)]][, "close"]
  high_mat = do.call(rbind, lapply(data_list, FUN = function(x) x[, "high"]))
  high = apply(high_mat, 2, max)
  low_mat = do.call(rbind, lapply(data_list, FUN = function(x) x[, "low"]))
  low = apply(high_mat, 2, min)
  roll_mean_close = close_roll_mean
  average = (high+low)/2
  arima_high = high_df[counter]
  arima_low = tail(low_df, -n)[counter]
  
  output = data.frame(open=open, high=high, low=low, close=close, average = average, roll_mean = roll_mean_close)
  output["high_arima"] = arima_high
  output["low_arima"] = arima_low
  rownames(output) = rownames(data_list[[1]])
  
  return(output)
}

long_signal = function(data, data_prev, asset = 3) {
  open = data[asset, "open"]
  close = data[asset, "close"]
  average = data[asset, "average"]
  open_prev = data_prev[asset, "open"]
  close_prev = data_prev[asset, "close"]
  average_prev = data_prev[asset, "average"]
  close_mean_prev = data_prev[asset, "roll_mean"]
  close_mean = data[asset, "roll_mean"]
  arima_high = data[asset, "high_arima"]
  arima_low = data[asset, "low_arima"]
  
  log.close = log(close)
  #is_long_signal = (average_prev > average)
  #is_long_signal = (close_mean < close_mean_prev)
  is_long_signal = log.close > arima_high
  return(list(bar_close=close, is_long_signal=is_long_signal))
}


short_signal = function(data, data_prev, asset = 3) {
  open = data[asset, "open"]
  close = data[asset, "close"]
  average = data[asset, "average"]
  open_prev = data_prev[asset, "open"]
  close_prev = data_prev[asset, "close"]
  average_prev = data_prev[asset, "average"]
  close_mean_prev = data_prev[asset, "roll_mean"]
  close_mean = data[asset, "roll_mean"]
  arima_high = data[asset, "high_arima"]
  arima_low = data[asset, "low_arima"]
  
  log.close = log(close)
  #is_short_signal = (average_prev < average)
  #is_short_signal = (close_mean > close_mean_prev)
  is_short_signal = log.close < arima_low
  return(list(bar_close=close, is_short_signal=is_short_signal))
}

last_element <- function(x) {
  lapply(x, tail, n = 1L)
}


fit_arima_model <- function(return = "high") {
  #close = data[asset, "close"]
  assets = names(data_format1)

  df_high = data.frame(matrix(ncol = nrow(data_format1[['AU.SHF']]), nrow = length(data_format1)))
  rownames(df_high) <- names(data_format1)
  
  df_low = data.frame(matrix(ncol = nrow(data_format1[['AU.SHF']]), nrow = length(data_format1)))
  rownames(df_high) <- names(data_format1)
  
  for (asset in assets){
    asset_data = data_format1[[asset]]
    diff.asset_data = data_format1[[asset]]
    difflog.asset_data = diff(log(asset_data$close))
    log.asset_data = log(asset_data$close)
    autoarima = auto.arima(log.asset_data, ic = "aic")
    res.autoarima = autoarima$res
    arch08=garch(res.autoarima,order=c(0,8),trace=F)
    forecast_arima_step1=forecast(autoarima,1,level=95)
    forecast_arima=forecast(autoarima,100,level=95)
    ht.arch08=arch08$fit[,1]^2
    plot(forecast_arima, main='ARIMA Auto,Low,High')
    fit212=fitted.values(autoarima)
    low=fit212-1.96*sqrt(ht.arch08)
    high=fit212+1.96*sqrt(ht.arch08)
    plot(log.asset_data,type='l',main='Log Apple,Low,High')
    lines(low,col='red')
    lines(high,col='blue')
    high_df = tapply(high, list(time_index = floor(time(high)), high_price = month.abb[cycle(high)]), c)
    low_df = tapply(low, list(time_index = floor(time(high)), high_price = month.abb[cycle(high)]), c)
    df_high[asset,] = high_df
    df_low[asset,] = low_df
  }
  
  if (return == "high") {
    return (df_high) 
  }
  if (return == "low") {
    return (df_low) 
  }
}

save(long_signal, short_signal, generate_bar, last_element, file = "functions.RData")