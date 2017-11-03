#Idea of the strategy

#Install the libraries
install.packages("quantmod")
install.packages("xts", repos="http://cloud.r-project.org")
require(quantmod)
require(xts)

#Set working directory
setwd("~/OneDrive/Документы/ботва/Statistical Prediction/MSBD 5013/R Platform/Week1")

#Load files
load("./Data/data_20170717_20170915.RData")
info = read.csv("./Data/information.csv")

AU = data_format1[['AU.SHF']]
AU_min <- xts(AU[,-1], order.by=AU[,1])
AU_10min <- to.period(AU_min, period = "minutes", k = 10)

# data_list - list containing n sequential elements from data_format2

generate_bar = function(data_list ){
  open = data_list[[1]][, "open"]
  close = data_list[[length(data_list)]][, "close"]
  high_mat = do.call(rbind, lapply(data_list, FUN = function(x) x[, "high"]))
  high = apply(high_mat, 2, max)
  low_mat = do.call(rbind, lapply(data_list, FUN = function(x) x[, "low"]))
  low = apply(high_mat, 2, min)
  average = (high+low)/2
  output = data.frame(open=open, high=high, low=low, close=close, average = average)
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
  is_long_signal = (average_prev > average)
  return(list(bar_close=close, is_long_signal=is_long_signal))
}


short_signal = function(data, data_prev, asset = 3) {
  open = data[asset, "open"]
  close = data[asset, "close"]
  average = data[asset, "average"]
  open_prev = data_prev[asset, "open"]
  close_prev = data_prev[asset, "close"]
  average_prev = data_prev[asset, "average"]
  is_short_signal = (average_prev < average)
  return(list(bar_close=close, is_short_signal=is_short_signal))
}

save(long_signal, short_signal, generate_bar, file = "functions.RData")