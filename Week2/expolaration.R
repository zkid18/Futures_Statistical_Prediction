#Idea of the strategy

#Install the libraries
install.packages("quantmod")
install.packages("Rcpp")
install.packages("xts", repos="http://cloud.r-project.org")
require(quantmod)
require(xts)
require(Rcpp)

#Set working directory
setwd("~/OneDrive/Документы/ботва/Statistical Prediction/MSBD 5013/R Platform/Week2")

#Load files
load("./Data/data_20170717_20170915.RData")
info = read.csv("./Data/information.csv")

AU = data_format1[['AU.SHF']]
AU_min <- xts(AU[,-1], order.by=AU[,1])
AU_10min <- to.period(AU_min, period = "minutes", k = 10)

# data_list - list containing n sequential elements from data_format2

generate_bar = function(data_list ){
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
  
  #print(df_close)
  #print(rm)
  close_roll_mean = rm[bar_len,]
  #print("X10")
  #print(close_roll_mean[3])
  #print(close_roll_mean)
  
  close = data_list[[length(data_list)]][, "close"]
  high_mat = do.call(rbind, lapply(data_list, FUN = function(x) x[, "high"]))
  high = apply(high_mat, 2, max)
  low_mat = do.call(rbind, lapply(data_list, FUN = function(x) x[, "low"]))
  low = apply(high_mat, 2, min)
  roll_mean_close = close_roll_mean
  average = (high+low)/2
  output = data.frame(open=open, high=high, low=low, close=close, average = average, roll_mean = roll_mean_close)
  
  rownames(output) = rownames(data_list[[1]])
  #print("Output")
  #print(output)
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
  #is_long_signal = (average_prev > average)
  is_long_signal = (close_mean < close_mean_prev)
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
  #is_short_signal = (average_prev < average)
  is_short_signal = (close_mean > close_mean_prev)
  return(list(bar_close=close, is_short_signal=is_short_signal))
}

last_element <- function(x) {
  lapply(x, tail, n = 1L)
}

save(long_signal, short_signal, generate_bar, last_element, file = "functions.RData")