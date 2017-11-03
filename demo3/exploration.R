# Idea of this strategy:
# 1. Watch candle stick plots of AU
# 2. Try to find some patterns which give long/short signals
install.packages("quantmod")
install.packages("xts", repos="http://cloud.r-project.org")

require(quantmod)
require(xts)


#setwd("/Users/vidal/Documents/Quant/MAFS/demo2")

load("./Data/data_20170717_20170915.RData")
info = read.csv("./Data/information.csv")

AU = data_format1[['AU.SHF']]
AU_min <- xts(AU[,-1], order.by=AU[,1])
AU_15min <- to.period(AU_min, period = "minutes", k = 15)
 
candleChart(AU_15min, theme='white', type='candles', subset = "first 5 days")

candleChart(AU_15min, theme='white', type='candles', subset = "2017-07-20 13:00:00/2017-07-21 10:00:00")

# Pattern for long signal: (unstrict) three white soliders
# For three sequential bars, if they have
# 1. OPEN(n) > OPEN(n-1)
# 2. CLOSE(n) > CLOSE(n-1)
# 3. CLOSE(n) > OPEN(n)
# then we long 10 lots of AU.SHF at next bar unless the current cash balance is less than 3,000,000

# stop loss point: When the price drop down to the close price of the first white soilder, clear all long position

# target profit point: When the price go up to (1+5%) times the close price of the third white soilder, clear all long position

generate_bar = function(data_list # list containing n sequential elements from data_format2
                        ){
  print("Data in the bar")
  print(data_list)
  
  open = data_list[[1]][, "open"]
  close = data_list[[length(data_list)]][, "close"]
  high_mat = do.call(rbind, lapply(data_list, FUN = function(x) x[, "high"]))
  high = apply(high_mat, 2, max)
  low_mat = do.call(rbind, lapply(data_list, FUN = function(x) x[, "low"]))
  low = apply(high_mat, 2, min)
  output = data.frame(open=open, high=high, low=low, close=close)
  rownames(output) = rownames(data_list[[1]])
  return(output)
}


white_soider = function(data, data_prev, asset=3){
  #print(data)
  #print(data_prev)
  open = data[asset, "open"]
  close = data[asset, "close"]
  open_prev = data_prev[asset, "open"]
  close_prev = data_prev[asset, "close"]
  is_white_soider = (open > open_prev) & (close > close_prev) & (close > open)
  return(list(bar_close=close, is_white_soider=is_white_soider))
}

# test function
white_soider(generate_bar(data_format2[16:30]), generate_bar(data_format2[1:15]))

# Pattern for short signal: (unstrict) three black craws
# For three sequential bars, if they have
# 1. OPEN(n) < OPEN(n-1)
# 2. CLOSE(n) < CLOSE(n-1)
# 3. CLOSE(n) < OPEN(n)
# then we short 10 lots of AU.SHF at next bar unless the current cash balance is less than 3,000,000

# stop loss point: When the price go up to the close price of the first black craw, clear all short position

# target profit point: When the price go up to (1-5%) times the close price of the third black craw, clear all long position


black_craw = function(data, data_prev, asset=3){
  #print(data)
  #print(data_prev)
  open = data[asset, "open"]
  close = data[asset, "close"]
  open_prev = data_prev[asset, "open"]
  close_prev = data_prev[asset, "close"]
  is_black_craw = (open < open_prev) & (close < close_prev) & (close < open)
  return(list(bar_close=close, is_black_craw=is_black_craw))
}

# test function
black_craw(generate_bar(data_format2[16:30]), generate_bar(data_format2[1:15]))

save(white_soider, black_craw, generate_bar, file = "functions.RData")
