# the working folder for your team
setwd("~/OneDrive/Документы/ботва/Statistical Prediction/MSBD 5013/R Platform/Week1")

# load data_XXXXXXXX_XXXXXXXX.RData and information.csv
# Note: You can load different RData so that you can test your strategy on different periods. Try to make your strategy profitable stably.
# load("D:/Research/MAFS/Data/data_20170717_20170915.RData")
load("./Data/data_20170717_20170915.RData")
info = read.csv("./Data/information.csv")

# source your strategy file
# Note: strategy.R should be in the folder whose name is your teamname. For example, if your teamname is "king", then you should put strategy.R and other facility files in a folder named as "king". After that, you can zip this folder and send the zipped file to the class cloud drive.
source(file = "strategy.R")

# import some packages for testing performance of your strategy
# Note: You should install the following packages in your computer firstly (by commands like install.packages("PerformanceAnalytics")) otherwise you will get error in your R console
require(PerformanceAnalytics)
require(xts)
require(TTR)
options(scipen=999)
options(digits=7)

# some important constants
# Note:
# 1. The following constants will not be changed during the whole regular season and playoff
# 2. DO NOT set your customer variables the same names as the following constants otherwise you may get unexpected error or results
initial_cash = 10000000
cash_balance_lower_limit = 1000000
transaction = 0.00005
time_list = names(data_format2)


# begin to test your strategy minute by minute
for(time in time_list[0:120]){
  
  # 1. initialization
  if(time == time_list[[1]]){
    cash_balance = initial_cash
    total_balance = initial_cash
    revenue = 0
    margin_balance = 0
    average_price_old = rowMeans(data_format2[[time]][1:4])
    position_old = rep(0, nrow(info))
    position_new = rep(0, nrow(info))
    details = data.frame()
    stop_signal = FALSE
  }
  
  
  # 2. calculate position & cash/margin/total balance & transaction cost etc.
  position_change = position_new - position_old
  average_price = rowMeans(data_format2[[time]][1:4])
  transaction_cost = sum(abs(position_change)*transaction*average_price*info$unit_per_lot)
  margin_change = abs(position_new)*average_price*info$margin_rate*info$unit_per_lot - abs(position_old)*average_price_old*info$margin_rate*info$unit_per_lot
  margin_balance = margin_balance + sum(margin_change)
  cash_balance = cash_balance - sum(margin_change) - transaction_cost
  revenue = sum(position_old*(average_price-average_price_old)*info$unit_per_lot)
  total_balance = total_balance + revenue - transaction_cost
  
  details = rbind(details, c(position_new, cash_balance, margin_balance, revenue, total_balance, transaction_cost))
  position_old = position_new
  average_price_old = average_price
  
  
  # 3. check special cases
  # if cash balance is less than lower limit, clear all futures at next minute and stop strategy (i.e. switch stop_signal to TRUE)
  if(cash_balance < cash_balance_lower_limit){
    position_new = rep(0, nrow(info))
    stop_signal = TRUE
    print(paste("Current cash balance is lower than:", as.character(cash_balance_lower_limit)))
    print("Your strategy is forced to stop.")
  }
  
  # when stop_signal is TRUE, stop the strategy part and keep the current balance (i.e., all cash)
  if(stop_signal)
    next
  
  # 4. update position for NEXT minute using students' strategy function
  output = strategy(time = time,
                    data = data_format2[[time]],
                    info = info,
                    initial_cash = initial_cash,
                    transaction = transaction,
                    cash_balance = cash_balance,
                    margin_balance = margin_balance,
                    total_balance = total_balance,
                    position_current = position_new,
                    memory = memory)
  position_new = output[['position']]
  memory = output[['memory']]
  print(time)
}

colnames(details) = c(as.character(info$code), "cash.balance", "margin.balance", "revenue", "total.balance", "transaction.cost")
rownames(details) = time_list

# show minute-level details about your strategy, including:
# 1. positions of all 13 futures (the first 13 columns)
# 2. cash/margin/total balance
# 3. revenue (profit or loss) at each minute
# 4. transaction cost at each minute
View(details)

# calculate summary statistics (Sharpe ratio, total return, average daily return and maximum dropdown)
balance_xts = xts(x=details$total.balance, order.by=as.POSIXct(time_list))
roc_min = ROC(balance_xts, n = 1, type = "discrete")[2:length(time_list)]
roc_daily = period.apply(roc_min, endpoints(roc_min, "days"), function(x) prod(1+x)-1)
roc_hour = period.apply(roc_min, seq(0, length(time_list)-1, 60), function(x) prod(1+x)-1)
total_ret = prod(1+roc_min)-1
sharpe_ratio = SharpeRatio.annualized(roc_daily)
daily_ret = mean(roc_daily)
max_drawdown = min(DrawdownPeak(roc_hour))
print(paste("Total Return:", as.character(total_ret)))
print(paste("Average Daily Return:", as.character(daily_ret)))
print(paste("Sharpe Ratio:", as.character(sharpe_ratio)))
print(paste("Maximum Drawdown:", as.character(max_drawdown)))

# Draw performance chart (hour-level)
charts.PerformanceSummary(R=roc_hour,
                          main="Peformance Summary")
