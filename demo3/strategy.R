#### Working area 1 ####

# You can import R packages here
# Note:
# The R packages you import must be in the standard CRAN repository and can work under R version 3.4.1

# require("package1")
# require("package2")
install.packages("quantmod")
install.packages("tseries")
install.packages("PerformanceAnalytics")
install.packages("timeSeries")
install.packages("xts", repos="http://cloud.r-project.org")

library(tseries)
library(timeSeries)
library(PerformanceAnalytics)
library(quantmod)
library(xts)

#### Working area 2 ####

# Here you can 
# 1. Load your facility files such as RData containing customer functions or trained models
# 2. Set some global contants
# Note:
# 1. You should put your facility files in the same folder as this strategy.R
# 2. When load files, ALWAYS use relative path such as load("functions.RData"). DO NOT use absolute path such as load("C:\Users\Peter\Documents\functions.RData")
# 3. Be careful about the variable names in your facility data and global constants. Make sure they are not conflict with the variable names in test.R (such as initial_cash, time_list, etc.)

load("functions.RData")
bar_length = 15
asset = 3 # i.e. AU.SHF
my_cash_balance_lower_limit = 3000000


#### Working area 3 ####

# You can set some initial values for memory variable here
memory = list(counter = 0,
              data_list = list(),
              bar = NULL,
              bar_prev = NULL,
              long_stop_loss = -Inf,
              short_stop_loss = Inf,
              long_profit_target = Inf,
              short_profit_target = -Inf,
              ws_check_table = data.frame(bar_close=numeric(),
                                          is_white_soider=logical()),
              bc_check_table = data.frame(bar_close=numeric(),
                                          is_black_craw=logical())
              )


#### Working area 4 ####

# Here is your strategy function
# Note:
# 1. DO NOT modify the function parameters
# 2. The strategy function AWAYS returns a list contains two elements - position and memory. Position is a vector of length 13 (i.e. number of futures in our project); memory is a customer list which will later work as an input parameter when test your strategy at the next minute

strategy = function(time, data, info, initial_cash, transaction, cash_balance, margin_balance, total_balance, position_current, memory=list()){
  
  # Load variables from memory
  counter = memory[["counter"]]
  data_list = memory[["data_list"]]
  bar = memory[["bar"]]
  bar_prev = memory[["bar_prev"]]
  ws_check_table = memory[["ws_check_table"]]
  bc_check_table = memory[["bc_check_table"]]
  long_stop_loss = memory[["long_stop_loss"]]
  short_stop_loss = memory[["short_stop_loss"]]
  long_profit_target = memory[["long_profit_target"]]
  short_profit_target = memory[["short_profit_target"]]
  
  # Update counter
  #counter shows the number of minutes passed
  counter = counter + 1
  #print(counter)
  # Generate OHLC data for every 15 minutes
  if(counter%%bar_length == 0){
    #print(bar_length)
    # save minute data to data_list
    data_list[[bar_length]] = data
    
    bar = generate_bar(data_list)
    
    if(!is.null(bar_prev)){
      
      # check patterns for a single bar
      print(counter)
      ws_check = white_soider(data = bar, data_prev = bar_prev, asset = asset)
      ws_check_table = rbind(ws_check_table, ws_check)
      bc_check = black_craw(data = bar, data_prev = bar_prev, asset = asset)
      bc_check_table = rbind(bc_check_table, bc_check)
      
      bar_num = nrow(ws_check_table)
      if(bar_num > 3){
        
        # long signal 
        # When there is a three white soider signal, long 10 lots of asset at next minute unless the current cash balance is less than 3,000,000
        print(sum(ws_check_table$is_white_soider[(bar_num-2):bar_num]))
        if(sum(ws_check_table$is_white_soider[(bar_num-2):bar_num])==3){
          if(cash_balance > my_cash_balance_lower_limit){
            print(asset)
            print(position_new)
            position_new[asset] = position_current[asset] + 10
            long_stop_loss = ws_check_table$bar_close[(bar_num-2)]
            long_profit_target = ws_check_table$bar_close[bar_num]*(1+0.05)
          }
        }
        
        # short signal 
        # When there is a three black craw signal, short 10 lots of asset at next minute unless the current cash balance is less than 3,000,000
        if(sum(bc_check_table$is_black_craw[(bar_num-2):bar_num])==2){
          if(cash_balance > my_cash_balance_lower_limit){
            position_new[asset] = position_current[asset] - 10
            short_stop_loss = bc_check_table$bar_close[(bar_num-2)]
            short_profit_target = bc_check_table$bar_close[bar_num]*(1-0.05)
          }
        }
      }
    }
    
    #print(bar_prev)
    #print(bar)
    
    bar_prev = bar
    
  } else {
    # save minute data to data_list
    data_list[[counter%%bar_length]] = data
    #print(counter%%bar_length)
    #print(data_list)
  }
  
  
  # close signal
  # When reach stop loss / target profit points, clear all long / short positions
  average_price = rowMeans(data[1:4])[asset]
  if(position_new[asset] > 0){
    if(average_price > long_profit_target | average_price < long_stop_loss)
      position_new[asset] = 0
  } else if(position_new[asset] < 0) {
    if(average_price > short_stop_loss | average_price < short_profit_target)
      position_new[asset] = 0
  } else {
    position_new[asset] = 0
  }
  
  
  # Update memory
  memory[["counter"]] = counter
  memory[["data_list"]] = data_list
  memory[["bar"]] = bar
  memory[["bar_prev"]] = bar_prev
  memory[["ws_check_table"]] = ws_check_table
  memory[["bc_check_table"]] = bc_check_table
  memory[["long_stop_loss"]] = long_stop_loss
  memory[["short_stop_loss"]] = short_stop_loss
  memory[["long_profit_target"]] = long_profit_target
  memory[["short_profit_target"]] = short_profit_target
  
  return(list(position=position_new, memory=memory))
  print(memory)
  
  #End of the function
}

