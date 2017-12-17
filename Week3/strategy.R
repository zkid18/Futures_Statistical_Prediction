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
bar_length = 10
asset = 8 # i.e. AU.SHF
my_cash_balance_lower_limit = 3000000


#### Working area 3 ####

# You can set some initial values for memory variable here
memory = list(counter = 0,
              long_counter = 0,
              short_counter = 0,
              data_list = list(),
              bar = NULL,
              bar_prev = NULL,
              long_check_table = data.frame(bar_close=numeric(), is_long_signal=logical()),
              short_check_table = data.frame(bar_close=numeric(), is_short_signal=logical())
)


#### Working area 4 ####

# Here is your strategy function
# Note:
# 1. DO NOT modify the function parameters
# 2. The strategy function AWAYS returns a list contains two elements - position and memory. Position is a vector of length 13 (i.e. number of futures in our project); memory is a customer list which will later work as an input parameter when test your strategy at the next minute

strategy = function(time, data, info, initial_cash, transaction, cash_balance, margin_balance, total_balance, position_current, memory=list()){
  

  # Load variables from memory
  counter = memory[["counter"]]
  long_counter = memory[["long_counter"]]
  short_counter = memory[["short_counter"]]
  data_list = memory[["data_list"]]
  bar = memory[["bar"]]
  bar_prev = memory[["bar_prev"]]
  long_check_table = memory[["long_check_table"]]
  short_check_table = memory[["short_check_table"]]
  
  # Update counter
  #counter shows the number of minutes passed
  counter = counter + 1
  #print(counter)
  # Generate OHLC data for every 15 minutes
  if(counter%%bar_length == 0){
    #print(bar_length)
    # save minute data to data_list
    data_list[[bar_length]] = data
    
    bar = generate_bar(data_list, counter)
    
    if(!is.null(bar_prev)){
      
      # check patterns for a single bar
      long_check = long_signal(data = bar, data_prev = bar_prev, asset = asset)
      long_check_table = rbind(long_check_table, long_check)
      short_check = short_signal(data = bar, data_prev = bar_prev, asset = asset)
      short_check_table = rbind(short_check_table, short_check)
      
      bar_num = counter/bar_length
      if(bar_num > 3){
        
        # When there is a three white soider signal, long 10 lots of asset at next minute unless the current cash balance is less than 3,000,000
        # if (last_element(long_check_table["is_long_signal"]) == TRUE) && cache_long == 3
        if (last_element(long_check_table["is_long_signal"]) == TRUE)  {
          print("Long counter")
          long_counter = long_counter + 1
          if ((cash_balance > my_cash_balance_lower_limit) && (long_counter == 3)) {
            position_new[asset] = position_current[asset] + 10
            long_counter = 0
            #print("Long balance")
            #print(position_current)
            #print(position_new)
            #print(position_old)
          }
        }
        
        # short signal 
        # When there is a three black craw signal, short 10 lots of asset at next minute unless the current cash balance is less than 3,000,000

        if (last_element(short_check_table["is_short_signal"]) == TRUE) {
          print("Short counter")
          short_counter = short_counter + 1
          print(short_counter)
          if((cash_balance > my_cash_balance_lower_limit) && (short_counter == 3)) {
            position_new[asset] = position_current[asset] - 10
            short_counter = 0
            #print("Short balance")
            #print(position_current)
            #print(position_new)
            #print(position_old)
          }
        }
      }
    }
    

    bar_prev = bar
    
  } else {
    # save minute data to data_list
    data_list[[counter%%bar_length]] = data
  }
  
  # Update memory
  memory[["counter"]] = counter
  memory[["data_list"]] = data_list
  memory[["bar"]] = bar
  memory[["bar_prev"]] = bar_prev
  memory[["long_check_table"]] = long_check_table
  memory[["short_check_table"]] = short_check_table
  memory[["long_counter"]] = long_counter
  memory[["short_counter"]] = short_counter
  
  return(list(position=position_new, memory=memory))
  #End of the function
}

