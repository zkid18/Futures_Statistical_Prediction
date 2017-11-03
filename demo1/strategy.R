#### Working area 1 ####

# You can import R packages here
# Note:
# The R packages you import must be in the standard CRAN repository and can work under R version 3.4.1

# require("package1")
# require("package2")


#### Working area 2 ####

# Here you can 
# 1. Load your facility files such as RData containing customer functions or trained models
# 2. Set some global contants
# Note:
# 1. You should put your facility files in the same folder as this strategy.R
# 2. When load files, ALWAYS use relative path such as load("functions.RData"). DO NOT use absolute path such as load("C:\Users\Peter\Documents\functions.RData")
# 3. Be careful about the variable names in your facility data and global constants. Make sure they are not conflict with the variable names in test.R (such as initial_cash, time_list, etc.)

asset = 3 # i.e. AU.SHF


#### Working area 3 ####

# You can set some initial values for memory variable here
memory = list()


#### Working area 4 ####

# Here is your strategy function
# Note:
# 1. DO NOT modify the function parameters
# 2. The strategy function AWAYS returns a list contains two elements - position and memory

strategy = function(time, data, info, initial_cash, transaction, cash_balance, margin_balance, total_balance, position_current, memory=list()){
  
  # Buy AU.SHF at the very beggining (first minute) using half cash_balance
  if(is.null(memory[['position']])){
    memory[['position']] = rep(0, nrow(data))
    average_price = rowMeans(data[, 1:4])[asset]
    lot_value = average_price*info$unit_per_lot[asset]*info$margin_rate[asset]
    memory[['position']][asset] = -round(0.5*initial_cash/(lot_value*(1+transaction)))
  }
  
  return(list(position=memory[['position']], memory=memory))
}
