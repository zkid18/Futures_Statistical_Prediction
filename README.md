# Futures Statistical Prediction
This repository premiraly contains information about project on statiscal prediction course. 
Later all folders, except Data_Wizards will be removed.

The best perfomance showed the strategy, based on ARIMA-GARCH model.

## Environment 

The environment consist of three parts:
1. exploration.R - describes the main logic for strategy
2. strategy.R - describes the implementation of the strategy
3. test.R - backtesting of strategy


## Arima model

Time series models like ARIMA and GARCH have been widely been used by traders to predict future stock prices. There are two major methods in time series analysis to predict future values of series: frequency-based and time-based methods. In this project, we have utilized time-based methods to predict future value of one of 13 stocks provided to us. ARIMA models investigates the autocorrelation of series i.e. how prices today are correlated with previous lag prices. Based on autocorrelation analysis, we employ Box-Jenkins methodology[1] to find out value of p and q in ARIMA (p, d, q) model.

The perfomace results are following:
1. Total return: 0.0158788375000005
2. Average daily return: 0.00321439375999129
3. Sharpe ratio: 6.29708234304602
4. Maximum drawdown: -0.013208355710459
