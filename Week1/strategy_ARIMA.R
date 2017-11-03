#install necessary packages
install.packages("quantmod")
install.packages("xts", repos="http://cloud.r-project.org")
install.packages("lattice")
install.packages("timeSeries")
install.packages("rugarch")
install.packages("tseries")
install.packages("TSA")
install.packages("forecast")
library(quantmod)

library(quantmod)
library(lattice)
library(timeSeries)
library(rugarch)
library(tseries)
library(forecast)

#load dataset
load("./Data/data_20170717_20170915.RData")
names(data_format1)

ag <- data_format1$AG.SHF
diff.ag = diff(ag$close)
plot(diff.ag, type = 'l', main = "AG dif")

log.ag = log(ag$close)
plot(log.ag, type = 'l', main = "AG log")

difflog.ag = diff(log(ag$close))
plot(difflog.ag, type = 'l', main = "AG diff log")

#Check taht the series is stationary enough to do any kind of time series modelling
print(adf.test(difflog.ag))

#Exploiting ACF and PACF plots
#After we have got the stationary time series, we must answer wheter it is AR or 
#MA process and what order of AR or MA process do we need to use

acf(difflog.ag)
pacf(difflog.ag)

fit = arima(log.ag, order = c(0, 0, 2),include.mean=FALSE)
AIC(fit)
fit = auto.arima(log.ag, ic = "aic")
predict <- predict(fit, n.ahead = 10*12)
print(predict)
