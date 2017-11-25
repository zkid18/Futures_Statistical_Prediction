install.packages("quantmod")
install.packages("xts", repos="http://cloud.r-project.org")
install.packages("lattice")
install.packages("timeSeries")
install.packages("rugarch")
library(quantmod)
library(lattice)
library(timeSeries)
library(rugarch)
load("/Users/zkid18/OneDrive/Документы/ботва/Statistical\ Prediction/MSBD\ 5013/R\ Platform/Data/data_20170717_20170915.RData")
names(data_format1)
ab<-data_format1[["AG.SHF"]]
View(ab)
a<- xts(ab$close,order.by=ab$time)
Returns = diff(log(a))
Returns[as.character(head(index(a),1))] = 0
head(Returns)
windowLength = 300
foreLength = length(Returns) - windowLength
forecasts <- vector(mode="character", length=foreLength)

for(d in 0:foreLength){
  ReturnsOffset = Returns[(1+d):(windowLength+d)]
  final.aic <- Inf
  final.order <- c(0,0,0)
  for (p in 0:5) for (q in 0:5){
    if (p==0 && q==0){
      next
    }
    arimaFit =  tryCatch( arima(ReturnsOffset, order=c(p, 0, q)), error=function(err) FALSE,warning=function(err) FALSE)
    if(!is.logical(arimaFit)){
      current.aic <- AIC(arimaFit)
      if(current.aic < final.aic){
        final.aic <- current.aic
        final.order <- c(p,0,q)
        final.arima <- arima(ReturnsOffset, order=final.order)
      }
    }
    else{
      next
    }
  }
  spec = ugarchspec(
    variance.model=list(garchOrder=c(1,1)),
    mean.model=list(armaOrder=c(
      final.order[1], final.order[3]
    ), include.mean=TRUE),
    distribution.model="sged"
  )
  fit = tryCatch(
    ugarchfit(
      spec, ReturnsOffset, solver ='hybrid'
    ), error=function(e) e, warning=function(w) w
  )

  if(is(fit, "warning")) {
    forecasts[d+1] = paste(
      index(ReturnsOffset[windowLength]), 1, sep=","
    )
    print(
      paste(
        index(ReturnsOffset[windowLength]), 1, sep=","
      )
    )
  } else {
    fore = ugarchforecast(fit, n.ahead=1)
    ind = fore@forecast$seriesFor
    forecasts[d+1] = paste(
      colnames(ind), ifelse(ind[1] < 0, -1, 1), sep=","
    )
    print(paste(colnames(ind), ifelse(ind[1] < 0, -1, 1), sep=","))
  }
}

write.csv(forecasts, file="forecasts.csv", row.names=FALSE)
