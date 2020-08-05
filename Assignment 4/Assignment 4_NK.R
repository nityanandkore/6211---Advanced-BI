#install.packages('forecast')
library(forecast)

getwd()
setwd("C:\\Users\\P2190101\\Desktop\\NK Personal\\NK Study\\UNCC\\6211 - Advanced BI\\Assignment 4")   # Set working directory

data <- read.csv('AustralianWines.csv')

x<- ts(data$Red,
       start=c(1980,1),
       frequency = 12) # what is the cycle
x

plot(x)

AustralianWines.lm <- tslm(x~trend)

summary(AustralianWines.lm)

nValid <-24
nTrain <- length(x) -nValid

trainNK.ts <-window(x,start=c(1980,1),end=c(1980,nTrain))
validNK.ts <- window(x,start=c(1980,nTrain+1),end=c(1980,nTrain+nValid))


#----------- Linear Trend -------------------
trainNK.lm<- tslm(trainNK.ts~trend)
summary(trainNK.lm)

trainNK.lm.pred <- forecast(trainNK.lm, h=nValid,level=0) #level=0 - does not need to provide confidence interval
accuracy(trainNK.lm.pred,validNK.ts)

# Visualize the linear trend model
par(mfrow = c(1, 1)) # Create single graph

plot(trainNK.lm.pred, ylim = c(1300, 2600),  ylab = "Red Wine Sale", xlab = "Time", 
     bty = "l", xaxt = "n", xlim = c(1980,1995),main = "", flty = 2)
axis(1, at = seq(1980,1995, 1), labels = format(seq(1980,1995, 1)))
lines(trainNK.lm.pred$fitted, lwd = 2, col = "blue")
lines(validNK.ts)

# Evaluate model performance
accuracy(trainNK.lm.pred,validNK.ts)


#------------------ Poly Trend -----------------

train.lm.poly.trend <- tslm(trainNK.ts~ trend+I(trend^2))
summary(train.lm.poly.trend)

train.lm.poly.trend.pred <- forecast(train.lm.poly.trend,h=nValid,level=0)
accuracy(train.lm.poly.trend.pred,validNK.ts)

#--------- lm with seasonality -----------------

train.lm.season <- tslm(trainNK.ts ~season)
summary(train.lm.season)

train.lm.trend.season <- tslm(trainNK.ts~trend+I(trend^2)+season)
summary(train.lm.trend.season)

train.lm.trend.season.pred <- forecast(train.lm.trend.season,h=nValid,level=0)
accuracy(train.lm.trend.season.pred,validNK.ts)
#----------- Moving Averages------------

library(zoo)
x
ma <- rollmean(x,k=12,align='right')
summary(ma)
ma

MAPE =mean(abs(ma-x)/x,na.rm=T)
MAPE

ses <-ses(trainNK.ts,alpha=0.2,h=36) #fixed alpha
autoplot(ses)
accuracy(ses,validNK.ts)

ses1 <- ses(trainNK.ts,alpha=NULL,h=36) # Model automatically selects alpha
autoplot(ses1)
accuracy(ses1,validNK.ts)


