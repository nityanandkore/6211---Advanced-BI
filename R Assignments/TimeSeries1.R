#install.packages('forecast')
library(forecast)

getwd()
setwd("C:\\Users\\P2190101\\Desktop\\NK Personal\\NK Study\\UNCC\\6211 - Advanced BI\\Part V - Time Series")   # Set working directory

data <- read.csv('Amtrak.csv')

x<- ts(data$Ridership,
       start=c(1991,1),
       frequency = 12) # what is the cycle
x

plot(x)


Amtrak.lm <- tslm(x~trend)

summary(Amtrak.lm)

nValid <-36
nTrain <- length(x) -nValid

train.ts <-window(x,start=c(1991,1),end=c(1991,nTrain))
valid.ts <- window(x,start=c(1991,nTrain+1),end=c(1991,nTrain+nValid))

#----------- Linear Trend -------------------
train.lm<- tslm(train.ts~trend)
summary(train.lm)

train.lm.pred <- forecast(train.lm, h=nValid,level=0) #level=0 - does not need to provide confidence interval
accuracy(train.lm.pred,valid.ts)

# Visualize the linear trend model
par(mfrow = c(1, 1)) # Create single graph

plot(train.lm.pred, ylim = c(1300, 2600),  ylab = "Ridership", xlab = "Time", 
     bty = "l", xaxt = "n", xlim = c(1991,2006),main = "", flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(train.lm.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)

#------------------ Poly Trend -----------------

train.lm.poly.trend <- tslm(train.ts~ trend+I(trend^2))
summary(train.lm.poly.trend)

train.lm.poly.trend.pred <- forecast(train.lm.poly.trend,h=nValid,level=0)
accuracy(train.lm.poly.trend.pred,valid.ts)

#--------- lm with seasonality -----------------

train.lm.season <- tslm(train.ts ~season)
summary(train.lm.season)

train.lm.trend.season <- tslm(train.ts~trend+I(trend^2)+season)
summary(train.lm.trend.season)

train.lm.trend.season.pred <- forecast(train.lm.trend.season,h=nValid,level=0)
accuracy(train.lm.trend.season.pred,valid.ts)

#--------- Poly with seasonality -----------------




#----------- Moving Averages------------

library(zoo)
x
ma <- rollmean(x,k=12,align='right')
summary(ma)
ma

MAPE =mean(abs(ma-x)/x,na.rm=T)
MAPE

ses <-ses(train.ts,alpha=0.2,h=36) #fixed alpha
autoplot(ses)
accuracy(ses,valid.ts)

ses1 <- ses(train.ts,alpha=NULL,h=36) # Model automatically selects alpha
summary(ses1)
accuracy(ses1,valid.ts)


