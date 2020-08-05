#install.packages('forecast')
library(forecast)

getwd()
setwd("C:\\Users\\P2190101\\Desktop\\NK Personal\\NK Study\\UNCC\\6211 - Advanced BI\\Part V - Time Series")   # Set working directory

data <- read.csv('Tractor-Sales.csv')

str(data)
head(data)

x <- ts(data[,2],start=c(2003,1),frequency = 12) #time series data is matrix - Yt-- convert it to time series
x

plot(x) # increasing trend, Year end decresing and increasing beginning of the year- seasonality, stationary: no constant mean/variance/covariance

#remove variance - homostaticity - make it constant

z<- log10(x)
plot(z) # but still not stationary becuase mean is not constant. 

# do first order differencing to make the constant mean

y<- diff(z)
plot(y) # Still difficult to tell whether its stationary

# do some test to check if its really stationary

acf(y,main='ACF Tractor Sales') # autocorrelation Function
#x- lags y- correlation (-1:+1)

ARIMAFIT<-auto.arima(z, approximation = FALSE,trace=TRUE) #ARIMA(2,1,2)(1,1,1)[12] -> Non seasonal PDQ, Seasonal PDQ, 12 - term period.

#Best model: ARIMA(0,1,1)(0,1,1)[12] -> exponential seasonal model

summary(ARIMAFIT)

pred <- predict(ARIMAFIT,n.ahead = 36) # predict for 36 months

pred # log result so we need to get original number by reversing log

par(mfrow = c(1,1))
plot(x,type='l',xlim=c(2004,2018),ylim=c(1,1600),xlab = 'Year',ylab = 'Tractor Sales')
lines(10^(pred$pred),col='blue') 
lines(10^(pred$pred+2*pred$se),col='orange')
lines(10^(pred$pred-2*pred$se),col='orange')

