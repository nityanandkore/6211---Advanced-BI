#install.packages('forecast')
library(forecast)
library(dplyr)

getwd()
setwd("C:\\Users\\P2190101\\Desktop\\NK Personal\\NK Study\\UNCC\\6211 - Advanced BI\\Final Project\\Group Project Option 2 and 3 HHUSA")   # Set working directory

data <- read.csv('SalesForce_Contact.csv')

as.Date(data$Date_of_Service_EntryNew__c, format = "%m/%d/%Y")

data$CreatedDate <- as.Date(data$CreatedDate, format = "%m/%d/%Y")
data$Years_In_Service <- as.numeric(data$Years_In_Service)
data$Date_of_Service_EntryNew__c <- as.Date(data$Date_of_Service_EntryNew__c, format = "%m/%d/%Y")
data$Date_of_SeparationNew__c  <- as.Date(data$Date_of_SeparationNew__c , format = "%m/%d/%Y")
summary(data)

str(data)

#format(as.Date(data$CreatedDate,format = "%m/%d/%Y"), format = '%Y/%m')

df.cnt<-data.frame(table(format(as.Date(data$CreatedDate,format = "%m/%d/%Y"), format = '%Y/%m')))

df.cnt

x<- ts(df.cnt$Freq,
       start=c(2007,1),
       frequency = 12) # what is the cycle
x

plot(x)

# log transformation to achieve homoscedasticity
z<- log10(x)
plot(z)


y <- diff(z)
plot(y)


acf(y,main='ACF Tractor Sales') # autocorrelation Function

library(forecast)
ARIMAfit <- auto.arima(z, approximation=FALSE,trace=TRUE)

summary(ARIMAfit)

# Use the best ARIMA model to forecast future scales
pred <- predict(ARIMAfit,n.ahead=24)
pred

# Plot the data
# Remember initial log-transformation?
par(mfrow = c(1,1))
plot(x,type='l',xlim=c(2008,2020),ylim=c(1,5000),xlab = 'Year',ylab = 'Total Registration')
lines(10^(pred$pred),col='blue') 
lines(10^(pred$pred+1*pred$se),col='orange')
lines(10^(pred$pred-2*pred$se),col='orange')
#=============================================================================================


