getwd()
setwd("C:\\Users\\P2190101\\Desktop\\NK Personal\\NK Study\\UNCC\\6211 - Advanced BI\\Part III - Survival Analysis")   # Set working directory

#install.packages('survival')
#install.packages('devtools')

library(survival)
library(ggplot2)
library(devtools)


df <- read.csv('heart_attack.csv')

devtools::install_github('sachsmc/ggkm')

km1 <-survfit(Surv(lenfol,fstat)~1,data=df)
summary(km1)
plot(km1,xlab='Time',ylab='Survival Probability')

km2<-survfit(Surv(lenfol,fstat)~gender,data=df)
summary(km2)
ggplot(df,aes(time=lenfol,status=fsta,color=factor(gender)))+geom_km()

coxph <-coxph(Surv(lenfol,fstat)~gender,data=df, method='breslow')
summary(coxph)

attach(df)

