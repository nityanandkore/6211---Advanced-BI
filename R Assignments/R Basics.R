x<-c(3,4,5)
y<-c('Monday','Tuesday','Wednesday')

m<- matrix(1:12,nrow=3)

print(m)

data()

df<- mtcars

df

View(df)
summary(df)
str(df)

ftable(xtabs(~cyl+gear,data=df))

getwd()

setwd("C:\\Users\\P2190101\\Desktop\\NK Personal\\NK Study\\UNCC\\6211 - Advanced BI\\Dataset")

getwd()

df2 <- read.csv('sample.csv')

str(df2)

df2<-read.csv('sample.csv',na.strings = c('NA','?',""))

str(df2)

df2$Date <-as.Date(df2$Date,format = "%m/%d/%y")

str(df2)


df3<-read.csv('sample.csv',na.strings = c('NA','?',""))
df3$Date <-as.Date(df2$Date,format = "%D")

str(df3)


f1 <- function(x){
  x*2
}

f2<- function (x,y){
  
  (x+y)/2
}

f1(24)

f2(10,20)

install.packages('ggplot2')

library(ggplot2)

df 
pl<- ggplot(df,aes(x=cyl))

pl<- pl + geom_bar()
pl
pl <- pl + geom_bar(aes(fill=factor(gear)))
pl


pl2 <- ggplot(df,aes(x=mpg))
pl2 <- pl2 + geom_histogram(binwidth = 3, fill='blue')
pl2

pl2 <- pl2 + ggtitle('Histogram of variable mpg') + theme_bw()
pl2


pl3 <- ggplot(df,aes(x=mpg,y=wt)) +geom_point()
pl3

pl3 <- ggplot(df,aes(x=mpg,y=wt)) +geom_point(aes(color=factor(cyl)))
pl3