#install.packages('caret')
#install.packages('car')
#install.packages('e1071', dependencies=TRUE)
#install.packages('pROC')
#install.packages('dplyr')
#install.packages('Hmisc')

library(caret)
library(car)
library(pROC)
library(dplyr)
library(Hmisc)


getwd()

setwd("C:\\Users\\P2190101\\Desktop\\NK Personal\\NK Study\\UNCC\\6211 - Advanced BI\\Dataset")

getwd()

df <- read.csv('HEMQ.csv',na.strings = c('NA',''))

summary(df)
str(df)

df$BAD <- factor(df$BAD)

str(df)

vif(glm(formula = BAD~.,family=binomial(link='logit'),data=df))

df$VALUE <-with(df,impute(VALUE,mean))
df$REASON <-with(df,impute(REASON,max))
df$JOB <- with(df,impute(JOB,max))
df$CLAGE <-with(df,impute(CLAGE,mean))
df$CLNO <-with(df,impute(CLNO,mean))

pl <-ggplot(df,aes(x=YOJ))+geom_histogram()
df$YOJ <- log10(df$YOJ+1)

pl

pl <- ggplot(df,aes(x=NINQ))+geom_histogram()
pl

combine.NINQ <- function(x){
  if(is.na(x)){
    return(NA)
  }else if (x>0){
    return('High')
  }else{
    return('Low')
  }
}

df$NINQ <-sapply(df$NINQ,combine.NINQ)

table(df$NINQ)

summary(df)

set.seed(101)

trainIndex <- createDataPartition(df$BAD,
                                  p=0.7,
                                  list=FALSE,
                                  times = 1)

df.train<-df[trainIndex,]
df.valid <- df[-trainIndex,]


baseline.model <- train(BAD~.,
                        data=df.train,
                        method='glm',
                        family='binomial',
                        na.action=na.pass)

summary(baseline.model)

prediction <- predict(baseline.model,
                      newdata=df.valid)

df.valid.nonmissing <- na.omit(df.valid)

confusionMatrix(prediction,df.valid.nonmissing$BAD)

pred.probabilities <- predict(baseline.model,
                              newdata=df.valid,
                              type='prob')

regression.ROC <- roc(predictor=pred.probabilities$'1',
                      response = df.valid.nonmissing$BAD,
                      levels=levels(df.valid.nonmissing$BAD))

plot(regression.ROC)

regression.ROC$auc
