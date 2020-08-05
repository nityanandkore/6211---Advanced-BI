library(caret)
library(ggplot2)

setwd("C:\\Users\\P2190101\\Desktop\\NK Personal\\NK Study\\UNCC\\6211 - Advanced BI\\Part II - Predictive Modeling\\Advanced Predictive Modeling")   # Set working directory

df <- read.csv('signal.csv')

str(df)

df$output_signal <- factor(df$output_signal)

pl<- ggplot(df, aes(x=input_signal1,y=input_signal2))
pl <- pl+ geom_point(aes(color=output_signal))

print(pl)

set.seed(101)

trainIndex <- createDataPartition(df$output_signal,
                                  p=0.7,
                                  list=FALSE,
                                  times=1)
df.train<- df[trainIndex,]
df.test <- df[-trainIndex,]

trControl <- trainControl(method='cv',               # Cross Validation - 10 fold here
                          number=10,
                          search='grid',
                          )
svm_linear <- train(output_signal~.,
                    data=df.train,
                    method='svmLinear',
                    trControl=trControl,
                    preProcess=c('center','scale'))

print(svm_linear)

linear_pred <- predict(svm_linear,df.test)
confusionMatrix(linear_pred, df.test$output_signal)

svm_radial <- train(output_signal~.,
                    data=df.train,
                    method='svmRadial',
                    trControl=trControl,
                    preProcess=c('center','scale'))

print(svm_radial)

radial_pred <- predict(svm_radial,df.test)
confusionMatrix(radial_pred, df.test$output_signal)

grid_radial <- expand.grid(sigma=c(0,0.5,0.75,1,1.3,1.5), 
                           C=c(0,0.05,0.25,0.5,0.75,1))

svm_radial_tune <- train(output_signal~.,
                         data=df.train,
                         method='svmRadial',
                         trControl=trControl,
                         preProcess=c('center','scale'),
                         tuneGrid=grid_radial)
print(svm_radial_tune)

radial_tune_pred <- predict(svm_radial_tune,df.test)
confusionMatrix(radial_tune_pred,df.test$output_signal)
