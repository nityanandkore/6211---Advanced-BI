#################### SVM #############################################
library(caret)
library(ggplot2)


setwd("C:\\Users\\P2190101\\Desktop\\NK Personal\\NK Study\\UNCC\\6211 - Advanced BI\\Assignment 2") 

getwd()

dfNK <- read.csv('BostonHousing.csv',na.strings = c('NA',''))   # Read the csv file

summary(dfNK)
str(dfNK)

dfNK <- dfNK[-c(13)]

dfNK$CAT..MEDV <- factor(dfNK$CAT..MEDV)

str(dfNK)

set.seed(101)

trainIndex <- createDataPartition(dfNK$CAT..MEDV,
                                  p=0.7,
                                  list=FALSE,
                                  times=1)

dfNK.train <-dfNK[trainIndex,]
dfNK.test <- dfNK[-trainIndex,]


trControl <- trainControl(method='cv',               # Cross Validation - 10 fold here
                          number=10,
                          search='grid',
)
#######################################################################################################
svm_linear <- train(CAT..MEDV~.,
                    data=dfNK.train,
                    method='svmLinear',
                    trControl=trControl,
                    preProcess=c('center','scale'))

print(svm_linear)

linear_pred <- predict(svm_linear,dfNK.test)
confusionMatrix(linear_pred, dfNK.test$CAT..MEDV)
#######################################################################################################

svm_radial <- train(CAT..MEDV~.,
                    data=dfNK.train,
                    method='svmRadial',
                    trControl=trControl,
                    preProcess=c('center','scale'))

print(svm_radial)

radial_pred <- predict(svm_radial,dfNK.test)
confusionMatrix(radial_pred, dfNK.test$CAT..MEDV)

grid_radial <- expand.grid(sigma=c(0,0.5,0.75,1,1.3,1.5), 
                           C=c(0,0.05,0.25,0.5,0.75,1))

svm_radial_tune <- train(CAT..MEDV~.,
                         data=dfNK.train,
                         method='svmRadial',
                         trControl=trControl,
                         preProcess=c('center','scale'),
                         tuneGrid=grid_radial)
print(svm_radial_tune)

radial_tune_pred <- predict(svm_radial_tune,dfNK.test)
confusionMatrix(radial_tune_pred,dfNK.test$CAT..MEDV)
#########################################################################################################