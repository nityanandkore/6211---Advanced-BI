#################### RANDOM FOREST #############################################

#install.packages('ISLR')
#install.packages('randomForest')


library(ISLR)
library(randomForest)
library(caret)

setwd("C:\\Users\\P2190101\\Desktop\\NK Personal\\NK Study\\UNCC\\6211 - Advanced BI\\Assignment 2") 

getwd()

dfNK <- read.csv('BostonHousing.csv',na.strings = c('NA',''))   # Read the csv file

summary(dfNK)
str(dfNK)

dfNK <- dfNK[-c(13)]

head(dfNK)

dfNK$CAT..MEDV <- factor(dfNK$CAT..MEDV)


set.seed(101)

trainIndex <- createDataPartition(dfNK$CAT..MEDV,
                                  p=0.7,
                                  list=FALSE,
                                  times=1)

dfNK.train <-dfNK[trainIndex,]
dfNK.test <- dfNK[-trainIndex,]


rf_NK <- train(CAT..MEDV~.,                    # Default Model
                    data=dfNK.train,
                    method='rf',
                    metric='Accuracy',
                    ntree=100)            
print(rf_NK)


tuneGrid <- expand.grid(.mtry=c(1:12)) # mtry is number of variables used in each tree. Only max total independent variables

rf_mtry <- train(CAT..MEDV~.,
                 data=dfNK.train,
                 method='rf',
                 metric='Accuracy',
                 tuneGrid=tuneGrid,
                 importance=TRUE,
                 ntree=100)
print(rf_mtry)

varImp(rf_mtry)

prediction <- predict(rf_mtry,dfNK.test)
confusionMatrix(prediction,dfNK.test$CAT..MEDV)

