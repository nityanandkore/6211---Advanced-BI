install.packages('ISLR')
install.packages('randomForest')


library(ISLR)
library(randomForest)
library(caret)

df <- College
summary(df)

subset(df,Grad.Rate>100)                     # Find records where the 

df['Cazenovia College','Grad.Rate'] <-100

set.seed(101)

trainIndex <- createDataPartition(df$Private,
                                  p=0.7,
                                  list=FALSE,
                                  times=1)

df.train <-df[trainIndex,]
df.test <- df[-trainIndex,]

rf_default <- train(Private~.,                    # Default Model
                    data=df.train,
                    method='rf',
                    metric='Accuracy',
                    ntree=100)            
print(rf_default)


tuneGrid <- expand.grid(.mtry=c(1:17)) # mtry is number of variables used in each tree. Only max total independent variables

rf_mtry <- train(Private~.,
                 data=df.train,
                 method='rf',
                 metric='Accuracy',
                 tuneGrid=tuneGrid,
                 importance=TRUE,
                 ntree=100)
print(rf_mtry)

varImp(rf_mtry)

prediction <- predict(rf_mtry,df.test)
confusionMatrix(prediction,df.test$Private)
