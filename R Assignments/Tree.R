#install.packages('rpart')
#install.packages('rpart.plot')

library(rpart)
library(rpart.plot)
library(pROC)
library(caret)

getwd()

setwd("C:\\Users\\P2190101\\Desktop\\NK Personal\\NK Study\\UNCC\\6211 - Advanced BI\\Dataset")

getwd()

dfNK <- read.csv("HEMQ.csv",na.strings = c('NA',''))

summary(dfNK)

dfNK$BAD <- factor(dfNK$BAD)

set.seed(101)

trainIndex <- createDataPartition(dfNK$BAD,
                                  p=0.7,
                                  list=FALSE,
                                  times=1)

dfNK.train <- dfNK[trainIndex,]
dfNK.valid <- dfNK[-trainIndex,]

tree.modelNK <- train(BAD~.,
                      data=dfNK.train,
                      method='rpart',
                      na.action=na.pass)
tree.modelNK

prp(tree.modelNK$finalModel,type=2,extra=106)

prediction.tree <- predict(tree.modelNK,
                           newdata = dfNK.valid,
                           na.action=na.pass)
confusionMatrix(prediction.tree,dfNK.valid$BAD)



tree.probabliities <- predict(tree.modelNK,
                              newdata=dfNK.valid,
                              type='prob',
                              na.action=na.pass)
tree.ROC <-roc(predictor=tree.probabliities$`1`,
               response = dfNK.valid$BAD,
               levels=levels(dfNK.valid$BAD))
plot(tree.ROC)

tree.ROC$auc
