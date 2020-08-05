#=====================================================================================================
#################  DECISION TREE #####################################################################
#=====================================================================================================

#install.packages('caret')
#install.packages('car')
#install.packages('e1071', dependencies=TRUE)
#install.packages('pROC')
#install.packages('dplyr')
#install.packages('Hmisc')

library(caret)               # Load all required package
library(car)
library(pROC)
library(dplyr)
library(Hmisc)
library(rpart)
library(rpart.plot)


getwd()

setwd("C:\\Users\\P2190101\\Desktop\\NK Personal\\NK Study\\UNCC\\6211 - Advanced BI\\Assignment 1")   # Set working directory

getwd()

dfNK <- read.csv('inq2015.csv',na.strings = c('NA',''))   # Read the csv file

summary(dfNK)                                         # Summary of new dataset
str(dfNK)                                             # Structure of new dataset

ExcludeList <- names(dfNK) %in% c("ACADEMIC_INTEREST_1", "ACADEMIC_INTEREST_2", "CONTACT_DATE","CONTACT_CODE1","IRSCHOOL","LEVEL_YEAR") # Exclude variables which are not required.

dfNK1 <- dfNK[!ExcludeList]                     # New dataset with excluded list of columns

dfNK1$Enroll <- factor(dfNK1$Enroll)            # Convert Int variables to factor
dfNK1$sex <- factor(dfNK1$sex)                  # Convert Int variables to factor
dfNK1$Instate <- factor(dfNK1$Instate)          # Convert Int variables to factor
dfNK1$premiere <- factor(dfNK1$premiere)        # Convert Int variables to factor
dfNK1$stucell <- factor(dfNK1$stucell)          # Convert Int variables to factor
dfNK1$mailq <- factor(dfNK1$mailq)              # Convert Int variables to factor
dfNK1$telecq <- factor(dfNK1$telecq)            # Convert Int variables to factor

set.seed(101)

trainIndex <- createDataPartition(dfNK1$Enroll,        # Create an index to partition the data for training and validation
                                  p=0.7,
                                  list=FALSE,
                                  times=1)
dfNK1.train <- dfNK1[trainIndex,]                      # Create Training Data
dfNK1.valid <-dfNK1[-trainIndex,]                      # Create Validation Data

tree.modelNK <- train(Enroll~.,
                      data=dfNK1.train,
                      method='rpart',
                      na.action=na.pass)
tree.modelNK

prp(tree.modelNK$finalModel,type=2,extra=106)

prediction.tree <- predict(tree.modelNK,
                           newdata = dfNK1.valid,
                           na.action=na.pass)

confusionMatrix(prediction.tree,dfNK1.valid$Enroll)

tree.probabliities <- predict(tree.modelNK,
                              newdata=dfNK1.valid,
                              type='prob',
                              na.action=na.pass)
tree.ROC <-roc(predictor=tree.probabliities$`1`,
               response = dfNK1.valid$Enroll,
               levels=levels(dfNK1.valid$Enroll))
plot(tree.ROC)

tree.ROC$auc

