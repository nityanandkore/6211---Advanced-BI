#=====================================================================================================
#################  REGRESSION MODEL ##################################################################
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


vif(glm(formula=Enroll~.,family = binomial(link='logit'),data=dfNK1))   # Using VIF to test Multicollinearity

# Impute columns
dfNK1$satscore <-with(dfNK1,impute(satscore,mean))     # Quantitative - Continuous Variable
dfNK1$avg_income <-with(dfNK1,impute(avg_income,mean)) # Quantitative - Continuous Variable
dfNK1$distance <-with(dfNK1,impute(distance,mean))     # Quantitative - Continuous Variable
dfNK1$sex <-with(dfNK1,impute(sex,max))                # Categorical - Ordinal variable
dfNK1$telecq<-with(dfNK1,impute(telecq,max))           # Categorical - Ordinal variable
dfNK1$distance <- log10(dfNK1$distance+1)              # Transforming distance to reduce skew and for proper distribution
dfNK1$avg_income <- log10(dfNK1$avg_income+1)          # Transforming avg_income to reduce skew and for proper distribution

summary(dfNK1)                                         # Summary of new dataset
str(dfNK1)                                             # Structure of new dataset
head(dfNK1)                                            # Top rows of new dataset               

set.seed(101)

trainIndex <- createDataPartition(dfNK1$Enroll,        # Create an index to partition the data for training and validation
                                  p=0.7,
                                  list=FALSE,
                                  times=1)
dfNK1.train <- dfNK1[trainIndex,]                      # Create Training Data
dfNK1.valid <-dfNK1[-trainIndex,]                      # Create Validation Data


regression.modelNK <- train(Enroll~.,                      # Create baseline model with the training Dataset
                        data=dfNK1.train,
                        method='glm',
                        family='binomial',
                        na.action=na.pass)

summary(regression.modelNK)                                 # Summary of baseline model 


#Evaluation model performance using the validation dataset

#Criteria 1: the confusion matrix
prediction <- predict(regression.modelNK,newdata=dfNK1.valid)

#Need to remove missing values from the validation dataset for evaluation
dfNK1.valid.nonmissing <- na.omit(dfNK1.valid)
confusionMatrix(prediction,dfNK1.valid.nonmissing$Enroll)

#Criteria 2: the ROC curve and area under the curve
pred.probabilities <- predict(regression.modelNK,newdata=dfNK1.valid,type='prob')

regression.ROC <- roc(predictor=pred.probabilities$`1`,
                      response=dfNK1.valid.nonmissing$Enroll ,
                      levels=levels(dfNK1.valid.nonmissing$Enroll))
plot(regression.ROC)
regression.ROC$auc
