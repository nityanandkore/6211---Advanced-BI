library(caret)
library(car)
library(pROC)
library(dplyr)
library(Hmisc)
library(lubridate)
library(rpart)
library(rpart.plot)

df <- read.csv('Contact+Feedback+Q3.csv',na.strings=c('NA',''))
View(df)
summary(df)

# Plotting a bar plot to understand number of categories and missing categories
pl1 <- ggplot(df, aes(x=f_Survey_Name__c))+geom_bar()+ theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
pl1

pl2 <- ggplot(df, aes(x=c_Military_Spouse_Caregiver__c))+geom_bar()+ theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
pl2

# Creating the time in service column
df$c_Date_of_Service_Entry__c <- date(mdy_hm(df$c_Date_of_Service_Entry__c))
df$c_Date_of_Service_EntryNew__c <- date(mdy_hm(df$c_Date_of_Service_EntryNew__c))
df$c_Date_of_Separation__c <- date(mdy_hm(df$c_Date_of_Separation__c))
df$c_Date_of_SeparationNew__c <- date(mdy_hm(df$c_Date_of_SeparationNew__c))

# Creating time in service column
df <- df %>% 
  mutate(time_in_service = c_Date_of_SeparationNew__c - c_Date_of_Service_EntryNew__c)
View(df)
str(df)

# Transforming to binary variable
combine.Survey <- function(x){
  if(is.na(x)){
    return(0)
  }else if (is.null(x)){
    return(0)
  }else{
    return(1)
  }
}

df$Survey <- sapply(df$f_Survey_Name__c,combine.Survey)
table(df$Survey)

# Transforming to binary variable
df$Caregiver <- ifelse(df$c_Military_Spouse_Caregiver__c==0, 0, 1)
table(df$Caregiver)

# Checking the distribution of the categorical variables
table(df$c_Enrolled_in_School__c)
table(df$c_Service_Rank__c)
table(df$c_Gender__c)

#Creating subset for modeling
dfmod <- subset(df, select = c(
  Survey,
  c_Gender__c,
  c_Service_Rank__c,
  time_in_service,
  Caregiver,
  c_Enrolled_in_School__c
))
View(dfmod)

# converting time in service to integer
dfmod$time_in_service <- as.integer(dfmod$time_in_service)
# removing negative values in time in service
dfmod <- dfmod %>% 
  filter(time_in_service > 0)

# Converting to factors
dfmod$Survey <- factor(dfmod$Survey)
dfmod$c_Gender__c <- factor(dfmod$c_Gender__c)
dfmod$c_Service_Rank__c <- factor(dfmod$c_Service_Rank__c)
dfmod$Caregiver <- factor(dfmod$Caregiver)
dfmod$c_Enrolled_in_School__c <- factor(dfmod$c_Enrolled_in_School__c)

# Imputing c_Service_Rank with 376 missing values
dfmod$c_Service_Rank__c <- with(dfmod, impute(c_Service_Rank__c,max))

# checking mulitcollinearity
vif(glm(formula=Survey~.,family=binomial(link='logit'),data=dfmod))

# Modeling
set.seed(101)

trainIndex <- createDataPartition(dfmod$Survey,
                                  p=0.7,
                                  list=FALSE,
                                  times=1)

DFMK.train<- dfmod[trainIndex,]
DFMK.valid<- dfmod[-trainIndex,]

# LOGISTIC REGRESSION

regressionMK.model<- train(Survey~.,
                           data=DFMK.train,
                           method='glm',
                           family='binomial',
                           na.action = na.pass)

summary(regressionMK.model)

predictionregressionMK <- predict(regressionMK.model,
                                  newdata = DFMK.valid)

DFMK.valid.nonmissing <- na.omit(DFMK.valid)

# Confusion Matrix
confusionMatrix(predictionregressionMK,DFMK.valid.nonmissing$Survey)

# ROC curves and Area Under ROC
pred.probabilitiesregressMK <- predict(regressionMK.model,
                                       newdata=DFMK.valid,
                                       type='prob')

View(pred.probabilitiesregressMK)

regression.ROCMK <- roc(predictor=pred.probabilitiesregressMK$`1`,
                        response = DFMK.valid.nonmissing$Survey,
                        levels = levels(DFMK.valid.nonmissing$Survey))

plot(regression.ROCMK)
regression.ROCMK$auc


