library(caret)               # Load all required package
library(car)
library(pROC)

getwd()

setwd("C:\\Users\\P2190101\\Desktop\\NK Personal\\NK Study\\UNCC\\6211 - Advanced BI\\Part II - Predictive Modeling\\Dimension Reduction and PCA")   # Set working directory

getwd()

dfNK <- read.csv('claim.csv',na.strings = c('NA','None','unknown',''))   # Read the csv file
dfNK <- na.omit(dfNK)                                                    # Omit NA data

str(dfNK)

dfNK$CLAIM_IND <- factor(dfNK$CLAIM_IND)    # Change dependant var from Int to Binary

set.seed(101)

trainIndex <- createDataPartition(dfNK$CLAIM_IND,
                                  p=0.7,
                                  list=FALSE,
                                  times=1)
dfNK.train <- dfNK[trainIndex,]
dfNK.valid <- dfNK[-trainIndex,]

model1 <- train (CLAIM_IND ~.,
                 data=dfNK.train,
                 method='glm',
                 family='binomial')
summary(model1)

m1.prediction <- predict(model1,dfNK.valid)

confusionMatrix(m1.prediction,dfNK.valid$CLAIM_IND)

######### Iteration 1: Apply PCA for numerical columns #####################

df_num <- dfNK[,c(1:6,11,13,15,17:18)]
pca <- prcomp(df_num,center=TRUE, scale.=TRUE)
print(pca)

plot(pca,type='l')

summary(pca)

pred_pca <- predict(pca,newdata = df_num)

df2 <- cbind.data.frame(dfNK[,c(7:10,12,14,16,19:20)], pred_pca[,c(1:4)]) # Combine other categorical variable with PCAs

head(df2)

set.seed(101)

trainIndex <- createDataPartition(df2$CLAIM_IND,
                                  p=0.7,
                                  list=FALSE,
                                  times=1)
df2.train <- df2[trainIndex,]
df2.valid <- df2[-trainIndex,]

model2 <- train (CLAIM_IND ~.,
                 data=df2.train,
                 method='glm',
                 family='binomial')
summary(model2)

cor(df_num)

###################### Iteration 2: Apply PCA on columns which are imp in terms of Finance
###################### so to predict whether claim will be filed

df_num2 <- dfNK[,c(4:6,13,18)]

pca2<- prcomp(df_num2,center=TRUE,scale.=TRUE)

summary(pca2)

pred_pca2 <- predict(pca2,newdata = df_num2)

df3<- cbind.data.frame(dfNK[,c(1:3,7:12,14:17,19:20)],pred_pca2[,1]) # Combine 
names(df3)[16]='PC1'


set.seed(101)

trainIndex <- createDataPartition(df3$CLAIM_IND,
                                  p=0.7,
                                  list=FALSE,
                                  times=1)
df3.train <- df3[trainIndex,]
df3.valid <- df3[-trainIndex,]

model3 <- train (CLAIM_IND ~.,
                 data=df3.train,
                 method='glm',
                 family='binomial')
summary(model3)
print(pca2)
