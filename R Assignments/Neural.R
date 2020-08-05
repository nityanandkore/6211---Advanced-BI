install.pacakges('neuralnet')
library(caret)
library(pROC)
library(Hmisc)
library(neuralnet)

df <- read.csv('HEMQ.csv',na.strings=c('NA',''))

df$VALUE <- with(df,impute(VALUE,mean))
df$REASON <- with(df, impute(REASON,max))
df$JOB <- with(df,impute(JOB,max))
df$CLAGE <- with(df,impute(CLAGE,mean))
df$CLNO <- with (df, impute(CLNO,mean))

df <- na.omit(df)

df_processed <- df[,c(1:4,7:13)]
maxs <- apply(df_processed,2,max)
mins <- apply(df_processed,2,min)

df_processed <- as.data.frame(scale(df_processed,center=mins,scale=maxs-mins))

df_processed <- cbind(df_processed,df$REASON=='HomeImp')
names(df_processed)[12] <- 'REASONH'

df_processed <- cbind(df_processed,df$JOB=='Mgr')
df_processed <- cbind(df_processed,df$JOB=='Office')
df_processed <- cbind(df_processed,df$JOB=='ProfExe')
df_processed <- cbind(df_processed,df$JOB=='Sales')
df_processed <- cbind(df_processed,df$JOB=='Self')

names(df_processed)[13] <- 'JOBM'
names(df_processed)[14] <- 'JOBO'
names(df_processed)[15] <- 'JOBP'
names(df_processed)[16] <- 'JOBSa'
names(df_processed)[17] <- 'JOBSe'

set.seed(101)
trainIndex <- createDataPartition(df_processed$BAD,
                                  p=0.7,
                                  list=FALSE,
                                  times=1)
df.train <- df_processed[trainIndex,]
df.valid <- df_processed[-trainIndex,]

nn <- neuralnet(BAD~.,
                data=df.train,
                hidden = c(5,3),
                linear.output = FALSE)


plot(nn)

df.valid$BAD <- factor(df.valid$BAD)

prediction.net <- predict(nn,newdata=df.valid)
prediction.net <- ifelse(prediction.net>0.5,1,0)
confusionMatrix(as.factor(prediction.net),df.valid$BAD)

net.probabilities <- predict(nn, newdata=df.valid,type='prob')
nn.ROC <- roc(predictor=net.probabilities,
              response=df.valid$BAD,
              levels=levels(df.valid$BAD))
plot(nn.ROC)
nn.ROC$auc
