
getwd()
setwd("C:\\Users\\P2190101\\Desktop\\NK Personal\\NK Study\\UNCC\\6211 - Advanced BI\\Assignment 3")   # Set working directory

# Read data
text <- read.csv('gastext.csv',stringsAsFactors = F)
# Convert column 3-15 to factor
text[,3:15]<-lapply(text[,3:15],factor)
str(text)

library(quanteda)

myCorpus <- corpus(text$Comment)
summary(myCorpus)                # How many Sentences , Types -> Unique words, Token -> Total words

#Step 1: Create Corpus
myDfm <- dfm(myCorpus)
topfeatures(myDfm)

#Step 2: Get Document Term ready
library(stopwords)
myDfm <- dfm(myCorpus,
             remove_punc = T,
             remove = c(stopwords("english")),
             stem = T)                           # Remove English STOP WORDS such as 'a''an','the' etc.

dim(myDfm)
topfeatures(myDfm,30)

###########################
####### Question 1 ########
###########################


tstat_freq <- textstat_frequency(myDfm)   # Simple frequency analysis - get unique list of Words
head(tstat_freq, 20)

# Visulize the most frequent terms
library(ggplot2)
myDfm %>% 
  textstat_frequency(n = 20) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal()

# Wordcloud
textplot_wordcloud(myDfm,max_words=200)


###########################
####### Question 2 ########
###########################

# Top 5 terms similar to "price"
term_sim <- textstat_simil(myDfm,
                           selection="price",
                           margin="feature",
                           method="correlation")
as.list(term_sim,n=5)

###########################
####### Question 3 ########
###########################


# Top 5 terms similar to "servic"
term_sim2 <- textstat_simil(myDfm,
                           selection="servic",
                           margin="feature",
                           method="correlation")
as.list(term_sim2,n=5)


###########################
####### Question 4 ########
###########################
library(topicmodels)
library(tidytext)

myDfm <- dfm_remove(myDfm, c('shower','point','productx','servic'))
myDfm <- as.matrix(myDfm)
myDfm <-myDfm[which(rowSums(myDfm)>0),]
myDfm <- as.dfm(myDfm)

myLda <- LDA(myDfm,k=4,control=list(seed=101))
myLda

# Term-topic probabilities
myLda_td <- tidy(myLda)
myLda_td

library(ggplot2)
library(dplyr)

top_terms <- myLda_td %>%
  group_by(topic) %>%
  top_n(8, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# View topic 8 terms in each topic
Lda_term<-as.matrix(terms(myLda,8))
View(Lda_term)

# Document-topic probabilities
ap_documents <- tidy(myLda, matrix = "gamma")
ap_documents

# View document-topic probabilities in a table
Lda_document<-as.data.frame(myLda@gamma)
View(Lda_document)


###########################
####### Question 5 ########
###########################
library(readxl)
library(caTools)
library(ROCR)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(caret)
library(pROC)
library(rpart)
library(rpart.plot)

text2 <- text[ -c(1,2) ]

# Data partition with the Caret package
# Set a random see so your "random" results are always same
set.seed(101)

trainIndex <- createDataPartition(text2$Target,
                                  p=0.75,
                                  list=FALSE,
                                  times=1)


# Create Training Data
text2.train <- text2[trainIndex,]

# Create Validation Data
text2.valid <-text2[-trainIndex,]


#######   TREE MODEL (Model 1)  ########

# Build a decision tree model
tree.model <- train(Target~.,
                    data=text2.train,
                    method="rpart",
                    na.action=na.pass)

# Display decision tree results
tree.model

# Display decision tree plot
prp(tree.model$finalModel,type=2,extra=106)

# Validate the model result

model1.prediction <- predict(tree.model,newdata=text2.valid,na.action = na.pass)
confusionMatrix(model1.prediction,text2.valid$Target)

tree.probabilities <- predict(tree.model,newdata=text2.valid,type='prob',na.action=na.pass)

tree.ROC <- roc(predictor=tree.probabilities$`1`,
                response=text2.valid$Target,
                levels=levels(text2.valid$Target))
plot(tree.ROC)

tree.ROC$auc

#############################################################################################################################
#######   non text and text mining (Model 2)  ########

set.seed(101)

docvars(myCorpus, "Target") <- text$Target

# We will first generate SVD columns based on the entire corpus

# Pre-process the training corpus
modelDfm <- dfm(myCorpus,
                remove_punc = T,
                remove=c(stopwords('english')),
                stem = T) 

#Remove highly infrequent words. Take only those which are at least 4 times and in two document
modelDfm <- dfm_trim(modelDfm,min_termfreq=4, min_docfreq = 2)

dim(modelDfm)

# Weight the predictiv DFM by tf-idf
modelDfm_tfidf <- dfm_tfidf(modelDfm)
dim(modelDfm_tfidf)

# Use SVD to reduce dimensionality
#install.packages('quanteda.textmodels')

library(quanteda.textmodels)
modelSvd <- textmodel_lsa(modelDfm_tfidf, nd=8) # We reduce dim to 8 column
head(modelSvd$docs)

# Add the author information as the first column
modelData <-cbind(docvars(myCorpus,"Target"),as.data.frame(modelSvd$docs))
colnames(modelData)[1] <- "Target"
head(modelData)

# Split the data into training & test

trainIndex <- createDataPartition(modelData$Target,
                                  p=0.7,
                                  list=FALSE,
                                  times=1)


# Create Training Data
modelData.train <- modelData[trainIndex,]

# Create Validation Data
modelData.valid <-modelData[-trainIndex,]


#######   TREE MODEL (Model 2)  ########

# Build a decision tree model
tree.model <- train(Target~.,
                    data=modelData.train,
                    method="rpart",
                    na.action=na.pass)

# Display decision tree results
tree.model

# Display decision tree plot
prp(tree.model$finalModel,type=2,extra=106)


# Validate the model result

model2.prediction <- predict(tree.model,newdata=modelData.valid,na.action = na.pass)
confusionMatrix(model2.prediction,modelData.valid$Target)

tree.probabilities <- predict(tree.model,newdata=modelData.valid,type='prob',na.action=na.pass)

tree.ROC <- roc(predictor=tree.probabilities$`1`,
                response=modelData.valid$Target,
                levels=levels(modelData.valid$Target))
plot(tree.ROC)

tree.ROC$auc
