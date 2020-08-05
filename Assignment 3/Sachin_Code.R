setwd('C:/dsba_summer2020/datasets') 

papers <- read.csv('gastext.csv',stringsAsFactors = F)
dim(papers)


library(quanteda)

myCorpus <- corpus(papers$Comment)
summary(myCorpus)

myDfm <- dfm(myCorpus)
dim(myDfm)

#remove stop words and perrforrm stemming 
library(stopwords)
myDfm <- dfm(myCorpus,
             remove_punc=T,
             remove=c(stopwords('english')),
             stem=T)
dim(myDfm)

# Simple frequency analyses
tstat_freq <- textstat_frequency(myDfm)
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


# You can also explore other similarity measures, such as cosine, 
text_sim <- textstat_simil(myDfm, 
                           selection="price",
                           margin="feature",
                           method="correlation")
as.list(text_sim,n=5)

# You can also explore other similarity measures, such as cosine, 
text_sim2 <- textstat_simil(myDfm, 
                            selection="servic",
                            margin="feature",
                            method="correlation")
as.list(text_sim2,n=5)


###############################################################
#Part III: Topic Modeling

myDfm <- dfm_remove(myDfm,c('shower','point'))

myDfm<-as.matrix(myDfm)

myDfm <- myDfm[which(rowSums(myDfm)>0),]

myDfm<-as.dfm(myDfm)



library(topicmodels)
library(tidytext)

# Explore the option with 10 topics
# You can explore with varying k numbers
myLda <- LDA(myDfm,k=4,control=list(seed=101))
myLda

# Term-topic probabilities
myLda_td <- tidy(myLda)
myLda_td

# Visulize most common terms in each topic
library(ggplot2)
library(dplyr)

top_terms <- myLda_td %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


# View topic 8 terms in each topic
Lda_term<-as.matrix(terms(myLda,5))
View(Lda_term)

# Document-topic probabilities
ap_documents <- tidy(myLda, matrix = "gamma")
ap_documents

# View document-topic probabilities in a table
Lda_document<-as.data.frame(myLda@gamma)
View(Lda_document)

# Decision Tree Model


# A table showing the overall structure of the dataset, 
# including variable names, data types, and whether the variables 
# will be used in your analyses.

install.packages('caret')
install.packages('car')
install.packages('pROC')
install.packages('Hmisc')
library(caret)
library(car)
library(pROC)
library(dplyr)
library(Hmisc)

papers2 <- read.csv('gastext.csv',stringsAsFactors = F)
#1. A table showing the overall structure of the dataset, including variable names, data types

papers2[,3:15]<-lapply(papers2[,3:15],factor)
papers2 = subset(papers2,select = -c(Comment))
View(papers2) 
summary(papers2)  

# Data partition with the Caret package
# Set a random see so your "random" results are the same as me (optional)
set.seed(101)
trainIndex <- createDataPartition(papers2$Target,
                                  p=0.7,
                                  list=FALSE,
                                  times=1)


# Create Training Data
papers2.train <- papers2[trainIndex,]

# Create Validation Data
papers2.valid <-papers2[-trainIndex,]


# Work on decision Tree

install.packages('rpart')
install.packages('rpart.plot')



# Load needed packages
library(caret)
library(pROC)
library(rpart)
library(rpart.plot)


# Build a decision tree model
tree.model <- train(Target~.,
                    data=papers2.train,
                    method="rpart",
                    na.action=na.pass)

# Display decision tree results
tree.model

# Display decision tree plot
prp(tree.model$finalModel,type=2,extra=106)




#Evaluation model performance using the validation dataset

#Criteria 1: the confusion matrix

prediction <- predict(tree.model,newdata=papers2.valid,na.action = na.pass)
confusionMatrix(prediction,papers2.valid$Target)

#Criteria 2: the ROC curve and area under the curve
tree.probabilities <- predict(tree.model,newdata=papers2.valid,type='prob',na.action=na.pass)

tree.ROC <- roc(predictor=tree.probabilities$`1`,
                response=papers2.valid$Target,
                levels=levels(papers2.valid$Target))
plot(tree.ROC)

tree.ROC$auc