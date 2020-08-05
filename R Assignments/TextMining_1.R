getwd()

setwd("C:\\Users\\P2190101\\Desktop\\NK Personal\\NK Study\\UNCC\\6211 - Advanced BI\\Part IV - Text Mining")   # Set working directory
papers <- read.csv('federalist.csv',stringsAsFactors = F)

dim(papers)

table(papers$Author)

papers <- papers[which(papers$Author=='HAMILTON' |
                         papers$Author=='MADISON' |
                         papers$Author=='UNKNOWN'),] #Keep only for Hamilton, Madison and UNKWON

papers <- papers[order(papers$Author),]  # Order by Writer

papers$Text <- substring(papers$Text,40) # Remove common/constant string from each text


# Initial Text Mining

library(quanteda)

#Step 1: Create Corpus

myCorpus <- corpus(papers$Text)
summary(myCorpus)  # How many Sentences , Types -> Unique words, Token -> Total words

#Step 2: Get Document Term ready

myDfm <- dfm(myCorpus)

dim(myDfm) # Dimensionality of data feature matrix

tstat_freq <- textstat_frequency(myDfm) # get unique list of 

head(tstat_freq,20)


library(ggplot2)

myDfm %>%
  textstat_frequency(n=20) %>%
  ggplot(aes(x=reorder(feature,frequency),y=frequency)) +
  geom_point()+
  labs(x=NULL,y='Frequency') +
  theme_minimal()

textplot_wordcloud(myDfm,max_words  =200)

# Remove unnecessary stop words and punctuation

library(stopwords)

myDfm <- dfm(myCorpus,
             remove_punc=T,
             remove=c(stopwords('english')),
             stem=T) #Stemming
dim(myDfm)   # Half of the words are gone

topfeatures(myDfm,30)

myStopWords1<- c('can','may','one','two','must','upon','might','shall')

myDfm <- dfm(myCorpus,
             remove_punc=T,
             remove=c(stopwords('english'),myStopWords1),
             stem=T) #Stemming
dim(myDfm)   # Half of the words are gone

topfeatures(myDfm,30)

textplot_wordcloud(myDfm,max_words = 200)

myStopWords2 <- c('state','govern','power','constitut','nation','peopl')

myDfm <- dfm_remove(myDfm,myStopWords2)

dim(myDfm)   # Half of the words are gone

topfeatures(myDfm,30)

textplot_wordcloud(myDfm,max_words = 200)

#Remove highly infrequent words. Take only those which are at least 4 times and in tow document

myDfm <- dfm_trim(myDfm,min_termfreq = 4, min_docfreq = 2)
dim(myDfm)

doc_dist <- textstat_dist(myDfm)
clust<- hclust(as.dist(doc_dist))
plot(clust,xlab='Distance',ylab=NULL)


text_sim <- textstat_simil(myDfm,
                           selection = 'text71',
                           margin = 'document',
                           method='correlation')
as.list(text_sim,n=10)

term_sim <- textstat_simil(myDfm,
                           selection = 'commerc',
                           margin='feature',
                           method='correlation')

as.list(term_sim,n=10)

# Topic Modeling

#install.packages("topicmodels")
library(topicmodels)
#install.packages("tidytext")
library(tidytext)

myLda <- LDA(myDfm, k=8, control=list(seed=101)) # K=8 number of topics

myLda_td <- tidy(myLda) # find the probability of relation between topics
myLda_td

library(ggplot2)
library(dplyr)


top_terms <- myLda_td %>%
  group_by(topic) %>%
  top_n(8,beta) %>%
  ungroup() %>%
  arrange(topic,-beta)

top_terms %>%
  mutate(term=reorder(term,beta)) %>%
  ggplot(aes(term,beta,fill=factor(topic)))+
  geom_bar(stat='identity',show.legend = FALSE) +
  facet_wrap(~topic,scales='free') +
  coord_flip()

# View topic 8 terms in each topic
Lda_term <-as.matrix(terms(myLda,8))

Lda_term
# Document-topic probabilities
ap_documents <- tidy(myLda,matrix='gamma')
ap_documents

# View document-topic probabilities in a table
Lda_document <- as.data.frame(myLda@gamma)
Lda_document

dim(myDfm)

docvars(myCorpus,"ID") <- papers$ID
docvars(myCorpus,'Autor') <- papers$Author

summary(myCorpus)

modelDfm <- dfm(myCorpus,
                remove_punc=T,
                remove = c(stopwords('english'),myStopWords1),
                stem=T)

modelDfm <- dfm_trim(modelDfm,min_termfreq = 4,min_docfreq = 2)

dim(modelDfm)

# 

modelDfm_tfidf <- dfm_tfidf(modelDfm)

dim(modelDfm_tfidf)

# Use SVD to reduce dimensionality
#install.packages('quanteda.textmodels')
library(quanteda.textmodels)

modelSvd <- textmodel_lsa(modelDfm_tfidf,nd=10) # We reduce dim to 10 column

head(modelSvd$docs)

modelData <- cbind(papers$Author,as.data.frame(modelSvd$docs))

colnames(modelData)[1] <-'Author'

head(modelData)

trainData <- subset(modelData, Author=='HAMILTON'| Author=='MADISON')
testData <- subset(modelData, Author=='UNKNOWN')

str(trainData)

trainData$Author <- factor(trainData$Author)

regModel <- glm(formula = Author~.,
                family=binomial(link=logit),
                data=trainData)

pred <- predict(regModel, newdata = trainData, type='response')
pred.Result <- ifelse(pred>0.5,1,0)

print(table(pred.Result,trainData$Author))

unknownPred <- predict(regModel, newdata = testData, type='response')

unknownPred <- cbind(testData$Author,as.data.frame(unknownPred))

unknownPred

