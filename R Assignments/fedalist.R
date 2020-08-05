papers <- read.csv('federalist.csv',stringsAsFactors = F)
dim(papers)

table(papers$Author)

papers <- papers[which(papers$Author=='HAMILTON'|
                         papers$Author=='MADISON'|
                   papers$Author=='UNKNOWN'),]
View(papers)

papers <- papers[order(papers$Author),]

papers$Text <- substring(papers$Text,40)

library(quanteda)

myCorpus <- corpus(papers$Text)
summary(myCorpus)

myDfm <- dfm(myCorpus)
dim(myDfm)

tstat_freq <- textstat_frequency(myDfm)
head(tstat_freq,20)

library(ggplot2)
myDfm %>%
  textstat_frequency(n=20) %>%
  ggplot(aes(x=reorder(feature, frequency),y=frequency))+
  geom_point()+
  labs(x=NULL,y='Frequency')+
  theme_minimal()

textplot_wordcloud(myDfm, max_words=200)

library(stopwords)
myDfm <- dfm(myCorpus,
             remove_punc=T,
             remove=c(stopwords('english')),
             stem=T)
dim(myDfm)

topfeatures(myDfm,30)

stopword1 <- c('may','one','two','can',
               'must','upon','might','shall')

myDfm <- dfm(myCorpus,
             remove_punc=T,
             remove=c(stopwords('english'),stopword1),
             stem=T)
dim(myDfm)
textplot_wordcloud(myDfm,max_words=200)

stopwords2 <- c('state','govern','power',
                'constitut','nation','peopl')
myDfm <- dfm_remove(myDfm,stopwords2)
topfeatures(myDfm,30)
dim(myDfm)

myDfm <- dfm_trim(myDfm, min_termfreq = 4, min_docfreq = 2)
dim(myDfm)

doc_dist <- textstat_dist(myDfm)
clust <- hclust(as.dist(doc_dist))
plot(clust,xlabt='Distance',ylab=NULL)

text_sim <- textstat_simil(myDfm,
                           selection='text71',
                           margin='document',
                           method='correlation')
as.list(text_sim,n=10)

term_sim <- textstat_simil(myDfm,
                           selection='commerc',
                           margin='feature',
                           method='correlation')
as.list(term_sim,n=10)

library(topicmodels)
library(tidytext)

myLda <- LDA(myDfm, k=8, control=list(seed=101))

myLda_td <- tidy(myLda)
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
  geom_bar(stat='identity',show.legend = FALSE)+
  facet_wrap(~topic,scales='free')+
  coord_flip()

Lda_term <- as.matrix(terms(myLda,8))
View(Lda_term)

ap_documents <- tidy(myLda,matrix='gamma')
ap_documents

Lda_document <- as.data.frame(myLda@gamma)
View(Lda_document)

dim(myDfm)

docvars(myCorpus,"ID") <- papers$ID
docvars(myCorpus,"Author") <- papers$Author
summary(myCorpus)

modelDfm <- dfm(myCorpus,
                remove_punc=T,
                remove=c(stopwords('english'),stopword1),
                stem=T)


modelDfm <- dfm_trim(modelDfm,min_termfreq = 4, min_docfreq = 2)
dim(modelDfm)

modelDfm_tfidf <- dfm_tfidf(modelDfm)
dim(modelDfm_tfidf)

library(quanteda.textmodels)
modelSvd <- textmodel_lsa(modelDfm_tfidf,nd=10)
head(modelSvd$docs)

modelData <- cbind(papers$Author,as.data.frame(modelSvd$docs))
colnames(modelData)[1] <- 'Author'
head(modelData)

trainData <- subset(modelData, Author=='HAMILTON'|Author=='MADISON')
testData <- subset(modelData,Author=='UNKNOWN')

str(trainData)
trainData$Author <- factor(trainData$Author)

regModel <- glm(formula=Author~.,
                family=binomial(link=logit),
                data=trainData)
pred <- predict(regModel,newdata=trainData,type='response')
pred.Result <- ifelse(pred>0.5,1,0)
print(table(pred.Result,trainData$Author))

unknownPred <- predict(regModel, newdata=testData,type='response')
unknownPred <- cbind(testData$Author,as.data.frame(unknownPred))
View(unknownPred)


