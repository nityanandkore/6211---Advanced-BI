#install.packages('quanteda')
library(quanteda)

getwd()

setwd("C:\\Users\\P2190101\\Desktop\\NK Personal\\NK Study\\UNCC\\6211 - Advanced BI\\Part IV - Text Mining")   # Set working directory


text <- read.csv('simple_text.csv',stringsAsFactors = F)
str(text)

myCorpus <- corpus(text$Text)

summary(myCorpus)

myDfm <- dfm(myCorpus)
topfeatures(myDfm)

myTokens <- tokens(myCorpus)
bigram <- tokens_ngrams(myTokens,n=1:2)
myDfm_bigram <- dfm(bigram)



