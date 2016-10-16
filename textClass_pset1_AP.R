# Text as Data
# Problem set 1
# Anton Prokopyev

# Clear workspace
rm(list = ls())

# Check packages
pkgs <-c('rjson','bitops','RCurl','e1071','syuzhet','reshape2','twitteR','ROAuth','httr','plyr','stringr','ggplot2','plotly', 'tm','SnowballC','quanteda','stm')
for(p in pkgs) if(p %in% rownames(installed.packages()) == FALSE) {install.packages(p)}
for(p in pkgs) suppressPackageStartupMessages(library(p, quietly=TRUE, character.only=TRUE))

library(devtools)
install_github('sentiment140', 'okugami79', force = TRUE)
library(sentiment)

# Load the data
getwd()
setwd("~/Desktop/GoogleDrive/DATA/textasdata")
#bostonDF <- read.csv("Tweets_sent_from__NotifyBoston_from_March_2014_-_March_2015.csv")
#tweetDF = bostonDF[,c(3)]
tweetDF <- read.csv("BostonTweets.csv")

# Convert to corpus
library(tm)
bostonVec <- VectorSource(tweetDF)
bostonCorpus <- Corpus(bostonVec)

# Cleaning 101
bostonCorpus <- tm_map(bostonCorpus, removeWords, stopwords("english"))

# Stemming
library(SnowballC)
bostonCorpus <- tm_map(bostonCorpus, stemDocument)

# Remove white space
bostonCorpus <- tm_map(bostonCorpus, stripWhitespace)
#inspect(bostonCorpus)

# Term document matrix 
TDM <- TermDocumentMatrix(bostonCorpus)

findFreqTerms(TDM, 50)

#import libraries to work with
library(plyr)
library(stringr)
library(e1071)    

#load up word polarity list and format it
afinn_list <- read.delim(file='AFINN/AFINN-111.txt', header=FALSE, stringsAsFactors=FALSE)
names(afinn_list) <- c('word', 'score')
afinn_list$word <- tolower(afinn_list$word)    

#categorize words as very negative to very positive
vNegTerms <- afinn_list$word[afinn_list$score==-5 | afinn_list$score==-4]
negTerms <- c(afinn_list$word[afinn_list$score==-3 | afinn_list$score==-2 | afinn_list$score==-1])
posTerms <- c(afinn_list$word[afinn_list$score==3 | afinn_list$score==2 | afinn_list$score==1])
vPosTerms <- c(afinn_list$word[afinn_list$score==5 | afinn_list$score==4])

#function to calculate number of words in each category within a sentence
sentimentScore <- function(sentences, vNegTerms, negTerms, posTerms, vPosTerms){
  final_scores <- matrix('', 0, 5)
  scores <- laply(sentences, function(sentence, vNegTerms, negTerms, posTerms, vPosTerms){
    initial_sentence <- sentence
    #remove unnecessary characters and split up by word 
    sentence <- gsub('[[:punct:]]', '', sentence)
    sentence <- gsub('[[:cntrl:]]', '', sentence)
    sentence <- gsub('\\d+', '', sentence)
    sentence <- tolower(sentence)
    wordList <- str_split(sentence, '\\s+')
    words <- unlist(wordList)
    #build vector with matches between sentence and each category
    vPosMatches <- match(words, vPosTerms)
    posMatches <- match(words, posTerms)
    vNegMatches <- match(words, vNegTerms)
    negMatches <- match(words, negTerms)
    #sum up number of words in each category
    vPosMatches <- sum(!is.na(vPosMatches))
    posMatches <- sum(!is.na(posMatches))
    vNegMatches <- sum(!is.na(vNegMatches))
    negMatches <- sum(!is.na(negMatches))
    score <- c(vNegMatches, negMatches, posMatches, vPosMatches)
    #add row to scores table
    newrow <- c(initial_sentence, score)
    final_scores <- rbind(final_scores, newrow)
    return(final_scores)
  }, vNegTerms, negTerms, posTerms, vPosTerms)
  return(scores)
}   