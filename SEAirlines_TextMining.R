dev.off() # Clear the graph window
cat('\014')  # Clear the console
rm(list=ls()) # Clear all user objects from the environment

# Import dataset

# Set working directory, change it to your work folder
setwd("~/Google Drive/ADS Master/2019 Fall/IST687/Final_Project")
# Adding jsonlite to our R library
library(jsonlite)
# Creating a dataframe from the json data.
df <- jsonlite::fromJSON("fall2019-survey-M03.json")
# Structure of the dataset
str(df)
# Summary of the dataset
summary(df)
# Viewing the dataset
View(df)


# Text Mining -- Sentiment Analysis

install.packages("tm")
library(tm)
install.packages("quanteda")
library(quanteda)
install.packages("tidyverse")
library(tidyverse)
install.packages("sentimentr")
library(sentimentr)

# Import diictories for positive and negative words
posWords <- scan("positive-words.txt",character(0),sep = "\n")
posWords <- posWords[-1:-34]
negWords <- scan("negative-words.txt",character(0),sep = "\n")
negWords <- negWords[-1:-34]

# Get the text from the dataset and create a word corpus 
dfText <- df$freeText[-which(is.na(df$freeText))]
words.vec <- VectorSource(dfText)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))

# Create the term-document matrix
tdm <- TermDocumentMatrix(words.corpus)
inspect(tdm)

# Count positive and negative word numbers
m <- as.matrix(tdm)
wordCounts <- rowSums(m)
wordCounts <- sort(wordCounts, decreasing = TRUE)
matchedP <- match(names(wordCounts), posWords, nomatch = 0)
matchedN <- match(names(wordCounts), negWords, nomatch = 0)
wordsP <- wordCounts[which(matchedP != 0)]
wordsN <- wordCounts[which(matchedN != 0)]

# Calculate the percentage of positive and negative words
totWordsNum <- sum(wordCounts)
posWordsNum <- sum(wordsP)
ratioP <- posWordsNum/totWordsNum
ratioP
negWordsNum <- sum(wordsN)
ratioN <- negWordsNum/totWordsNum
ratioN

# Analysis for each record:
sentence_freetext <- get_sentences(df$freeText) # TO get sentences from the string in each freeText 
sentence_freetext 
sentiment_for_all <- sentence_freetext %>% sentiment_by(by = NULL) # to get average sentiment for the text 
View(sentiment_for_all)
sentence_freetext %>% sentiment_by(by = NULL) %>% highlight() # html file highlighting the text into 2 colors based on sentiment and the avg sentiment for that particular text 

# Word Cloud:
toParas <- Corpus(words.vec)
toParas <- corpus(toParas)
paras <- corpus_reshape(toParas, to="paragraphs")
dfTextClean <- dfm(paras, stem=TRUE, remove_punct=TRUE, remove=stopwords("english"))
dfTextClean <- dfm_trim(dfTextClean, min_termfreq=3)
textplot_wordcloud(dfTextClean, color = rainbow(8))



