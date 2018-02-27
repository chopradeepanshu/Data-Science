setwd("c:\\Deeps\\R")
getwd()

library(twitteR)
library(RCurl)
library(tm)
library(wordcloud)
library(stringr)
library(dplyr)
library(SnowballC)
library(glue)


files <- list.files("Data/PresidentSpeech-1989-2017")
fileName <- glue("Data/PresidentSpeech-1989-2017/", files[1], sep = "")
# get rid of any sneaky trailing spaces
fileName <- trimws(fileName)
# read in the new file
fileText <- glue(read_file(fileName))
# remove any dollar signs (they're special characters in R)
fileText <- gsub("\\$", "", fileText) 

#This will create a complete object of the file with MetaData
CorpusFileText = Corpus(VectorSource(fileText))

CorpusFileText = CorpusFileText %>%
tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("english")) %>% # StopWords are the words in English like - I, the, a, this etc. which are not diagonistic, hence we remove them
  tm_map(stripWhitespace) %>%
  tm_map(stemDocument) # Will String down the words to the core - Running, run, runs to run

# How many times each word occured within our document , we need to create DocumentTermMatrix.
dtm = DocumentTermMatrix(CorpusFileText)
dtm
sort(colSums(as.matrix(dtm)), decreasing = T)

#We will remove any element that doesn't appear in atleast 1% of the entries (or documents).
dtm = removeSparseTerms(dtm, 0.99)
dtm

# This will give the Frequency of Words in Descending Order.
wordFrequency = sort(colSums(as.matrix(dtm)), decreasing = T)
head(wordFrequency)


# Now we will calculate the Absolute Frequency and Relative Frequency.
#Absolute Frequency - Count of the word.
#Relative Frequency - Count of the word w.r.t document.
# Relative Frequency is very important coz, with absolute frequency you will only get the count, but this will give you the count w.r.t lenght of document.

frequencyTable = data.frame(word = names(wordFrequency), 
                            absoluteFrequency = wordFrequency,
                            relativeFrequency = wordFrequency/length(wordFrequency))

View(frequencyTable)
