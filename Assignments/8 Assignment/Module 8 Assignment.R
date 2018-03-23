setwd("c:\\Deeps\\R")
getwd()

library(tm)
library(RCurl)
library(wordcloud)
library(stringr)
library(dplyr)

fileText = read.delim2(file = "Assignments/8 Assignment/RestReviews.tsv",
                       quote = "",
                       stringsAsFactors = F)

CorpusRR <- VCorpus(VectorSource(fileText$Review)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stripWhitespace) %>%
  tm_map(stemDocument)


dtm = DocumentTermMatrix(CorpusRR)

dtm = removeSparseTerms(dtm, 0.99)

sort(colSums(as.matrix(dtm)), decreasing = T)


dataFrame = data.frame(as.matrix(dtm), stringsAsFactors = False)
dataFrame = cbind(dataFrame, fileText$Liked)

colnames(dtm)

library(e1071)
model = naiveBayes(dataFrame$`fileText$Liked` ~ ., data = dataFrame)
