setwd("c:\\Deeps\\R")
getwd()
library(twitteR)
library(tidyverse)
library(tidytext)
library(glue)
library(stringr)
library(plyr)
library(wordcloud)
library(tm)
pos.words = scan("Data/positiveWords.txt", what = 'character')
neg.words = scan("Data/negativeWords.txt", what = 'character')


score.sentiment = function(tweets, pos.words, neg.words, .progress = 'none') {
  
  scores = laply(tweets, function(tweet, pos.words, neg.words) {
    
    tweet = gsub('[[:punct:]]', '', tweet) # removes punctuation 
    tweet = gsub('[[:cntrl:]]', '', tweet) # removes control characters
    tweet = gsub('\\d+', '', tweet) # removes numbers
    tweet=str_replace_all(tweet,"[^[:graph:]]", " ") 
    
    #tweet = tolower(tweet) # makes all letters lowercase
    
    word.list = str_split(tweet, '\\s+') # splits the tweets by word in a list
    
    words = unlist(word.list) # turns the list into vector
    
    pos.matches = match(words, pos.words) ## returns matching 
    #values for words from list 
    neg.matches = match(words, neg.words)
    
    pos.matches = !is.na(pos.matches) ## converts matching values to true of false
    neg.matches = !is.na(neg.matches)
    
    score = sum(pos.matches) - sum(neg.matches) # true and false are 
    #treated as 1 and 0 so they can be added
    
    return(score)
    
  }, pos.words, neg.words )
  
  scores.df = data.frame(score=scores, text=tweets)
  
  return(scores.df)
  
}

setup_twitter_oauth("ZWdr216m2VVgJZxDBmncaeOIV", 
                    "mqSNZFXjXV9cXTNcRhAdTQgIsfnmM0dbQpIFeNmYLzw5QX8RJM",
                    "147905014-5PZyY3fIQSsl6lxqHBGEjptaiixwNkKjLfSLaQqn",
                    "Elunoc08U824xekcIy3MsOMuudhSmvtU3NhU8BG0CXqMK")

#Filtering out Tweets
FordTweets = searchTwitter("@FordIndia OR #FordIndia", n = 1000, lang = "en", resultType = "recent", since = "2018-01-01")
MarutiTweets = searchTwitter("@Maruti_Corp OR #maruti", n = 1000, lang = "en", resultType = "recent", since = "2018-01-01")

Ford.df  = twListToDF(FordTweets)
write.csv(Ford.df, file = "Data/Twitter/FordTweets.csv")

Maruti.df  = twListToDF(MarutiTweets)
write.csv(Maruti.df, file = "Data/Twitter/MarutiTweets.csv")

FordDataset = read.csv("C:/Deeps/R/Data/Twitter/FordTweets.csv")
FordDataset$text = as.factor(FordDataset$text)
FordScore = score.sentiment(FordDataset$text, pos.words, neg.words, .progress = "text")
write.csv(FordScore, file = "C:/Deeps/R/Data/Twitter/FordScore.csv", row.names = T)


MarutiDataset = read.csv("C:/Deeps/R/Data/Twitter/MarutiTweets.csv")
MarutiDataset$text = as.factor(MarutiDataset$text)
MarutiScore = score.sentiment(MarutiDataset$text, pos.words, neg.words, .progress = "text")
write.csv(MarutiScore, file = "C:/Deeps/R/Data/Twitter/MarutiScore.csv", row.names = T)
# We can convert into Text. We need to extract Text from Tweets.

FordText = lapply(FordTweets, function(x) x$getText())
MarutiText = lapply(MarutiTweets, function(x) x$getText())


analysisFord = score.sentiment(FordText, pos.words, neg.words, .progress = 'none') # calls sentiment function
table(analysisFord$score)
mean(analysisFord$score)
#hist(analysisFord$score, col = "blue")


analysisMaruti = score.sentiment(MarutiText, pos.words, neg.words, .progress = 'none') # calls sentiment function
table(analysisMaruti$score)
mean(analysisMaruti$score)
#hist(analysisMaruti$score, col = "green")


# Comparing Scores
FordPlot = hist(analysisFord$score, xlab = 'Score', ylab = 'Frequency', main = "Ford Live Score")
MarutiPlot = hist(analysisMaruti$score, xlab = 'Score', ylab = 'Frequency', main = "Maruti Live Score")


plot(MarutiPlot, 
     col = rgb(1,0,0),
     main = "Ford Vs Maruti",
     xlab = "Scores")
plot(FordPlot, col = rgb(0,1,0), add = T)


# Creating word Cloud Around Ford and Maruti

FordCoupus <- VCorpus(VectorSource(FordDataset$text)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stripWhitespace) %>%
  tm_map(stemDocument)


FordDTM = DocumentTermMatrix(FordCoupus)
FordDTM = removeSparseTerms(FordDTM, 0.99)

wordsWithFrequency = sort(colSums(as.matrix(FordDTM)), decreasing = T)
View(wordsWithFrequency)

FordDataFrame = data.frame(word = names(wordsWithFrequency), freq = wordsWithFrequency)

wordcloud(words = FordDataFrame$word, freq = FordDataFrame$freq, min.freq = 1, max.words = 200, 
          random.order = FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


MarutiCoupus <- VCorpus(VectorSource(MarutiDataset$text)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stripWhitespace) %>%
  tm_map(stemDocument)


MarutiDTM = DocumentTermMatrix(MarutiCoupus)
MarutiDTM = removeSparseTerms(MarutiDTM, 0.99)

wordsWithFrequency = sort(colSums(as.matrix(MarutiDTM)), decreasing = T)
View(wordsWithFrequency)

MarutiDataFrame = data.frame(word = names(wordsWithFrequency), freq = wordsWithFrequency)

wordcloud(words = MarutiDataFrame$word, freq = MarutiDataFrame$freq, min.freq = 1, max.words = 200, 
          random.order = FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


