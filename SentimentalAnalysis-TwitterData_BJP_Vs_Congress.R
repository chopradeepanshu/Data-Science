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
BJPTweets = searchTwitter("@BJP4India", n = 1000, lang = "en", resultType = "recent")
CongressTweets = searchTwitter("@INCIndia", n = 1000, lang = "en", resultType = "recent")

BJP.df  = twListToDF(BJPTweets)
write.csv(BJP.df, file = "Data/Twitter/BJPTweets.csv")

Congress.df  = twListToDF(CongressTweets)
write.csv(Congress.df, file = "Data/Twitter/CongressTweets.csv")

BJPDataset = read.csv("C:/Deeps/R/Data/Twitter/BJPTweets.csv")
BJPDataset$text = as.factor(BJPDataset$text)
BJPScore = score.sentiment(BJPDataset$text, pos.words, neg.words, .progress = "text")
write.csv(BJPScore, file = "C:/Deeps/R/Data/Twitter/BJPScore.csv", row.names = T)


CongressDataset = read.csv("C:/Deeps/R/Data/Twitter/CongressTweets.csv")
CongressDataset$text = as.factor(CongressDataset$text)
CongressScore = score.sentiment(CongressDataset$text, pos.words, neg.words, .progress = "text")
write.csv(CongressScore, file = "C:/Deeps/R/Data/Twitter/CongressScore.csv", row.names = T)
# We can convert into Text. We need to extract Text from Tweets.

BJPText = lapply(BJPTweets, function(x) x$getText())
CongressText = lapply(CongressTweets, function(x) x$getText())


analysisBJP = score.sentiment(BJPText, pos.words, neg.words, .progress = 'none') # calls sentiment function
table(analysisBJP$score)
mean(analysisBJP$score)
#hist(analysisBJP$score, col = "blue")


analysisCongress = score.sentiment(CongressText, pos.words, neg.words, .progress = 'none') # calls sentiment function
table(analysisCongress$score)
mean(analysisCongress$score)
#hist(analysisCongress$score, col = "green")


# Comparing Scores
BJPPlot = hist(analysisBJP$score)
CongressPlot = hist(analysisCongress$score)

  
plot(BJPPlot, 
     col = rgb(0,1,0),
    main = "BJP Vs Congress",
    xlab = "Scores")
plot(CongressPlot, col = rgb(1,0,0), add = T)


# Creating word Cloud Around BJP and Congress

BJPCoupus <- VCorpus(VectorSource(BJPDataset$text)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stripWhitespace) %>%
  tm_map(stemDocument)


BJPDTM = DocumentTermMatrix(BJPCoupus)
BJPDTM = removeSparseTerms(BJPDTM, 0.99)

wordsWithFrequency = sort(colSums(as.matrix(BJPDTM)), decreasing = T)
View(wordsWithFrequency)

BJPDataFrame = data.frame(word = names(wordsWithFrequency), freq = wordsWithFrequency)

wordcloud(words = BJPDataFrame$word, freq = BJPDataFrame$freq, min.freq = 1, max.words = 200, 
          random.order = FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


CongressCoupus <- VCorpus(VectorSource(CongressDataset$text)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stripWhitespace) %>%
  tm_map(stemDocument)


CongressDTM = DocumentTermMatrix(CongressCoupus)
CongressDTM = removeSparseTerms(CongressDTM, 0.99)

wordsWithFrequency = sort(colSums(as.matrix(CongressDTM)), decreasing = T)
View(wordsWithFrequency)

CongressDataFrame = data.frame(word = names(wordsWithFrequency), freq = wordsWithFrequency)

wordcloud(words = CongressDataFrame$word, freq = CongressDataFrame$freq, min.freq = 1, max.words = 200, 
          random.order = FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


