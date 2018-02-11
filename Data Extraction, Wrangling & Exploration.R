setwd("C:\\Deeps\\R")
getwd()



# Downloading CSV File data from Internet.
linkaddress = "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
download.file(linkaddress,destfile = "Data/directlink1.csv")

# for more data sets view
# https://archive.ics.uci.edu/ml/datasets.html
list.files("Data/")

# Loading data from Excel File.
# While Loading the xlsx file, we need to install package of xlsx. This can be done by clicking install --> xlsx --> install button
# Use this Library xlsx before doing any thing on xlsx.
library("xlsx")

insuranceData = read.xlsx("Data/M3_Insurance_Data.xlsx",sheetIndex = 1,header = T)
isub = insuranceData[1:5,]

colIndex=c(2:3)
rowIndex = c(2:10)
insuranceData = read.xlsx("Data/M3_Insurance_Data.xlsx",sheetIndex = 1,
                           colIndex=colIndex, 
                           rowIndex=rowIndex,
                           header = T)

# Loading data from XML File.
#page 20 package:XML
library(XML)
webLink = "http://news.bbc.co.uk/2/hi/uk_politics/8044207.stm"
mps.doc = htmlParse(webLink)
mps.tabs = readHTMLTable(mps.doc)
View(mps.tabs$expenses_table)


# Reading data from CSV File which is already downloaded and saved on drive.
movie = read.csv("Data/Data Extraction, Wrangling & Exploration.csv")
View(movie)
class(movie)

#Study
#movie is not a data set and if we want to get some values from it, the output is called to be data frame.
# Movie Data Frame having 1 - 5 Rows and only 5th  Columns
movie_df = movie[1:5, 5]

#Specifying the names of the Column instead of 5
movie_df = movie[1:5, "director_facebook_likes"]

# Get the Multiple Columns with the Rows.
movie_df = movie[1:5, c("movie_title", "color", "director_name", "director_facebook_likes", "duration", "gross")]

View(movie_df)

#Practical - Start with only Relevant data which you need for Case Study.
# We need Movie title, Duration, Gross, Budget, Color, imdb score, title_year

#Step1: Get all the rows with Above Columns

movie_df = movie[, c("movie_title", "duration", "budget", "gross", "imdb_score", "title_year")]

#Step2:  Remove all the Null values from the Columns
movie_df = na.omit(movie_df)

#Step3:  Remove any special character if exists in the Movie Title.
movie_df$movie_title =  sub(pattern = "Â", replacement = "", movie_df$movie_title)

#After the above step we can say we have the processed data on which we can do our analysis.

# Case Study 1: Get the Top 10 Rated Movies.
head(movie_df[order(movie_df$imdb_score, decreasing = T), c("movie_title","title_year", "imdb_score")], 10)

# Case Study 2: Get the Top 10 Gross Movies.
head(movie_df[order(movie_df$gross, decreasing = T), c("movie_title","title_year", "gross")], 10)

# Case Study 3: Get Top 10 Movies greater than 180 minutes
head(movie_df[order(movie_df$duration > 180, decreasing = T), c("movie_title","title_year", "duration")], 10)

# Case Study 4: What is the Average Score of the Movie in the data Set
mean(movie_df$imdb_score) # which is 6.4, it means if we want to watch good movies it should have imdb_score some where > 8

# Case Study 5: Top 10 movies with the highest profit
# First, we need to calculate profit and add this column to the dataset using the cbind command
movie_df$profit = movie_df$gross - movie_df$budget
cbind(movie_df, movie_df$profit)

head(movie_df[order(movie_df$profit, decreasing = T), c("movie_title", "title_year", "profit")], 10)

#--------------------------------------------------------------------
# Visualization
#--------------------------------------------------------------------
# Case Study 6: Display List of Movies by Year

movieGroupByYear = aggregate(movie_df$movie_title, by = list(movie_df$title_year), FUN = length)
plot(movieGroupByYear)  

# Case Study 7: Display Average IMDB Score Year by Year
imdbScoreYearByYear = aggregate(movie_df$imdb_score, by = list(movie_df$title_year), FUN = mean)
plot(imdbScoreYearByYear)  


# Using Twitter Package
library(twitteR)
setup_twitter_oauth("ZWdr216m2VVgJZxDBmncaeOIV", 
                    "mqSNZFXjXV9cXTNcRhAdTQgIsfnmM0dbQpIFeNmYLzw5QX8RJM",
                    "147905014-5PZyY3fIQSsl6lxqHBGEjptaiixwNkKjLfSLaQqn",
                    "Elunoc08U824xekcIy3MsOMuudhSmvtU3NhU8BG0CXqMK")

tweets = searchTwitter("#modi", since = "2018-02-10")
twitter_df = twListToDF(tweets)
head(twitter_df)
