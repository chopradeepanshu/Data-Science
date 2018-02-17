setwd("C:\\Deeps\\R")
getwd()

# Excercise 1 : Perform Linear Regression
#Task 1: Import the dataset into R
videogame.data = read.csv("Data/M4_Videogame.csv")

colnames(videogame.data)

class(videogame.data$NA_players)
class(videogame.data$EU_players)
class(videogame.data$JP_players)
class(videogame.data$Global_players)
class(videogame.data$Other_players)

videogame.dataframe = videogame.data[, c(7,8,9,10, 11)]

View(videogame.dataframe)

cr = cor(videogame.dataframe)
cr

# Creating Scatter Plot Matrix.
library(corrplot)
corrplot(cr, type = "lower")
corrplot(cr, method = "number")

set.seed(2)
library(caTools) # sample.split function is already available in this package


split = sample.split(videogame.dataframe$Other_players, SplitRatio = 0.7)

# We divided the data with the ratio of 0.7
split

trainingData = subset(videogame.dataframe, split == "TRUE")
testingData = subset(videogame.dataframe, split == "FALSE")

model = lm(trainingData$Other_players~., data=trainingData)
summary(model)

predict = predict(model, testingData)

# To compare Predicted Values and Actual Values, we can use Plot
plot(testingData$Other_players, type = "l" , lty = 1, col = "black", xlab = "")
lines(predict, type = "l", col = "blue")



