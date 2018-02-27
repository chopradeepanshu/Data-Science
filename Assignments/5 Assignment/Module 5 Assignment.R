setwd("C:\\Deeps\\R\\Assignments\\5 Assignment")
getwd()

#Analyze the information given in the Affairs dataset and create classifiers using it.
#The dataset can be loaded into R using the library:

library(AER)
data(Affairs)

# Task 1: Using the affairs column of our data
# Create a new column with nominal values YES and NO
# Convert it into factor
Affairs$Output <- ifelse(Affairs$affairs > 5, "YES", "NO")
class(Affairs$Output) # Character
Affairs$Output = factor(Affairs$Output) 
class(Affairs$Output) # Factor


# Task 2: Create a classifier with our data using Decision tree algorithm
# Plot the Decision tree
# Calculate the accuracy using confusion matrix

# Split the data in 70/30 Ratio
set.seed(3)
id = sample(2, nrow(Affairs), prob = c(0.7, 0.3), replace = TRUE)
trainingData = Affairs[id==1, ]
testingData = Affairs[id==2, ]

# Building Decision Trees
library(rpart)
model = rpart(Output~., data = trainingData)
model

# Plot the Graph of Decision Tree.
plot(model, margin = 0.1)
text(model, use.n = TRUE, pretty = TRUE, cex = 0.8)

# Prediction of Test Data Set
prediction =  predict(model, newdata = testingData, type = "class")
prediction


# For getting the confusion matrix we can use the library caret
library(caret)
confusionMatrix(table(prediction, testingData$Output))
# Accuracy = 98%


# Task 3: Create another classifier with our data using random forest algorithm
# Calculate the accuracy using confusion matrix
# Find out the importance of attributes using importance() function


# Building Random Forest
library(randomForest)
model = randomForest(Output~., data = trainingData)
model


prediction = predict(model, newdata = testingData, type =  "class")
prediction

library(caret)
confusionMatrix(table(prediction, testingData$Output))
# Accuracy 98%

#Find out the importance of attributesusing importance() function
importance(model, type = 2)

#MeanDecreaseGini
#affairs             80.7506426
#gender               0.2977727
#age                  2.3642045
#yearsmarried         2.1138391
#children             0.4172370
#religiousness        2.4991462
#education            1.5222138
#occupation           1.1597329
#rating               4.3736481
