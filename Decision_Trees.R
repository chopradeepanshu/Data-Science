# Perform Classification Using Decision Trees
# Decision Tree Model can be developed by 2 Ways - Information Gain Vs Gini Index
# There is no Major difference in between but we generally prefer Information Gain method.

setwd("C:\\Deeps\\R")
# Import the data 
diabetesData = read.csv("Data/M4_Diabetes.csv")
library(rpart)
library(rpart.plot)
library(caret)


# Split the data in 70/30 Ratio
set.seed(3)
id = sample(2, nrow(diabetesData), prob = c(0.7, 0.3), replace = TRUE)
trainingData = diabetesData[id==1, ]
testingData = diabetesData[id==2, ]

#Building decision trees
# rpart is the package which is used in Decision Tree making. rpart = Recurrsive Partition.

#INFORMATION GAIN
#Training the Decision Tree classifier with criterion as information gain

model = rpart(Is_Diabetic~.-No.of_times_pregnant -skin_fold_thickness, data = trainingData)
model
summary(model)
plot(model, uniform=TRUE, main="Classification Tree for Kyphosis")
text(model, use.n=TRUE, all=TRUE, cex=.8)

prp(model, box.palette = "Blues", tweak = 1.2)

# Prediction of Test Data Set
prediction =  predict(model, newdata = testingData, type = "class")
prediction

#Now compare it with Actual Values.
table(prediction, testingData$Is_Diabetic)

# For getting the confusion matrix we can use the library caret
confusionMatrix(table(prediction, testingData$Is_Diabetic))
# Accuracy = 71%


