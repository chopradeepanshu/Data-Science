#Lets use the 'census income' dataset and apply various decision tree methods to predict 
#whether a person's income will exceed $50K/yr.

salaryData <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data", header=F) #import

names(salaryData) = c("age", "workclass", "fnlwgt", "education", "educationnum", "maritalstatus", "occupation", "relationship", "race", "sex", "capitalgain", "capitalloss", "hoursperweek", "nativecountry", "salary")

library(rpart)
library(rpart.plot)
library(caret)

# Split the data in 70/30 Ratio
set.seed(3)
id = sample(2, nrow(salaryData), prob = c(0.7, 0.3), replace = TRUE)
trainingData = salaryData[id==1, ]
testingData = salaryData[id==2, ]


#Building decision trees
# rpart is the package which is used in Decision Tree making. rpart = Recurrsive Partition.

#INFORMATION GAIN
#Training the Decision Tree classifier with criterion as information gain

model = rpart(salary~., data = trainingData)
model
summary(model)

prp(model, box.palette = "Blues", tweak = 1.2)

# Prediction of Test Data Set
prediction =  predict(model, newdata = testingData, type = "class")
prediction

#Now compare it with Actual Values.
table(prediction, testingData$salary)

# For getting the confusion matrix we can use the library caret
confusionMatrix(table(prediction, testingData$salary))
# Accuracy = 84%

