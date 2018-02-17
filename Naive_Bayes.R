# Perform Classification Using Naive Bayes.

# Import the data 
diabetesData = read.csv("Data/M4_Diabetes.csv")


# Split the data in 70/30 Ratio
set.seed(3)
id = sample(2, nrow(diabetesData), prob = c(0.7, 0.3), replace = TRUE)
trainingData = diabetesData[id==1, ]
testingData = diabetesData[id==2, ]

# Building Naive Bayes.
library(e1071)
model = naiveBayes(Is_Diabetic~., data = trainingData)
model

prediction = predict(model, newdata = testingData, type = "class")
prediction

# Create the confusion Matrix
library(caret)
confusionMatrix(table(prediction, testingData$Is_Diabetic))

# Accuracy = 74%


