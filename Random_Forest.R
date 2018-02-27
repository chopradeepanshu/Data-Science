# Perform Classification Using Random Forest

# Import the data 
diabetesData = read.csv("Data/M4_Diabetes.csv")


# Split the data in 70/30 Ratio
set.seed(3)
id = sample(2, nrow(diabetesData), prob = c(0.7, 0.3), replace = TRUE)
trainingData = diabetesData[id==1, ]
testingData = diabetesData[id==2, ]

# Building Random Forest
library(randomForest)
model = randomForest(Is_Diabetic~., data = trainingData, ntree = 5)
model

# This Plot will help you to identify which Attribute is Important in creating Decision
varImpPlot(model)

prediction = predict(model, newdata = testingData, type =  "class")
prediction

library(caret)
confusionMatrix(table(prediction, testingData$Is_Diabetic))
# Accuracy = 72.8

