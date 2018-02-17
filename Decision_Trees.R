# Perform Classification Using Decision Trees

# Import the data 
diabetesData = read.csv("Data/M4_Diabetes.csv")


# Split the data in 70/30 Ratio
set.seed(3)
id = sample(2, nrow(diabetesData), prob = c(0.7, 0.3), replace = TRUE)
trainingData = diabetesData[id==1, ]
testingData = diabetesData[id==2, ]

#Building decision trees
library(rpart)
model = rpart(Is_Diabetic~., data = trainingData)

model

# Plot the Graph of Decision Tree.
plot(model, margin = 0.1)
text(model, use.n = TRUE, pretty = TRUE, cex = 0.8)


# Create Subset and Verify
temp = trainingData[diabetesData$glucose_conc <154.5 & diabetesData$BMI < 26.35 , ]
table(temp$Is_Diabetic)

# Prediction of Test Data Set
prediction =  predict(model, newdata = testingData, type = "class")
prediction

#Now compare it with Actual Values.
table(prediction, testingData$Is_Diabetic)

# For getting the confusion matrix we can use the library caret
library(caret)
confusionMatrix(table(prediction, testingData$Is_Diabetic))
# Accuracy = 71%


