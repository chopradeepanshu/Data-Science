#**************************
#Decision tree on CLean DataSet
# In Medical Science we work on Sensitivity and Specifity of the data. We need to Emphasisize more on Sensitivity
#************************

setwd("C:\\Deeps\\R")
# Import the data 
library(rpart)
library(rpart.plot)
library(caret)


diabetesData = read.csv("Data/M4_Diabetes.csv")
# Clean the data - Remove Rows having Glucose = 0, BP = 0, BMI = 0
diabetesData = na.omit(diabetesData)
diabetesData = diabetesData[!(diabetesData$Glucose == 0),]
diabetesData = diabetesData[!(diabetesData$BP == 0),]
diabetesData = diabetesData[!(diabetesData$BMI == 0),]
diabetesData$output = ifelse(diabetesData$Is_Diabetic == "YES", 1, 0)

diabetesData = diabetesData[, c(-9)]
diabetesData = diabetesData[, c(-1,-4)] 

# Split the data in 70/30 Ratio
set.seed(3)
id = sample(2, nrow(diabetesData), prob = c(0.7, 0.3), replace = TRUE)
trainingData = diabetesData[id==1, ]
testingData = diabetesData[id==2, ]

LinearModel = lm(output ~ ., data = trainingData)
LogisticModel = glm(output~., data = trainingData, family = "binomial")



summary(LinearModel)
summary(LogisticModel)

prediction = predict(LogisticModel, testingData, type = "response")


library(ROCR)
ROCPredicted = prediction(prediction, testingData$output)
ROCPerformance = performance(ROCPredicted, "tpr", "fpr")

plot(ROCPerformance, colorize = TRUE, print.cutoffs.at=seq(0.1, by=0.1))


threshold=0.5
predicted_values<-ifelse(predict(LogisticModel,type="response")>threshold,1,0)
actual_values<-LogisticModel$y
conf_matrix<-table(predicted_values,actual_values)
conf_matrix

library(caret)

sensitivity(conf_matrix)
specificity(conf_matrix)

