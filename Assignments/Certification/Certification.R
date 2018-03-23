setwd("C:\\Deeps\\R")
getwd()

# Set the Directory and load the dataset into R, verify that the data is loaded correctly
employeeData = read.csv("Assignments/Certification/338_cert_proj_datasets_v3.0.csv")
employeeOriginalData = employeeData
#For finding the insights out of our data several techniques can be used
#Find the correlation values of the attributes of our data
employeeData$department = factor(employeeData$department)
employeeData$department = as.integer(employeeData$department)


as.integer(employeeData$salary)
employeeData$salary = factor(employeeData$salary)
employeeData$salary = as.integer(employeeData$salary)

# Creating Scatter Plot Matrix. In this graph we will see that the TAX and RAD are highly Corelated. which is 0.91
library(corrplot)
cr = cor(employeeData)
corrplot(cr, type = "lower")
corrplot(cr, method = "number")

#Visualize the characteristics of the whole data and only the people who left, use plots and histograms
employeeData_left = subset(employeeData, employeeData$left == "1")

library(ggplot2)

#satisfaction_level
hist(x = employeeData_left$satisfaction_level, xlab = "Satisfaction Level", 
     ylab = "Employees Left", main = "Satisfaction Level Vs Employee Left", 
     col = "red", xlim = c(0,1), ylim = c(1, 1000))

#Salary
hist(x = employeeData_left$salary, xlab = "Salary 1 = High, 2 = Low, 3 = Medium", 
     ylab = "Employees Left", main = "Salary Vs Employee Left", 
     col = "red", breaks = 3, xlim = c(1,3), ylim = c(1, 1000))

#last_evaluation
hist(x = employeeData_left$last_evaluation, xlab = "Last Evaluation", 
     ylab = "Employees Left", main = "Last Evaluation Vs Employee Left", 
     col = "red", xlim = c(0,1), ylim = c(1, 1000))

#average_montly_hours
hist(x = employeeData_left$average_montly_hours, xlab = "Avg Monthly Hours", 
     ylab = "Employees Left", main = "Avg Monthly Hours Vs Employee Left", 
     col = "red", xlim = c(126,310), ylim = c(1, 1000))

#Time Spend
hist(x = employeeData_left$time_spend_company, xlab = "Time Spend", 
     ylab = "Employees Left", main = "Time Spend Vs Employee Left", 
     col = "red", xlim = c(2,6), ylim = c(1, 1000), breaks = 5)

#Work_accident
hist(x = employeeData_left$Work_accident, xlab = "Work Accident", 
     ylab = "Employees Left", main = "Work Accident Vs Employee Left", 
     col = "red", xlim = c(0,1), ylim = c(1, 1000))


#promotion_last_5years
hist(x = employeeData_left$promotion_last_5years, xlab = "Promotion Last 5 Years", 
     ylab = "Employees Left", main = "Promotion Last 5 Years Vs Employee Left", 
     col = "red", xlim = c(0,1), ylim = c(1, 1000))

max(employeeData_left$department)
colnames(employeeData_left)

#department
hist(x = employeeData_left$department, xlab = "Department", 
     ylab = "Employees Left", main = "Department Vs Employee Left", 
     col = "red", xlim = c(1,10), ylim = c(1, 1000))



#Analyse the department wise turnouts and find out the percentage of employees leaving from each department

departmentWiseTurnout = aggregate(employeeOriginalData$left == 1, by = list(employeeOriginalData$department), FUN = length)
departmentWiseTurnout

totalEmployeesLeft =  sum(employeeData_left$left)

departmentWiseChurnOutRate = as.data.frame(departmentWiseTurnout)
departmentWiseChurnOutRate$Pct <- (departmentWiseChurnOutRate$x / totalEmployeesLeft)*100
departmentWiseChurnOutRate = setNames(departmentWiseChurnOutRate, c("Department", "Left", "Percentage"))
head(departmentWiseChurnOutRate[order(departmentWiseChurnOutRate$Left, decreasing = T), c("Department", "Left", "Percentage")], 10)


# Build a classification model to forecast what are the attributes of people who leave the company

#---------------------------------------------------------
# Using Decisison Trees
#---------------------------------------------------------

library(rpart)
library(rpart.plot)
library(caret)

set.seed(3)
id = sample(2, nrow(employeeData), prob = c(0.7, 0.3), replace = TRUE)
trainingDataDT = employeeData[id==1, ]
testingDataDT = employeeData[id==2, ]

trainingDataDT$left = ifelse(trainingDataDT$left == 1, "YES", "NO")
testingDataDT$left = ifelse(testingDataDT$left == 1, "YES", "NO")

decisionTreeModel = rpart(left~., data = trainingDataDT)
decisionTreeModel
summary(decisionTreeModel)

prp(decisionTreeModel, box.palette = "Blues", tweak = 1.2)


# Prediction of Test Data Set
prediction =  predict(decisionTreeModel, newdata = testingDataDT, type = "class")
prediction

#Now compare it with Actual Values.
table(prediction, testingDataDT$left)

# For getting the confusion matrix we can use the library caret
decisionTreeMatrix = confusionMatrix(table(prediction, testingDataDT$left))


#---------------------------------------------------------
# Using Random Forest
#---------------------------------------------------------
library(randomForest)
set.seed(3)
id = sample(2, nrow(employeeData), prob = c(0.7, 0.3), replace = TRUE)
trainingDataRF = employeeData[id==1, ]
testingDataRF = employeeData[id==2, ]

randomForestModel = randomForest(as.factor(left)~., data = trainingDataRF, ntree = 5)
randomForestModel

# This Plot will help you to identify which Attribute is Important in creating Decision
varImpPlot(randomForestModel)

prediction = predict(randomForestModel, newdata = testingDataRF, type =  "class")
prediction
randomForestMatrix = confusionMatrix(table(prediction, testingDataRF$left))


#---------------------------------------------------------
# Using Naive Baye's
#---------------------------------------------------------
library(e1071)
set.seed(3)
id = sample(2, nrow(employeeData), prob = c(0.7, 0.3), replace = TRUE)
trainingDataNB = employeeData[id==1, ]
testingDataNB = employeeData[id==2, ]

naiveBayesModel = naiveBayes(as.factor(left)~., data = trainingDataNB)
print(naiveBayesModel)

prediction = predict(naiveBayesModel, newdata = testingDataNB, type =  "class")
prediction
naiveBayesMatrix = confusionMatrix(table(prediction, testingDataNB$left))

#---------------------------------------------------------
# Using SVM
#---------------------------------------------------------

library(e1071)

# Split the data in 70/30 Ratio
set.seed(3)
id = sample(2, nrow(employeeData), prob = c(0.7, 0.3), replace = TRUE)
trainingDataSVM = employeeData[id==1, ]
testingDataSVM = employeeData[id==2, ]

svmLinearModel = svm(as.factor(left)~., data = trainingDataSVM)
svmLinearModel
plot(svmLinearModel, trainingDataSVM)

prediction <- predict(svmLinearModel, testingDataSVM)
summary(prediction)

library(caret)

svmMatrix = confusionMatrix(table(predicted_Svm, testingDataSVM$left))
print(paste('SVM Model Accuracy (in %)',svmMatrix$overall[1]*100))


print(paste('Decision Tree Accuracy (in %)',decisionTreeMatrix$overall[1]*100))

print(paste('Random Forest Accuracy (in %)',randomForestMatrix$overall[1]*100))

print(paste('Naive Bayes Accuracy (in %)',naiveBayesMatrix$overall[1]*100))

print(paste('SVM Model Accuracy (in %)',svmMatrix$overall[1]*100))

algorithm = c("Decision Tree", "Random Forest", "Naive Bayes", "SVM")
accuracy = c(decisionTreeMatrix$overall[1]*100, randomForestMatrix$overall[1]*100, naiveBayesMatrix$overall[1]*100,svmMatrix$overall[1]*100)

output = data.frame(algorithm, accuracy)

#Final Outcome
output[order(output$accuracy, decreasing = T), ]



