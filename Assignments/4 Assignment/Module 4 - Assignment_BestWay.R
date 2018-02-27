#****************************************************************
#                       LINEAR REGRESSION
#****************************************************************

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

# Split the data in 70/30 Ratio
set.seed(3)
id = sample(2, nrow(videogame.dataframe), prob = c(0.7, 0.3), replace = TRUE)
trainingData = videogame.dataframe[id==1, ]
testingData = videogame.dataframe[id==2, ]

model = lm(trainingData$Other_players~., data=trainingData)
summary(model)

predict = predict(model, testingData)

# DATA TO BE PREDICTED FOR ALL
# To compare Predicted Values and Actual Values, we can use Plot
plot(testingData$Other_players, type = "l" , lty = 1, col = "black", xlab = "")
lines(predict, type = "l", col = "blue")


# Task 2: Plot the values predicted by our model and the actual values to check deviation
#between them
# DATA TO BE PREDICTED FOR FIRST 100 ROWS, INORDER TO VIEW IT CLEARLY.

testingDataSubSet = testingData[1:100, ]
# To compare Predicted Values and Actual Values, we can use Plot
plot(testingData$Other_players[1:100], type = "l" , lty = 1, col = "blue", xlab = "")
lines(predict[1:100], type = "l", col = "green")

#****************************************************************
#                       LOGISTIC REGRESSION
#****************************************************************

#Analyze the information given in the Employee_Data dataset and predict the values using logistic regression model.

#Task 1:  Using the Emp_sal column of our data create a logistic regression model
#	Create a test set and training set
#	Build a model using all other columns and predict the values.
#	Create a confusion matrix with cut off value as 0.4

setwd("C:\\Deeps\\R")
getwd()

employeeData = read.csv("Data/M4_Employee_Data.csv")


employeeData$marital_Status = as.factor(employeeData$marital_Status)
employeeData$marital_Status = as.numeric(employeeData$marital_Status)

employeeData$Occ_Of_Emp = as.character(employeeData$Occ_Of_Emp)
employeeData$Occ_Of_Emp = ifelse(employeeData$Occ_Of_Emp == "?", "Unknown", employeeData$Occ_Of_Emp)
employeeData$Occ_Of_Emp = as.factor(employeeData$Occ_Of_Emp)
employeeData$Occ_Of_Emp = as.numeric(employeeData$Occ_Of_Emp)

employeeData$Emp_rel_status = as.factor(employeeData$Emp_rel_status)
employeeData$Emp_rel_status = as.numeric(employeeData$Emp_rel_status)

employeeData$Emp_race_type = as.factor(employeeData$Emp_race_type)
employeeData$Emp_race_type = as.numeric(employeeData$Emp_race_type)

employeeData$sex_of_emp = as.factor(employeeData$sex_of_emp)
employeeData$sex_of_emp = as.numeric(employeeData$sex_of_emp)

employeeData$country_of_res = as.character(employeeData$country_of_res)
employeeData$country_of_res = ifelse(employeeData$country_of_res == "?", "Unknown", employeeData$country_of_res)
employeeData$country_of_res = as.factor(employeeData$country_of_res)
employeeData$country_of_res = as.numeric(employeeData$country_of_res)

# Categorize the Salary "High" or "Low" based on the codition in order to ease the problem.
employeeData$salary = ifelse(employeeData$Emp_Sal == ">50K", "1", "0")
#employeeData$salary = as.factor(employeeData$salary)
employeeData$salary = as.numeric(employeeData$salary)

employeeData = employeeData[,c(-2, -3, -4, -15)] # Remove the Emp_Sal Column

View(employeeData)

cr = cor(employeeData)
cr

# Creating Scatter Plot Matrix. In this graph we will see that which Attribute is is highly associated with each Attribute
library(corrplot)
corrplot(cr, type = "lower")
corrplot(cr, method = "number")

# Build Logistic Model.

# Split the data in 70/30 Ratio
set.seed(3)
id = sample(2, nrow(employeeData), prob = c(0.7, 0.3), replace = TRUE)
trainingData = employeeData[id==1, ]
testingData = employeeData[id==2, ]

colnames(employeeData)

model = glm(salary~., data=trainingData, family = "binomial")
model
#All Attributes check :: Residual = 17510	AIC = 17530

# Lets check by removing Age
model = glm(salary~. -Age_Of_emp , data=trainingData, family = "binomial")
model
# Residual = 17930	AIC = 17950. Residual and AIC got increased, hence Age cannot be removed from Model.

# Lets check by removing Edu_Cat
model = glm(salary~. -Edu_Cat , data=trainingData, family = "binomial")
model
# Residual = 19420	AIC = 19440. Residual and AIC got increased, hence Edu_Cat cannot be removed from Model.

# Lets check by removing marital_Status
model = glm(salary~. -marital_Status , data=trainingData, family = "binomial")
model
# Residual = 17780	AIC = 17800. Residual and AIC got increased, hence marital_Status cannot be removed from Model.

# Lets check by removing Occ_Of_Emp
model = glm(salary~. -Occ_Of_Emp , data=trainingData, family = "binomial")
model
# Residual = 17520	AIC = 17540 Residual and AIC got increased, hence Occ_Of_Emp cannot be removed from Model.

# Lets check by removing Emp_rel_status
model = glm(salary~. -Emp_rel_status , data=trainingData, family = "binomial")
model
# Residual = 17560	AIC = 17580 Residual and AIC got increased, hence Emp_rel_status cannot be removed from Model.

# Lets check by removing Emp_race_type
model = glm(salary~. -Emp_race_type , data=trainingData, family = "binomial")
model
# Residual = 17560	AIC = 17580 Residual and AIC got increased, hence Emp_race_type cannot be removed from Model.


# Lets check by removing sex_of_emp
model = glm(salary~. -sex_of_emp , data=trainingData, family = "binomial")
model
# Residual = 17750	AIC = 17770 Residual and AIC got increased, hence sex_of_emp cannot be removed from Model.

# Lets check by removing capital_gain
model = glm(salary~. -capital_gain , data=trainingData, family = "binomial")
model
# Residual = 18930	AIC = 18950 Residual and AIC got increased, hence capital_gain cannot be removed from Model.


# Lets check by removing capital_loss
model = glm(salary~. -capital_loss , data=trainingData, family = "binomial")
model
# Residual = 17840	AIC = 17850 Residual and AIC got increased, hence capital_loss cannot be removed from Model.

# Lets check by removing Work_hour_in_week
model = glm(salary~. -Work_hour_in_week , data=trainingData, family = "binomial")
model
# Residual = 17840	AIC = 17850 Residual and AIC got increased, hence Work_hour_in_week cannot be removed from Model.

# Lets check by removing country_of_res
model = glm(salary~. -country_of_res , data=trainingData, family = "binomial")
model
# Residual = 17840	AIC = 17850 Residual and AIC got increased, hence country_of_res cannot be removed from Model.



predictValues = predict(model, testingData, type = "response")
predictValues

# We can also make a threashold for the YES/NO. this can be done by ROC Curve
res = predict(model, testingData, type = "response")

library(ROCR)
ROCPredicted = prediction(res, testingData$salary)
ROCPerformance = performance(ROCPredicted, "tpr", "fpr")
# tpr = "True Positive Rate"
# fpr = "False Positive Rate"

plot(ROCPerformance, colorize = TRUE, print.cutoffs.at=seq(0.1, by=0.1))

# Looking at the graph we can say there is too much bend at 0.4. Hence we can take the threshold at 0.4

# We can see from 0.4 ,the False Positive Rate Increased. Hence we can take ROC = 0.4  
table(ActualValue = testingData$salary, PredictedValue = res > 0.4)

# Lets see the Confusion Matrix
#              PredictedValue
# ActualValue FALSE TRUE
#            0  6652  699
#            1  1019 1335

#(6652+1335)/(6652+1335+699+1019)
#Which is 0.8229778. Means 82% Accuracy in the model.
