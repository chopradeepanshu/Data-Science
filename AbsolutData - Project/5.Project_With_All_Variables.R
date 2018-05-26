setwd("C:\\Deeps\\R")
getwd()

# Set the Directory and load the dataset into R, verify that the data is loaded correctly
patientData = read.csv("Data/Patient_Data.csv")
patientOriginalData = patientData

# Exploratory Data Analysis

#Mean
Hemoglobin.Mean = mean(patientData$Level_of_Hemoglobin, na.rm = TRUE)
Genetic_Pedigree.Mean = mean(patientData$Genetic_Pedigree_Coefficient, na.rm = TRUE)
Age.Mean = mean(patientData$Age, na.rm = TRUE)
BMI.Mean = mean(patientData$BMI, na.rm = TRUE)
Physical_Activity.Mean = mean(patientData$Physical_activity, na.rm = TRUE)
Salt_Content.Mean = mean(patientData$salt_content_in_the_diet, na.rm = TRUE)
Alcohol_Consumption.Mean = mean(patientData$alcohol_consumption_per_day, na.rm = TRUE)

#Median
Hemoglobin.Median = median(patientData$Level_of_Hemoglobin, na.rm = TRUE)
Genetic_Pedigree.Median = median(patientData$Genetic_Pedigree_Coefficient, na.rm = TRUE)
Age.Median = median(patientData$Age, na.rm = TRUE)
BMI.Median = median(patientData$BMI, na.rm = TRUE)
Physical_Activity.Median = median(patientData$Physical_activity, na.rm = TRUE)
Salt_Content.Median = median(patientData$salt_content_in_the_diet, na.rm = TRUE)
Alcohol_Consumption.Median = median(patientData$alcohol_consumption_per_day, na.rm = TRUE)

#Check for the Outliers in the Continuous Variables.

#The normal range for hemoglobin is:
#For men, 13.5 to 17.5 grams per deciliter
#For women, 12.0 to 15.5 grams per deciliter

Hemoglobin.Outliers <- boxplot.stats(patientData$Level_of_Hemoglobin)$out  # outlier values.
boxplot(patientData$Level_of_Hemoglobin, main="Hemoglobin Data", boxwex=1)
mtext(paste("Outliers: ", paste(Hemoglobin.Outliers, collapse=", ")), cex=1)


Genetic_Pedigree_Coefficient.Outliers <- boxplot.stats(patientData$Genetic_Pedigree_Coefficient)$out  # outlier values.
boxplot(patientData$Genetic_Pedigree_Coefficient, main="Genetic Pedigree", boxwex=1)
mtext(paste("Outliers: ", paste(Genetic_Pedigree_Coefficient.Outliers, collapse=", ")), cex=1)

Age.Outliers <- boxplot.stats(patientData$Age)$out  # outlier values.
boxplot(patientData$Age, main="Age", boxwex=1)
mtext(paste("Outliers: ", paste(Age.Outliers, collapse=", ")), cex=1)

BMI.Outliers <- boxplot.stats(patientData$BMI)$out  # outlier values.
boxplot(patientData$BMI, main="BMI", boxwex=1)
mtext(paste("Outliers: ", paste(BMI.Outliers, collapse=", ")), cex=1)

Physical_Activity.Outliers <- boxplot.stats(patientData$Physical_activity)$out  # outlier values.
boxplot(patientData$Physical_activity, main="Physical_Activity", boxwex=1)
mtext(paste("Outliers: ", paste(Physical_Activity.Outliers, collapse=", ")), cex=1)

Salt_Content.Outliers <- boxplot.stats(patientData$salt_content_in_the_diet)$out  # outlier values.
boxplot(patientData$salt_content_in_the_diet, main="salt_content_in_the_diet", boxwex=1)
mtext(paste("Outliers: ", paste(Salt_Content.Outliers, collapse=", ")), cex=1)

Alcohol_Consumption.Outliers <- boxplot.stats(patientData$alcohol_consumption_per_day)$out  # outlier values.
boxplot(patientData$alcohol_consumption_per_day, main="alcohol_consumption_per_day", boxwex=1)
mtext(paste("Outliers: ", paste(Alcohol_Consumption.Outliers, collapse=", ")), cex=1)

# Check the Percentage of the Null Values for Each Columns in the Provided Data
na_count = sapply(patientData, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count = (na_count/nrow(patientData))*100
View(na_count)

# Null Values in Genetic Pedigree Cofficient Column is 4.6 %.
# Imputing Genetic Pedigree Cofficient Column with Mean Values
patientData$Genetic_Pedigree_Coefficient[is.na(patientData$Genetic_Pedigree_Coefficient)] <- Genetic_Pedigree.Mean

# Null Values in Pregnancy Column is 77.9 %
# Imputing Pregnancy column with default Value. Lets take it 0
patientData$Pregnancy[is.na(patientData$Pregnancy)] <- 0

# Null Values in Alcohol Consumption Column is 12.1 %
# Imputing Alcohol Consumption column with default Value. Lets take it 0
patientData$alcohol_consumption_per_day[is.na(patientData$alcohol_consumption_per_day)] <- 0


# Lets check for the Ratio of the Categorical value with 0/1
BP.Normal.Count = length(which(patientData$Blood_Pressure_Abnormality == 0)) 
BP.Abnormal.Count = length(which(patientData$Blood_Pressure_Abnormality == 1)) 

BP.Normal.Percentage = (BP.Normal.Count/nrow(patientData))*100
BP.Abnormal.Percentage = (BP.Abnormal.Count/nrow(patientData))*100

# Which shows that half of our data belongs to Normal BP and half to Abnormal BP.

#-----------------------------------------------
# Correlation Matrix
#-----------------------------------------------
library(corrplot)
cr = cor(patientData)
corrplot(cr, type = "lower", method = "number")

#-----------------------------------------------
# Implementing Machine Learning Models
#-----------------------------------------------

# Split the data in 70/30 Ratio
set.seed(3)
id = sample(2, nrow(patientData), prob = c(0.7, 0.3), replace = TRUE)
trainingData = patientData[id==1, ]
testingData = patientData[id==2, ]

#---------------------------------------------------------
# Prediction Using Logistic Model with Haemoglobin and Genetic History as Independent Variables
#---------------------------------------------------------
LogisticModel = glm(Blood_Pressure_Abnormality~ Level_of_Hemoglobin + 
                    Sex
                    +Chronic_kidney_disease
                    +Adrenal_and_thyroid_disorders, data = trainingData, family = "binomial")

# p-Value of Genetic_Pedigree_Coefficient is greater than .05 hence we can exclude from our Analysis
#LogisticModel = glm(Blood_Pressure_Abnormality~ Level_of_Hemoglobin, data = trainingData, family = "binomial")

summary(LogisticModel)

#anova(LogisticModel, test="Chisq")

prediction = predict(LogisticModel, testingData, type = "response")

library(ROCR)
ROCPredicted = prediction(prediction, testingData$Blood_Pressure_Abnormality)
ROCPerformance = performance(ROCPredicted, "tpr", "fpr")

plot(ROCPerformance, colorize = TRUE, print.cutoffs.at=seq(0.1, by=0.1))

threshold=0.6
predicted_values<-ifelse(predict(LogisticModel,type="response")>threshold,1,0)

actual_values<-LogisticModel$y
conf_matrix<-table(predicted_values,actual_values)
conf_matrix

logisticRegressionMatrix = confusionMatrix(conf_matrix)
#Accuracy = 61.26
auc <- performance(ROCPredicted, measure = "auc")
auc <- auc@y.values[[1]]
auc


#---------------------------------------------------------
# Prediction using Decisison Trees Model with Haemoglobin and Genetic History as Independent Variables
#---------------------------------------------------------
library(rpart)
library(rpart.plot)
library(caret)

#Building decision trees

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
DecisionTreeModel <- train(as.factor(Blood_Pressure_Abnormality) ~ Level_of_Hemoglobin + Genetic_Pedigree_Coefficient
                           +Smoking
                           +BMI
                           +Physical_activity
                           +salt_content_in_the_diet
                           +alcohol_consumption_per_day
                           +Level_of_Stress
                           +Age
                           +Sex
                           +Pregnancy
                           +Chronic_kidney_disease
                           +Adrenal_and_thyroid_disorders,
                           data = trainingData, method = "rpart",
                           parms = list(split = "information"),
                           trControl=trctrl,
                           tuneLength = 10)


summary(DecisionTreeModel)

prp(DecisionTreeModel$finalModel, box.palette = "Reds", tweak = 1.2)

# Prediction of Test Data Set
prediction =  predict(DecisionTreeModel, newdata = testingData)

table(prediction)
table(testingData$Blood_Pressure_Abnormality)

test_pred <- predict(DecisionTreeModel, newdata = testingData)
confusionMatrix(test_pred, testingData$Blood_Pressure_Abnormality )  #check accuracy

#Now compare it with Actual Values.
table(prediction, testingData$Blood_Pressure_Abnormality)

# For getting the confusion matrix we can use the library caret
decisionTreeMatrix = confusionMatrix(table(prediction, testingData$Blood_Pressure_Abnormality))


#---------------------------------------------------------
# Prediction using Random Forest Model with Haemoglobin and Genetic History as Independent Variables
#---------------------------------------------------------
library(randomForest)

randomForestModel = randomForest(as.factor(Blood_Pressure_Abnormality)~Level_of_Hemoglobin + Genetic_Pedigree_Coefficient
                                 +Smoking
                                 +BMI
                                 +Physical_activity
                                 +salt_content_in_the_diet
                                 +alcohol_consumption_per_day
                                 +Level_of_Stress
                                 +Age
                                 +Sex
                                 +Pregnancy
                                 +Chronic_kidney_disease
                                 +Adrenal_and_thyroid_disorders, data = trainingData, ntree = 5)
summary(randomForestModel)

# This Plot will help you to identify which Attribute is Important in creating Decision
varImpPlot(randomForestModel)

prediction = predict(randomForestModel, newdata = testingData, type =  "class")
prediction
table(prediction, testingData$Blood_Pressure_Abnormality)
randomForestMatrix = confusionMatrix(table(prediction, testingData$Blood_Pressure_Abnormality))

#---------------------------------------------------------
# Prediction using Naive Baye's with Haemoglobin and Genetic History as Independent Variables
#---------------------------------------------------------
library(e1071)

naiveBayesModel = naiveBayes(as.factor(Blood_Pressure_Abnormality)~Level_of_Hemoglobin + Genetic_Pedigree_Coefficient
                             +Smoking
                             +BMI
                             +Physical_activity
                             +salt_content_in_the_diet
                             +alcohol_consumption_per_day
                             +Level_of_Stress
                             +Age
                             +Sex
                             +Pregnancy
                             +Chronic_kidney_disease
                             +Adrenal_and_thyroid_disorders, data = trainingData)
print(naiveBayesModel)

prediction = predict(naiveBayesModel, newdata = testingData, type =  "class")
prediction
naiveBayesMatrix = confusionMatrix(table(prediction, testingData$Blood_Pressure_Abnormality))


#---------------------------------------------------------
# Prediction using SVM with Haemoglobin and Genetic History as Independent Variables
#---------------------------------------------------------

library(e1071)

svmLinearModel = svm(as.factor(Blood_Pressure_Abnormality)~Level_of_Hemoglobin + Genetic_Pedigree_Coefficient
                     +Smoking
                     +BMI
                     +Physical_activity
                     +salt_content_in_the_diet
                     +alcohol_consumption_per_day
                     +Level_of_Stress
                     +Age
                     +Sex
                     +Pregnancy
                     +Chronic_kidney_disease
                     +Adrenal_and_thyroid_disorders, data = trainingData)
svmLinearModel

prediction <- predict(svmLinearModel, testingData)
summary(prediction)

library(caret)

svmMatrix = confusionMatrix(table(prediction, testingData$Blood_Pressure_Abnormality))


print(paste('Logistic  Model Accuracy (in %)',logisticRegressionMatrix$overall[1]*100))

print(paste('Decision Tree Accuracy (in %)',decisionTreeMatrix$overall[1]*100))

print(paste('Random Forest Accuracy (in %)',randomForestMatrix$overall[1]*100))

print(paste('Naive Bayes Accuracy (in %)',naiveBayesMatrix$overall[1]*100))

print(paste('SVM Model Accuracy (in %)',svmMatrix$overall[1]*100))

algorithm = c("Logistic Regression", "Decision Tree", "Random Forest", "Naive Bayes", "SVM")
accuracy = c(logisticRegressionMatrix$overall[1]*100, decisionTreeMatrix$overall[1]*100, randomForestMatrix$overall[1]*100, naiveBayesMatrix$overall[1]*100,svmMatrix$overall[1]*100)

output = data.frame(algorithm, accuracy)

#Final Outcome
output[order(output$accuracy, decreasing = T), ]
