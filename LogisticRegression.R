setwd("C:\\Deeps\\R")
getwd()
# Perform Logistic Regression.

# Import the data 
diabetesData = read.csv("Data/M4_Diabetes.csv")


# Split the data in 80/20 Ratio
set.seed(3)
id = sample(2, nrow(diabetesData), prob = c(0.8, 0.2), replace = TRUE)
training = diabetesData[id==1, ]
testing = diabetesData[id==2, ]

# Create Model
model = glm(Is_Diabetic~., training, family = "binomial")

# Summary of the model
summary(model)
#    Null deviance: 777.64  on 597  degrees of freedom - When we are not using any of the Independent Variable and we only using the Intercept. Means using on Bo and not using B1X1...
#    Residual deviance: 572.67  on 589  degrees of freedom - When we incluide the independent variables. This will come down as compare to Null Deviance.
#    AIC: 590.67 - As minimum as possible.
# Looking at the summary we will find Intercept, No.of_times_pregnant, glucose_conc, BMI,Diabetes_pedigree_fn is having stars which shows significant values.
# Now we need to optimize the model. If we look at age it show its not significant but we cannot simply remove it from model. we need to check its significance.

model = glm(Is_Diabetic~.-Age, data = training, family = "binomial")
summary(model)
#Null deviance: 777.64  on 597  degrees of freedom
#Residual deviance: 576.24  on 590  degrees of freedom - Value Increased
#AIC: 592.24 - AIC Increased

# Both AIC and Residual Deviance got increased, hence we cannot remove this variable.

model = glm(Is_Diabetic~.-skin_fold_thickness, data = training, family = "binomial")
summary(model)
#Null deviance: 777.64  on 597  degrees of freedom
#Residual deviance: 572.69  on 590  degrees of freedom
#AIC: 588.69

# AIC Got reduced and same as Residual deviance. Hence we can remove  skin_fold_thickness as it is not significant.
# Same thing we need to do with all those independent variables.

# We will predict the values of the test dataset and then categorize them according to threashold value which is 0.5

prediction = predict(model, testing, type = "response")
table(ActualValue = testing$Is_Diabetic, PredictedValue = prediction > 0.3)

#            PredictedValue
#ActualValue FALSE TRUE
#       NO     81   33
#       YES    10   46

# CHeck the Accuracy of the model by summing RIGHT DIAGONAL WAY
#(81+46)/(81+33+10+46)
# Which is 0.7470588 which means 74% Accuracy.

# COmpare the data with the res and testing to see whether the patient is Diabetic or not.
prediction


# We can also make a threashold for the YES/NO. this can be done by ROC Curve

library(ROCR)
ROCPredicted = prediction(prediction, testing$Is_Diabetic)
ROCPerformance = performance(ROCPredicted, "tpr", "fpr")
# tpr = "True Positive Rate"
# fpr = "False Positive Rate"

plot(ROCPerformance, colorize = TRUE, print.cutoffs.at=seq(0.1, by=0.1))

# We can see from 0.4 ,the False Positive Rate Increased. Hence we can take ROC = 0.4  
table(testing$Is_Diabetic, PredictedValue = prediction > 0.4)

# When Threshold at 0.4
#ActualValue FALSE TRUE
#NO           84   20
#YES          21   39
#(84+39)/(84+39+20+21)
#which is 0.7511696 75% Accuracy..

#Hence we can consider ROC Curve for a Threshold at 0.4


