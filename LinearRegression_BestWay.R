setwd("C:\\Deeps\\R")
getwd()

# We will use the Car Dataset which is already available in R.
head(cars)  # display the first 6 observations

# Step1: Check Scatter Plot (for relation), Box Plot (For outliers), Density Plot (For Bell Shaped Curve)

scatter.smooth(x=cars$speed, y=cars$dist, main="Dist ~ Speed")  # scatterplot

par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(cars$speed, main="Speed", sub=paste("Outlier rows: ", paste(boxplot.stats(cars$speed)$out, collapse=" ")))  # box plot for 'speed'
boxplot(cars$dist, main="Distance", sub=paste("Outlier rows: ", paste(boxplot.stats(cars$dist)$out, collapse=" ")))  # box plot for 'distance'


library(e1071)
par(mfrow=c(1, 2))  # divide graph area in 2 columns

# create density plot for 'speed'
plot(density(cars$speed), main="Density Plot: Speed", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))
polygon(density(cars$speed), col="red")

# create density plot for 'dist'
plot(density(cars$dist), main="Density Plot: Distance", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$dist), 2)))
polygon(density(cars$dist), col="red")


# Step 2: CHeck for Coorelation between the Variables.
round(cor(cars$speed, cars$dist), 2)

# Step 3: Model Building
model = lm(cars$speed ~ cars$dist)

summary(model)

# Step4: Check for AIC and BIC

AIC(model)  # AIC =>  260.7755
BIC(model)  # BIC => 266.5115

# Step5 : Create Training and Test data

set.seed(100)  # setting seed to reproduce results of random sampling

trainingRowIndex = sample(1:nrow(cars), 0.8*nrow(cars))  # row incices for training data
trainingData = cars[trainingRowIndex, ]  # model training data

testData  = cars[-trainingRowIndex, ]   # test data

# Step 6: Build the model on training data

lmMod = lm(dist ~ speed, data=trainingData)  # build the model

distPred = predict(lmMod, testData)  # predict distance

# Step 7: summary (lmMod)  # model summary

summary(lmMod)

# Calculate: akaike information criterion AIC
AIC (lmMod)  

actuals_preds = data.frame(cbind(actuals=testData$dist, predicteds=distPred))  # make actuals_predicteds dataframe.

correlation_accuracy <- cor(actuals_preds)  # 82.7%
correlation_accuracy

min_max_accuracy <- mean (apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  # => 58.42%, min_max accuracy
min_max_accuracy
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  # => 48.38%, mean absolute percentage deviation
mape
