# Implementing Linear Regression with Multiple Variables.
library(MASS)
data("Boston")
View(Boston)

# For understanding the Boston Data
#??Boston

# To View the Coorelation of the Variables.
plot(Boston$crim, Boston$medv, cex = 0.5, xlab = "Price", ylab = "Crime Rate")
cr = cor(Boston)

# Creating Scatter Plot Matrix. In this graph we will see that the TAX and RAD are highly Corelated. which is 0.91
library(corrplot)
corrplot(cr, type = "lower")
corrplot(cr, method = "number")

# We will split the data into Training and Testing Sets.
set.seed(2)
library(caTools) # sample.split function is already available in this package

split = sample.split(Boston$medv, SplitRatio = 0.7)

# We divided the data with the ratio of 0.7
split

trainingData = subset(Boston, split == "TRUE")
testingData = subset(Boston, split == "FALSE")


#Linear Regression Model

colnames(trainingData)

# To create the Linear Regression Model we will use all the Columns firstly on the Training Data Set

model = lm(medv ~ crim + zn + indus + chas + nox + rm + age + dis + rad + tax+ ptratio + black + lstat, data = trainingData)
# We can achive the above model using below statement.
#model = lm(medv~., data = trainingData)


summary(model)
# Check for the P Value. The more lower the P Value the More Significant the Variable is for your model.
# That means we should consider the Parameter to build that model.
# Generally we try to keep this p value less than 0.05 or less than 5 %.
# If the value is less than 5 % that means its a good P Value.
# So here the *** or ** or * means Good we can consider this attribute in our model and all will be less that 5 % or 0.05.
# Look at the values - We can remove the unsignificant values like: indus, age because its P Value is greater than 5 %.
# If R Square is near to 1 - Its a good model and vice versa.
# Here. R-squared:  0.7408,	Adjusted R-squared:  0.7312 

# Now again, we need to create a model by excluding the insignificant attributes like Indus and Age on the Training Dataset
model = lm(medv ~ crim + zn + chas + nox + rm + dis + rad + tax+ ptratio + black + lstat, data = trainingData)

summary(model)

# Now Predict the Model with the remaining 30% Testing Data Set
predict = predict(model, testingData)

# To compare Predicted Values and Actual Values, we can use Plot
plot(testingData$medv, type = "l" , lty = 1, col = "green", xlab = "")
lines(predict, type = "l", col = "blue")


newData = data.frame(crim = 0.23899, rad = 5, tax= 250, zn = 13.7, chas = 0, nox = 0.479, rm = 6.9989, dis= 5.976, ptratio = 16.9, black = 390.6, lstat = 6.1)

predict = predict(model, newData, interval = "confidence")
predict
