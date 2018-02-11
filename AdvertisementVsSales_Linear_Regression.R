setwd("C:\\Deeps\\R")
getwd()

# Step 1: Import the data Set AdvertisementVsSales_Linear_Regression.csv

salesData = read.csv("Data/AdvertisementVsSales_Linear_Regression.csv", header = T)

# Check the Columns Names of the Sheet
head(salesData)
advertisement = salesData[,2]
sales = salesData[,3]
# Step 2: Plot the Graph between Advertisement and Sales
plot(advertisement, sales, main = "Advertisement Vs Sales", xlab = "Advertisement", ylab = "Sales")

# Step 3: Check the Co-relation between Advertisement and Sales
cor(sales, advertisement)

#The following guidelines have been proposed for determining the Coorelation:
#Coefficient, r
#Strength of Association	    Positive	    Negative
#Small	                      .1 to .3	    -0.1 to -0.3
#Medium	                      .3 to .5    	-0.3 to -0.5
#Large	                      .5 to 1.0   	-0.5 to -1.0

# In our case We have a Strong Coorelation as we look from the above table which is 0.9988322

# Step 4: We need to implement the Linear Model which is lm in R
# Fit the Linear Regression Model to the given data

mod = lm(sales ~ advertisement)
# The above statment means - Predicting the Sales using the Variable Advertisement 
# Important thing to Notice the First Variable should be Y and Second Should be X.

# Step 4: Get the Summary of the Model
summary(mod)

mod
# We can ask for the attributes of the model.
attributes(mod)

# We can extract the coefficient attribute from the model by
mod$coefficients

# Step 5: Add the Regression Line to the Model
abline(mod)

# We can add the Color to the Line and Add the width to it.
abline(mod, col = 2, lwd = 3)

confint(mod)


# Get the predictions
newData = data.frame(advertisement = 500)
predict(mod, newData, interval = "confidence")
