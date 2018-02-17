setwd("C:\\Deeps\\R")
getwd()

# Step 1: Import the data Set.LungCapData_Linear_Regression.xls

library("xlsx")
LungCapData = read.xlsx("Data/LungCapData_Linear_Regression.xlsx",sheetIndex = 1,header = T)

# Check the Columns Names of the Sheet
names(LungCapData)

# Check the Type of Variable for Age and LungCap
class(LungCapData$Age) # Numeric
class(LungCapData$LungCap) # Numeric

head(LungCapData)

# Step 2: Plot the Graph between Age and LungCap

Age = LungCapData[1:6, 2]
LungCap = LungCapData[1:6, 1]
plot(Age, LungCap, main = "ScatterPlot", xlab = "Age", ylab = "Lung Capacity")

# Step 3: Check the Co-relation between Age and Lung Capacity
cor(Age, LungCap)

#The following guidelines have been proposed for determining the Coorelation:
  
#Coefficient, r
#Strength of Association	    Positive	    Negative
#Small	                      .1 to .3	    -0.1 to -0.3
#Medium	                      .3 to .5    	-0.3 to -0.5
#Large	                      .5 to 1.0   	-0.5 to -1.0

# In our case We have a Strong Coorelation as we look from the above table which is 0.8196749

# Step 4: We need to implement the Linear Model which is lm in R
# Fit the Linear Regression Model to the given data

mod = lm(LungCap ~ Age)
# The above statment means - Predicting the Lung Capacity using the Variable Age. 
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
newData = data.frame(Age = 12)
predict(mod, newData, interval = "prediction")
