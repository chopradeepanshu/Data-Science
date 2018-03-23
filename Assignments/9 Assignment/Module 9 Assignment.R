setwd("C:\\Deeps\\R")
getwd()


library(ggplot2)
library(tseries)
library(forecast)

# Task 1
milk = read.csv("Data/M9_Monthly_Milkproduction.csv", header = TRUE)
names(milk)[names(milk) == 'X1962.01'] <- 'Year'
names(milk)[names(milk) == 'X589'] <- 'Values'

trainingSet = ts(milk, start=c(1962,1), end = c(1974,12), frequency=12)
testingSet =  ts(milk, start=c(1975,1), end = c(1975,12), frequency=12)

#Plot the data for analyzing trend and seasonality.
autoplot(trainingSet) + labs(x ="Year", y = "Milk Production", title="Milk Production from 1962 till 1974") 

plot(decompose(trainingSet))


forecastETS =  HoltWinters(trainingSet, gamma = NULL)
plot(forecast(forecastETS))
