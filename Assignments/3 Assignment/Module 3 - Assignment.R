setwd("C:\\Deeps\\R")
getwd()

#Task 1: Import the dataset into R
diseaseData = read.csv("Data/Disease_data (Assignment Dataset).csv")

# Convert Age column to type Numeric
diseaseData$Age[which(diseaseData$Age=="seven")] = 7
View(diseaseData$Age)

diseaseData$Age = as.numeric(diseaseData$Age)
class(diseaseData$Age)

#Task 2: 
# Omit NULL values from non-numerical data fields
diseaseData = na.omit(diseaseData)
diseaseData$timesupper = strptime(x = diseaseData$timesupper, format = "yyyy-MM-dd hh:mm:ss" )
View(diseaseData$timeFrame)


# Task 3: Which is the most consumed food item among the patients.
# For this we need to make all the Food Items as Factor
diseaseData$baked_hamburgur = as.factor(diseaseData$baked_hamburgur)
diseaseData$baked_hamburgur = as.numeric(diseaseData$baked_hamburgur)

diseaseData$spinach = as.factor(diseaseData$spinach)
diseaseData$spinach = as.numeric(diseaseData$spinach)

diseaseData$mashed_potato = as.factor(diseaseData$mashed_potato)
diseaseData$mashed_potato = as.numeric(diseaseData$mashed_potato)

diseaseData$cabbages = as.factor(diseaseData$cabbages)
diseaseData$cabbages = as.numeric(diseaseData$cabbages)

diseaseData$jello = as.factor(diseaseData$jello)
diseaseData$jello = as.numeric(diseaseData$jello)

diseaseData$rolls = as.factor(diseaseData$rolls)
diseaseData$rolls = as.numeric(diseaseData$rolls)

diseaseData$brown = as.factor(diseaseData$brown)
diseaseData$brown = as.numeric(diseaseData$brown)

diseaseData$milk = as.factor(diseaseData$milk)
diseaseData$milk = as.numeric(diseaseData$milk)

diseaseData$coffee = as.factor(diseaseData$coffee)
diseaseData$coffee = as.numeric(diseaseData$coffee)

diseaseData$water = as.factor(diseaseData$water)
diseaseData$water = as.numeric(diseaseData$water)

diseaseData$cakes = as.factor(diseaseData$cakes)
diseaseData$cakes = as.numeric(diseaseData$cakes)

diseaseData$vanilla = as.factor(diseaseData$vanilla)
diseaseData$vanilla = as.numeric(diseaseData$vanilla)

diseaseData$chocolate = as.factor(diseaseData$chocolate)
diseaseData$chocolate = as.numeric(diseaseData$chocolate)

diseaseData$fruitsalad = as.factor(diseaseData$fruitsalad)
diseaseData$fruitsalad = as.numeric(diseaseData$fruitsalad)

calculateData = data.frame("hamburger" = sum(diseaseData$baked_hamburgur))


calculateData = data.frame("hamburger" = sum(diseaseData$baked_hamburgur), "spinach" = sum(diseaseData$spinach),
                           "mashed_potato" = sum(diseaseData$mashed_potato),"cabbages" = sum(diseaseData$cabbages),
                           "jello" = sum(diseaseData$jello),"rolls" = sum(diseaseData$rolls),
                           "brown" = sum(diseaseData$brown),"milk" = sum(diseaseData$milk),
                           "coffee" = sum(diseaseData$coffee),"water" = sum(diseaseData$water),
                           "cakes" = sum(diseaseData$cakes),"vanila" = sum(diseaseData$vanilla),
                           "chocolate" = sum(diseaseData$chocolate),"fruits" = sum(diseaseData$fruitsalad))

most_consumed_food = max(colnames(calculateData))

# Task 2: Find the average age of people who are ill using Boxplot.
avg_ill_age = mean(diseaseData$Age)
boxplot(diseaseData$Age)

# Task3:  Visualize gender ratio from the data.

plot(diseaseData$sex)
