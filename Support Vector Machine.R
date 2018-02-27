# Perform Classification Using Support Vector Machine.

# Building Support Vector Machine.
library(e1071)
data("iris")
View(iris)

plot(iris$Petal.Length, iris$Petal.Width, col = iris$Species)

# Split the data in 70/30 Ratio
set.seed(3)
id = sample(2, nrow(iris), prob = c(0.7, 0.3), replace = TRUE)
trainingData = iris[id==1, ]
testingData = iris[id==2, ]

model = svm(Species~., data = trainingData, kernel = "linear", cost = 0.1, scale = F)

plot(model, trainingData)
