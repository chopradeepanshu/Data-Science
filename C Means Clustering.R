setwd("C:\\Deeps\\R")
getwd()

movieData = read.csv("Data/movie_metadata.csv")

movieData = na.omit(movieData)

facebook = movieData[1:50, c(5,6,8,14, 25, 28)]

library(e1071)
cmeans = cmeans(facebook, 5)
cmeans

#library(animation)
#kmeans.ani(facebook, centers = 3)


library(factoextra)
fviz_cluster(list(data = facebook, cluster=cmeans$cluster), 
             ellipse.type = "norm",
             ellipse.level = 0.68,
             palette = "jco",
             ggtheme = theme_minimal())
