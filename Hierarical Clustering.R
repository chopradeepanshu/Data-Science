setwd("C:\\Deeps\\R")
getwd()

movieData = read.csv("Data/movie_metadata.csv")

movieData = na.omit(movieData)

movieDataFrame = movieData[1:20, c(9, 23)]

rownames(movieDataFrame) = movieData$movie_title[1:20]

distance = dist(as.matrix(movieDataFrame))

hc = hclust(distance)
plot(hc)
