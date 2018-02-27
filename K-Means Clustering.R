setwd("C:\\Deeps\\R")
getwd()

#---------------------------------------------------------------------------
# Basic Understanding of K Means Clustering.
#---------------------------------------------------------------------------
# Take Random Set of Data Point
x = runif(50)
y = runif(50)
data = cbind(x,y)
plot(data)

# Now Let us find the Clusters in the data
kmodel = kmeans(x, centers = 4)
library(animation)

kmeans.ani(data, centers = 4)


library(factoextra)
fviz_cluster(list(data = data, cluster=kmodel$cluster), 
             ellipse.type = "norm",
             ellipse.level = 0.68,
             palette = "jco",
             ggtheme = theme_minimal())
#---------------------------------------------------------------------------
# Use Case on Movie Dataset using C Means Clustering.
#---------------------------------------------------------------------------
movieData = read.csv("Data/movie_metadata.csv")

movieData = na.omit(movieData)

facebook = movieData[1:20, c(5,6,8,14, 25, 28)]

row.names(facebook) =  movieData$movie_title[1:20]

# Lets cluster the given set of movies based on the Facebook Likes.

# K Means

# Identify the max number of Clusters to be there for Clustering.
maxCluster= 15
wss = sapply(1:maxCluster, function(k){kmeans(facebook, k, nstart = 10) $tot.withinss})
plot(1:maxCluster, wss, type = "b", pch = 19, frame = FALSE, xlab = "Number of Clusters K", ylab = "Total Within-Clusters Sum of Squares")  
abline(v = 3, lty = 10)

# From the plot we can see that the elbow mark is at 3.
# Apply the model

kmodel = kmeans(facebook, 3) # 3 is going to create 3 Clusters.
kmodel
# If we look at the results
#director_facebook_likes
#2376.            --> Centroid for the Cluster 1
#1099.6250        --> Centroid for the Cluster 2
#730.1667         --> Centroid for the Cluster 3

library(animation)
kmeans.ani(facebook, centers = 3)

library(factoextra)
fviz_cluster(list(data = facebook, cluster=kmodel$cluster), 
             ellipse.type = "norm",
             ellipse.level = 0.68,
             palette = "jco",
             ggtheme = theme_minimal())



