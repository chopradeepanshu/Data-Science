setwd("C:\\Deeps\\R")
getwd()

#---------------------------------------------------------------------------
# Use Case on Insurance Dataset using K Means Clustering.
#---------------------------------------------------------------------------

library(xlsx)
insuranceData = read.xlsx("Data/M3_Insurance_Data.xlsx", sheetIndex = 1, header=TRUE)
set.seed(22)

trainingData = insuranceData[, c(1,2)]

# Find the optimal number of clusters for our data
maxCluster = 15
wss = sapply(1:maxCluster, function(k){kmeans(trainingData, k, nstart = 10) $tot.withinss})

plot(1:maxCluster, wss, type = "b", pch = 19, frame = FALSE, xlab = "Number of Clusters K", ylab = "Total Within-Clusters Sum of Squares")  
abline(v = 3, lty = 3)

# Looking at the Graph, it has been identified that at x = 3 we can see the maximum bend, hence we can take Number of cluster = 3

kmodel = kmeans(trainingData, 3) # 3 is going to create 3 Clusters.
kmodel

# Looking at the model we can see that the Premium Paid by the Person has been clearly classified into 3 Categories
# Cluster means:
#  Premiums.Paid          Age
#1     24789.706          60.64706
#2      6105.405          41.37838
#3     13193.478          44.54348

# Lets plot the cluster as a Graph
library(animation)
kmeans.ani(trainingData, centers = 3)

#---------------------------------------------------------------------------
# Use Case on Insurance Dataset using C Means Clustering.
#---------------------------------------------------------------------------

#Task 3: Perform C-means clustering on the data and save the membership dataframe in csv format.

library(e1071)
cmeans = cmeans(trainingData, 3)
cmeans$membership
write.csv(cmeans$membership, file = "Data/InsuranceDataFrame_C_Mean_Clust_Membership.csv")

library(factoextra)
fviz_cluster(list(data = trainingData, cluster=cmeans$cluster), 
             ellipse.type = "norm",
             ellipse.level = 0.68,
             palette = "jco",
             ggtheme = theme_minimal())
