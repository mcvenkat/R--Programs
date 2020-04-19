library(ggplot2)
# Prepare Data 
data = mtcars  

# We need to scale the data to have zero mean and unit variance 
data <- scale(data)  

# Determine number of clusters 
wss <- (nrow(data)-1)*sum(apply(data,2,var)) 
for (i in 2:dim(data)[2]) { 
  wss[i] <- sum(kmeans(data, centers = i)$withinss) 
}  

# Plot the clusters 
plot(1:dim(data)[2], wss, type = "b", xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

# K-Means Cluster Analysis
fit <- kmeans(data, 5) # 5 cluster solution 

# get cluster means  
aggregate(data,by = list(fit$cluster),FUN = mean) 

# append cluster assignment 
data <- data.frame(data, fit$cluster) 