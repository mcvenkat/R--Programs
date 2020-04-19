# Load data
data("USArrests")
my_data <- USArrests
# Remove any missing value (i.e, NA values for not available)
my_data <- na.omit(my_data)
# Scale variables
my_data <- scale(my_data)
# View the firt 3 rows
head(my_data, n = 3)

# Install factoextra
install.packages("factoextra")
# Install cluster package
install.packages("cluster")

library("cluster")
library("factoextra")
res.dist <- get_dist(USArrests, stand = TRUE, method = "pearson")
fviz_dist(res.dist, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))


#Distance Measures - Unsupervised Learning

# Load the dataset
data(USArrests)
# Subset of the data
set.seed(123)
ss <- sample(1:50, 10) # Take 10 random rows
df <- USArrests[ss, ] # Subset the 10 rows
# Remove any missing value (i.e, NA values for not available)
# That might be present in the data
df <- na.omit(df)
# View the firt 6 rows of the data
head(df, n = 6)

#Descriptive Statistics
desc_stats <- data.frame(
  Min = apply(USArrests, 2, min), # minimum
  Med = apply(USArrests, 2, median), # median
  Mean = apply(USArrests, 2, mean), # mean
  SD = apply(USArrests, 2, sd), # Standard deviation
  Max = apply(USArrests, 2, max) # Maximum
)
desc_stats <- round(desc_stats, 1)
head(desc_stats)

#Standardization
df.scaled <- scale(df)
head(round(df.scaled, 2))

#R Functions for computing distances
#Standard dist() function
# Compute Euclidean pairwise distances
dist.eucl <- dist(df.scaled, method = "euclidean")
# View a subset of the distance matrices
round(as.matrix(dist.eucl)[1:6, 1:6], 1)

# Compute correlation matrix
res.cor <- cor(t(df.scaled),  method = "pearson")
# Compute distance matrix
dist.cor <- as.dist(1 - res.cor)
round(as.matrix(dist.cor)[1:6, 1:6], 1)

res1.cor <- cor(t(df.scaled),  method = "spearman")
# Compute distance matrix
dist.cor <- as.dist(1 - res1.cor)
round(as.matrix(dist.cor)[1:6, 1:6], 1)

res2.cor <- cor(t(df.scaled),  method = "kendall")
# Compute distance matrix
dist.cor <- as.dist(1 - res2.cor)
round(as.matrix(dist.cor)[1:6, 1:6], 1)

#Function daisy() in cluster package
library(cluster)
# Load data
data(flower)
head(flower)

str(flower)
# Distance matrix
dd <- as.matrix(daisy(flower))
head(round(dd[, 1:6], 2))

#Visualizing distance matrices

install.packages("corrplot")
library("corrplot")
# Euclidean distance
corrplot(as.matrix(dist.eucl), is.corr = FALSE, method = "color")

# Visualize only the upper triangle
corrplot(as.matrix(dist.eucl), is.corr = FALSE, method = "color",
         order="hclust", type = "upper")

# Visualize only the lower triangle
corrplot(as.matrix(dist.eucl), is.corr = FALSE, method = "color",
         order="hclust", type = "lower")

# Use hierarchical clustering dendogram to visualize clusters
# of similar observations
plot(hclust(dist.eucl, method = "ward.D2"))

# Use heatmap
heatmap(as.matrix(dist.eucl), symm = TRUE,
        distfun = function(x) as.dist(x))


set.seed(123)
# Two-dimensional data format
df <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
            matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))
colnames(df) <- c("x", "y")
head(df)

# Compute k-means
set.seed(123)
km.res <- kmeans(df, 2, nstart = 25)
# Cluster number for each of the observations
km.res$cluster

plot(df, col = km.res$cluster, pch = 19, frame = TRUE,
     main = "K-means with k = 2")
points(km.res$centers, col = 1:2, pch = 8, cex = 3)


set.seed(123)
km.res <- kmeans(df, 4, nstart = 25)
plot(df, col = km.res$cluster, pch = 19, frame = TRUE,
     main = "K-means with k = 4")
points(km.res$centers, col = 1:4, pch = 8, cex = 3)

km.res

set.seed(123)
# K-means with nstart = 1
km.res <- kmeans(df, 4, nstart = 1)
km.res$tot.withinss

# K-means with nstart = 25
km.res <- kmeans(df, 4, nstart = 25)
km.res$tot.withinss

#Application of Kmeans Clustering on Real data
# Load the data set
data("USArrests")
# Remove any missing value (i.e, NA values for not available)
# That might be present in the data
df <- na.omit(USArrests)
# View the firt 6 rows of the data
head(df, n = 6)

#Descriptive Statistics
desc_stats <- data.frame(
  Min = apply(USArrests, 2, min), # minimum
  Med = apply(USArrests, 2, median), # median
  Mean = apply(USArrests, 2, mean), # mean
  SD = apply(USArrests, 2, sd), # Standard deviation
  Max = apply(USArrests, 2, max) # Maximum
)

desc_stats <- round(desc_stats, 1)
head(desc_stats)

df <- scale(df)
head(df)

library(factoextra)
set.seed(123)
fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)

# Compute k-means clustering with k = 4
set.seed(123)
km.res <- kmeans(df, 4, nstart = 25)
print(km.res)

#Plot the result
fviz_cluster(km.res, data = df)

#Compute PAM
library("cluster")
# Load data
data("USArrests")
# Scale the data and compute pam with k = 4
pam.res <- pam(scale(USArrests), 4)
head(pam.res$cluster)
clusplot(pam.res, main = "Cluster plot, k = 4", 
         color = TRUE)
fviz_cluster(pam.res)
#Silhouette plot
plot(silhouette(pam.res),  col = 2:5)

#Alternate Silhouette plot
fviz_silhouette(silhouette(pam.res)) 

# Compute silhouette
sil <- silhouette(pam.res)[, 1:3]
# Objects with negative silhouette
neg_sil_index <- which(sil[, 'sil_width'] < 0)
sil[neg_sil_index, , drop = FALSE]

#CLARA - Clustering Large Applications
set.seed(1234)
# Generate 500 objects, divided into 2 clusters.
x <- rbind(cbind(rnorm(200,0,8), rnorm(200,0,8)),
           cbind(rnorm(300,50,8), rnorm(300,50,8)))
head(x)
# Compute clara
clarax <- clara(x, 2, samples=50)
# Cluster plot
fviz_cluster(clarax, stand = FALSE, geom = "point",xlab="X",ylab="Y",
             pointsize = 1)
# Silhouette plot
plot(silhouette(clarax),  col = 2:3, main = "Silhouette plot")  
# Medoids
clarax$medoids

# Clustering
head(clarax$clustering, 20)

#Clusplot
set.seed(123)
# K-means clustering
km.res <- kmeans(scale(USArrests), 4, nstart = 25)
# Use clusplot function
library(cluster)
clusplot(scale(USArrests), km.res$cluster,  main = "Cluster plot",
         color=TRUE, labels = 2, lines = 0)

#Alternate method
clusplot(pam.res, main = "Cluster plot, k = 4", 
         color = TRUE)

library("factoextra")
# Visualize kmeans clustering
fviz_cluster(km.res, USArrests)

# Visualize pam clustering
pam.res <- pam(scale(USArrests), 4)
fviz_cluster(pam.res)

# Change frame type
fviz_cluster(pam.res, ellipse.type = "t")

# Remove ellipse fill color
# Change frame level
fviz_cluster(pam.res, ellipse.type = "t",
             ellipse.alpha = 0, ellipse.level = 0.7)

# Show point only
fviz_cluster(pam.res, geom = "point")

# Show text only
fviz_cluster(pam.res, geom = "text")

# Change the color and theme
fviz_cluster(pam.res) + 
  scale_color_brewer(palette = "Set2")+
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()