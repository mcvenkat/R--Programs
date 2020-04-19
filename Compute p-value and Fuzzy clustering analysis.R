#Compute p-value for hierarchical clustering
install.packages("pvclust")
library(pvclust)

# Data preparation
set.seed(123)
data("lung")
ss <- sample(1:73, 30) # extract 20 samples out of
my_data <- lung[, ss]

# Compute pvclust
res.pv <- pvclust(my_data, method.dist="cor", 
                  method.hclust="average", nboot = 10)

# Default plot
plot(res.pv, hang = -1, cex = 0.5)
pvrect(res.pv)

#Fuzzy Clustering Analysis
install.packages("cluster")
install.packages("e1071")

#for data visualization
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/factoextra")


library(cluster)
library(e1071)
library(factoextra)

set.seed(123)
# Load the data
data("USArrests")
# Subset of USArrests
ss <- sample(1:50, 20)
df <- scale(USArrests[ss,])
# Compute fuzzy clustering
res.fanny <- fanny(df, 4)
# Cluster plot using fviz_cluster()
# You can use also : clusplot(res.fanny)
fviz_cluster(res.fanny, frame.type = "norm",
             frame.level = 0.68)
# Silhouette plot
fviz_silhouette(res.fanny, label = TRUE)

print(res.fanny)

res.fanny$membership

# Visualize using corrplot
library(corrplot)
corrplot(res.fanny$membership, is.corr = FALSE)

# Dunn's partition coefficient
res.fanny$coeff

# Observation groups
res.fanny$clustering

set.seed(123)

cm <- cmeans(df, 4)
cm
fviz_cluster(list(data = df, cluster=cm$cluster), ellipse.type = "norm",
             ellipse.level = 0.68)

