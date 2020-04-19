#for data visualization
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/factoextra")

my_data <- scale(iris[, -5])
get_clust_tendency(my_data, n = 50,
                   gradient = list(low = "steelblue",  high = "white"))

library(factoextra)
my_data <- scale(USArrests)
fviz_nbclust(my_data, kmeans, method = "gap_stat")

install.packages("NbClust")
library("NbClust")
set.seed(123)
res.nbclust <- NbClust(my_data, distance = "euclidean",
                       min.nc = 2, max.nc = 10, 
                       method = "complete", index ="all") 
#Visualize using factoextra:

factoextra::fviz_nbclust(res.nbclust) + theme_minimal()

#Compute and Visualize hierarchical clustering
my_data <- scale(iris[, -5])

# Enhanced hierarchical clustering, cut in 3 groups
res.hc <- eclust(my_data, "hclust", k = 3, graph = FALSE) 
# Visualize
fviz_dend(res.hc, rect = TRUE, show_labels = FALSE)

# Visualize the silhouette plot
fviz_silhouette(res.hc)

# Silhouette width of observations
sil <- res.hc$silinfo$widths[, 1:3]
# Objects with negative silhouette
neg_sil_index <- which(sil[, 'sil_width'] < 0)
sil[neg_sil_index, , drop = FALSE]

#Choose appropriate cluster algorithm
my_data <- scale(USArrests)
# Compute clValid
install.packages("clValid")
library("clValid")
intern <- clValid(my_data, nClust = 2:6, 
                  clMethods = c("hierarchical","kmeans","pam"),
                  validation = "internal")
# Summary
summary(intern)