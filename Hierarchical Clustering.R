if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/factoextra")

install.packages("cluster")
install.packages("dendextend")

library(cluster)
library(factoextra)
library(dendextend)

# Load the data set
data("USArrests")
# Remove any missing value (i.e, NA values for not available)
# That might be present in the data
df <- na.omit(USArrests)
# View the firt 6 rows of the data
head(df, n = 6)

#Descriptive Statistics
desc_stats <- data.frame(
  Min = apply(df, 2, min), # minimum
  Med = apply(df, 2, median), # median
  Mean = apply(df, 2, mean), # mean
  SD = apply(df, 2, sd), # Standard deviation
  Max = apply(df, 2, max) # Maximum
)
desc_stats <- round(desc_stats, 1)
head(desc_stats)

df <- scale(df)
head(df)

# Dissimilarity matrix
d <- dist(df, method = "euclidean")
# Hierarchical clustering using Ward's method
res.hc <- hclust(d, method = "ward.D2" )
# Plot the obtained dendrogram
plot(res.hc, cex = 0.6, hang = -1)

# Agglomerative Nesting (Hierarchical Clustering)
agnes(x, metric = "euclidean", stand = FALSE, method = "average")
# DIvisive ANAlysis Clustering
diana(x, metric = "euclidean", stand = FALSE)


library("cluster")
# Compute agnes()
res.agnes <- agnes(df, method = "ward")
# Agglomerative coefficient
res.agnes$ac

# Plot the tree using pltree()
pltree(res.agnes, cex = 0.6, hang = -1,
       main = "Dendrogram of agnes") 

# plot.hclust()
plot(as.hclust(res.agnes), cex = 0.6, hang = -1)
# plot.dendrogram()
plot(as.dendrogram(res.agnes), cex = 0.6, 
     horiz = TRUE)

# Compute diana()
res.diana <- diana(df)
# Plot the tree
pltree(res.diana, cex = 0.6, hang = -1,
       main = "Dendrogram of diana")

# Divise coefficient; amount of clustering structure found
res.diana$dc

# plot.hclust()
plot(as.hclust(res.diana), cex = 0.6, hang = -1)
# plot.dendrogram()
plot(as.dendrogram(res.diana), cex = 0.6, 
     horiz = TRUE)

# Cut tree into 4 groups
grp <- cutree(res.hc, k = 4)
# Number of members in each cluster
table(grp)

plot(res.hc, cex = 0.6)
rect.hclust(res.hc, k = 4, border = 2:5)

fviz_cluster(list(data = df, cluster = grp))

#Hierarchical clustering and correlation based distance

res.cor <- cor(t(df), method = "pearson")
d.cor <- as.dist(1 - res.cor)
plot(hclust(d.cor, method = "ward.D2"), cex = 0.6)

#Comparing two dendrograms
# Subset containing 10 rows
set.seed(123)
ss <- sample(1:50, 10)
df <- df[ss,]

# Compute distance matrix
res.dist <- dist(df, method = "euclidean")
# Compute 2 hierarchical clusterings
hc1 <- hclust(res.dist, method = "average")
hc2 <- hclust(res.dist, method = "ward.D2")
# Create two dendrograms
dend1 <- as.dendrogram (hc1)
dend2 <- as.dendrogram (hc2)
# Create a list of dendrograms
dend_list <- dendlist(dend1, dend2)

#Tanglegram
tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, # Turn-off dashed lines
           common_subtrees_color_lines = FALSE, # Turn-off line colors
           common_subtrees_color_branches = TRUE, # Color common branches 
           main = paste("entanglement =", round(entanglement(dend_list), 2))
)

#Correlation matrix between a list of dendrogram

# Cophenetic correlation coefficient
cor_cophenetic(dend1, dend2)

# Baker correlation coefficient
cor_bakers_gamma(dend1, dend2)

# Subset data
set.seed(123)
ss <- sample(1:150, 10 )
# Create multiple dendrograms by chaining
dend1 <- df %>% dist %>% hclust("com") %>% as.dendrogram
dend2 <- df %>% dist %>% hclust("single") %>% as.dendrogram
dend3 <- df %>% dist %>% hclust("ave") %>% as.dendrogram
dend4 <- df %>% dist %>% hclust("centroid") %>% as.dendrogram
# Compute correlation matrix
dend_list <- dendlist("Complete" = dend1, "Single" = dend2,
                      "Average" = dend3, "Centroid" = dend4)
cors <- cor.dendlist(dend_list)
# Print correlation matrix
round(cors, 2)

# Visualize the correlation matrix using corrplot package
library(corrplot)
corrplot(cors, "pie", "lower")