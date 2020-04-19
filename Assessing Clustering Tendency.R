#for data visualization
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/factoextra")

#for assessing clustering tendency
install.packages("clustertend")

#for visual assessment of clustering tendency
install.packages("seriation")

library(factoextra)
library(clustertend)
library(seriation)

# Load the data
data("faithful")
df <- faithful
head(df)

ggplot(df, aes(x=eruptions, y=waiting)) +
  geom_point() +  # Scatter plot
  geom_density_2d() # Add 2d density estimation

# Generate randomly uniform dataset
set.seed(123)
n <- nrow(df)
random_df <- data.frame(
  x = runif(nrow(df), min(df$eruptions), max(df$eruptions)),
  y = runif(nrow(df), min(df$waiting), max(df$waiting)))
# Plot the data
ggplot(random_df, aes(x, y)) + geom_point()

#K-means on faithful dataset
set.seed(123)
# K-means on faithful dataset
km.res1 <- kmeans(df, 2)
fviz_cluster(list(data = df, cluster = km.res1$cluster),
             ellipse.type = "norm", geom = "point", stand = FALSE)

# K-means on the random dataset
km.res2 <- kmeans(random_df, 2)
fviz_cluster(list(data = random_df, cluster = km.res2$cluster),
             ellipse.type = "norm", geom = "point", stand = FALSE)

# Hierarchical clustering on the random dataset
fviz_dend(hclust(dist(random_df)), k = 2,  cex = 0.5)

# Compute Hopkins statistic for faithful dataset
set.seed(123)
hopkins(faithful, n = nrow(faithful)-1)

# Compute Hopkins statistic for a random dataset
set.seed(123)
hopkins(random_df, n = nrow(random_df)-1)

#Visual Assessment of cluster Tendency: VAT
# faithful data: ordered dissimilarity image
df_scaled <- scale(faithful)
df_dist <- dist(df_scaled) 
dissplot(df_dist)

# faithful data: ordered dissimilarity image
random_df_scaled <- scale(random_df)
random_df_dist <- dist(random_df_scaled) 
dissplot(random_df_dist)

set.seed(123)
km.res <- kmeans(scale(faithful), 2)
dissplot(df_dist, labels = km.res$cluster)

#A single function for Hopkins tendency and VAT
# Cluster tendency
clustend <- get_clust_tendency(scale(faithful), 100)
# Hopkins statistic
clustend$hopkins_stat

# Customize the plot
clustend$plot + 
  scale_fill_gradient(low = "steelblue", high = "white")
