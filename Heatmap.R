if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/factoextra")

install.packages("cluster")

install.packages("mtcars")
library(mtcars)
df <- as.matrix(scale(mtcars))

data("USArrests")
my_data <- USArrests
# Remove any missing value (i.e, NA values for not available)
my_data <- na.omit(my_data)

df <- as.matrix(scale(USArrests))

#Default heat map
warnings()

# Use custom colors
col<- colorRampPalette(c("red", "white", "blue"))(256)
heatmap(scale(as.matrix(USArrests)), scale = "none",
        col =  col)

# Enhanced heat map
install.packages("gplots")
library("gplots")
heatmap.2(df, scale = "none", col = bluered(100), 
          trace = "none", density.info = "none")

#Interactive heatmap
if (!require("devtools")) install.packages("devtools")
devtools::install_github("rstudio/d3heatmap")
library("d3heatmap")
d3heatmap(scale(USArrests), colors = "RdBu",
          k_row = 4, k_col = 2)

#Enhancing heatmaps using dendextend
library(dendextend)
# order for rows
Rowv  <- USArrests %>% scale %>% dist %>% hclust %>% as.dendrogram %>%
  set("branches_k_color", k = 3) %>% set("branches_lwd", 1.2) %>%
  ladderize
# Order for columns
# We must transpose the data
Colv  <- USArrests %>% scale %>% t %>% dist %>% hclust %>% as.dendrogram %>%
  set("branches_k_color", k = 2, value = c("orange", "blue")) %>%
  set("branches_lwd", 1.2) %>%
  ladderize

heatmap(scale(USArrests), Rowv = Rowv, Colv = Colv,
        scale = "none")

library(gplots)
heatmap.2(scale(USArrests), scale = "none", col = bluered(100), 
          Rowv = Rowv, Colv = Colv,
          trace = "none", density.info = "none")

library("d3heatmap")
d3heatmap(scale(USArrests), colors = "RdBu",
          Rowv = Rowv, Colv = Colv)

#Complex Heatmap
if (!require("devtools")) install.packages("devtools")
devtools::install_github("jokergoo/ComplexHeatmap")

library("ComplexHeatmap")
Yes

#Single Heatmap
data(mtcars)
head(mtcars[, 1:6])
df <- scale(mtcars)
heatmap(df, name = "mtcars")

library("circlize")
heatmap(df, name = "mtcars",
        col = colorRamp2(c(-2, 0, 2), c("green", "white", "red")))

library("RColorBrewer")
heatmap(df, name = "mtcars",
        col = colorRamp2(c(-2, 0, 2), brewer.pal(n=3, name="RdBu")))

heatmap(df, name = "mtcars", col = mycol,
        column_title = "Column title",
        row_title = "Row title")


# install.packages("dendextend")
library(dendextend)
row_dend = hclust(dist(df)) # row clustering
col_dend = hclust(dist(t(df))) # column clustering
heatmap(df, name = "mtcars", col = colors(distinct = TRUE),
        cluster_rows = color_branches(row_dend, k = 4),
        cluster_columns = color_branches(col_dend, k = 2))

heatmap(df, name = "mtcars", clustering_distance_rows = "pearson",
        clustering_distance_columns = "pearson")

# Split by combining multiple variables
heatmap(df, name ="mtcars", col = colors(distinct = T),
        split = data.frame(cyl = mtcars$cyl, am = mtcars$am))

heatmap(df, name = "mtcars", col = colors(distinct = T), 
        split = mtcars$cyl )

warnings()

# install.packages("cluster")
library("cluster")
set.seed(2)
pa = pam(df, k = 3)
heatmap(df, name = "mtcars", col = colors(distinct=TRUE),
        split = paste0("pam", pa$clustering))

heatmap(df, name = "mtcars", show_row_names = TRUE)


