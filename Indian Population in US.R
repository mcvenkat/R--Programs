install.packages("xlsx")
library(xlsx)

file <- system.file("population", "Indian Population in US.xlsx", package = "xlsx",full.names=T)
file.exists("Indian Population in US.xlsx")
res<-read.xlsx(file,1)#Reading first sheet
head(res)
df<-data.frame(res)
