install.packages("tidyverse")
library(tidyverse)
install.packages("magrittr")
library(magrittr)


# MAP WORLD

map_data("world") %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon()

# MAP INDIA

map_data("world") %>%
  filter(region == 'India') %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon()

?map_data

map_data("world2") %>%
  filter(region == 'India') %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon()


# packages for manipulating and mapping spatial data
library(maptools)
# set the working directory
# replace dir with your own path
#setwd("dir")
# read the shapefile with the digital boundaries
india <- readShapePoly("india_st")
summary(india)
attributes(india)
# read the literacy file
literacy <- read.csv("literacy.csv")
summary(literacy)

rrt <- literacy$X2001
brks <- quantile(rrt, seq(0,1,1/7), na.rm=T)
cols <- grey(2:(length(brks))/length(brks))
dens <- (2:length(brks))*3

plot(india,col=cols[findInterval(rrt, brks, all.inside=TRUE)])



install.packages("rgdal") #needed to load shapefile
library(rgdal)

# obtain India administrative shapefiles and unzip
download.file("http://biogeo.ucdavis.edu/data/diva/adm/IND_adm.zip", 
              destfile = "IND_adm.zip")
unzip("IND_adm.zip", overwrite = TRUE)
getwd()
# load shapefiles
?readOGR
india <- readOGR(dsn = "C:/Users/vchakravart3/Documents", "IND_adm1")

# check they've loaded correctly with a plot
plot(india)

# all fine. Let's plot an example variable using ggplot2
install.packages("ggplot2")
library(ggplot2)
install.packages("rgeos")  # for fortify() with SpatialPolygonsDataFrame types
library(rgeos)
install.packages("maptools")#needed for fortify()
library(maptools)

india@data$test <- sample(65000:200000000, size = nrow(india@data),
                          replace = TRUE)

# breaks the shapefile down to points for compatibility with ggplot2
indiaF <- fortify(india, region = "ID_1")
indiaF <- merge(indiaF, india, by.x = "id", by.y = "ID_1")

# plots the polygon and fills them with the value of 'test'
ggplot() +
  geom_polygon(data = indiaF, aes(x = long, y = lat, group = group,
                                  fill = test)) +
  coord_equal()

dput(df)
structure(list(State = c("Kerala", "Bihar", "Assam", "Chandigarh"
), A = c("39", "6", "55", "66"), B = c("5", "54", "498", "11"
), C = c("34", "13", "89", "44"), D = c("29", "63", "15", "33"
), E = c("11", "81", "48", "71")), .Names = c("State", "A", "B", 
                                              "C", "D", "E"), row.names = c("Kerala", "Bihar", "Assam", "Chandigarh"
                                              ), class = "data.frame") 
