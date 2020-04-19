library(ggplot2)
library(gganimate)
install.packages("gapminder")
library(gapminder)
?gganimate
theme_set(theme_bw())
p <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, color = continent, frame = year)) +
  geom_point() +
  scale_x_log10()
gganimate(p)

library(ggplot2)
install.packages("rvg")
library(rvg)
install.packages("gdtools")
library(gdtools)
install.packages("ggiraph")
library(ggiraph)

mytheme_main <- theme( panel.background = element_blank(), 
                       panel.grid.major = element_line(colour = "#dddddd"), 
                       axis.ticks = element_line(colour = "#dddddd") )

mytheme_map <- theme(
  panel.background = element_blank(), axis.title.x = element_blank(),
  axis.text = element_blank(), axis.line.x = element_blank(),
  axis.line.y = element_blank(), axis.title.y = element_blank(),
  axis.ticks.x = element_blank(), axis.ticks.y = element_blank() )

dataset$tooltip <- row.names(dataset)

# geom_point_interactive example
gg_point_1 <- ggplot(dataset, aes(x = disp, y = qsec, 
                                  color = wt, tooltip = tooltip ) ) + 
  geom_point_interactive(size=3)

# htmlwidget call
ggiraph(code = {print(gg_point_1 + mytheme_main)}, width = 7, height = 6)

library(raster)
states    <- c('Tamil Nadu', 'Kerala', 'Karnataka', 'Andhra Pradesh', 'Telangana', 'Odisha', 'Madhya Pradesh', 'Jharkhand', 'Maharashtra','Bihar','Uttar Pradesh')
provinces <- c("British Columbia", "Alberta")

india <- getData("GADM",country="INDIA",level=1)
canada <- getData("GADM",country="CAN",level=1)

india.states <- india[india$NAME_1 %in% states,]
ca.provinces <- canada[canada$NAME_1 %in% provinces,]

india.bbox <- bbox(india.states)
ca.bbox <- bbox(ca.provinces)
library(ggplot2)
ggplot(india.states,aes(x=long,y=lat,group=group))+
  geom_path()+
  geom_path(data=india.states)+
  coord_map()


India <- getData("GADM", country = "India", level = 2)  
Karnataka <- subset(India, NAME_1 == "Karnataka")
map <- fortify(Karnataka);  
map$id <- as.integer(map$id);  
dat <- data.frame(id = 216:242, district = Karnataka@data$NAME_2);  
map_df <- inner_join(map, dat, by = "id");  
centers <- data.frame(gCentroid(Karnataka, byid = TRUE));  
centers$state <- dat$district;  


ggplot() +
  geom_map(data = map_df, map = map_df,
           aes(map_id = id, x = long, y = lat, group = group),
           color = "#ffffff", fill = "#bbbbbb", size = 0.25) +
  geom_text(data = centers, aes(label = state, x = x, y = y), size = 2) +
  coord_map() + labs(x = "", y = "", title = "Districts of Karnataka") 



install.packages("sp",force=T)
install.packages("RColorBrewer")
library(sp)
library("RColorBrewer")
library(raster)
India <- getData("GADM", country = "India", level = 3)  
spplot(India, "NAME_1", scales=list(draw=T), colorkey=F, main="India")

# map of India with states coloured with an arbitrary fake data
India$NAME_1 = as.factor(India$NAME_1)
India$fake.data = runif(length(India$NAME_1))
spplot(India,"NAME_1",  col.regions=rgb(1,India$fake.data,1), colorkey=T, main="Indian States")


# map of West Bengal ( or any other state )
wb1 = (India[India$NAME_1=="West Bengal",])
spplot(wb1,"NAME_1", col.regions=rgb(0,0,1), main = "West Bengal, India",scales=list(draw=T), colorkey =F)

# map of Karnataka ( or any other state )
kt1 = (India[India$NAME_1=="Karnataka",])
spplot(kt1,"NAME_1", col.regions=rgb(0,1,0), main = "Karnataka, India",scales=list(draw=T), colorkey =F)


# plotting districts of a State, in this case West Bengal
wb2 = (India[India$NAME_1=="West Bengal",])
spplot(wb2,"NAME_1", main = "West Bengal Districts", colorkey =F)

#To identify each district with a beautiful colour we can use the following commands :
# colouring the districts with rainbow of colours
wb2$NAME_2 = as.factor(wb2$NAME_2)
col = rainbow(length(levels(wb2$NAME_2)))
spplot(wb2,"NAME_2",  col.regions=col, colorkey=T)

# colouring the districts with some simulated, fake data
wb2$NAME_2 = as.factor(wb2$NAME_2)
wb2$fake.data = runif(length(wb2$NAME_1)) 
spplot(wb2,"NAME_2",  col.regions=rgb(0,wb2$fake.data, 0), colorkey=T)

# colouring the districts with range of colours
col_no = as.factor(as.numeric(cut(wb2$fake.data, c(0,0.2,0.4,0.6,0.8,1))))
levels(col_no) = c("<20%", "20-40%", "40-60%","60-80%", ">80%")
wb2$col_no = col_no
myPalette = brewer.pal(5,"Greens")
spplot(wb2, "col_no", col=grey(.9), col.regions=myPalette, main="District Wise Data")

#plotting districts and sub-divisions / taluk
wb3 = (India[India$NAME_1=="West Bengal",])
wb3$NAME_3 = as.factor(wb3$NAME_3)
col = rainbow(length(levels(wb3$NAME_3)))
spplot(wb3,"NAME_3", main = "Taluk, District - West Bengal", colorkey=T,col.regions=col,scales=list(draw=T))

n24pgns3 = (wb3[wb3$NAME_2=="North 24 Parganas",])
spplot(n24pgns3,"NAME_3", colorkey =F, scales=list(draw=T), main = "24 Pgns (N) West Bengal")

# now draw the map of Basirhat subdivision
# recreate North 24 Parganas data
n24pgns3 = (wb3[wb3$NAME_2=="North 24 Parganas",])
basirhat3 = (n24pgns3[n24pgns3$NAME_3=="Basirhat",])
spplot(basirhat3,"NAME_3", colorkey =F, scales=list(draw=T), main = "Basirhat,24 Pgns (N) West Bengal")

# zoomed in data
wb2 = (India[India$NAME_1=="West Bengal",])
wb2$NAME_2 = as.factor(wb2$NAME_2)
col = rainbow(length(levels(wb2$NAME_2)))
spplot(wb2,"NAME_2",  col.regions=col,scales=list(draw=T),ylim=c(23.5,25),xlim=c(87,89), colorkey=T)


# Marking towns on the GADM maps
# On a map of West Bengal, we will now mark some towns
#
#load level 2 india data downloaded from http://gadm.org/country
# 

library(ggmap) # -- for geocoding, obtaining city locations
ind2 <- getData("GADM", country = "India", level = 2)  

# plotting districts of a State, in this case West Bengal
wb2 = (ind2[ind2$NAME_1=="West Bengal",])

nam = c("Purulia","Bankura","Midnapur")
pos = geocode(nam)
tlat = pos$lat+0.05    # -- the city name will be above the marker
cities = data.frame(nam, pos$lon,pos$lat,tlat)
names(cities)[2] = "lon"
names(cities)[3] = "lat"

text1 = list("panel.text", cities$lon, cities$tlat, cities$nam,col="red", cex = 0.75)
mark1 = list("panel.points", cities$lon, cities$lat, col="blue")
text2 = list("panel.text",87.0,26.0,"GADM map", col = "dark green", cex = 1.2)
spplot(wb2, "NAME_1",
       sp.layout=list(text1,mark1, text2),
       main="West Bengal Districts",
       colorkey=FALSE, scales=list(draw=TRUE))

#Geospatial representation of economic data
require(animation)
require(sp)
library(maptools)
require(RColorBrewer)
require(classInt)
require(rgdal)
library(ggmap)
library(ggplot2)
library(raster)
library(rgeos)
require(gpclib)
library(plyr)
gpclibPermit()

ind1 <- getData("GADM", country = "India", level = 1)  

gadm$regions = as.factor(regions)
# create a color pallette with rainbow layers
color = rainbow(length(levels(gadm$regions)))
# plot gadm file according to regios data with established color pattern
spplot(gadm, "regions", col.regions = color, main = "Russian regions")



library(maptools)
library(ggplot2)
library(ggmap)

# read administrative boundaries (change folder appropriately)
eurMap <- readShapePoly(fn="NUTS_2010_60M_SH/Shape/data/NUTS_RG_60M_2010")

# read downloaded data (change folder appropriately)
eurEdu <- read.csv("educ_thexp_1_Data.csv", stringsAsFactors = F)
eurEdu$Value <- as.double(eurEdu$Value) #format as numeric

# merge map and data
eurEduMapDf <- merge(eurMapDf, eurEdu, by.x="id", by.y="GEO")
eurEduMapDf <- eurEduMapDf[order(eurEduMapDf$order),]

#limit data to main Europe
europe.limits <- geocode(c("Cape Fligely, Rudolf Island, Franz Josef Land, Russia", "Gavdos, Greece", "Faja Grande, Azores", "Severny Island, Novaya Zemlya, Russia"))

eurEduMapDf <- subset(eurEduMapDf, long > min(europe.limits$lon) & long < max(europe.limits$lon) & lat > min(europe.limits$lat) & lat < max(europe.limits$lat))

# ggplot mapping
# data layer
m0 <- ggplot(data=eurEduMapDf)
# empty map (only borders)
m1 <- m0 + geom_path(aes(x=long, y=lat, group=group), color='gray') + coord_equal()

# fill with education expenditure data
m2 <- m1 + geom_polygon(aes(x=long, y=lat, group=group, fill=Value))

# inverse order (to have visible borders)
m0 <- ggplot(data=eurEduMapDf)
m1 <- m0 + geom_polygon(aes(x=long, y=lat, group=group, fill=Value)) + coord_equal()
m2 <- m1 + geom_path(aes(x=long, y=lat, group=group), color='black')
m2

# over a GoogleMap (not working if not correctly projected)
map <- get_map(location = 'Europe', zoom=4)
m0 <- ggmap(map)
m1 <- m0 + geom_polygon(aes(x=long, y=lat, group=group, fill=Value), data=eurEduMapDf, alpha=.9)
m2 <- m1 + geom_path(aes(x=long, y=lat, group=group), data=eurEduMapDf, color='black')

# add text
library(doBy)
txtVal <- summaryBy(long + lat + Value ~ id, data=eurEduMapDf, FUN=mean, keep.names=T)
m3 <- m2 + geom_text(aes(x=long, y=lat, label=Value), data=txtVal, col="yellow", cex=3)