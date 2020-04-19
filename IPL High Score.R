# From IPLT20.com get the highest individual score for each year. Input this in a excel work sheet
#2017	126
#2016	129
#2015	133
#2013	175
#2012	128
#2011	120
#2010	127
#2009	114
#2008	158

# Based on this try to predict the highest individual score
#To read data from .xlsx file
install.packages("xlsx")
library(xlsx)

file <- system.file("scores", "IPL High Score.xlsx", package = "xlsx")
res<-read.xlsx(file,1)#Reading first sheet

head(res)
df<-data.frame(res)
df
library(ggplot2)

#Histogram of the scores
hist(df$Score,breaks="Sturges",freq=NULL,include.lowest = TRUE,right=TRUE,density=NULL)

#Draw a 3d Scatterplot of the scores
scatterplot3d::scatterplot3d(df$Year,y=df$Score,z=NULL,color = par("col"))

# Predict next score based on values
mod=lm(df$Score ~df$Year)
gr<-predict(mod,df,se.fit=FALSE,interval = "prediction")
plot(gr)

dr<-data.frame(gr)
ggplot(dr,aes(x=df$Year,y=df$Score))+
  geom_point()+
  geom_smooth(fullrange=TRUE,mapping=NULL,data=dr,stat="smooth")
  xlim(1,11)

#The graph continues to be close to 125 which would be the average next highest score