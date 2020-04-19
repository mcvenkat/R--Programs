install.packages("dplyr")
install.packages("RCurl")
install.packages("jsonlite")
install.packages("png")
install.packages("plotrix")
library(dplyr)
library(RCurl)
library(jsonlite)
library(png)
library(plotrix)

## Read in the player movement data for this specific play for this game
url = "http://stats.nba.com/stats/locations_getmoments/?eventid=308&gameid=0041400235"
url = "https://gist.githubusercontent.com/pravj/ea6b8ac5c14d41b81d87c7863b01ee3a/raw/a6d92935ae90a61524266c2c8640190abb2aa935/NBA-CHA-TOR-event.json"
 "
x<-getURL(url)
the.data<-fromJSON(x)

## read in the full court image and rasterize it in R
con <- url("http://tcbanalytics.com/uploads/fullcourt.png", open="rb")
rawpng <- readBin(con, what='raw', n=50000)
close(con)
the.court <- readPNG(rawpng)
plot(0:94, xlim=c(0,94), ylim=c(50,0),  type="n")
lim <- par()
rasterImage(the.court, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4] )

## Read in the home and away team player information
home.team <- the.data$home
away.team <- the.data$visitor

#This will capture the movement moment data and provide column 
#headers when we are done compiling all of the data.


moments <- the.data$moments
headers = c("team_id", "player_id", "x_loc", "y_loc", "radius", "game_clock", "shot_clock", "quarter")

quarters <- unlist(lapply(moments, function(x) x[1]))
game.clock <- unlist(lapply(moments, function(x) x[3]))
shot.clock <- unlist(lapply(moments, function(x) x[4]))

## Add the quarter, game clock and shot clock information to each moment 
moment.details <- lapply(moments, function(x) x[[6]])
x<-mapply(function(a,b,c,d) cbind(a,b,c,d), moment.details, game.clock, shot.clock, quarters)
all.movement<-do.call('rbind', x)
colnames(all.movement) <- headers
all.movement<-data.frame(all.movement)
all.movement<-all.movement[order(all.movement$game_clock),]

home.players <- home.team$players
away.players <- away.team$players
colnames(home.players)[3] <- "player_id"
colnames(away.players)[3] <- "player_id"

moments <- the.data$moments
headers = c("team_id", "player_id", "x_loc", "y_loc", "radius", "game_clock", "shot_clock", "quarter")

quarters <- unlist(lapply(moments, function(x) x[1]))
game.clock <- unlist(lapply(moments, function(x) x[3]))
shot.clock <- unlist(lapply(moments, function(x) x[4]))

## Add the quarter, game clock and shot clock information to each moment 
moment.details <- lapply(moments, function(x) x[[6]])
x<-mapply(function(a,b,c,d) cbind(a,b,c,d), moment.details, game.clock, shot.clock, quarters)
all.movement<-do.call('rbind', x)
colnames(all.movement) <- headers
all.movement<-data.frame(all.movement)
all.movement<-all.movement[order(all.movement$game_clock),]

home.players <- home.team$players
away.players <- away.team$players
colnames(home.players)[3] <- "player_id"
colnames(away.players)[3] <- "player_id"
#Next we add the player name information to each set of movement data.

## Add the player name information to each movement moment
home.movements<-merge(home.players, all.movement, by="player_id")
away.movements<-merge(away.players, all.movement, by="player_id")
home.movements <- home.movements[order(home.movements$game_clock, decreasing = TRUE),]
away.movements <- away.movements[order(away.movements$game_clock, decreasing = TRUE),]
all.movements <- rbind(home.movements, away.movements)

## Use James Harden as an example
james <- home.movements[which(home.movements$lastname == "Harden"),]
lines(james$x_loc, james$y_loc, type="b", col=cut(james$game_clock, breaks=3))

## Add the player name information to each movement moment
home.movements<-merge(home.players, all.movement, by="player_id")
away.movements<-merge(away.players, all.movement, by="player_id")
home.movements <- home.movements[order(home.movements$game_clock, decreasing = TRUE),]
away.movements <- away.movements[order(away.movements$game_clock, decreasing = TRUE),]
all.movements <- rbind(home.movements, away.movements)

## Use James Harden as an example
james <- home.movements[which(home.movements$lastname == "Harden"),]
lines(james$x_loc, james$y_loc, type="b", col=cut(james$game_clock, breaks=3))


#We can also calculate player distance traveled:

## Function to calculate player distance traveled
travelDist <- function(xloc, yloc){
  diffx <- diff(xloc)
  diffy <- diff(yloc)
  diffx2 <- diffx ^ 2
  diffy2 <- diffy ^ 2
  a<- diffx2 + diffy2
  b<-sqrt(a)
  return (sum(b)) 
}

travelDist(james$x_loc, james$y_loc)
[1] 197.4482   

## Calculate distance traveled for each player
player.groups <- group_by(all.movements, player_id)
dist.traveled.players <- summarise(player.groups, totalDist=travelDist(x_loc, y_loc))
all.players <- rbind(home.players, away.players)
player.travel <- merge(all.players, dist.traveled.players, by="player_id")
arrange(player.travel, desc(totalDist))

## Function to calculate player distance traveled
travelDist <- function(xloc, yloc){
  diffx <- diff(xloc)
  diffy <- diff(yloc)
  diffx2 <- diffx ^ 2
  diffy2 <- diffy ^ 2
  a<- diffx2 + diffy2
  b<-sqrt(a)
  return (sum(b)) 
}

travelDist(james$x_loc, james$y_loc)
[1] 197.4482   

## Calculate distance traveled for each player
player.groups <- group_by(all.movements, player_id)
dist.traveled.players <- summarise(player.groups, totalDist=travelDist(x_loc, y_loc))
all.players <- rbind(home.players, away.players)
player.travel <- merge(all.players, dist.traveled.players, by="player_id")
arrange(player.travel, desc(totalDist))