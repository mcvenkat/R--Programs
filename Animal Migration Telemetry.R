install.packages("momentuHMM")
library(momentuHMM)
install.packages("rgdal")
library(rgdal)


URL <- paste0("https://www.datarepository.movebank.org/bitstream/handle/",
              "10255/move.373/Elliptical%20Time-Density%20Model%20%28Wall%",
              "20et%20al.%202014%29%20African%20Elephant%20Dataset%20%",
              "28Source-Save%20the%20Elephants%29.csv")
rawData <- read.csv(url(URL))

# select and rename relevant columns
rawData <- rawData[,c(11,3,4,5,6)]
colnames(rawData) <- c("ID","time","lon","lat","temp")
# only keep first track
rawData <- subset(rawData,ID==unique(ID)[1])
head(rawData)

# convert times from factors to POSIX
rawData$time <- as.POSIXct(rawData$time,tz="GMT")
# project to UTM coordinates using package rgdal

llcoord <- SpatialPoints(rawData[,3:4],
                         proj4string=CRS("+proj=longlat +datum=WGS84"))
utmcoord <- spTransform(llcoord,CRS("+proj=utm +zone=30 ellps=WGS84"))
#longitude and latitude coords for satellite plot
utmcoord <- spTransform(llcoord,CRS("+proj=longlat"))

lldata<-data.frame(ID=rawData$ID,x=attr(utmcoord,"coords")[,1],
                   y=attr(utmcoord,"coords")[,2])

plotSat(lldata,zoom=8)
# add UTM locations to data frame
rawData$x <- attr(utmcoord,"coords")[,1]
rawData$y <- attr(utmcoord,"coords")[,2]

# initial parameters for crawl fit
inits <- list(a = c(rawData$x[1],0,rawData$y[1],0),
              P = diag(c(5000^2, 10*3600^2, 5000^2, 10*3600^2)))
# fit crawl model
crwOut <- crawlWrap(obsData=rawData, timeStep="hour", initial.state=inits,
                    theta=c(4,-10), fixPar=c(NA,NA))

# create momentuHMMData object from crwData object
elephantData <- prepData(data=crwOut, covNames="temp")
# add cosinor covariate based on hour of day
elephantData$hour <- as.integer(strftime(elephantData$time, format = "%H", tz="GMT"))

#acf plot
acf(elephantData$step[!is.na(elephantData$step)],lag.max=300)

# label states
stateNames <- c("encamped","exploratory")
# distributions for observation processes
dist = list(step = "gamma", angle = "wrpcauchy")
# initial parameters
Par0_m1 <- list(step=c(100,500,100,200),angle=c(0.3,0.7))
# fit model
m1 <- fitHMM(data = elephantData, nbStates = 2, dist = dist, Par0 = Par0_m1,
             estAngleMean = list(angle=FALSE), stateNames = stateNames)
# formula for transition probabilities
formula <- ~ temp * cosinor(hour, period = 24)
# initial parameters (obtained from nested model m1)
Par0_m2 <- getPar0(model=m1, formula=formula)
# fit model
m2 <- fitHMM(data = elephantData, nbStates = 2, dist = dist, Par0 = Par0_m2$Par,
             beta0=Par0_m2$beta, stateNames = stateNames, formula=formula)

# formulas for parameters of state-dependent observation distributions
DM <- list(step = list(mean = ~ temp * cosinor(hour, period = 24),
                       sd = ~ temp * cosinor(hour, period = 24)),
           angle = list(concentration = ~ temp))
# initial parameters (obtained from nested model m2)
Par0_m3 <- getPar0(model=m2, formula=formula, DM=DM)
# fit model
m3 <- fitHMM(data = elephantData, nbStates = 2, dist = dist, Par0 = Par0_m3$Par,
             beta0 = Par0_m3$beta, DM = DM, stateNames = stateNames,
             formula = formula)

# decode most likely state sequence
states <- viterbi(m3)
# derive percentage of time spent in each state
table(states)/nrow(elephantData)

plot(m3, plotCI = TRUE, covs = data.frame(hour=12))

# compute pseudo-residuals for the steps and the angles
pr <- pseudoRes(m3)
# plot the ACF of step pseudo-residuals
acf(pr$stepRes[!is.na(pr$stepRes)],lag.max = 300)



