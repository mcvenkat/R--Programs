if (!require("cricketr")){ 
  install.packages("cricketr") 
}

if (!require("forecast")){ 
  install.packages("forecast") 
}

if (!require("TTR")){ 
  install.packages("TTR") 
}
library(TTR)

library(forecast)
library(cricketr)

bradman <- getPlayerData(4188,dir="C:/Users/vchakravart3/Documents",file="bradman.csv",type="batting",homeOrAway=c(1,2),
                          result=c(1,2,4))
par(mfrow=c(1,3))
par(mar=c(4,4,2,2))
batsmanRunsFreqPerf("./bradman.csv","Donald Bradman")
batsmanMeanStrikeRate("./bradman.csv","Donald Bradman")
batsmanRunsRanges("./bradman.csv","Donald Bradman")
batsman4s("./bradman.csv","bradman")
batsman6s("./bradman.csv","bradman")
batsmanDismissals("./bradman.csv","bradman")
battingPerf3d("./bradman.csv","Donald bradman")
batsmanAvgRunsGround("./bradman.csv","Donald Bradman")
batsmanAvgRunsOpposition("./bradman.csv","Bradman")
batsmanRunsLikelihood("./bradman.csv","Donald Bradman")
batsmanPerfBoxHist("./bradman.csv","Donald bradman")
getPlayerDataSp(4188,tdir="C:/Users/vchakravart3/Documents",tfile="bradmansp.csv",ttype="batting")



gavaskar <- getPlayerData(28794,dir="C:/Users/vchakravart3/Documents",file="gavaskar.csv",type="batting",homeOrAway=c(1,2),
                       result=c(1,2,4))
par(mfrow=c(1,3))
par(mar=c(4,4,2,2))
batsmanRunsFreqPerf("./gavaskar.csv","Sunil Gavaskar")
batsmanMeanStrikeRate("./gavaskar.csv","Sunil Gavaskar")
batsmanRunsRanges("./gavaskar.csv","Sunil Gavaskar")
batsman4s("./gavaskar.csv","gavaskar")
batsman6s("./gavaskar.csv","gavaskar")
batsmanDismissals("./gavaskar.csv","gavaskar")
battingPerf3d("./gavaskar.csv","Sunil Gavaskar")
batsmanAvgRunsGround("./gavaskar.csv","Sunil Gavaskar")
batsmanAvgRunsOpposition("./gavaskar.csv","Gavaskar")
batsmanRunsLikelihood("./gavaskar.csv","Sunil Gavaskar")
batsmanPerfBoxHist("./gavaskar.csv","Sunil Gavaskar")
getPlayerDataSp(28794,tdir="C:/Users/vchakravart3/Documents",tfile="gavaskarsp.csv",ttype="batting")


tendulkar <- getPlayerData(35320,dir="C:/Users/vchakravart3/Documents",file="tendulkar.csv",type="batting",homeOrAway=c(1,2),
                          result=c(1,2,4))
par(mfrow=c(1,3))
par(mar=c(4,4,2,2))
batsmanRunsFreqPerf("./tendulkar.csv","Sachin Tendulkar")
batsmanMeanStrikeRate("./tendulkar.csv","Sachin Tendulkar")
batsmanRunsRanges("./tendulkar.csv","Sachin Tendulkar")
batsman4s("./tendulkar.csv","tendulkar")
batsman6s("./tendulkar.csv","tendulkar")
batsmanDismissals("./tendulkar.csv","tendulkar")
battingPerf3d("./tendulkar.csv","Sachin Tendulkar")
batsmanAvgRunsGround("./tendulkar.csv","Sachin Tendulkar")
batsmanAvgRunsOpposition("./tendulkar.csv","Tendulkar")
batsmanRunsLikelihood("./tendulkar.csv","Sachin Tendulkar")
batsmanPerfBoxHist("./tendulkar.csv","Sachin Tendulkar")
getPlayerDataSp(35320,tdir="C:/Users/vchakravart3/Documents",tfile="tendulkarsp.csv",ttype="batting")


batsmanContributionWonLost("bradmansp.csv","bradman")
batsmanContributionWonLost("gavaskarsp.csv","gavaskar")
batsmanContributionWonLost("tendulkarsp.csv","tendulkar")


batsmanPerfHomeAway("bradmansp.csv","bradman")
batsmanPerfHomeAway("gavaskarsp.csv","gavaskar")
batsmanPerfHomeAway("tendulkarsp.csv","tendulkar")

frames <- list("./bradman.csv","./gavaskar.csv","./tendulkar.csv")
names <- list("bradman","gavaskar","tendulkar")
relativeBatsmanSR(frames,names)

frames <- list("./bradman.csv","./gavaskar.csv","./tendulkar.csv")
names <- list("bradman","gavaskar","tendulkar")
relativeRunsFreqPerf(frames,names)


batsmanMovingAverage("./bradman.csv","Donald Bradman")
batsmanMovingAverage("./gavaskar.csv","Sunil Gavaskar")
batsmanMovingAverage("./tendulkar.csv","Sachin Tendulkar")


batsmanPerfForecast("./bradman.csv","Donald Bradman")
batsmanPerfForecast("./gavaskar.csv","Sunil gavaskar")
batsmanPerfForecast("./tendulkar.csv","Sachin tendulkar")
