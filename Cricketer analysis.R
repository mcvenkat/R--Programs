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
?cricketr
kohli <- getPlayerData(253802,dir="C:/Users/vchakravart3/Documents",file="kohli.csv",type="batting",homeOrAway=c(1,2),
                       result=c(1,2,4))
par(mfrow=c(1,3))
par(mar=c(4,4,2,2))
getwd()
?getPlayerData
?batsmanRunsFreqPerf
batsmanRunsFreqPerf("./kohli.csv","Virat kohli")
batsmanMeanStrikeRate("./kohli.csv","Virat kohli")
batsmanRunsRanges("./kohli.csv","Virat kohli")
batsman4s("./kohli.csv","kohli")
batsman6s("./kohli.csv","kohli")
batsmanDismissals("./kohli.csv","kohli")
battingPerf3d("./kohli.csv","Virat kohli")
batsmanAvgRunsGround("./kohli.csv","Virat kohli")
batsmanAvgRunsOpposition("./kohli.csv","kohli")
batsmanRunsLikelihood("./kohli.csv","Virat kohli")
batsmanPerfBoxHist("./kohli.csv","Virat kohli")
getPlayerDataSp(253802,tdir="C:/Users/vchakravart3/Documents",tfile="kohlisp.csv",ttype="batting")


smith <- getPlayerData(267192,dir="C:/Users/vchakravart3/Documents",file="smith.csv",type="batting",homeOrAway=c(1,2),
                       result=c(1,2,4))
batsmanRunsFreqPerf("./smith.csv","Steven Smith")
batsmanMeanStrikeRate("./smith.csv","Steven Smith")
batsmanRunsRanges("./smith.csv","Steven Smith")
batsman4s("./smith.csv","smith")
batsman6s("./smith.csv","smith")
batsmanDismissals("./smith.csv","smith")
battingPerf3d("./smith.csv","Steven Smith")
batsmanAvgRunsGround("./smith.csv","Steven Smith")
batsmanAvgRunsOpposition("./smith.csv","smith")
batsmanRunsLikelihood("./smith.csv","Steven Smith")
batsmanPerfBoxHist("./smith.csv","Steven Smith")

getPlayerDataSp(267192,tdir="C:/Users/vchakravart3/Documents",tfile="smithsp.csv",ttype="batting")


pujara <- getPlayerData(32540,dir="C:/Users/vchakravart3/Documents",file="pujara.csv",type="batting",homeOrAway=c(1,2),
                        result=c(1,2,4))
par(mfrow=c(1,3))
par(mar=c(4,4,2,2))
batsmanRunsFreqPerf("./pujara.csv","Cheteshwar Pujara")
batsmanMeanStrikeRate("./pujara.csv","Cheteshwar Pujara")
batsmanRunsRanges("./pujara.csv","Cheteshwar Pujara")
batsman4s("./pujara.csv","pujara")
batsman6s("./pujara.csv","pujara")
batsmanDismissals("./pujara.csv","pujara")
battingPerf3d("./pujara.csv","Cheteshwar Pujara")
batsmanAvgRunsGround("./pujara.csv","Cheteshwar Pujara")
batsmanAvgRunsOpposition("./pujara.csv","pujara")
batsmanRunsLikelihood("./pujara.csv","Cheteshwar Pujara")
batsmanPerfBoxHist("./pujara.csv","Cheteshwar Pujara")
getPlayerDataSp(32540,tdir="C:/Users/vchakravart3/Documents",tfile="pujarasp.csv",ttype="batting")


williamson <- getPlayerData(277906,dir="C:/Users/vchakravart3/Documents",file="williamson.csv",type="batting",homeOrAway=c(1,2),
                            result=c(1,2,4))
par(mfrow=c(1,3))
par(mar=c(4,4,2,2))
batsmanRunsFreqPerf("./williamson.csv","Kane Williamson")
batsmanMeanStrikeRate("./williamson.csv","Kane Williamson")
batsmanRunsRanges("./williamson.csv","Kane Williamson")
batsman4s("./williamson.csv","williamson")
batsman6s("./williamson.csv","williamson")
batsmanDismissals("./williamson.csv","williamson")
battingPerf3d("./williamson.csv","Kane Williamson")
batsmanAvgRunsGround("./williamson.csv","Kane Williamson")
batsmanAvgRunsOpposition("./williamson.csv","williamson")
batsmanRunsLikelihood("./williamson.csv","Kane Williamson")
batsmanPerfBoxHist("./williamson.csv","Kane Williamson")
getPlayerDataSp(277906,tdir="C:/Users/vchakravart3/Documents",tfile="williamsonsp.csv",ttype="batting")


batsmanContributionWonLost("kohlisp.csv","kohli")
batsmanContributionWonLost("smithsp.csv","smith")
batsmanContributionWonLost("pujarasp.csv","pujara")
batsmanContributionWonLost("williamsonsp.csv","williamson")


batsmanPerfHomeAway("kohlisp.csv","kohli")
batsmanPerfHomeAway("smithsp.csv","smith")
batsmanPerfHomeAway("pujarasp.csv","pujara")
batsmanPerfHomeAway("williamsonsp.csv","williamson")

frames <- list("./kohli.csv","./smith.csv","./pujara.csv","./williamson.csv")
names <- list("Kohli","Smith","pujara","williamson")
relativeBatsmanSR(frames,names)

frames <- list("./kohli.csv","./smith.csv","./pujara.csv","./williamson.csv")
names <- list("Kohli","Smith","Pujara","Williamson")
relativeRunsFreqPerf(frames,names)


batsmanMovingAverage("./kohli.csv","Virat kohli")
batsmanMovingAverage("./smith.csv","Steven Smith")
batsmanMovingAverage("./pujara.csv","Cheteshwar Pujara")
batsmanMovingAverage("./williamson.csv","Kane Williamson")


batsmanPerfForecast("./kohli.csv","Virat kohli")
batsmanPerfForecast("./smith.csv","Steven Smith")
batsmanPerfForecast("./pujara.csv","Cheteshwar Pujara")
batsmanPerfForecast("./williamson.csv","Kane Williamson")


checkBatsmanInForm("./kohli.csv","Virat kohli")
checkBatsmanInForm("./smith.csv","Steven Smith")
checkBatsmanInForm("./pujara.csv","Cheteshwar Pujara")
checkBatsmanInForm("./williamson.csv","Kane Williamson")

par(mfrow=c(1,2))
par(mar=c(4,4,2,2))
battingPerf3d("./kohli.csv","kohli")
battingPerf3d("./smith.csv","smith")
battingPerf3d("./pujara.csv","pujara")
battingPerf3d("./williamson.csv","williamson")

BF <- seq( 10, 400,length=15)
Mins <- seq(30,600,length=15)
newDF <- data.frame(BF,Mins)
Kohli <- batsmanRunsPredict("./kohli.csv","Kohli",newdataframe=newDF)
Smith <- batsmanRunsPredict("./smith.csv","Smith",newdataframe=newDF)
Pujara <- batsmanRunsPredict("./pujara.csv","Pujara",newdataframe=newDF)
Williamson <- batsmanRunsPredict("./williamson.csv","Williamson",newdataframe=newDF)

batsmen <-cbind((kohli$Runsna.rm=TRUE),(smith$Runsna.rm=TRUE),(pujara$Runsna.rm=TRUE),(williamson$Runsna.rm=TRUE))
colnames(batsmen) <- c("Kohli","Smith","Pujara","Williamson")
newDF <- data.frame(newDF$BF,newDF$Mins)
colnames(newDF) <- c("BallsFaced","MinsAtCrease")
predictedRuns <- cbind(newDF,batsmen)
predictedRuns

#Bowlers
par(mfrow=c(1,3))
par(mar=c(4,4,2,2))
bowlerWktsFreqPercent("./kumble.csv","Anil Kumble")
bowlerWktsFreqPercent("./warne.csv","Shane Warne")

par(mfrow=c(1,3))
par(mar=c(4,4,2,2))
bowlerWktsRunsPlot("./kumble.csv","Kumble")
bowlerWktsRunsPlot("./warne.csv","Warne")
bowlerWktsRunsPlot("./murali.csv","Muralitharan")

bowlerAvgWktsGround("./murali.csv","Muralitharan")
bowlerAvgWktsOpposition("./murali.csv","Muralitharan")

frames <- list("./kumble.csv","./murali.csv","warne.csv")
names <- list("Anil KUmble","M Muralitharan","Shane Warne")
relativeBowlingPerf(frames,names)

frames <- list("./kumble.csv","./murali.csv","warne.csv")
names <- list("Anil KUmble","M Muralitharan","Shane Warne")
relativeBowlingER(frames,names)


par(mfrow=c(1,3))
par(mar=c(4,4,2,2))
bowlerMovingAverage("./kumble.csv","Anil Kumble")
bowlerMovingAverage("./warne.csv","Shane Warne")
bowlerMovingAverage("./murali.csv","M Muralitharan")

par(mfrow=c(1,3))
par(mar=c(4,4,2,2))
bowlerPerfForecast("./kumble.csv","Anil Kumble")
bowlerPerfForecast("./warne.csv","Shane Warne")
bowlerPerfForecast("./murali.csv","M Muralitharan")

par(mfrow=c(1,3))
par(mar=c(4,4,2,2))
bowlerContributionWonLost("kumblesp.csv","Kumble")
bowlerContributionWonLost("warnesp.csv","Warne")
bowlerContributionWonLost("muralisp.csv","Murali")


par(mfrow=c(1,3))
par(mar=c(4,4,2,2))
bowlerPerfHomeAway("kumblesp.csv","Kumble")
bowlerPerfHomeAway("warnesp.csv","Warne")
bowlerPerfHomeAway("muralisp.csv","Murali")

checkBowlerInForm("./kumble.csv","Anil Kumble")
bowlerWktsFreqPercent("./murali.csv","M Muralitharan")