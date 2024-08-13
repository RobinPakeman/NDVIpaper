##Ideas

###model3<-lmer(CleanedNDVI..0.15.~sin(Time*2*pi)+cos(Time*2*pi)+(1|Year),data=AgFesMol2, REML=FALSE)

library(MASS)
library(Hmisc)
library(censReg)
library(VGAM)

##split data into training and prediciton parts

setwd("C:/Users/RP40274/OneDrive - The James Hutton Institute/Documents/Pakeman/Projects(FF+outercore)/StKilda/Satellite/Paper/SupplementaryMaterial")

AllData<-read.csv("WeatherForAnalysis.csv", header=TRUE)

AllData$Date2<-as.Date(AllData$Date, format = "%d/%m/%Y")
AllData$Month<-format(AllData$Date2, "%m")
AllData$Year<-format(AllData$Date2, "%Y")


AllData$Season<-NA
AllData$Season[AllData$Month == "01"]<-"Winter"
AllData$Season[AllData$Month == "02"]<-"Winter"
AllData$Season[AllData$Month == "03"]<-"Spring"
AllData$Season[AllData$Month == "04"]<-"Spring"
AllData$Season[AllData$Month == "05"]<-"Spring"
AllData$Season[AllData$Month == "06"]<-"Summer"
AllData$Season[AllData$Month == "07"]<-"Summer"
AllData$Season[AllData$Month == "08"]<-"Summer"
AllData$Season[AllData$Month == "09"]<-"Autumn"
AllData$Season[AllData$Month == "10"]<-"Autumn"
AllData$Season[AllData$Month == "11"]<-"Autumn"
AllData$Season[AllData$Month == "12"]<-"Winter"


colnames(AllData)[colnames(AllData)=="BrianansAirAvgWind"]<-"BrAvWD"
colnames(AllData)[colnames(AllData)=="BrianansAirTempAvg"]<-"BrAvTG"
colnames(AllData)[colnames(AllData)=="BrianansAirTempMax"]<-"BrAvTX"
colnames(AllData)[colnames(AllData)=="BrianansAirTempMin"]<-"BrAvTN"
colnames(AllData)[colnames(AllData)=="BrianansPrecipTotal"]<-"BrTtRR"
colnames(AllData)[colnames(AllData)=="BrianansWindDir"]<-"BrDrWD"
colnames(AllData)[colnames(AllData)=="BriananswindMax"]<-"BrMxWD"

colnames(AllData)[colnames(AllData)=="QuarryAirAvgWind"]<-"QuAvWD"
colnames(AllData)[colnames(AllData)=="QuarryAirTempAvg"]<-"QuAvTG"
colnames(AllData)[colnames(AllData)=="QuarryAirTempMax"]<-"QuAvTX"
colnames(AllData)[colnames(AllData)=="QuarryAirTempMin"]<-"QuAvTN"
colnames(AllData)[colnames(AllData)=="QuarryPrecipTotal"]<-"QuTtRR"
colnames(AllData)[colnames(AllData)=="QuarryWindDir"]<-"QuDrWD"
colnames(AllData)[colnames(AllData)=="QuarrywindMax"]<-"QuMxWD"

colnames(AllData)[colnames(AllData)=="SignalsAirAvgWind"]<-"SgAvWD"
colnames(AllData)[colnames(AllData)=="SignalsAirTempAvg"]<-"SgAvTG"
colnames(AllData)[colnames(AllData)=="SignalsAirTempMax"]<-"SgAvTX"
colnames(AllData)[colnames(AllData)=="SignalsAirTempMin"]<-"SgAvTN"
colnames(AllData)[colnames(AllData)=="SignalsPrecipTotal"]<-"SgTtRR"
colnames(AllData)[colnames(AllData)=="SignalsWindDir"]<-"SgDrWD"
colnames(AllData)[colnames(AllData)=="SignalswindMax"]<-"SgMxWD"

colnames(AllData)[colnames(AllData)=="StornowayWindDailySpd"]<-"STAvWD"
colnames(AllData)[colnames(AllData)=="StornowayTempDailyMax"]<-"STAvTX"
colnames(AllData)[colnames(AllData)=="StornowayTempDailyMin"]<-"STAvTN"
colnames(AllData)[colnames(AllData)=="StornowayRainDaily"]<-"STTtRR"
colnames(AllData)[colnames(AllData)=="StornowayWindDailyDirDeg"]<-"STDrWD"
colnames(AllData)[colnames(AllData)=="StornowayWindDailyMax"]<-"STMxWD"
colnames(AllData)[colnames(AllData)=="StornowayWindDailyDirRads"]<-"STRaWD"


####Convert polar wind to x,y velocities
####Stornoway converted from knots to m/s

AllData$SgNSWD<-AllData$SgAvWD*cos(AllData$SgDrWD*pi/180)
AllData$SgEWWD<-AllData$SgAvWD*sin(AllData$SgDrWD*pi/180)
AllData$SgNSmax<-AllData$SgMxWD*cos(AllData$SgDrWD*pi/180)
AllData$SgEWmax<-AllData$SgMxWD*sin(AllData$SgDrWD*pi/180)

AllData$BrNSWD<-AllData$BrAvWD*cos(AllData$BrDrWD*pi/180)
AllData$BrEWWD<-AllData$BrAvWD*sin(AllData$BrDrWD*pi/180)
AllData$BrNSmax<-AllData$BrMxWD*cos(AllData$BrDrWD*pi/180)
AllData$BrEWmax<-AllData$BrMxWD*sin(AllData$BrDrWD*pi/180)

AllData$QuNSWD<-AllData$QuAvWD*cos(AllData$QuDrWD*pi/180)
AllData$QuEWWD<-AllData$QuAvWD*sin(AllData$QuDrWD*pi/180)
AllData$QuNSmax<-AllData$QuMxWD*cos(AllData$QuDrWD*pi/180)
AllData$QuEWmax<-AllData$QuMxWD*sin(AllData$QuDrWD*pi/180)

AllData$STNSWD<-AllData$STAvWD*cos(AllData$STDrWD*pi/180)/1.94384
AllData$STEWWD<-AllData$STAvWD*sin(AllData$STDrWD*pi/180)/1.94384
AllData$STNSmax<-AllData$STMxWD*cos(AllData$STDrWD*pi/180)/1.94384
AllData$STEWmax<-AllData$STMxWD*sin(AllData$STDrWD*pi/180)/1.94384


##WIND
##Split data into N-S and E-W vectors and model as simple Gaussian process
##Bessac et al.2016
##Autoregressive model - test T-1 as a variable?
###Direction is radians/degrees from north, with west being negative and east positive!!!!
###Stornoway in knots, Hirta in m/s Divide by 1.94384 to go from knots to m/s
##To convert into vectors
##NS component = velocity * cos x
##EW component = velocity * sin x

AllData$STWeather<-NA
AllData$STWeather[AllData$STDrWD>=0 & AllData$STDrWD<90]<-"NE"
AllData$STWeather[AllData$STDrWD>=90 & AllData$STDrWD<=180]<-"SE"
AllData$STWeather[AllData$STDrWD<0 & AllData$STDrWD>=-90]<-"NW"
AllData$STWeather[AllData$STDrWD< -90 & AllData$STDrWD>=-179.999]<-"SW"



###CalculateStornoway average temp (max+min)/2
AllData$STAvTG<-(AllData$STAvTN+AllData$STAvTX)/2

##Set up temperature to do GDD
AllData$SgGDD<-AllData$SgAvTG-5
AllData$SgGDD[AllData$SgGDD<0] <- 0
AllData$BrGDD<-AllData$BrAvTG-5
AllData$BrGDD[AllData$BrGDD<0] <- 0
AllData$QuGDD<-AllData$QuAvTG-5
AllData$QuGDD[AllData$QuGDD<0] <- 0
AllData$STGDD<-AllData$STAvTG-5
AllData$STGDD[AllData$STGDD<0] <- 0


##Dataset with weather station data from 01/09/2000
TrainData<-AllData[7550:14670,]
PredData<-AllData[1:7549,]

##Monthly means for St Brianans
TrainBrAvWDmean<-aggregate.data.frame(TrainData$BrAvWD,list(TrainData$Year, TrainData$Month), mean, na.rm=TRUE, na.action=na.pass)
colnames(TrainBrAvWDmean)<-c("Year","Month","BrAvWD")
TrainBrAvWDlen<-aggregate(TrainData$BrAvWD,list(TrainData$Year, TrainData$Month), FUN = (function(x) {length(which(!is.na(x)))}))
colnames(TrainBrAvWDlen)<-c("Year","Month","BrAvWDlength")
TrainBrAvWD<-merge(TrainBrAvWDmean,TrainBrAvWDlen)
TrainBrAvWD$BrAvWD[TrainBrAvWD$BrAvWDlength<15]<-NA

TrainMonth<-TrainBrAvWD[,1:3]

TrainBrAvTGmean<-aggregate.data.frame(TrainData$BrAvTG,list(TrainData$Year, TrainData$Month), mean, na.rm=TRUE, na.action=na.pass)
colnames(TrainBrAvTGmean)<-c("Year","Month","BrAvTG")
TrainBrAvTGlen<-aggregate(TrainData$BrAvTG,list(TrainData$Year, TrainData$Month), FUN = (function(x) {length(which(!is.na(x)))}))
colnames(TrainBrAvTGlen)<-c("Year","Month","BrAvTGlength")
TrainBrAvTG<-merge(TrainBrAvTGmean,TrainBrAvTGlen)
TrainBrAvTG$BrAvTG[TrainBrAvTG$BrAvTGlength<15]<-NA

TrainMonth<-merge(TrainMonth,TrainBrAvTG[,1:3])

TrainBrTtRR<-aggregate.data.frame(TrainData$BrTtRR,list(TrainData$Year, TrainData$Month), sum, na.rm=TRUE)
colnames(TrainBrTtRR)<-c("Year","Month","BrTtRR")
TrainBrTtRRlen<-aggregate(TrainData$BrTtRR,list(TrainData$Year, TrainData$Month), FUN = (function(x) {length(which(!is.na(x)))}))
colnames(TrainBrTtRRlen)<-c("Year","Month","BrTtRRlength")
TrainBrTtRRlen2<-aggregate(TrainData$BrTtRR,list(TrainData$Year, TrainData$Month), FUN = length)
colnames(TrainBrTtRRlen2)<-c("Year","Month","BrTtRRlength2")
TrainBrTtRRtemp<-merge(TrainBrTtRR,TrainBrTtRRlen)
TrainBrTtRR<-merge(TrainBrTtRRtemp,TrainBrTtRRlen2)
TrainBrTtRR$BrTtRR[TrainBrTtRR$BrTtRRlength<15]<-NA
TrainBrTtRR$BrTtRRadj<-TrainBrTtRR$BrTtRR*TrainBrTtRR$BrTtRRlength2/TrainBrTtRR$BrTtRRlength

TrainMonth<-merge(TrainMonth,TrainBrTtRR[,c(1,2,6)])

TrainBrGDD<-aggregate.data.frame(TrainData$BrGDD,list(TrainData$Year, TrainData$Month), sum, na.rm=TRUE)
colnames(TrainBrGDD)<-c("Year","Month","BrGDD")
TrainBrGDDlen<-aggregate(TrainData$BrGDD,list(TrainData$Year, TrainData$Month), FUN = (function(x) {length(which(!is.na(x)))}))
colnames(TrainBrGDDlen)<-c("Year","Month","BrGDDlength")
TrainBrGDDlen2<-aggregate(TrainData$BrGDD,list(TrainData$Year, TrainData$Month), FUN = length)
colnames(TrainBrGDDlen2)<-c("Year","Month","BrGDDlength2")
TrainBrGDDtemp<-merge(TrainBrGDD,TrainBrGDDlen)
TrainBrGDD<-merge(TrainBrGDDtemp,TrainBrGDDlen2)
TrainBrGDD$BrGDD[TrainBrGDD$BrGDDlength<15]<-NA
TrainBrGDD$BrGDDadj<-TrainBrGDD$BrGDD*TrainBrGDD$BrGDDlength2/TrainBrGDD$BrGDDlength

TrainMonth<-merge(TrainMonth,TrainBrGDD[,c(1,2,6)])


##Monthly means for Quarry
TrainQuAvWDmean<-aggregate.data.frame(TrainData$QuAvWD,list(TrainData$Year, TrainData$Month), mean, na.rm=TRUE, na.action=na.pass)
colnames(TrainQuAvWDmean)<-c("Year","Month","QuAvWD")
TrainQuAvWDlen<-aggregate(TrainData$QuAvWD,list(TrainData$Year, TrainData$Month), FUN = (function(x) {length(which(!is.na(x)))}))
colnames(TrainQuAvWDlen)<-c("Year","Month","QuAvWDlength")
TrainQuAvWD<-merge(TrainQuAvWDmean,TrainQuAvWDlen)
TrainQuAvWD$QuAvWD[TrainQuAvWD$QuAvWDlength<15]<-NA

TrainMonth<-merge(TrainMonth,TrainQuAvWD[,1:3])

TrainQuAvTGmean<-aggregate.data.frame(TrainData$QuAvTG,list(TrainData$Year, TrainData$Month), mean, na.rm=TRUE, na.action=na.pass)
colnames(TrainQuAvTGmean)<-c("Year","Month","QuAvTG")
TrainQuAvTGlen<-aggregate(TrainData$QuAvTG,list(TrainData$Year, TrainData$Month), FUN = (function(x) {length(which(!is.na(x)))}))
colnames(TrainQuAvTGlen)<-c("Year","Month","QuAvTGlength")
TrainQuAvTG<-merge(TrainQuAvTGmean,TrainQuAvTGlen)
TrainQuAvTG$QuAvTG[TrainQuAvTG$QuAvTGlength<15]<-NA

TrainMonth<-merge(TrainMonth,TrainQuAvTG[,1:3])

TrainQuTtRR<-aggregate.data.frame(TrainData$QuTtRR,list(TrainData$Year, TrainData$Month), sum, na.rm=TRUE)
colnames(TrainQuTtRR)<-c("Year","Month","QuTtRR")
TrainQuTtRRlen<-aggregate(TrainData$QuTtRR,list(TrainData$Year, TrainData$Month), FUN = (function(x) {length(which(!is.na(x)))}))
colnames(TrainQuTtRRlen)<-c("Year","Month","QuTtRRlength")
TrainQuTtRRlen2<-aggregate(TrainData$QuTtRR,list(TrainData$Year, TrainData$Month), FUN = length)
colnames(TrainQuTtRRlen2)<-c("Year","Month","QuTtRRlength2")
TrainQuTtRRtemp<-merge(TrainQuTtRR,TrainQuTtRRlen)
TrainQuTtRR<-merge(TrainQuTtRRtemp,TrainQuTtRRlen2)
TrainQuTtRR$QuTtRR[TrainQuTtRR$QuTtRRlength<15]<-NA
TrainQuTtRR$QuTtRRadj<-TrainQuTtRR$QuTtRR*TrainQuTtRR$QuTtRRlength2/TrainQuTtRR$QuTtRRlength

TrainMonth<-merge(TrainMonth,TrainQuTtRR[,c(1,2,6)])

TrainQuGDD<-aggregate.data.frame(TrainData$QuGDD,list(TrainData$Year, TrainData$Month), sum, na.rm=TRUE)
colnames(TrainQuGDD)<-c("Year","Month","QuGDD")
TrainQuGDDlen<-aggregate(TrainData$QuGDD,list(TrainData$Year, TrainData$Month), FUN = (function(x) {length(which(!is.na(x)))}))
colnames(TrainQuGDDlen)<-c("Year","Month","QuGDDlength")
TrainQuGDDlen2<-aggregate(TrainData$QuGDD,list(TrainData$Year, TrainData$Month), FUN = length)
colnames(TrainQuGDDlen2)<-c("Year","Month","QuGDDlength2")
TrainQuGDDtemp<-merge(TrainQuGDD,TrainQuGDDlen)
TrainQuGDD<-merge(TrainQuGDDtemp,TrainQuGDDlen2)
TrainQuGDD$QuGDD[TrainQuGDD$QuGDDlength<15]<-NA
TrainQuGDD$QuGDDadj<-TrainQuGDD$QuGDD*TrainQuGDD$QuGDDlength2/TrainQuGDD$QuGDDlength

TrainMonth<-merge(TrainMonth,TrainQuGDD[,c(1,2,6)])


##Monthly means for Signals
TrainSgAvWDmean<-aggregate.data.frame(TrainData$SgAvWD,list(TrainData$Year, TrainData$Month), mean, na.rm=TRUE, na.action=na.pass)
colnames(TrainSgAvWDmean)<-c("Year","Month","SgAvWD")
TrainSgAvWDlen<-aggregate(TrainData$SgAvWD,list(TrainData$Year, TrainData$Month), FUN = (function(x) {length(which(!is.na(x)))}))
colnames(TrainSgAvWDlen)<-c("Year","Month","SgAvWDlength")
TrainSgAvWD<-merge(TrainSgAvWDmean,TrainSgAvWDlen)
TrainSgAvWD$SgAvWD[TrainSgAvWD$SgAvWDlength<15]<-NA

TrainMonth<-merge(TrainMonth,TrainSgAvWD[,1:3])

TrainSgAvTGmean<-aggregate.data.frame(TrainData$SgAvTG,list(TrainData$Year, TrainData$Month), mean, na.rm=TRUE, na.action=na.pass)
colnames(TrainSgAvTGmean)<-c("Year","Month","SgAvTG")
TrainSgAvTGlen<-aggregate(TrainData$SgAvTG,list(TrainData$Year, TrainData$Month), FUN = (function(x) {length(which(!is.na(x)))}))
colnames(TrainSgAvTGlen)<-c("Year","Month","SgAvTGlength")
TrainSgAvTG<-merge(TrainSgAvTGmean,TrainSgAvTGlen)
TrainSgAvTG$SgAvTG[TrainSgAvTG$SgAvTGlength<15]<-NA

TrainMonth<-merge(TrainMonth,TrainSgAvTG[,1:3])

TrainSgTtRR<-aggregate.data.frame(TrainData$SgTtRR,list(TrainData$Year, TrainData$Month), sum, na.rm=TRUE)
colnames(TrainSgTtRR)<-c("Year","Month","SgTtRR")
TrainSgTtRRlen<-aggregate(TrainData$SgTtRR,list(TrainData$Year, TrainData$Month), FUN = (function(x) {length(which(!is.na(x)))}))
colnames(TrainSgTtRRlen)<-c("Year","Month","SgTtRRlength")
TrainSgTtRRlen2<-aggregate(TrainData$SgTtRR,list(TrainData$Year, TrainData$Month), FUN = length)
colnames(TrainSgTtRRlen2)<-c("Year","Month","SgTtRRlength2")
TrainSgTtRRtemp<-merge(TrainSgTtRR,TrainSgTtRRlen)
TrainSgTtRR<-merge(TrainSgTtRRtemp,TrainSgTtRRlen2)
TrainSgTtRR$SgTtRR[TrainSgTtRR$SgTtRRlength<15]<-NA
TrainSgTtRR$SgTtRRadj<-TrainSgTtRR$SgTtRR*TrainSgTtRR$SgTtRRlength2/TrainSgTtRR$SgTtRRlength

TrainMonth<-merge(TrainMonth,TrainSgTtRR[,c(1,2,6)])

TrainSgGDD<-aggregate.data.frame(TrainData$SgGDD,list(TrainData$Year, TrainData$Month), sum, na.rm=TRUE)
colnames(TrainSgGDD)<-c("Year","Month","SgGDD")
TrainSgGDDlen<-aggregate(TrainData$SgGDD,list(TrainData$Year, TrainData$Month), FUN = (function(x) {length(which(!is.na(x)))}))
colnames(TrainSgGDDlen)<-c("Year","Month","SgGDDlength")
TrainSgGDDlen2<-aggregate(TrainData$SgGDD,list(TrainData$Year, TrainData$Month), FUN = length)
colnames(TrainSgGDDlen2)<-c("Year","Month","SgGDDlength2")
TrainSgGDDtemp<-merge(TrainSgGDD,TrainSgGDDlen)
TrainSgGDD<-merge(TrainSgGDDtemp,TrainSgGDDlen2)
TrainSgGDD$SgGDD[TrainSgGDD$SgGDDlength<15]<-NA
TrainSgGDD$SgGDDadj<-TrainSgGDD$SgGDD*TrainSgGDD$SgGDDlength2/TrainSgGDD$SgGDDlength

TrainMonth<-merge(TrainMonth,TrainSgGDD[,c(1,2,6)])




##Monthly means for Stornoway
TrainSTAvWDmean<-aggregate.data.frame(TrainData$STAvWD,list(TrainData$Year, TrainData$Month), mean, na.rm=TRUE, na.action=na.pass)
colnames(TrainSTAvWDmean)<-c("Year","Month","STAvWD")
TrainSTAvWDlen<-aggregate(TrainData$STAvWD,list(TrainData$Year, TrainData$Month), FUN = (function(x) {length(which(!is.na(x)))}))
colnames(TrainSTAvWDlen)<-c("Year","Month","STAvWDlength")
TrainSTAvWD<-merge(TrainSTAvWDmean,TrainSTAvWDlen)
TrainSTAvWD$STAvWD[TrainSTAvWD$STAvWDlength<15]<-NA

TrainMonth<-merge(TrainMonth,TrainSTAvWD[,1:3])

TrainSTAvTGmean<-aggregate.data.frame(TrainData$STAvTG,list(TrainData$Year, TrainData$Month), mean, na.rm=TRUE, na.action=na.pass)
colnames(TrainSTAvTGmean)<-c("Year","Month","STAvTG")
TrainSTAvTGlen<-aggregate(TrainData$STAvTG,list(TrainData$Year, TrainData$Month), FUN = (function(x) {length(which(!is.na(x)))}))
colnames(TrainSTAvTGlen)<-c("Year","Month","STAvTGlength")
TrainSTAvTG<-merge(TrainSTAvTGmean,TrainSTAvTGlen)
TrainSTAvTG$STAvTG[TrainSTAvTG$STAvTGlength<15]<-NA

TrainMonth<-merge(TrainMonth,TrainSTAvTG[,1:3])

TrainSTTtRR<-aggregate.data.frame(TrainData$STTtRR,list(TrainData$Year, TrainData$Month), sum, na.rm=TRUE)
colnames(TrainSTTtRR)<-c("Year","Month","STTtRR")
TrainSTTtRRlen<-aggregate(TrainData$STTtRR,list(TrainData$Year, TrainData$Month), FUN = (function(x) {length(which(!is.na(x)))}))
colnames(TrainSTTtRRlen)<-c("Year","Month","STTtRRlength")
TrainSTTtRRlen2<-aggregate(TrainData$STTtRR,list(TrainData$Year, TrainData$Month), FUN = length)
colnames(TrainSTTtRRlen2)<-c("Year","Month","STTtRRlength2")
TrainSTTtRRtemp<-merge(TrainSTTtRR,TrainSTTtRRlen)
TrainSTTtRR<-merge(TrainSTTtRRtemp,TrainSTTtRRlen2)
TrainSTTtRR$STTtRR[TrainSTTtRR$STTtRRlength<15]<-NA
TrainSTTtRR$STTtRRadj<-TrainSTTtRR$STTtRR*TrainSTTtRR$STTtRRlength2/TrainSTTtRR$STTtRRlength

TrainMonth<-merge(TrainMonth,TrainSTTtRR[,c(1,2,6)])

TrainSTGDD<-aggregate.data.frame(TrainData$STGDD,list(TrainData$Year, TrainData$Month), sum, na.rm=TRUE)
colnames(TrainSTGDD)<-c("Year","Month","STGDD")
TrainSTGDDlen<-aggregate(TrainData$STGDD,list(TrainData$Year, TrainData$Month), FUN = (function(x) {length(which(!is.na(x)))}))
colnames(TrainSTGDDlen)<-c("Year","Month","STGDDlength")
TrainSTGDDlen2<-aggregate(TrainData$STGDD,list(TrainData$Year, TrainData$Month), FUN = length)
colnames(TrainSTGDDlen2)<-c("Year","Month","STGDDlength2")
TrainSTGDDtemp<-merge(TrainSTGDD,TrainSTGDDlen)
TrainSTGDD<-merge(TrainSTGDDtemp,TrainSTGDDlen2)
TrainSTGDD$STGDD[TrainSTGDD$STGDDlength<15]<-NA
TrainSTGDD$STGDDadj<-TrainSTGDD$STGDD*TrainSTGDD$STGDDlength2/TrainSTGDD$STGDDlength

TrainMonth<-merge(TrainMonth,TrainSTGDD[,c(1,2,6)])

TrainSTNSWDmean<-aggregate.data.frame(TrainData$STNSWD,list(TrainData$Year, TrainData$Month), mean, na.rm=TRUE, na.action=na.pass)
colnames(TrainSTNSWDmean)<-c("Year","Month","STNSWD")
TrainSTNSWDlen<-aggregate(TrainData$STNSWD,list(TrainData$Year, TrainData$Month), FUN = (function(x) {length(which(!is.na(x)))}))
colnames(TrainSTNSWDlen)<-c("Year","Month","STNSWDlength")
TrainSTNSWD<-merge(TrainSTNSWDmean,TrainSTNSWDlen)
TrainSTNSWD$STNSWD[TrainSTNSWD$STNSWDlength<15]<-NA

TrainMonth<-merge(TrainMonth,TrainSTNSWD[,1:3])

TrainSTEWWDmean<-aggregate.data.frame(TrainData$STEWWD,list(TrainData$Year, TrainData$Month), mean, na.rm=TRUE, na.action=na.pass)
colnames(TrainSTEWWDmean)<-c("Year","Month","STEWWD")
TrainSTEWWDlen<-aggregate(TrainData$STEWWD,list(TrainData$Year, TrainData$Month), FUN = (function(x) {length(which(!is.na(x)))}))
colnames(TrainSTEWWDlen)<-c("Year","Month","STEWWDlength")
TrainSTEWWD<-merge(TrainSTEWWDmean,TrainSTEWWDlen)
TrainSTEWWD$STEWWD[TrainSTEWWD$STEWWDlength<15]<-NA

TrainMonth<-merge(TrainMonth,TrainSTEWWD[,1:3])
TrainMonth$Time<-(as.numeric(TrainMonth$Year)-1980)+((as.numeric(TrainMonth$Month)-0.5)/12)


##Prediction data fr Stornoway
PredSTAvWDmean<-aggregate.data.frame(PredData$STAvWD,list(PredData$Year, PredData$Month), mean, na.rm=TRUE, na.action=na.pass)
colnames(PredSTAvWDmean)<-c("Year","Month","STAvWD")
PredSTAvWDlen<-aggregate(PredData$STAvWD,list(PredData$Year, PredData$Month), FUN = (function(x) {length(which(!is.na(x)))}))
colnames(PredSTAvWDlen)<-c("Year","Month","STAvWDlength")
PredSTAvWD<-merge(PredSTAvWDmean,PredSTAvWDlen)
PredSTAvWD$STAvWD[PredSTAvWD$STAvWDlength<15]<-NA

PredMonth<-(PredSTAvWD[,1:3])

PredSTAvTGmean<-aggregate.data.frame(PredData$STAvTG,list(PredData$Year, PredData$Month), mean, na.rm=TRUE, na.action=na.pass)
colnames(PredSTAvTGmean)<-c("Year","Month","STAvTG")
PredSTAvTGlen<-aggregate(PredData$STAvTG,list(PredData$Year, PredData$Month), FUN = (function(x) {length(which(!is.na(x)))}))
colnames(PredSTAvTGlen)<-c("Year","Month","STAvTGlength")
PredSTAvTG<-merge(PredSTAvTGmean,PredSTAvTGlen)
PredSTAvTG$STAvTG[PredSTAvTG$STAvTGlength<15]<-NA

PredMonth<-merge(PredMonth,PredSTAvTG[,1:3])

PredSTTtRR<-aggregate.data.frame(PredData$STTtRR,list(PredData$Year, PredData$Month), sum, na.rm=TRUE)
colnames(PredSTTtRR)<-c("Year","Month","STTtRR")
PredSTTtRRlen<-aggregate(PredData$STTtRR,list(PredData$Year, PredData$Month), FUN = (function(x) {length(which(!is.na(x)))}))
colnames(PredSTTtRRlen)<-c("Year","Month","STTtRRlength")
PredSTTtRRlen2<-aggregate(PredData$STTtRR,list(PredData$Year, PredData$Month), FUN = length)
colnames(PredSTTtRRlen2)<-c("Year","Month","STTtRRlength2")
PredSTTtRRtemp<-merge(PredSTTtRR,PredSTTtRRlen)
PredSTTtRR<-merge(PredSTTtRRtemp,PredSTTtRRlen2)
PredSTTtRR$STTtRR[PredSTTtRR$STTtRRlength<15]<-NA
PredSTTtRR$STTtRRadj<-PredSTTtRR$STTtRR*PredSTTtRR$STTtRRlength2/PredSTTtRR$STTtRRlength

PredMonth<-merge(PredMonth,PredSTTtRR[,c(1,2,6)])

PredSTGDD<-aggregate.data.frame(PredData$STGDD,list(PredData$Year, PredData$Month), sum, na.rm=TRUE)
colnames(PredSTGDD)<-c("Year","Month","STGDD")
PredSTGDDlen<-aggregate(PredData$STGDD,list(PredData$Year, PredData$Month), FUN = (function(x) {length(which(!is.na(x)))}))
colnames(PredSTGDDlen)<-c("Year","Month","STGDDlength")
PredSTGDDlen2<-aggregate(PredData$STGDD,list(PredData$Year, PredData$Month), FUN = length)
colnames(PredSTGDDlen2)<-c("Year","Month","STGDDlength2")
PredSTGDDtemp<-merge(PredSTGDD,PredSTGDDlen)
PredSTGDD<-merge(PredSTGDDtemp,PredSTGDDlen2)
PredSTGDD$STGDD[PredSTGDD$STGDDlength<15]<-NA
PredSTGDD$STGDDadj<-PredSTGDD$STGDD*PredSTGDD$STGDDlength2/PredSTGDD$STGDDlength

PredMonth<-merge(PredMonth,PredSTGDD[,c(1,2,6)])

PredSTNSWDmean<-aggregate.data.frame(PredData$STNSWD,list(PredData$Year, PredData$Month), mean, na.rm=TRUE, na.action=na.pass)
colnames(PredSTNSWDmean)<-c("Year","Month","STNSWD")
PredSTNSWDlen<-aggregate(PredData$STNSWD,list(PredData$Year, PredData$Month), FUN = (function(x) {length(which(!is.na(x)))}))
colnames(PredSTNSWDlen)<-c("Year","Month","STNSWDlength")
PredSTNSWD<-merge(PredSTNSWDmean,PredSTNSWDlen)
PredSTNSWD$STNSWD[PredSTNSWD$STNSWDlength<15]<-NA

PredMonth<-merge(PredMonth,PredSTNSWD[,1:3])

PredSTEWWDmean<-aggregate.data.frame(PredData$STEWWD,list(PredData$Year, PredData$Month), mean, na.rm=TRUE, na.action=na.pass)
colnames(PredSTEWWDmean)<-c("Year","Month","STEWWD")
PredSTEWWDlen<-aggregate(PredData$STEWWD,list(PredData$Year, PredData$Month), FUN = (function(x) {length(which(!is.na(x)))}))
colnames(PredSTEWWDlen)<-c("Year","Month","STEWWDlength")
PredSTEWWD<-merge(PredSTEWWDmean,PredSTEWWDlen)
PredSTEWWD$STEWWD[PredSTEWWD$STEWWDlength<15]<-NA

PredMonth<-merge(PredMonth,PredSTEWWD[,1:3])
PredMonth$Time<-(as.numeric(PredMonth$Year)-1980)+((as.numeric(PredMonth$Month)-0.5)/12)

########################Models

###wind


modw11<-lm(cbind(TrainMonth$SgAvWD,TrainMonth$BrAvWD,TrainMonth$QuAvWD)~TrainMonth$STAvWD)
summary(modw11)

modw11a<-lm(cbind(TrainMonth$SgAvWD,TrainMonth$BrAvWD,TrainMonth$QuAvWD)~TrainMonth$STAvWD+sin(TrainMonth$Time*2*pi)+cos(TrainMonth$Time*2*pi))
summary(modw11a)
anova(modw11a,modw11)

modw12<-lm(cbind(TrainMonth$SgAvWD,TrainMonth$BrAvWD,TrainMonth$QuAvWD)~TrainMonth$STAvWD+TrainMonth$STAvTG+TrainMonth$STTtRRadj+TrainMonth$STNSWD++TrainMonth$STEWWD+sin(TrainMonth$Time*2*pi)+cos(TrainMonth$Time*2*pi))
summary(modw12)

###Temperature
modt11<-lm(cbind(TrainMonth$SgAvTG,TrainMonth$BrAvTG,TrainMonth$QuAvTG)~TrainMonth$STAvTG)
summary(modt11)

modt11a<-lm(cbind(TrainMonth$SgAvTG,TrainMonth$BrAvTG,TrainMonth$QuAvTG)~TrainMonth$STAvTG+sin(TrainMonth$Time*2*pi)+cos(TrainMonth$Time*2*pi))
summary(modt11a)
anova(modt11a,modt11)

modt12<-lm(cbind(TrainMonth$SgAvTG,TrainMonth$BrAvTG,TrainMonth$QuAvTG)~TrainMonth$STAvWD+TrainMonth$STAvTG+TrainMonth$STTtRRadj+TrainMonth$STNSWD++TrainMonth$STEWWD+sin(TrainMonth$Time*2*pi)+cos(TrainMonth$Time*2*pi))
summary(modt12)


###Rainfall
modr11<-lm(cbind(TrainMonth$SgTtRRadj,TrainMonth$BrTtRRadj,TrainMonth$QuTtRRadj)~TrainMonth$STTtRRadj)
summary(modr11)

modr11a<-lm(cbind(TrainMonth$SgTtRRadj,TrainMonth$BrTtRRadj,TrainMonth$QuTtRRadj)~TrainMonth$STTtRRadj+sin(TrainMonth$Time*2*pi)+cos(TrainMonth$Time*2*pi))
summary(modr11a)
anova(modr11a,modr11)

modr12<-lm(cbind(TrainMonth$SgTtRRadj,TrainMonth$BrTtRRadj,TrainMonth$QuTtRRadj)~TrainMonth$STAvWD+TrainMonth$STAvTG+TrainMonth$STTtRRadj+TrainMonth$STNSWD+TrainMonth$STEWWD+sin(TrainMonth$Time*2*pi)+cos(TrainMonth$Time*2*pi))
summary(modr12)


###GDD
modg11<-lm(cbind(TrainMonth$SgGDDadj,TrainMonth$BrGDDadj,TrainMonth$QuGDDadj)~TrainMonth$STGDDadj)
summary(modg11)

modg11a<-lm(cbind(TrainMonth$SgGDDadj,TrainMonth$BrGDDadj,TrainMonth$QuGDDadj)~TrainMonth$STGDDadj+sin(TrainMonth$Time*2*pi)+cos(TrainMonth$Time*2*pi))
summary(modg11a)
anova(modg11a,modg11)

modg12<-lm(cbind(TrainMonth$SgGDDadj,TrainMonth$BrGDDadj,TrainMonth$QuGDDadj)~TrainMonth$STAvWD+TrainMonth$STGDDadj+TrainMonth$STTtRRadj+TrainMonth$STNSWD+TrainMonth$STEWWD+sin(TrainMonth$Time*2*pi)+cos(TrainMonth$Time*2*pi))
summary(modg12)


####All

moda11<-lm(cbind(SgAvWD,SgAvTG,SgTtRRadj,SgGDDadj,BrAvWD,BrAvTG,BrTtRRadj,BrGDDadj,QuAvWD,QuAvTG,QuTtRRadj,QuGDDadj)~STAvWD+STAvTG+STTtRRadj+STGDDadj+STNSWD+STEWWD, data=TrainMonth)
summary(moda11)

moda12<-lm(cbind(SgAvWD,SgAvTG,SgTtRRadj,SgGDDadj,BrAvWD,BrAvTG,BrTtRRadj,BrGDDadj,QuAvWD,QuAvTG,QuTtRRadj,QuGDDadj)~STAvWD+STAvTG+STTtRRadj+STGDDadj+STNSWD+STEWWD+sin(Time*2*pi)+cos(Time*2*pi), data=TrainMonth)
summary(moda12)



#############################
##Re-prediction of training data set

Train_predicted<-as.data.frame(predict(moda12, TrainMonth[,c("STAvWD","STAvTG","STTtRRadj","STGDDadj","STNSWD","STEWWD","Time")]))
colnames(Train_predicted)<-paste("pred", colnames(Train_predicted), sep="_")

Train_predicted<-cbind(Train_predicted,TrainMonth)
plot(Train_predicted$SgAvWD,Train_predicted$pred_SgAvWD, bty="L")
plot(Train_predicted$BrAvWD,Train_predicted$pred_BrAvWD, bty="L")
plot(Train_predicted$QuAvWD,Train_predicted$pred_QuAvWD, bty="L")

plot(Train_predicted$SgAvTG,Train_predicted$pred_SgAvTG, bty="L")
plot(Train_predicted$BrAvTG,Train_predicted$pred_BrAvTG, bty="L")
plot(Train_predicted$QuAvTG,Train_predicted$pred_QuAvTG, bty="L")

plot(Train_predicted$SgTtRRadj,Train_predicted$pred_SgTtRRadj, bty="L")
plot(Train_predicted$BrTtRRadj,Train_predicted$pred_BrTtRRadj, bty="L")
plot(Train_predicted$QuTtRRadj,Train_predicted$pred_QuTtRRadj, bty="L")

plot(Train_predicted$SgGDDadj,Train_predicted$pred_SgGDDadj, bty="L")
plot(Train_predicted$BrGDDadj,Train_predicted$pred_BrGDDadj, bty="L")
plot(Train_predicted$QuGDDadj,Train_predicted$pred_QuGDDadj, bty="L")


##Create precicted data for prior to weather station installation

PredWeather<-as.data.frame(predict(moda12, PredMonth))

PredWeather<-cbind(PredWeather,PredMonth)

write.csv(Train_predicted,"TrainPredicted.csv")

write.csv(PredWeather,"PredictedMonthyl.csv")

write.csv(TrainMonth,"TrainingMonthly.csv")









