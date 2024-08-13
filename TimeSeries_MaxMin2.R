##To try analysing the Landsat time series by fitting maxima and minima and see how the sheep and weather affect things
##LINE 907
library(reshape2)
library(lme4)
library(lmerTest)
library(mgcv)
library(tidyr)
library(MuMIn)

setwd("C:/Users/RP40274/OneDrive - The James Hutton Institute/Documents/Pakeman/Projects(FF+outercore)/StKilda/Satellite/Paper/SupplementaryMaterial")

AllHabs<-read.csv("AllHabitats_WithDates2022.csv")
AllHabs1<-AllHabs[,1:8]

AllDates<-read.csv("AllDates.csv")
AllDates$DecDay<-AllDates$Time-trunc(AllDates$Time)
AllDates$Year1<-as.factor(AllDates$Year)

AllDates2<-AllDates[!(AllDates$Year1==2003 | AllDates$Year1==2012),]


#####Cast the dataframe
AllHabsCast<-dcast(AllHabs1,Year+Month+Day+Date+Time ~ Vegetation, value.var="CleanedNDVI..0.15.")
AllHabsCast<-AllHabsCast[order(AllHabsCast$Time),]
AllHabsCast$DecDay<-AllHabsCast$Time-trunc(AllHabsCast$Time)
AllHabsCast$Year1<-as.factor(AllHabsCast$Year)


######Hierachical generalised additive models
##Pedersen EJ, Miller DL, Simpson GL, Ross N. 2019. Hierarchical generalized additive models in ecology: an
##introduction with mgcv. PeerJ 7:e6876 DOI 10.7717/peerj.6876 

fred<-as.data.frame(seq(1985,2023, 0.0025))
colnames(fred)<-c("Time")
fred$Year1<-floor(fred$Time)
fred$DecDay<-fred$Time-fred$Year1

###AgFesMol code

mod32<-gam(AgFesMol ~ s(DecDay, k=3, m=2) + s(DecDay, Year1, k=3, bs="fs", m=2), na.action=na.omit, data =AllHabsCast, method="REML")
summary(mod32)
##gam.check(mod32)
##plot(predict.gam(mod32, AllDates[,c(4,5)], allow.new.levels = TRUE) )

mod32pred<-as.data.frame(predict.gam(mod32, fred[,c(2,3)], allow.new.levels = TRUE))
mod32pred1<-cbind(fred,mod32pred)
colnames(mod32pred1)[4]<-c("NDVI")

MxDay <- merge(aggregate(NDVI~Year1, mod32pred1, max, na.action=na.omit), mod32pred1, by=c("Year1","NDVI"))
MxDay$Species<-"AgFesMol"
MxDay1<-MxDay

DayMax<-mean(MxDay$DecDay)

AllHabsCast$TimeXX<-AllHabsCast$Time+(1-DayMax)
AllHabsCast$DecDayXX<-AllHabsCast$TimeXX-trunc(AllHabsCast$TimeXX)
AllHabsCast$YearXX<-trunc(AllHabsCast$TimeXX)
AllHabsCast$YearXX<-as.factor(AllHabsCast$YearXX+1985)

mod33<-gam(AgFesMol ~ s(DecDayXX, k=3, m=2) + s(DecDayXX, YearXX, k=3, bs="fs", m=2), na.action=na.omit, data =AllHabsCast, method="REML")
summary(mod33)

george<-as.data.frame(fred$Time)
colnames(george)<-c("TimeXX")
george$TimeXX<-george$TimeXX+(1-DayMax)
george$DecDayXX<-george$TimeXX-trunc(george$TimeXX)
george$YearXX<-as.factor(trunc(george$TimeXX))


mod33pred<-as.data.frame(predict.gam(mod33, george[,c(2,3)], allow.new.levels = TRUE))
mod33pred1<-cbind(george,mod33pred)
colnames(mod33pred1)[4]<-c("NDVI")



###Cliff code

mod32<-gam(Cliff ~ s(DecDay, k=3, m=2) + s(DecDay, Year1, k=3, bs="fs", m=2), na.action=na.omit, data =AllHabsCast, method="REML")
summary(mod32)
##gam.check(mod32)

##plot(predict.gam(mod32, AllDates[,c(4,5)], allow.new.levels = TRUE) )

mod32pred<-as.data.frame(predict.gam(mod32, fred[,c(2,3)], allow.new.levels = TRUE))
mod32pred1<-cbind(fred,mod32pred)
colnames(mod32pred1)[4]<-c("NDVI")

MxDay <- merge(aggregate(NDVI~Year1, mod32pred1, max, na.action=na.omit), mod32pred1, by=c("Year1","NDVI"))
MxDay$Species<-"Cliff"

DayMax<-mean(MxDay$DecDay)

AllHabsCast$TimeXX<-AllHabsCast$Time+(1-DayMax)
AllHabsCast$DecDayXX<-AllHabsCast$TimeXX-trunc(AllHabsCast$TimeXX)
AllHabsCast$YearXX<-trunc(AllHabsCast$TimeXX)
AllHabsCast$YearXX<-as.factor(AllHabsCast$YearXX+1985)

mod33<-gam(Cliff ~ s(DecDayXX, k=3, m=2) + s(DecDayXX, YearXX, k=3, bs="fs", m=2), na.action=na.omit, data =AllHabsCast, method="REML")
summary(mod33)

george<-as.data.frame(fred$Time)
colnames(george)<-c("TimeXX")
george$TimeXX<-george$TimeXX+(1-DayMax)
george$DecDayXX<-george$TimeXX-trunc(george$TimeXX)
george$YearXX<-as.factor(trunc(george$TimeXX))


mod33pred<-as.data.frame(predict.gam(mod33, george[,c(2,3)], allow.new.levels = TRUE))
mod33pred1<-cbind(george,mod33pred)
colnames(mod33pred1)[4]<-c("NDVI")


MxDay1<-rbind(MxDay1,MxDay)

###DryHeath code

mod32<-gam(DryHeath ~ s(DecDay, k=3, m=2) + s(DecDay, Year1, k=3, bs="fs", m=2), na.action=na.omit, data =AllHabsCast, method="REML")
summary(mod32)
##gam.check(mod32)

##plot(predict.gam(mod32, AllDates[,c(4,5)], allow.new.levels = TRUE) )

mod32pred<-as.data.frame(predict.gam(mod32, fred[,c(2,3)], allow.new.levels = TRUE))
mod32pred1<-cbind(fred,mod32pred)
colnames(mod32pred1)[4]<-c("NDVI")

MxDay <- merge(aggregate(NDVI~Year1, mod32pred1, max, na.action=na.omit), mod32pred1, by=c("Year1","NDVI"))
MxDay$Species<-"DryHeath"

DayMax<-mean(MxDay$DecDay)

AllHabsCast$TimeXX<-AllHabsCast$Time+(1-DayMax)
AllHabsCast$DecDayXX<-AllHabsCast$TimeXX-trunc(AllHabsCast$TimeXX)
AllHabsCast$YearXX<-trunc(AllHabsCast$TimeXX)
AllHabsCast$YearXX<-as.factor(AllHabsCast$YearXX+1985)

mod33<-gam(DryHeath ~ s(DecDayXX, k=3, m=2) + s(DecDayXX, YearXX, k=3, bs="fs", m=2), na.action=na.omit, data =AllHabsCast, method="REML")
summary(mod33)

george<-as.data.frame(fred$Time)
colnames(george)<-c("TimeXX")
george$TimeXX<-george$TimeXX+(1-DayMax)
george$DecDayXX<-george$TimeXX-trunc(george$TimeXX)
george$YearXX<-as.factor(trunc(george$TimeXX))


mod33pred<-as.data.frame(predict.gam(mod33, george[,c(2,3)], allow.new.levels = TRUE))
mod33pred1<-cbind(george,mod33pred)
colnames(mod33pred1)[4]<-c("NDVI")


MxDay1<-rbind(MxDay1,MxDay)

###Eriophorum code

mod32<-gam(Eriophorum ~ s(DecDay, k=3, m=2) + s(DecDay, Year1, k=3, bs="fs", m=2), na.action=na.omit, data =AllHabsCast, method="REML")
summary(mod32)
##gam.check(mod32)

##plot(predict.gam(mod32, AllDates[,c(4,5)], allow.new.levels = TRUE) )

mod32pred<-as.data.frame(predict.gam(mod32, fred[,c(2,3)], allow.new.levels = TRUE))
mod32pred1<-cbind(fred,mod32pred)
colnames(mod32pred1)[4]<-c("NDVI")

MxDay <- merge(aggregate(NDVI~Year1, mod32pred1, max, na.action=na.omit), mod32pred1, by=c("Year1","NDVI"))
MxDay$Species<-"Eriophorum"

DayMax<-mean(MxDay$DecDay)

AllHabsCast$TimeXX<-AllHabsCast$Time+(1-DayMax)
AllHabsCast$DecDayXX<-AllHabsCast$TimeXX-trunc(AllHabsCast$TimeXX)
AllHabsCast$YearXX<-trunc(AllHabsCast$TimeXX)
AllHabsCast$YearXX<-as.factor(AllHabsCast$YearXX+1985)

mod33<-gam(Eriophorum ~ s(DecDayXX, k=3, m=2) + s(DecDayXX, YearXX, k=3, bs="fs", m=2), na.action=na.omit, data =AllHabsCast, method="REML")
summary(mod33)

george<-as.data.frame(fred$Time)
colnames(george)<-c("TimeXX")
george$TimeXX<-george$TimeXX+(1-DayMax)
george$DecDayXX<-george$TimeXX-trunc(george$TimeXX)
george$YearXX<-as.factor(trunc(george$TimeXX))


mod33pred<-as.data.frame(predict.gam(mod33, george[,c(2,3)], allow.new.levels = TRUE))
mod33pred1<-cbind(george,mod33pred)
colnames(mod33pred1)[4]<-c("NDVI")


MxDay1<-rbind(MxDay1,MxDay)

###FestucaRubra code

mod32<-gam(FestucaRubra ~ s(DecDay, k=3, m=2) + s(DecDay, Year1, k=3, bs="fs", m=2), na.action=na.omit, data =AllHabsCast, method="REML")
summary(mod32)
##gam.check(mod32)

##plot(predict.gam(mod32, AllDates[,c(4,5)], allow.new.levels = TRUE) )

mod32pred<-as.data.frame(predict.gam(mod32, fred[,c(2,3)], allow.new.levels = TRUE))
mod32pred1<-cbind(fred,mod32pred)
colnames(mod32pred1)[4]<-c("NDVI")

MxDay <- merge(aggregate(NDVI~Year1, mod32pred1, max, na.action=na.omit), mod32pred1, by=c("Year1","NDVI"))
MxDay$Species<-"FestucaRubra"

DayMax<-mean(MxDay$DecDay)

AllHabsCast$TimeXX<-AllHabsCast$Time+(1-DayMax)
AllHabsCast$DecDayXX<-AllHabsCast$TimeXX-trunc(AllHabsCast$TimeXX)
AllHabsCast$YearXX<-trunc(AllHabsCast$TimeXX)
AllHabsCast$YearXX<-as.factor(AllHabsCast$YearXX+1985)

mod33<-gam(FestucaRubra ~ s(DecDayXX, k=3, m=2) + s(DecDayXX, YearXX, k=3, bs="fs", m=2), na.action=na.omit, data =AllHabsCast, method="REML")
summary(mod33)

george<-as.data.frame(fred$Time)
colnames(george)<-c("TimeXX")
george$TimeXX<-george$TimeXX+(1-DayMax)
george$DecDayXX<-george$TimeXX-trunc(george$TimeXX)
george$YearXX<-as.factor(trunc(george$TimeXX))


mod33pred<-as.data.frame(predict.gam(mod33, george[,c(2,3)], allow.new.levels = TRUE))
mod33pred1<-cbind(george,mod33pred)
colnames(mod33pred1)[4]<-c("NDVI")


MxDay1<-rbind(MxDay1,MxDay)

###Holcus code

mod32<-gam(Holcus ~ s(DecDay, k=3, m=2) + s(DecDay, Year1, k=3, bs="fs", m=2), na.action=na.omit, data =AllHabsCast, method="REML")
summary(mod32)
##gam.check(mod32)

##plot(predict.gam(mod32, AllDates[,c(4,5)], allow.new.levels = TRUE) )

mod32pred<-as.data.frame(predict.gam(mod32, fred[,c(2,3)], allow.new.levels = TRUE))
mod32pred1<-cbind(fred,mod32pred)
colnames(mod32pred1)[4]<-c("NDVI")

MxDay <- merge(aggregate(NDVI~Year1, mod32pred1, max, na.action=na.omit), mod32pred1, by=c("Year1","NDVI"))
MxDay$Species<-"Holcus"

DayMax<-mean(MxDay$DecDay)

AllHabsCast$TimeXX<-AllHabsCast$Time+(1-DayMax)
AllHabsCast$DecDayXX<-AllHabsCast$TimeXX-trunc(AllHabsCast$TimeXX)
AllHabsCast$YearXX<-trunc(AllHabsCast$TimeXX)
AllHabsCast$YearXX<-as.factor(AllHabsCast$YearXX+1985)

mod33<-gam(Holcus ~ s(DecDayXX, k=3, m=2) + s(DecDayXX, YearXX, k=3, bs="fs", m=2), na.action=na.omit, data =AllHabsCast, method="REML")
summary(mod33)

george<-as.data.frame(fred$Time)
colnames(george)<-c("TimeXX")
george$TimeXX<-george$TimeXX+(1-DayMax)
george$DecDayXX<-george$TimeXX-trunc(george$TimeXX)
george$YearXX<-as.factor(trunc(george$TimeXX))


mod33pred<-as.data.frame(predict.gam(mod33, george[,c(2,3)], allow.new.levels = TRUE))
mod33pred1<-cbind(george,mod33pred)
colnames(mod33pred1)[4]<-c("NDVI")


MxDay1<-rbind(MxDay1,MxDay)

###HolcusAgrostis code

mod32<-gam(HolcusAgrostis ~ s(DecDay, k=3, m=2) + s(DecDay, Year1, k=3, bs="fs", m=2), na.action=na.omit, data =AllHabsCast, method="REML")
summary(mod32)
##gam.check(mod32)

##plot(predict.gam(mod32, AllDates[,c(4,5)], allow.new.levels = TRUE) )

mod32pred<-as.data.frame(predict.gam(mod32, fred[,c(2,3)], allow.new.levels = TRUE))
mod32pred1<-cbind(fred,mod32pred)
colnames(mod32pred1)[4]<-c("NDVI")

MxDay <- merge(aggregate(NDVI~Year1, mod32pred1, max, na.action=na.omit), mod32pred1, by=c("Year1","NDVI"))
MxDay$Species<-"HolcusAgrostis"

DayMax<-mean(MxDay$DecDay)

AllHabsCast$TimeXX<-AllHabsCast$Time+(1-DayMax)
AllHabsCast$DecDayXX<-AllHabsCast$TimeXX-trunc(AllHabsCast$TimeXX)
AllHabsCast$YearXX<-trunc(AllHabsCast$TimeXX)
AllHabsCast$YearXX<-as.factor(AllHabsCast$YearXX+1985)

mod33<-gam(HolcusAgrostis ~ s(DecDayXX, k=3, m=2) + s(DecDayXX, YearXX, k=3, bs="fs", m=2), na.action=na.omit, data =AllHabsCast, method="REML")
summary(mod33)

george<-as.data.frame(fred$Time)
colnames(george)<-c("TimeXX")
george$TimeXX<-george$TimeXX+(1-DayMax)
george$DecDayXX<-george$TimeXX-trunc(george$TimeXX)
george$YearXX<-as.factor(trunc(george$TimeXX))


mod33pred<-as.data.frame(predict.gam(mod33, george[,c(2,3)], allow.new.levels = TRUE))
mod33pred1<-cbind(george,mod33pred)
colnames(mod33pred1)[4]<-c("NDVI")


MxDay1<-rbind(MxDay1,MxDay)

###HolcusAgrostisSphagnum code

mod32<-gam(HolcusAgrostisSphagnum ~ s(DecDay, k=3, m=2) + s(DecDay, Year1, k=3, bs="fs", m=2), na.action=na.omit, data =AllHabsCast, method="REML")
summary(mod32)
##gam.check(mod32)

##plot(predict.gam(mod32, AllDates[,c(4,5)], allow.new.levels = TRUE) )

mod32pred<-as.data.frame(predict.gam(mod32, fred[,c(2,3)], allow.new.levels = TRUE))
mod32pred1<-cbind(fred,mod32pred)
colnames(mod32pred1)[4]<-c("NDVI")

MxDay <- merge(aggregate(NDVI~Year1, mod32pred1, max, na.action=na.omit), mod32pred1, by=c("Year1","NDVI"))
MxDay$Species<-"HolcusAgrostisSphagnum"

DayMax<-mean(MxDay$DecDay)

AllHabsCast$TimeXX<-AllHabsCast$Time+(1-DayMax)
AllHabsCast$DecDayXX<-AllHabsCast$TimeXX-trunc(AllHabsCast$TimeXX)
AllHabsCast$YearXX<-trunc(AllHabsCast$TimeXX)
AllHabsCast$YearXX<-as.factor(AllHabsCast$YearXX+1985)

mod33<-gam(HolcusAgrostisSphagnum ~ s(DecDayXX, k=3, m=2) + s(DecDayXX, YearXX, k=3, bs="fs", m=2), na.action=na.omit, data =AllHabsCast, method="REML")
summary(mod33)

george<-as.data.frame(fred$Time)
colnames(george)<-c("TimeXX")
george$TimeXX<-george$TimeXX+(1-DayMax)
george$DecDayXX<-george$TimeXX-trunc(george$TimeXX)
george$YearXX<-as.factor(trunc(george$TimeXX))


mod33pred<-as.data.frame(predict.gam(mod33, george[,c(2,3)], allow.new.levels = TRUE))
mod33pred1<-cbind(george,mod33pred)
colnames(mod33pred1)[4]<-c("NDVI")

MxDay1<-rbind(MxDay1,MxDay)

###HolcusPoa code

mod32<-gam(HolcusPoa ~ s(DecDay, k=3, m=2) + s(DecDay, Year1, k=3, bs="fs", m=2), na.action=na.omit, data =AllHabsCast, method="REML")
summary(mod32)
##gam.check(mod32)

##plot(predict.gam(mod32, AllDates[,c(4,5)], allow.new.levels = TRUE) )

mod32pred<-as.data.frame(predict.gam(mod32, fred[,c(2,3)], allow.new.levels = TRUE))
mod32pred1<-cbind(fred,mod32pred)
colnames(mod32pred1)[4]<-c("NDVI")

MxDay <- merge(aggregate(NDVI~Year1, mod32pred1, max, na.action=na.omit), mod32pred1, by=c("Year1","NDVI"))
MxDay$Species<-"HolcusPoa"

DayMax<-mean(MxDay$DecDay)

AllHabsCast$TimeXX<-AllHabsCast$Time+(1-DayMax)
AllHabsCast$DecDayXX<-AllHabsCast$TimeXX-trunc(AllHabsCast$TimeXX)
AllHabsCast$YearXX<-trunc(AllHabsCast$TimeXX)
AllHabsCast$YearXX<-as.factor(AllHabsCast$YearXX+1985)

mod33<-gam(HolcusPoa ~ s(DecDayXX, k=3, m=2) + s(DecDayXX, YearXX, k=3, bs="fs", m=2), na.action=na.omit, data =AllHabsCast, method="REML")
summary(mod33)

george<-as.data.frame(fred$Time)
colnames(george)<-c("TimeXX")
george$TimeXX<-george$TimeXX+(1-DayMax)
george$DecDayXX<-george$TimeXX-trunc(george$TimeXX)
george$YearXX<-as.factor(trunc(george$TimeXX))


mod33pred<-as.data.frame(predict.gam(mod33, george[,c(2,3)], allow.new.levels = TRUE))
mod33pred1<-cbind(george,mod33pred)
colnames(mod33pred1)[4]<-c("NDVI")

MxDay1<-rbind(MxDay1,MxDay)

###Lair code

mod32<-gam(Lair ~ s(DecDay, k=3, m=2) + s(DecDay, Year1, k=3, bs="fs", m=2), na.action=na.omit, data =AllHabsCast, method="REML")
summary(mod32)
##gam.check(mod32)

##plot(predict.gam(mod32, AllDates[,c(4,5)], allow.new.levels = TRUE) )

mod32pred<-as.data.frame(predict.gam(mod32, fred[,c(2,3)], allow.new.levels = TRUE))
mod32pred1<-cbind(fred,mod32pred)
colnames(mod32pred1)[4]<-c("NDVI")

MxDay <- merge(aggregate(NDVI~Year1, mod32pred1, max, na.action=na.omit), mod32pred1, by=c("Year1","NDVI"))
MxDay$Species<-"Lair"

DayMax<-mean(MxDay$DecDay)

AllHabsCast$TimeXX<-AllHabsCast$Time+(1-DayMax)
AllHabsCast$DecDayXX<-AllHabsCast$TimeXX-trunc(AllHabsCast$TimeXX)
AllHabsCast$YearXX<-trunc(AllHabsCast$TimeXX)
AllHabsCast$YearXX<-as.factor(AllHabsCast$YearXX+1985)

mod33<-gam(Lair ~ s(DecDayXX, k=3, m=2) + s(DecDayXX, YearXX, k=3, bs="fs", m=2), na.action=na.omit, data =AllHabsCast, method="REML")
summary(mod33)

george<-as.data.frame(fred$Time)
colnames(george)<-c("TimeXX")
george$TimeXX<-george$TimeXX+(1-DayMax)
george$DecDayXX<-george$TimeXX-trunc(george$TimeXX)
george$YearXX<-as.factor(trunc(george$TimeXX))


mod33pred<-as.data.frame(predict.gam(mod33, george[,c(2,3)], allow.new.levels = TRUE))
mod33pred1<-cbind(george,mod33pred)
colnames(mod33pred1)[4]<-c("NDVI")

MxDay1<-rbind(MxDay1,MxDay)

###Luzula code

mod32<-gam(Luzula ~ s(DecDay, k=3, m=2) + s(DecDay, Year1, k=3, bs="fs", m=2), na.action=na.omit, data =AllHabsCast, method="REML")
summary(mod32)
##gam.check(mod32)

##plot(predict.gam(mod32, AllDates[,c(4,5)], allow.new.levels = TRUE) )

mod32pred<-as.data.frame(predict.gam(mod32, fred[,c(2,3)], allow.new.levels = TRUE))
mod32pred1<-cbind(fred,mod32pred)
colnames(mod32pred1)[4]<-c("NDVI")

MxDay <- merge(aggregate(NDVI~Year1, mod32pred1, max, na.action=na.omit), mod32pred1, by=c("Year1","NDVI"))
MxDay$Species<-"Luzula"

DayMax<-mean(MxDay$DecDay)

AllHabsCast$TimeXX<-AllHabsCast$Time+(1-DayMax)
AllHabsCast$DecDayXX<-AllHabsCast$TimeXX-trunc(AllHabsCast$TimeXX)
AllHabsCast$YearXX<-trunc(AllHabsCast$TimeXX)
AllHabsCast$YearXX<-as.factor(AllHabsCast$YearXX+1985)

mod33<-gam(Luzula ~ s(DecDayXX, k=3, m=2) + s(DecDayXX, YearXX, k=3, bs="fs", m=2), na.action=na.omit, data =AllHabsCast, method="REML")
summary(mod33)

george<-as.data.frame(fred$Time)
colnames(george)<-c("TimeXX")
george$TimeXX<-george$TimeXX+(1-DayMax)
george$DecDayXX<-george$TimeXX-trunc(george$TimeXX)
george$YearXX<-as.factor(trunc(george$TimeXX))


mod33pred<-as.data.frame(predict.gam(mod33, george[,c(2,3)], allow.new.levels = TRUE))
mod33pred1<-cbind(george,mod33pred)
colnames(mod33pred1)[4]<-c("NDVI")

MxDay1<-rbind(MxDay1,MxDay)

###Molinia code

mod32<-gam(Molinia ~ s(DecDay, k=3, m=2) + s(DecDay, Year1, k=3, bs="fs", m=2), na.action=na.omit, data =AllHabsCast, method="REML")
summary(mod32)
##gam.check(mod32)

##plot(predict.gam(mod32, AllDates[,c(4,5)], allow.new.levels = TRUE) )

mod32pred<-as.data.frame(predict.gam(mod32, fred[,c(2,3)], allow.new.levels = TRUE))
mod32pred1<-cbind(fred,mod32pred)
colnames(mod32pred1)[4]<-c("NDVI")

MxDay <- merge(aggregate(NDVI~Year1, mod32pred1, max, na.action=na.omit), mod32pred1, by=c("Year1","NDVI"))
MxDay$Species<-"Molinia"

DayMax<-mean(MxDay$DecDay)

AllHabsCast$TimeXX<-AllHabsCast$Time+(1-DayMax)
AllHabsCast$DecDayXX<-AllHabsCast$TimeXX-trunc(AllHabsCast$TimeXX)
AllHabsCast$YearXX<-trunc(AllHabsCast$TimeXX)
AllHabsCast$YearXX<-as.factor(AllHabsCast$YearXX+1985)

mod33<-gam(Molinia ~ s(DecDayXX, k=3, m=2) + s(DecDayXX, YearXX, k=3, bs="fs", m=2), na.action=na.omit, data =AllHabsCast, method="REML")
summary(mod33)

george<-as.data.frame(fred$Time)
colnames(george)<-c("TimeXX")
george$TimeXX<-george$TimeXX+(1-DayMax)
george$DecDayXX<-george$TimeXX-trunc(george$TimeXX)
george$YearXX<-as.factor(trunc(george$TimeXX))


mod33pred<-as.data.frame(predict.gam(mod33, george[,c(2,3)], allow.new.levels = TRUE))
mod33pred1<-cbind(george,mod33pred)
colnames(mod33pred1)[4]<-c("NDVI")

MxDay1<-rbind(MxDay1,MxDay)

###NardusJuncus code

mod32<-gam(NardusJuncus ~ s(DecDay, k=2, m=2) + s(DecDay, Year1, k=2, bs="fs", m=2), na.action=na.omit, data =AllHabsCast, method="REML")
summary(mod32)
##gam.check(mod32)

##plot(predict.gam(mod32, AllDates[,c(4,5)], allow.new.levels = TRUE) )

mod32pred<-as.data.frame(predict.gam(mod32, fred[,c(2,3)], allow.new.levels = TRUE))
mod32pred1<-cbind(fred,mod32pred)
colnames(mod32pred1)[4]<-c("NDVI")

MxDay <- merge(aggregate(NDVI~Year1, mod32pred1, max, na.action=na.omit), mod32pred1, by=c("Year1","NDVI"))
MxDay$Species<-"NardusJuncus"

DayMax<-mean(MxDay$DecDay)

AllHabsCast$TimeXX<-AllHabsCast$Time+(1-DayMax)
AllHabsCast$DecDayXX<-AllHabsCast$TimeXX-trunc(AllHabsCast$TimeXX)
AllHabsCast$YearXX<-trunc(AllHabsCast$TimeXX)
AllHabsCast$YearXX<-as.factor(AllHabsCast$YearXX+1985)

mod33<-gam(NardusJuncus ~ s(DecDayXX, k=2, m=2) + s(DecDayXX, YearXX, k=2, bs="fs", m=2), na.action=na.omit, data =AllHabsCast, method="REML")
summary(mod33)

george<-as.data.frame(fred$Time)
colnames(george)<-c("TimeXX")
george$TimeXX<-george$TimeXX+(1-DayMax)
george$DecDayXX<-george$TimeXX-trunc(george$TimeXX)
george$YearXX<-as.factor(trunc(george$TimeXX))


mod33pred<-as.data.frame(predict.gam(mod33, george[,c(2,3)], allow.new.levels = TRUE))
mod33pred1<-cbind(george,mod33pred)
colnames(mod33pred1)[4]<-c("NDVI")

MxDay1<-rbind(MxDay1,MxDay)

###NardusRacomitrium code

mod32<-gam(NardusRacomitrium ~ s(DecDay, k=3, m=2) + s(DecDay, Year1, k=3, bs="fs", m=2), na.action=na.omit, data =AllHabsCast, method="REML")
summary(mod32)
##gam.check(mod32)

##plot(predict.gam(mod32, AllDates[,c(4,5)], allow.new.levels = TRUE) )

mod32pred<-as.data.frame(predict.gam(mod32, fred[,c(2,3)], allow.new.levels = TRUE))
mod32pred1<-cbind(fred,mod32pred)
colnames(mod32pred1)[4]<-c("NDVI")

MxDay <- merge(aggregate(NDVI~Year1, mod32pred1, max, na.action=na.omit), mod32pred1, by=c("Year1","NDVI"))
MxDay$Species<-"NardusRacomitrium"

DayMax<-mean(MxDay$DecDay)

AllHabsCast$TimeXX<-AllHabsCast$Time+(1-DayMax)
AllHabsCast$DecDayXX<-AllHabsCast$TimeXX-trunc(AllHabsCast$TimeXX)
AllHabsCast$YearXX<-trunc(AllHabsCast$TimeXX)
AllHabsCast$YearXX<-as.factor(AllHabsCast$YearXX+1985)

mod33<-gam(NardusRacomitrium ~ s(DecDayXX, k=3, m=2) + s(DecDayXX, YearXX, k=3, bs="fs", m=2), na.action=na.omit, data =AllHabsCast, method="REML")
summary(mod33)

george<-as.data.frame(fred$Time)
colnames(george)<-c("TimeXX")
george$TimeXX<-george$TimeXX+(1-DayMax)
george$DecDayXX<-george$TimeXX-trunc(george$TimeXX)
george$YearXX<-as.factor(trunc(george$TimeXX))


mod33pred<-as.data.frame(predict.gam(mod33, george[,c(2,3)], allow.new.levels = TRUE))
mod33pred1<-cbind(george,mod33pred)
colnames(mod33pred1)[4]<-c("NDVI")

MxDay1<-rbind(MxDay1,MxDay)

###Plantago code

mod32<-gam(Plantago ~ s(DecDay, k=3, m=2) + s(DecDay, Year1, k=3, bs="fs", m=2), na.action=na.omit, data =AllHabsCast, method="REML")
summary(mod32)
##gam.check(mod32)

##plot(predict.gam(mod32, AllDates[,c(4,5)], allow.new.levels = TRUE) )

mod32pred<-as.data.frame(predict.gam(mod32, fred[,c(2,3)], allow.new.levels = TRUE))
mod32pred1<-cbind(fred,mod32pred)
colnames(mod32pred1)[4]<-c("NDVI")

MxDay <- merge(aggregate(NDVI~Year1, mod32pred1, max, na.action=na.omit), mod32pred1, by=c("Year1","NDVI"))
MxDay$Species<-"Plantago"

DayMax<-mean(MxDay$DecDay)

AllHabsCast$TimeXX<-AllHabsCast$Time+(1-DayMax)
AllHabsCast$DecDayXX<-AllHabsCast$TimeXX-trunc(AllHabsCast$TimeXX)
AllHabsCast$YearXX<-trunc(AllHabsCast$TimeXX)
AllHabsCast$YearXX<-as.factor(AllHabsCast$YearXX+1985)

mod33<-gam(Plantago ~ s(DecDayXX, k=3, m=2) + s(DecDayXX, YearXX, k=3, bs="fs", m=2), na.action=na.omit, data =AllHabsCast, method="REML")
summary(mod33)

george<-as.data.frame(fred$Time)
colnames(george)<-c("TimeXX")
george$TimeXX<-george$TimeXX+(1-DayMax)
george$DecDayXX<-george$TimeXX-trunc(george$TimeXX)
george$YearXX<-as.factor(trunc(george$TimeXX))


mod33pred<-as.data.frame(predict.gam(mod33, george[,c(2,3)], allow.new.levels = TRUE))
mod33pred1<-cbind(george,mod33pred)
colnames(mod33pred1)[4]<-c("NDVI")

MxDay1<-rbind(MxDay1,MxDay)


###PoorAgrostisFestuca code

mod32<-gam(PoorAgrostisFestuca ~ s(DecDay, k=3, m=2) + s(DecDay, Year1, k=3, bs="fs", m=2), na.action=na.omit, data =AllHabsCast, method="REML")
summary(mod32)
##gam.check(mod32)

##plot(predict.gam(mod32, AllDates[,c(4,5)], allow.new.levels = TRUE) )

mod32pred<-as.data.frame(predict.gam(mod32, fred[,c(2,3)], allow.new.levels = TRUE))
mod32pred1<-cbind(fred,mod32pred)
colnames(mod32pred1)[4]<-c("NDVI")

MxDay <- merge(aggregate(NDVI~Year1, mod32pred1, max, na.action=na.omit), mod32pred1, by=c("Year1","NDVI"))
MxDay$Species<-"PoorAgrostisFestuca"

DayMax<-mean(MxDay$DecDay)

AllHabsCast$TimeXX<-AllHabsCast$Time+(1-DayMax)
AllHabsCast$DecDayXX<-AllHabsCast$TimeXX-trunc(AllHabsCast$TimeXX)
AllHabsCast$YearXX<-trunc(AllHabsCast$TimeXX)
AllHabsCast$YearXX<-as.factor(AllHabsCast$YearXX+1985)

mod33<-gam(PoorAgrostisFestuca ~ s(DecDayXX, k=3, m=2) + s(DecDayXX, YearXX, k=3, bs="fs", m=2), na.action=na.omit, data =AllHabsCast, method="REML")
summary(mod33)

george<-as.data.frame(fred$Time)
colnames(george)<-c("TimeXX")
george$TimeXX<-george$TimeXX+(1-DayMax)
george$DecDayXX<-george$TimeXX-trunc(george$TimeXX)
george$YearXX<-as.factor(trunc(george$TimeXX))


mod33pred<-as.data.frame(predict.gam(mod33, george[,c(2,3)], allow.new.levels = TRUE))
mod33pred1<-cbind(george,mod33pred)
colnames(mod33pred1)[4]<-c("NDVI")

MxDay1<-rbind(MxDay1,MxDay)


###RichAgrostisFestuca code

mod32<-gam(RichAgrostisFestuca ~ s(DecDay, k=3, m=2) + s(DecDay, Year1, k=3, bs="fs", m=2), na.action=na.omit, data =AllHabsCast, method="REML")
summary(mod32)
##gam.check(mod32)

##plot(predict.gam(mod32, AllDates[,c(4,5)], allow.new.levels = TRUE) )

mod32pred<-as.data.frame(predict.gam(mod32, fred[,c(2,3)], allow.new.levels = TRUE))
mod32pred1<-cbind(fred,mod32pred)
colnames(mod32pred1)[4]<-c("NDVI")

MxDay <- merge(aggregate(NDVI~Year1, mod32pred1, max, na.action=na.omit), mod32pred1, by=c("Year1","NDVI"))
MxDay$Species<-"RichAgrostisFestuca"

DayMax<-mean(MxDay$DecDay)

AllHabsCast$TimeXX<-AllHabsCast$Time+(1-DayMax)
AllHabsCast$DecDayXX<-AllHabsCast$TimeXX-trunc(AllHabsCast$TimeXX)
AllHabsCast$YearXX<-trunc(AllHabsCast$TimeXX)
AllHabsCast$YearXX<-as.factor(AllHabsCast$YearXX+1985)

mod33<-gam(RichAgrostisFestuca ~ s(DecDayXX, k=3, m=2) + s(DecDayXX, YearXX, k=3, bs="fs", m=2), na.action=na.omit, data =AllHabsCast, method="REML")
summary(mod33)

george<-as.data.frame(fred$Time)
colnames(george)<-c("TimeXX")
george$TimeXX<-george$TimeXX+(1-DayMax)
george$DecDayXX<-george$TimeXX-trunc(george$TimeXX)
george$YearXX<-as.factor(trunc(george$TimeXX))


mod33pred<-as.data.frame(predict.gam(mod33, george[,c(2,3)], allow.new.levels = TRUE))
mod33pred1<-cbind(george,mod33pred)
colnames(mod33pred1)[4]<-c("NDVI")

MxDay1<-rbind(MxDay1,MxDay)


###Rumex code

mod32<-gam(Rumex ~ s(DecDay, k=3, m=2) + s(DecDay, Year1, k=3, bs="fs", m=2), na.action=na.omit, data =AllHabsCast, method="REML")
summary(mod32)
##gam.check(mod32)

##plot(predict.gam(mod32, AllDates[,c(4,5)], allow.new.levels = TRUE) )

mod32pred<-as.data.frame(predict.gam(mod32, fred[,c(2,3)], allow.new.levels = TRUE))
mod32pred1<-cbind(fred,mod32pred)
colnames(mod32pred1)[4]<-c("NDVI")

MxDay <- merge(aggregate(NDVI~Year1, mod32pred1, max, na.action=na.omit), mod32pred1, by=c("Year1","NDVI"))
MxDay$Species<-"Rumex"

DayMax<-mean(MxDay$DecDay)

AllHabsCast$TimeXX<-AllHabsCast$Time+(1-DayMax)
AllHabsCast$DecDayXX<-AllHabsCast$TimeXX-trunc(AllHabsCast$TimeXX)
AllHabsCast$YearXX<-trunc(AllHabsCast$TimeXX)
AllHabsCast$YearXX<-as.factor(AllHabsCast$YearXX+1985)

mod33<-gam(Rumex ~ s(DecDayXX, k=3, m=2) + s(DecDayXX, YearXX, k=3, bs="fs", m=2), na.action=na.omit, data =AllHabsCast, method="REML")
summary(mod33)

george<-as.data.frame(fred$Time)
colnames(george)<-c("TimeXX")
george$TimeXX<-george$TimeXX+(1-DayMax)
george$DecDayXX<-george$TimeXX-trunc(george$TimeXX)
george$YearXX<-as.factor(trunc(george$TimeXX))


mod33pred<-as.data.frame(predict.gam(mod33, george[,c(2,3)], allow.new.levels = TRUE))
mod33pred1<-cbind(george,mod33pred)
colnames(mod33pred1)[4]<-c("NDVI")


MxDay1<-rbind(MxDay1,MxDay)


###Sphagnum code

mod32<-gam(Sphagnum ~ s(DecDay, k=3, m=2) + s(DecDay, Year1, k=3, bs="fs", m=2), na.action=na.omit, data =AllHabsCast, method="REML")
summary(mod32)
##gam.check(mod32)

##plot(predict.gam(mod32, AllDates[,c(4,5)], allow.new.levels = TRUE) )

mod32pred<-as.data.frame(predict.gam(mod32, fred[,c(2,3)], allow.new.levels = TRUE))
mod32pred1<-cbind(fred,mod32pred)
colnames(mod32pred1)[4]<-c("NDVI")

MxDay <- merge(aggregate(NDVI~Year1, mod32pred1, max, na.action=na.omit), mod32pred1, by=c("Year1","NDVI"))
MxDay$Species<-"Sphagnum"

DayMax<-mean(MxDay$DecDay)

AllHabsCast$TimeXX<-AllHabsCast$Time+(1-DayMax)
AllHabsCast$DecDayXX<-AllHabsCast$TimeXX-trunc(AllHabsCast$TimeXX)
AllHabsCast$YearXX<-trunc(AllHabsCast$TimeXX)
AllHabsCast$YearXX<-as.factor(AllHabsCast$YearXX+1985)

mod33<-gam(Sphagnum ~ s(DecDayXX, k=3, m=2) + s(DecDayXX, YearXX, k=3, bs="fs", m=2), na.action=na.omit, data =AllHabsCast, method="REML")
summary(mod33)

george<-as.data.frame(fred$Time)
colnames(george)<-c("TimeXX")
george$TimeXX<-george$TimeXX+(1-DayMax)
george$DecDayXX<-george$TimeXX-trunc(george$TimeXX)
george$YearXX<-as.factor(trunc(george$TimeXX))


mod33pred<-as.data.frame(predict.gam(mod33, george[,c(2,3)], allow.new.levels = TRUE))
mod33pred1<-cbind(george,mod33pred)
colnames(mod33pred1)[4]<-c("NDVI")


MxDay1<-rbind(MxDay1,MxDay)


###WetHeath code

mod32<-gam(WetHeath ~ s(DecDay, k=3, m=2) + s(DecDay, Year1, k=3, bs="fs", m=2), na.action=na.omit, data =AllHabsCast, method="REML")
summary(mod32)
##gam.check(mod32)

##plot(predict.gam(mod32, AllDates[,c(4,5)], allow.new.levels = TRUE) )

mod32pred<-as.data.frame(predict.gam(mod32, fred[,c(2,3)], allow.new.levels = TRUE))
mod32pred1<-cbind(fred,mod32pred)
colnames(mod32pred1)[4]<-c("NDVI")

MxDay <- merge(aggregate(NDVI~Year1, mod32pred1, max, na.action=na.omit), mod32pred1, by=c("Year1","NDVI"))
MxDay$Species<-"WetHeath"

DayMax<-mean(MxDay$DecDay)

AllHabsCast$TimeXX<-AllHabsCast$Time+(1-DayMax)
AllHabsCast$DecDayXX<-AllHabsCast$TimeXX-trunc(AllHabsCast$TimeXX)
AllHabsCast$YearXX<-trunc(AllHabsCast$TimeXX)
AllHabsCast$YearXX<-as.factor(AllHabsCast$YearXX+1985)

mod33<-gam(WetHeath ~ s(DecDayXX, k=3, m=2) + s(DecDayXX, YearXX, k=3, bs="fs", m=2), na.action=na.omit, data =AllHabsCast, method="REML")
summary(mod33)

george<-as.data.frame(fred$Time)
colnames(george)<-c("TimeXX")
george$TimeXX<-george$TimeXX+(1-DayMax)
george$DecDayXX<-george$TimeXX-trunc(george$TimeXX)
george$YearXX<-as.factor(trunc(george$TimeXX))


mod33pred<-as.data.frame(predict.gam(mod33, george[,c(2,3)], allow.new.levels = TRUE))
mod33pred1<-cbind(george,mod33pred)
colnames(mod33pred1)[4]<-c("NDVI")

MxDay1<-rbind(MxDay1,MxDay)

###Hirta code

mod32<-gam(Hirta ~ s(DecDay, k=3, m=2) + s(DecDay, Year1, k=3, bs="fs", m=2), na.action=na.omit, data =AllHabsCast, method="REML")
summary(mod32)
##gam.check(mod32)

##plot(predict.gam(mod32, AllDates[,c(4,5)], allow.new.levels = TRUE) )

mod32pred<-as.data.frame(predict.gam(mod32, fred[,c(2,3)], allow.new.levels = TRUE))
mod32pred1<-cbind(fred,mod32pred)
colnames(mod32pred1)[4]<-c("NDVI")

MxDay <- merge(aggregate(NDVI~Year1, mod32pred1, max, na.action=na.omit), mod32pred1, by=c("Year1","NDVI"))
MxDay$Species<-"Hirta"

DayMax<-mean(MxDay$DecDay)

AllHabsCast$TimeXX<-AllHabsCast$Time+(1-DayMax)
AllHabsCast$DecDayXX<-AllHabsCast$TimeXX-trunc(AllHabsCast$TimeXX)
AllHabsCast$YearXX<-trunc(AllHabsCast$TimeXX)
AllHabsCast$YearXX<-as.factor(AllHabsCast$YearXX+1985)

mod33<-gam(Hirta ~ s(DecDayXX, k=3, m=2) + s(DecDayXX, YearXX, k=3, bs="fs", m=2), na.action=na.omit, data =AllHabsCast, method="REML")
summary(mod33)

george<-as.data.frame(fred$Time)
colnames(george)<-c("TimeXX")
george$TimeXX<-george$TimeXX+(1-DayMax)
george$DecDayXX<-george$TimeXX-trunc(george$TimeXX)
george$YearXX<-as.factor(trunc(george$TimeXX))


mod33pred<-as.data.frame(predict.gam(mod33, george[,c(2,3)], allow.new.levels = TRUE))
mod33pred1<-cbind(george,mod33pred)
colnames(mod33pred1)[4]<-c("NDVI")

MxDay1<-rbind(MxDay1,MxDay)


write.csv(MxDay1,"MxDay1.csv")


###
ClearDays<-as.data.frame(tapply(AllHabs1$CleanedNDVI..0.15., list(AllHabs1$Year,AllHabs1$Vegetation), function(x) {length(x[!is.na(x)])}))
ClearDays<-cbind(rownames(ClearDays), data.frame(ClearDays, row.names=NULL))
colnames(ClearDays)[1] <- "Year1"

ClearDaysMelt<-melt(ClearDays, id = c("Year1"))
colnames(ClearDaysMelt)[2] <- "Species"
colnames(ClearDaysMelt)[3] <- "ClearDays"

MxDay1<-merge(MxDay1, ClearDaysMelt, by=c("Year1","Species"))

colnames(ClearDaysMelt)[1] <- "YearXX"

write.csv(MxDay1,"MxDay1.csv")

#################################################
####read.csv back into 

MxDay1<-read.csv("MxDay1.csv")

MxNDVI<-dcast(MxDay1,Year1 ~ Species, value.var="NDVI")
MxCD<-dcast(MxDay1,Year1 ~ Species, value.var="ClearDays")
##MxTrue<-dcast(MxDay1,Year1 ~ Species, value.var="DecDay")

sink("TraitsMaxMin.txt")


###Looking to see if max and min show same positive slopes weighted by clear days

colnames(MxCD) <- paste0('W_', colnames(MxCD))

MxNDVI<-cbind(MxNDVI,MxCD)

for (i in 2:2){
  mod41<-lm(MxNDVI[,i]~MxNDVI$Year1, weights= MxNDVI[,i+22], na.action=na.omit)
  print(colnames(MxNDVI[i]))
  print(summary(mod41))
  
  c1<-as.data.frame(colnames(MxNDVI[i]))
  c2<-as.data.frame(t(summary(mod41)$coefficients[2,]))
  MxNDVIslope<-cbind(c1,c2)
}

for (i in 3:22){
  mod41<-lm(MxNDVI[,i]~MxNDVI$Year1, weights= MxNDVI[,i+22], na.action=na.omit)
  print(colnames(MxNDVI[i]))
  print(summary(mod41))
  
  c1<-as.data.frame(colnames(MxNDVI[i]))
  c2<-as.data.frame(t(summary(mod41)$coefficients[2,]))
  tempMxNDVI<-cbind(c1,c2)
  MxNDVIslope<-rbind(MxNDVIslope,tempMxNDVI)
}



Traits<-read.csv("Traits.csv")

names(MxNDVIslope)[1]<-paste("VegType")

MxTrNDVI<-merge(MxNDVIslope,Traits, all=TRUE)

MxTrNDVI<-MxTrNDVI[,-c(1,3:5)]

MxTrNDVI2<-MxTrNDVI[complete.cases(MxTrNDVI),]

options(na.action = "na.fail")

mod54<-lm(Estimate~SLA, data=MxTrNDVI2)
summary(mod54)

ggplot(MxTrNDVI2, aes(x=Traits$SLA, y=Estimate)) +
  geom_point(color="#69b3a2") +
  theme_classic() + geom_segment(aes(x=13, y=0.00179798, xend=34, yend=0.00267946))

mod54<-lm(Estimate~N, data=MxTrNDVI2)
summary(mod54)

mod55<-lm(Estimate~F, data=MxTrNDVI2)
summary(mod55)

mod56<-lm(Estimate~LDMC, data=MxTrNDVI2)
summary(mod56)

sink()