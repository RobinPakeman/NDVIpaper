##TimeSeries
##Test for correlation between max/min NDVI and sheep numbers

####To correlate maxNDVI with August heights and biomass
####and minNDVI with March heghts and biomass

##library(tidyr)
library(ggplot2)

library(reshape2)

setwd("C:/Users/RP40274/OneDrive - The James Hutton Institute/Documents/Pakeman/Projects(FF+outercore)/StKilda/Satellite/Paper/SupplementaryMaterial")

##Predmean<-read.csv("Means.csv", check.names=FALSE)
##MeanDay<-melt(Predmean,id.vars="VegType")
##colnames(MeanDay)<-c("Species","Year", "NDVI")

MxDay1<-read.csv("MxDay1.csv")

Popn<-read.csv("HirtaPop_simplified.csv")

MxDay2<-merge(MxDay1,Popn, by.x="Year1", by.y="HirtaCountYear")

##MeanDay2<-merge(MeanDay, Popn, by.x="Year", by.y="HirtaCountYear")

NAO<-read.csv("NAO.csv")

##Import weather data

PredWeather<-read.csv("PredictedMonthyl.csv")

TrainMonth<-read.csv("TrainGapsFilled.csv")


PredWeather<-PredWeather[,-1]

##Weather<-rbind(PredWeather,TrainMonth,Added)
Weather<-rbind(PredWeather,TrainMonth)

Weather$Season<-NA
Weather$Season[Weather$Month == "1"]<-"Winter"
Weather$Season[Weather$Month == "2"]<-"Winter"
Weather$Season[Weather$Month == "3"]<-"Spring"
Weather$Season[Weather$Month == "4"]<-"Spring"
Weather$Season[Weather$Month == "5"]<-"Spring"
Weather$Season[Weather$Month == "6"]<-"Summer"
Weather$Season[Weather$Month == "7"]<-"Summer"
Weather$Season[Weather$Month == "8"]<-"Summer"
Weather$Season[Weather$Month == "9"]<-"Autumn"
Weather$Season[Weather$Month == "10"]<-"Autumn"
Weather$Season[Weather$Month == "11"]<-"Autumn"
Weather$Season[Weather$Month == "12"]<-"Winter"

Weather$Half<-NA
Weather$Half[Weather$Month == "1"]<-"Winter"
Weather$Half[Weather$Month == "2"]<-"Winter"
Weather$Half[Weather$Month == "3"]<-"Winter"
Weather$Half[Weather$Month == "4"]<-"Winter"
Weather$Half[Weather$Month == "5"]<-"Summer"
Weather$Half[Weather$Month == "6"]<-"Summer"
Weather$Half[Weather$Month == "7"]<-"Summer"
Weather$Half[Weather$Month == "8"]<-"Summer"
Weather$Half[Weather$Month == "9"]<-"Summer"
Weather$Half[Weather$Month == "10"]<-"Summer"
Weather$Half[Weather$Month == "11"]<-"Winter"
Weather$Half[Weather$Month == "12"]<-"Winter"

Weather$YearAdj<-NA
Weather$YearAdj[Weather$Month == "1"]<--1
Weather$YearAdj[Weather$Month == "2"]<--1
Weather$YearAdj[Weather$Month == "3"]<--1
Weather$YearAdj[Weather$Month == "4"]<--1
Weather$YearAdj[Weather$Month == "5"]<-0
Weather$YearAdj[Weather$Month == "6"]<-0
Weather$YearAdj[Weather$Month == "7"]<-0
Weather$YearAdj[Weather$Month == "8"]<-0
Weather$YearAdj[Weather$Month == "9"]<-0
Weather$YearAdj[Weather$Month == "10"]<-0
Weather$YearAdj[Weather$Month == "11"]<-0
Weather$YearAdj[Weather$Month == "12"]<-0

Weather$WinterYear<-Weather$Year+Weather$YearAdj

SummerWeather<-Weather[Weather$Half == "Summer",]
WinterWeather<-Weather[Weather$Half == "Winter",]

SummerWeatherMean<-aggregate(cbind(SgAvWD,SgAvTG) ~ Year, data=SummerWeather, FUN= mean, na.rm=TRUE)
SummerWeatherSum<-aggregate(cbind(SgTtRRadj,SgGDDadj)~ Year, data=SummerWeather, FUN= sum, na.rm=TRUE)

SummerAgg<-merge(SummerWeatherMean,SummerWeatherSum,by.x="Year",by.y="Year")

WinterWeatherMean<-aggregate(cbind(SgAvWD,SgAvTG) ~ WinterYear, data=WinterWeather, FUN= mean, na.rm=TRUE)
WinterWeatherSum<-aggregate(cbind(SgTtRRadj,SgGDDadj)~ WinterYear, data=WinterWeather, FUN= sum, na.rm=TRUE)

WinterAgg<-merge(WinterWeatherMean,WinterWeatherSum,by.x="WinterYear",by.y="WinterYear")
WinterAgg<-merge(WinterAgg, NAO, by.x="WinterYear", by.y="Year")


SummerSTMean<-aggregate(cbind(STAvWD,STAvTG)~Year, data=SummerWeather, FUN= mean, na.rm=TRUE)
SummerSTSum<-aggregate(cbind(STTtRRadj)~Year, data=SummerWeather, FUN= sum, na.rm=TRUE)
SummerSTagg<-merge(SummerSTMean, SummerSTSum, by.x="Year",by.y="Year")

WinterSTMean<-aggregate(cbind(STAvWD,STAvTG)~WinterYear, data=WinterWeather, FUN= mean, na.rm=TRUE)
WinterSTSum<-aggregate(cbind(STTtRRadj)~WinterYear, data=WinterWeather, FUN= sum, na.rm=TRUE)
WinterSTagg<-merge(WinterSTMean, WinterSTSum, by.x="WinterYear",by.y="WinterYear")

write.csv(SummerAgg, "SummerAgg.csv")
write.csv(WinterAgg, "winterAgg.csv")


sink("results_cleardays.txt")


###Need to merge weather data with MxDay2 

MxDay3<-merge(MxDay2, WinterAgg, by.x="Year1", by.y="WinterYear")
##MxDay3<-merge(MxDay3, NAO, by.x="Year1", by.y = "Year")

veg <- unique(MxDay3$Species)

###Delta as a function of biomass
for (sp in veg){
  MxDay3_cut<-MxDay3[MxDay3$Species == sp,]
  print(sp)
  mod1<-lm(delta~NDVI, weights = ClearDays, data = MxDay3_cut)
  print(summary(mod1))
}

##Sheeep numbers
mod2<-lm(HirtaTotal~Year1, data = MxDay3_cut)
print(summary(mod2))

tiff("Numbers.tiff", width = 1600, height = 1000, units = "px", res = 400 )

ggplot(MxDay3_cut, aes(Year1, HirtaTotal)) +    # ggplot2 plot with labels
  geom_point() +
  geom_smooth(method=lm, se=FALSE, colour="black", linetype="dotted")+
  labs(x="Years", y="Sheep count")+
  theme_classic()

dev.off()

###Delta as a function of population size

mod1<-lm(delta~ HirtaTotal, data = MxDay3_cut, weights=ClearDays)
print(summary(mod1))

##Delta as a function of weather only

mod1<-lm(delta~SgAvWD, data = MxDay3_cut, weights=ClearDays)
print(summary(mod1))

mod1<-lm(delta~SgAvTG, data = MxDay3_cut, weights=ClearDays)
print(summary(mod1))

mod1<-lm(delta~SgGDDadj, data = MxDay3_cut, weights=ClearDays)
print(summary(mod1))

mod1<-lm(delta~SgTtRRadj, data = MxDay3_cut, weights=ClearDays)
print(summary(mod1))

mod1<-lm(delta~ WinterNAO, data = MxDay3_cut, weights=ClearDays)
print(summary(mod1))


##Delta as a function of August biomass and winter weather
for (sp in veg){
  MxDay3_cut<-MxDay3[MxDay3$Species == sp,]
  print(sp)
  mod1<-lm(delta~NDVI+ SgAvWD+ SgAvTG + SgTtRRadj, weights = ClearDays, data = MxDay3_cut)
  print(summary(mod1))
}


##Delta as a function of population size and winter weather

mod1<-lm(delta~ WinterNAO + HirtaTotal, data = MxDay3_cut, weights=ClearDays)
print(summary(mod1))

mod1<-lm(delta~ HirtaTotal + SgAvWD, data = MxDay3_cut, weights=ClearDays)
print(summary(mod1))

mod1<-lm(delta~ HirtaTotal + SgAvWD + WinterNAO, data = MxDay3_cut, weights=ClearDays)
print(summary(mod1))



#############################

##Delta as a function of August biomass and winter weather and population
for (sp in veg){
  MxDay3_cut<-MxDay3[MxDay3$Species == sp,]
  print(sp)
  mod1<-lm(delta~NDVI+ SgAvWD+ SgGDDadj + SgTtRRadj+ HirtaTotal, data = MxDay3_cut, weights=ClearDays)
  print(summary(mod1))
}


####Final set
for (sp in veg){
  MxDay3_cut<-MxDay3[MxDay3$Species == sp,]
  print(sp)
  mod1<-lm(delta~NDVI+ SgAvWD+ HirtaTotal, data = MxDay3_cut, weights=ClearDays)
  print(summary(mod1))
  print(AIC(mod1))
}

for (sp in veg){
  MxDay3_cut<-MxDay3[MxDay3$Species == sp,]
  print(sp)
  mod1<-lm(delta~NDVI+ WinterNAO + HirtaTotal, data = MxDay3_cut, weights=ClearDays)
  print(summary(mod1))
  print(AIC(mod1))
}


##Check for inclusion of vegetation in model
for (sp in veg){
  MxDay3_cut<-MxDay3[MxDay3$Species == sp,]
  print(sp)
  mod1<-lm(delta~NDVI+ WinterNAO + HirtaTotal, data = MxDay3_cut, weights=ClearDays)
  print(summary(mod1))
  mod2<-lm(delta~WinterNAO + HirtaTotal, data = MxDay3_cut, weights=ClearDays)
  print(anova(mod1,mod2))
}

for (sp in veg){
  MxDay3_cut<-MxDay3[MxDay3$Species == sp,]
  print(sp)
  mod1<-lm(delta~NDVI+ SgAvWD + HirtaTotal, data = MxDay3_cut, weights=ClearDays)
  print(summary(mod1))
  mod2<-lm(delta~SgAvWD + HirtaTotal, data = MxDay3_cut, weights=ClearDays)
  print(anova(mod1,mod2))
}


###Check for best out of NAO and wind
for (sp in veg){
  MxDay3_cut<-MxDay3[MxDay3$Species == sp,]
  print(sp)
  mod1<-lm(delta~NDVI+ WinterNAO + SgAvWD + HirtaTotal, data = MxDay3_cut, weights=ClearDays)
  print(summary(mod1))
}


##Weather winter graphs and time trends
mod4<-lm(SgAvWD~WinterYear, data = WinterAgg)
print(summary(mod4))

mod4<-lm(SgAvWD~WinterYear, data = subset(WinterAgg, WinterYear>1999))
print(summary(mod4))

tiff("WinterWind.tiff", width = 1600, height = 1000, units = "px", res = 400 )

ggplot(WinterAgg, aes(WinterYear, SgAvWD)) +    # ggplot2 plot with labels
  geom_point() +
  geom_smooth(method=lm, se=FALSE, colour="black", linetype="dotted")+
  xlab("Years")+
  ylab(expression(Winter~wind~speed~m~s^{-1}))+
  theme_classic()

dev.off()


mod4<-lm(SgAvTG~WinterYear, data = WinterAgg)
print(summary(mod4))

mod4<-lm(SgAvTG~WinterYear, data = subset(WinterAgg, WinterYear>1999))
print(summary(mod4))

tiff("WinterTemp.tiff", width = 1600, height = 1000, units = "px", res = 400 )

ggplot(WinterAgg, aes(WinterYear, SgAvTG)) +    # ggplot2 plot with labels
  geom_point() +
  geom_smooth(method=lm, se=FALSE, colour="black", linetype="dotted")+
  xlab("Years")+
  ylab(expression("Winter temperature °C"))+
  theme_classic()

dev.off()


mod4<-lm(SgGDDadj~WinterYear, data = WinterAgg)
print(summary(mod4))

mod4<-lm(SgTtRRadj~WinterYear, data = WinterAgg)
print(summary(mod4))

mod4<-lm(SgTtRRadj~WinterYear, subset(WinterAgg, WinterYear>1999))
print(summary(mod4))

tiff("WinterRain.tiff", width = 1600, height = 1000, units = "px", res = 400 )

ggplot(WinterAgg, aes(WinterYear, SgTtRRadj)) +    # ggplot2 plot with labels
  geom_point() +
  geom_smooth(method=lm, se=FALSE, colour="black", linetype="dotted")+
  xlab("Years")+
  ylab(expression("Winter rainfall mm"))+
  theme_classic()

dev.off()


tiff("WinterNAO.tiff", width = 1600, height = 1000, units = "px", res = 400 )

ggplot(WinterAgg, aes(WinterYear, WinterNAO)) +    # ggplot2 plot with labels
  geom_point() +
  geom_smooth(method=lm, se=FALSE, colour="black", linetype="dotted")+
  xlab("Years")+
  ylab(expression("Winter NAO"))+
  theme_classic()

dev.off()

mod4<-lm(WinterNAO~WinterYear, data = WinterAgg)
print(summary(mod4))




##Weather summer
mod4<-lm(SgAvWD~Year, data = SummerAgg)
print(summary(mod4))

mod4<-lm(SgAvWD~Year, data = subset(SummerAgg, Year>1999))
print(summary(mod4))

mod4<-lm(SgAvTG~Year, data = SummerAgg)
print(summary(mod4))

mod4<-lm(SgAvTG~Year, data = subset(SummerAgg, Year>1999))
print(summary(mod4))

mod4<-lm(SgGDDadj~Year, data = SummerAgg)
print(summary(mod4))

mod4<-lm(SgTtRRadj~Year, data = SummerAgg)
print(summary(mod4))

mod4<-lm(SgTtRRadj~Year, data = subset(SummerAgg, Year>1999))
print(summary(mod4))



sink()

tiff("Wind.tiff", width = 1600, height = 1600, units = "px", res = 400 )

expr1<-expression(Average~wind~speed~m~s^-1)
ggplot(WinterAgg, aes(x=WinterYear, y=SgAvWD)) +
  geom_point(color="#69b3a2") +
  labs(x = "Year", y=expr1) +
  theme_classic() + geom_segment(aes(x=1979, y=4.236, xend=2019, yend=4.653))

dev.off()

tiff("Temp.tiff", width = 1600, height = 1600, units = "px", res = 400 )

expr1<-expression("Average Temperature (°C)")
ggplot(WinterAgg, aes(x=WinterYear, y=SgAvTG)) +
  geom_point(color="#69b3a2") +
  labs(x = "Year", y=expr1) +
  theme_classic() + geom_segment(aes(x=1979, y=7.029, xend=2019, yend=7.652))

dev.off()

tiff("Rain.tiff", width = 1600, height = 1600, units = "px", res = 400 )

expr1<-expression("Total Rainfall (mm)")
ggplot(WinterAgg, aes(x=WinterYear, y=SgTtRRadj)) +
  geom_point(color="#69b3a2") +
  labs(x = "Year", y=expr1) +
  theme_classic() + geom_segment(aes(x=1979, y=515.5, xend=2019, yend=927.1))

dev.off()


####Stornoway graphs

tiff("STWind.tiff", width = 1600, height = 1600, units = "px", res = 400 )

expr1<-expression(Average~wind~speed~m~s^-1)
ggplot(WinterSTagg, aes(x=WinterYear, y=STAvWD)) +
  geom_point(color="#69b3a2") +
  labs(x = "Year", y=expr1) +
  theme_classic() + geom_segment(aes(x=1979, y=12.086, xend=2019, yend=13.655))

dev.off()

mod4<-lm(STAvWD~WinterYear, data = WinterSTagg)
print(summary(mod4))


tiff("STTemp.tiff", width = 1600, height = 1600, units = "px", res = 400 )

expr1<-expression("Average Temperature (°C)")
ggplot(WinterSTagg, aes(x=WinterYear, y=STAvTG)) +
  geom_point(color="#69b3a2") +
  labs(x = "Year", y=expr1) +
  theme_classic() + geom_segment(aes(x=1979, y=5.1698, xend=2019, yend=6.2865))

dev.off()

mod4<-lm(STAvTG~WinterYear, data = WinterSTagg)
print(summary(mod4))


tiff("STRain.tiff", width = 1600, height = 1600, units = "px", res = 400 )

expr1<-expression("Total Rainfall (mm)")
ggplot(WinterSTagg, aes(x=WinterYear, y=STTtRRadj)) +
  geom_point(color="#69b3a2") +
  labs(x = "Year", y=expr1) +
  theme_classic() + geom_segment(aes(x=1979, y=615.55, xend=2019, yend=1450.586))

dev.off()


##Stornoway weather trends
mod4<-lm(STTtRRadj~WinterYear, data = WinterSTagg)
print(summary(mod4))


mod4<-lm(STAvWD~Year, data = SummerSTagg)
print(summary(mod4))

mod4<-lm(STAvTG~Year, data = SummerSTagg)
print(summary(mod4))

mod4<-lm(STTtRRadj~Year, data = SummerSTagg)
print(summary(mod4))
