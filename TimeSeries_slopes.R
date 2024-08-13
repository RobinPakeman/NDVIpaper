##To analyse the increase in NDVI with time for the Landsat time series

library(reshape2)
library(lme4)
library(lmerTest)
library(mgcv)
library(tidyr)
library(ggplot2)
library(ggrepel)

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

for (i in 6:6){


  AHCtemp<-as.data.frame(cbind(AllHabsCast$Time,AllHabsCast[,i],AllHabsCast$Year))  
  colnames(AHCtemp)<-c("Time","NDVI","Year")
  AHCtemp$Year1<-as.factor(AHCtemp$Year)
  AHCtemp1<-AHCtemp[!is.na(AHCtemp$NDVI),]


  model2<-lmer(NDVI~Time+sin(Time*2*pi)+cos(Time*2*pi)+(1|Year1),data=AHCtemp1, REML=FALSE, na.action=na.pass)
  model3<-lmer(NDVI~sin(Time*2*pi)+cos(Time*2*pi)+(1|Year1),data=AHCtemp1, REML=FALSE)
  anova(model2,model3)
  model4<-lmer(NDVI~Time+sin(Time*2*pi)+cos(Time*2*pi)+(1|Year1),data=AHCtemp1, REML=TRUE)

 ## model5<-lmer(NDVI~Year1+sin(Time*2*pi)+cos(Time*2*pi)+(1|Year1),data=AHCtemp1, REML=TRUE)
  
##model2<-lmer(AllHabsCast[,i]~Time+sin(Time*2*pi)+cos(Time*2*pi)+(1|Year1),data=AllHabsCast, REML=FALSE, na.action=na.pass)
##model3<-lmer(AllHabsCast[,i]~sin(Time*2*pi)+cos(Time*2*pi)+(1|Year1),data=AllHabsCast, REML=FALSE)
##anova(model2,model3)
##model4<-lmer(AllHabsCast[,i]~Time+sin(Time*2*pi)+cos(Time*2*pi)+(1|Year1),data=AllHabsCast, REML=TRUE)

  c1<-as.data.frame(colnames(AllHabsCast[i]))
  c2a<-as.data.frame(t(summary(model4)$coefficients[1,]))
  c2b<-as.data.frame(t(summary(model4)$coefficients[2,]))
  c3<-as.data.frame(anova(model2,model3)$'Pr(>Chisq)'[2])
  Slopes<-cbind(c1,c2a,c2b,c3)

}

##Just taking one vegetation type Agrostis-Festuca-Molinia grassland
##Then following analysis method set out by Crawley in The R Book

AHC<-as.data.frame(AllHabsCast[,c(1,5)])
AHC$Year1<-as.factor(AHC$Year)
AHC<-AHC[AHC$Year1 != 2003, ]
AHC<-AHC[AHC$Year1 != 2012, ]   
mod_p<-as.data.frame(predict(model4, newdata=AHC))
AHC2<-cbind(AHC,mod_p)
colnames(AHC2)[4] <-"Pred"
AHC2$Time1<-1985+AHC2$Time

ggplot(AllHabsCast, aes(x=Time, y=AgFesMol)) +
  geom_line( color="#69b3a2") + 
  geom_point(color="#69b3a2") +
  theme_classic() + geom_segment(aes(x=0, y=0.3752, xend=34, yend=0.49721))

tiff("AgFesMol_NDVI.tiff", width = 1600, height = 1000, units = "px", res = 400 )

ggplot(AHC2, aes(x=Time1, y=Pred)) +
  geom_line( color="#69b3a2") + 
  geom_point(color="#69b3a2") +
  labs(x = "Year", y="NDVI") +
  theme_classic() + geom_segment(aes(x=1985, y=0.3752, xend=2019, yend=0.49721))

dev.off()

for (i in 7:26){
  
## AllHabsCast$Year1<-as.factor(AllHabsCast$Year)
  ##model2<-lmer(AllHabsCast[,i]~Time+sin(Time*2*pi)+cos(Time*2*pi)+(1|Year1),data=AllHabsCast, REML=FALSE)
  ##model3<-lmer(AllHabsCast[,i]~sin(Time*2*pi)+cos(Time*2*pi)+(1|Year1),data=AllHabsCast, REML=FALSE)
  ##anova(model2,model3)
  ##model4<-lmer(AllHabsCast[,i]~Time+sin(Time*2*pi)+cos(Time*2*pi)+(1|Year1),data=AllHabsCast, REML=TRUE)
  AHCtemp<-as.data.frame(cbind(AllHabsCast$Time,AllHabsCast[,i],AllHabsCast$Year))  
  AHCtemp1<-AHCtemp[!is.na(AHCtemp[,2]),]
  colnames(AHCtemp1)<-c("Time","NDVI","Year1")
  
  model2<-lmer(NDVI~Time+sin(Time*2*pi)+cos(Time*2*pi)+(1|Year1),data=AHCtemp1, REML=FALSE, na.action=na.pass)
  model3<-lmer(NDVI~sin(Time*2*pi)+cos(Time*2*pi)+(1|Year1),data=AHCtemp1, REML=FALSE)
  anova(model2,model3)
  model4<-lmer(NDVI~Time+sin(Time*2*pi)+cos(Time*2*pi)+(1|Year1),data=AHCtemp1, REML=TRUE)
  
  
  c1<-as.data.frame(colnames(AllHabsCast[i]))
  c2a<-as.data.frame(t(summary(model4)$coefficients[1,]))
  c2b<-as.data.frame(t(summary(model4)$coefficients[2,]))
  c3<-as.data.frame(anova(model2,model3)$'Pr(>Chisq)'[2])
  c123<-cbind(c1,c2a,c2b,c3)
  Slopes<-rbind(Slopes,c123)
}

colnames(Slopes)<-c("VegType","Intercept","IntSE","Intdf","Intt","Int_Pr","Slope","SlopeSE","Slopedf","Slopet","SlopePr","Pr-chisq")

write.csv(Slopes,"Slopes2.csv")

Slopes<-read.csv("Slopes2.csv")

sink("TraitsResults.txt")

Traits<-read.csv("Traits.csv")

SlTr<-merge(Slopes,Traits, all=TRUE)

for (i in 13:16){
  print(colnames(SlTr[i]))
  mod1<-lm(SlTr$Slope~SlTr[,i])
  print(summary(mod1))
##  plot(SlTr[,i],SlTr$Slope, xlab=colnames(SlTr[i]))
}


plot(SlTr[,15],SlTr$Slope, xlab=colnames(SlTr[15]), ylab="Slopes", bty="L")
text(SlTr[,15],SlTr$Slope,labels=SlTr$Vegtype, pos=1, offset = 0.5, cex=0.7)

tiff("LDMC.tiff", width = 1600, height = 1000, units = "px", res = 400 )

ggplot(SlTr, aes(LDMC, Slope, label = ShortName)) +    # ggplot2 plot with labels
  geom_point() +
  geom_smooth(method=lm, se=FALSE, colour="black", linetype="dotted")+
  geom_text_repel(size=2, aes(label = ShortName))+
  theme_classic()

dev.off()

tiff("SLA.tiff", width = 1600, height = 1000, units = "px", res = 400 )

ggplot(SlTr, aes(SLA, Slope, label = ShortName)) +    # ggplot2 plot with labels
  geom_point() +
  geom_smooth(method=lm, se=FALSE, colour="black", linetype="dotted")+
  geom_text_repel(size=2, aes(label = ShortName))+
  theme_classic()

dev.off()

tiff("N.tiff", width = 1600, height = 1000, units = "px", res = 400 )

ggplot(SlTr, aes(N, Slope, label = ShortName)) +    # ggplot2 plot with labels
  geom_point() +
  geom_smooth(method=lm, se=FALSE, colour="black", linetype="dotted")+
  geom_text_repel(size=2, aes(label = ShortName))+
  theme_classic()

dev.off()


sink()