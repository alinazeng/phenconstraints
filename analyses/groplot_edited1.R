
###Start of code to make a plot of when different phenological phases (growth, flowering, fruiting) 
#occur during the growing season at Arnold ARboretum, for Sally Gee's thesis data
#STarted by Ailene, Feb 1, 2016, then handed off to Sally!
#Edited by sally

##New edits with offsetting lines and error bars
#started 7/10/2016

##Second edits with plotting whole growing season started 7/24/16

##Read in data:
setwd("~/git/phenconstraints")
dat<-read.csv("data/growingseason_doy.csv", header=T)
dat2<-read.csv("data/growingseason_doy2.csv", header = T)
dim(dat)
head(dat)
names(dat)
names(dat2)

library(RColorBrewer)
#Assign colors, using the RcolorBrewer package- each species gets a different color
cols <- colorRampPalette(brewer.pal(8,"Accent"))(25)

##data are currently by individual, but we want to plot them by species. 
## convert this to a summary by species (with mean start and end date across species for each phase)

LDstartm<-tapply(dat$LDStart_DOY,dat$Species,mean, na.rm=T)
LDendm<-tapply(dat$LDEnd_DOY,dat$Species,mean, na.rm=T)
FLstartm<-tapply(dat$FloStart_DOY,dat$Species,mean, na.rm=T)
FLendm<-tapply(dat$FloEnd_DOY,dat$Species,mean, na.rm=T)
FRstartm<-tapply(dat$FruStart_DOY,dat$Species,mean, na.rm=T)
FRendm<-tapply(dat$FruEnd_DOY,dat$Species,mean, na.rm=T)
SENstartm<-tapply(dat$SenStart_DOY,dat$Species,mean, na.rm=T)
SENendm<-tapply(dat$SenEnd_DOY,dat$Species,mean, na.rm=T)

## finding the standard deviation
LDstart.sd<-tapply(dat$LDStart_DOY,dat$Species, sd, na.rm=T)
LDend.sd<-tapply(dat$LDEnd_DOY,dat$Species,sd, na.rm=T)
FLstart.sd<-tapply(dat$FloStart_DOY,dat$Species,sd, na.rm=T)
FLend.sd<-tapply(dat$FloEnd_DOY,dat$Species,sd, na.rm=T)
FRstart.sd<-tapply(dat$FruStart_DOY,dat$Species,sd, na.rm=T)
FRend.sd<-tapply(dat$FruEnd_DOY,dat$Species,sd, na.rm=T)
SENstart.sd<-tapply(dat$SenStart_DOY,dat$Species,sd, na.rm=T)
SENend.sd<-tapply(dat$SenEnd_DOY,dat$Species,sd, na.rm=T)

# tried length () to get sample size but that did not work. It said I was passing it two arguments.
#LDstart.n<-tapply(dat$LDStart_DOY,dat$Species,length(length(dat$Species[i])), na.rm=T)
#used table() instead and returns a vector

samplesize <- table(dat$Species)

#standard error = standard deviation of DOY/sqrt of sample size of each species
LDstart.se<- LDstart.sd/sqrt(samplesize)
LDend.se<- LDend.sd/sqrt(samplesize)
FLstart.se<-FLstart.sd/sqrt(samplesize)
FLend.se<-FLend.sd/sqrt(samplesize)
FRstart.se<-FRstart.sd/sqrt(samplesize)
FRend.se<-FRend.sd/sqrt(samplesize)
SENstart.se<-SENstart.sd/sqrt(samplesize)
SENend.se<-SENend.sd/sqrt(samplesize)

### 8/8 sort values from earliest LD start date to latest 

lLDstartm<-sort(LDstartm, decreasing = FALSE)
lLDendm<-LDendm[names(LDstartm)]
lFLstartm<-LDstartm[names(LDstartm)]
lFLendm<-FLendm[names(LDstartm)]
lFRstartm<-FRstartm[names(LDstartm)]
lFRendm<-FRendm[names(LDstartm)]
lSENstartm<-SENstartm[names(LDstartm)]
lSENendm<-SENendm[names(LDstartm)]

lLDstart.sd<-LDstart.sd[names(LDstartm)]
lLDend.sd<-LDend.sd[names(LDstartm)]
lFLstart.sd<-FLstart.sd[names(LDstartm)]
lFLend.sd<-FLend.sd[names(LDstartm)]
lFRstart.sd<-FRstart.sd[names(LDstartm)]
lFRend.sd<-FRend.sd[names(LDstartm)]
lSENstart.sd<-SENstart.sd[names(LDstartm)]
lSENend.sd<-SENend.sd[names(LDstartm)]

lsamplesize <- samplesize[names(LDstartm)]

lLDstart.se<- LDstart.se[names(LDstartm)]
lLDend.se<- LDend.se[names(LDstartm)]
lFLstart.se<-FLstart.se[names(LDstartm)]
lFLend.se<-FLend.se[names(LDstartm)]
lFRstart.se<-FRstart.se[names(LDstartm)]
lFRend.se<-FRend.se[names(LDstartm)]
lSENstart.se<-SENstart.se[names(LDstartm)]
lSENend.se<-SENend.se[names(LDstartm)]

##sort by flowering time

fFLstartm<-sort(FLstartm, decreasing = FALSE)
fLDendm<-LDendm[names(fFLstartm)]
fLDstartm<-LDstartm[names(fFLstartm)]
fFLendm<-FLendm[names(fFLstartm)]
fFRstartm<-FRstartm[names(fFLstartm)]
fFRendm<-FRendm[names(fFLstartm)]
fSENstartm<-SENstartm[names(fFLstartm)]
fSENendm<-SENendm[names(fFLstartm)]

fLDstart.sd<-LDstart.sd[names(fFLstartm)]
fLDend.sd<-LDend.sd[names(fFLstartm)]
fFLstart.sd<-FLstart.sd[names(fFLstartm)]
fFLend.sd<-FLend.sd[names(fFLstartm)]
fFRstart.sd<-FRstart.sd[names(fFLstartm)]
fFRend.sd<-FRend.sd[names(fFLstartm)]
fSENstart.sd<-SENstart.sd[names(fFLstartm)]
fSENend.sd<-SENend.sd[names(fFLstartm)]


fsamplesize <- samplesize[names(fFLstartm)]

fLDstart.se<- LDstart.se[names(fFLstartm)]
fLDend.se<- LDend.se[names(fFLstartm)]
fFLstart.se<-FLstart.se[names(fFLstartm)]
fFLend.se<-FLend.se[names(fFLstartm)]
fFRstart.se<-FRstart.se[names(fFLstartm)]
fFRend.se<-FRend.se[names(fFLstartm)]
fSENstart.se<-SENstart.se[names(fFLstartm)]
fSENend.se<-SENend.se[names(fFLstartm)]



##Now Plot
#figure out what max and min values are for all phases, so that i know what the x axis limits should be
max(dat[,12:19],na.rm=T)
min(dat[,12:19],na.rm=T)
quartz(height=10,width=10)#for pc you replace "quartz" with X11
par(mai=c(1,3,.2,.1), omi=c(.7,.1,.2,.2))
plot(10,10, type="p", cex=.8,pch=21, col="white", bty="L", xlab="Day of Year",ylab=" ", ylim=c(1,50), yaxt='n',xlim=c(110,340),las=1)
axis(side=2,at=c(seq(from =2, to = 50, by = 2)),labels=(paste(rev(names(LDstartm)))),las=1)

#Start with Aesculus
species<-names(LDstartm)


### 7/24/16
y<-rev(seq(from =2, to = 50, by = 2))
for(i in 1:length(species)){
  lines(c(LDstartm[i],SENendm[i]),c(y[i],y[i]), col="seagreen",lwd=4)
  lines(c(FLstartm[i],FRendm[i]),c(y[i]-0.4,y[i]-0.4),col="lightsalmon", lwd=4)
  lines(c(FLstartm[i],FLendm[i]),c(y[i]-0.4,y[i]-0.4), col="violetred", lty = 3, lwd=3)
  lines(c(LDstartm[i],LDendm[i]),c(y[i],y[i]), col="palegreen", lty=3, lwd=3)
  lines(c(FRstartm[i],FRendm[i]),c(y[i]-0.4,y[i]-0.4), col="darkorchid", lty=3, lwd=3)
  lines(c(SENstartm[i],SENendm[i]),c(y[i],y[i]), col="yellow2", lty=3,lwd=3)
}


# error bars

for(i in 1:length(species)){
  arrows(LDstartm[i],y[i], LDstartm[i]-LDstart.se[i],y[i], length =0.02, angle=90, code=2, lty = 1,lwd = 1)
  arrows(LDendm[i],y[i], LDendm[i]+LDend.se[i],y[i], length =0.02, angle=90, code=2, lty = 1,lwd = 1)
  arrows(FLstartm[i],y[i]-0.4, FLstartm[i]-FLstart.se[i],y[i]-0.4, length =0.02, angle=90, code=2, lty = 1,lwd = 1)
  arrows(FLendm[i],y[i]-0.4, FLendm[i]+FLend.se[i],y[i]-0.4, length =0.02, angle=90, code=2, col = "white",lty = 1,lwd = 1)
  arrows(FRstartm[i],y[i]-0.4, FRstartm[i]-FRstart.se[i],y[i]-0.4, length =0.02, angle=90, code=2, col = "white", lty = 1,lwd = 1)
  arrows(FRendm[i],y[i]-0.4, FRendm[i]+FRend.se[i],y[i]-0.4, length =0.02, angle=90, code=2, lty = 1,lwd = 1)
  arrows(SENstartm[i],y[i], SENstartm[i]-SENstart.se[i],y[i], length =0.02, angle=90, code=2, lty = 1,lwd = 1)
  arrows(SENendm[i],y[i], SENendm[i]+SENend.se[i],y[i], length =0.02, angle=90, code=2, lty = 1,lwd = 1)
}


##Plot by Leafout date
quartz(height=10,width=10)#for pc you replace "quartz" with X11
par(mai=c(1,3,.2,.1), omi=c(.7,.1,.2,.2))
plot(10,10, type="p", cex=.8,pch=21, col="white", bty="L", xlab="Day of Year",ylab=" ", ylim=c(1,50), yaxt='n',xlim=c(110,340),las=1)
axis(side=2,at=c(seq(from =2,to=50, by=2)),labels=(paste(rev(names(lLDstartm)))),las=1)

#Start with Aesculus
lspecies<-names(lLDstartm)

y<-rev(seq(from =2,to=50, by=2))
for(i in 1:length(lspecies)){
  lines(c(lLDstartm[i],lSENendm[i]),c(y[i],y[i]), col="seagreen",lwd=4)
  lines(c(lFLstartm[i],lFRendm[i]),c(y[i]-0.4,y[i]-0.4),col="darkorchid", lwd=4)
  lines(c(lFLstartm[i],lFLendm[i]),c(y[i]-0.4,y[i]-0.4), col="lightsalmon", lty = 3, lwd=3)
  lines(c(lLDstartm[i],lLDendm[i]),c(y[i],y[i]), col="palegreen", lty=3, lwd=3)
  lines(c(lFRstartm[i],lFRendm[i]),c(y[i]-0.4,y[i]-0.4), col="lavenderblush", lty=3, lwd=3)
  lines(c(lSENstartm[i],lSENendm[i]),c(y[i],y[i]), col="yellow2", lty=3,lwd=3)
}


# error bars

for(i in 1:length(lspecies)){
  arrows(lLDstartm[i],y[i], lLDstartm[i]-lLDstart.se[i],y[i], length =0.02, angle=90, code=2, lty = 1,lwd = 1)
  arrows(lLDendm[i],y[i], lLDendm[i]+lLDend.se[i],y[i], length =0.02, angle=90, code=2, lty = 1,lwd = 1)
  arrows(lFLstartm[i],y[i]-0.4, lFLstartm[i]-lFLstart.se[i],y[i]-0.4, length =0.02, angle=90, code=2, lty = 1,lwd = 1)
  arrows(lFLendm[i],y[i]-0.4, lFLendm[i]+lFLend.se[i],y[i]-0.4, length =0.02, angle=90, code=2, col = "white",lty = 1,lwd = 1)
  arrows(lFRstartm[i],y[i]-0.4, lFRstartm[i]-lFRstart.se[i],y[i]-0.4, length =0.02, angle=90, code=2, col = "white", lty = 1,lwd = 1)
  arrows(lFRendm[i],y[i]-0.4, lFRendm[i]+lFRend.se[i],y[i]-0.4, length =0.02, angle=90, code=2, lty = 1,lwd = 1)
  arrows(lSENstartm[i],y[i], lSENstartm[i]-lSENstart.se[i],y[i], length =0.02, angle=90, code=2, lty = 1,lwd = 1)
  arrows(lSENendm[i],y[i], lSENendm[i]+lSENend.se[i],y[i], length =0.02, angle=90, code=2, lty = 1,lwd = 1)
}


##Plot by flowering date
quartz(height=10,width=10)#for pc you replace "quartz" with X11
par(mai=c(1,3,.2,.1), omi=c(.7,.1,.2,.2))
plot(10,10, type="p", cex=.8,pch=21, col="white", bty="L", xlab="Day of Year",ylab=" ", ylim=c(1,50), yaxt='n',xlim=c(110,340),las=1)
axis(side=2,at=c(seq(from =2,to=50, by=2)),labels=(paste(rev(names(fLDstartm)))),las=1)

fspecies<-names(fFLstartm)

y<-rev(seq(from =2,to=50, by=2))
for(i in 1:length(fspecies)){
  lines(c(fLDstartm[i],fSENendm[i]),c(y[i],y[i]), col="seagreen",lwd=4)
  lines(c(fFLstartm[i],fFRendm[i]),c(y[i]-0.8,y[i]-0.8),col="sandybrown", lwd=4)
  lines(c(fFLstartm[i],fFLendm[i]),c(y[i]-0.8,y[i]-0.8), col="violetred", lty = 3, lwd=3)
  lines(c(fLDstartm[i],fLDendm[i]),c(y[i],y[i]), col="palegreen", lty=3, lwd=3)
  lines(c(fFRstartm[i],fFRendm[i]),c(y[i]-0.8,y[i]-0.8), col="darkorchid", lty=3, lwd=3)
  lines(c(fSENstartm[i],fSENendm[i]),c(y[i],y[i]), col="yellow2", lty=3,lwd=3)
}


# error bars

for(i in 1:length(fspecies)){
  arrows(fLDstartm[i],y[i], fLDstartm[i]-fLDstart.se[i],y[i], length =0.05, angle=90, code=2, col = "black", lty = 1,lwd = 2)
  arrows(fLDendm[i],y[i], fLDendm[i]+fLDend.se[i],y[i], length =0.05, angle=90, code=2, col = "black", lty = 1,lwd = 2)
  arrows(fFLstartm[i],y[i]-0.8, fFLstartm[i]-fFLstart.se[i],y[i]-0.8, length =0.05, angle=90, code=2, col = "black", lty = 1,lwd = 2)
  arrows(fFLendm[i],y[i]-0.8, fFLendm[i]+fFLend.se[i],y[i]-0.8, length =0.05, angle=90, code=2, col = "black",lty = 1,lwd = 2)
  arrows(fFRstartm[i],y[i]-0.8, fFRstartm[i]-fFRstart.se[i],y[i]-0.8, length =0.05, angle=90, code=2, col = "black", lty = 1,lwd = 2)
  arrows(fFRendm[i],y[i]-0.8, fFRendm[i]+fFRend.se[i],y[i]-0.8, length =0.05, angle=90, code=2,col = "black", lty = 1,lwd = 2)
  arrows(fSENstartm[i],y[i], fSENstartm[i]-fSENstart.se[i],y[i], length =0.05, angle=90, code=2,col = "black", lty = 1,lwd = 2)
  arrows(fSENendm[i],y[i], fSENendm[i]+fSENend.se[i],y[i], length =0.05, angle=90, code=2, col = "black", lty = 1,lwd = 2)
}

###########################################
# 10/5/16 & 10/18/16 plotting all possible combinations
# plotting the difference between flowering doy and leaf developement doy over the season

BB_Flodoy = fFLstartm - fLDstartm

min(fFLstartm)
max(fFLstartm)
min(BB_Flodoy)
max(BB_Flodoy)
min(fLDstartm)
max(fLDstartm)

Flo_Frudoy = fFLstartm - fFRstartm #will get negative values
min(Flo_Frudoy)
max(Flo_Frudoy)
min(fFRstartm)
max(fFRstartm)

## Added best fit line 10/24

BB_Flodoy = fFLstartm - fLDstartm
plot(fLDstartm, BB_Flodoy, xlab = "Budburst DOY", ylab = "Flowering - Budburst DOY",bg=cols[as.numeric(as.factor(fspecies))])
abline(lm(BB_Flodoy~fLDstartm))
plot(fFLstartm, BB_Flodoy, xlab = "Flowering DOY", ylab = "Flowering - Budburst DOY")
abline(lm(BB_Flodoy~fFLstartm))
plot(fFRstartm, BB_Flodoy, xlab = "Fruiting DOY", ylab = "Flowering - Budburst DOY")
abline(lm(BB_Flodoy~fFRstartm))
plot(fSENstartm, BB_Flodoy, xlab = "Senescence DOY", ylab = "Flowering - Budburst DOY")
abline(lm(BB_Flodoy ~ fSENstartm))



BB_Frudoy = fFRstartm - fLDstartm
plot(fLDstartm, BB_Frudoy, xlab = "Budburst DOY", ylab = "Fruiting - Budburst DOY")
abline(lm(BB_Frudoy~fLDstartm))
plot(fFLstartm, BB_Frudoy, xlab = "Flowering DOY", ylab = "Fruiting - Budburst DOY")
abline(lm(BB_Frudoy~fFLstartm))
plot(fFRstartm, BB_Frudoy, xlab = "Fruiting DOY", ylab = "Fruiting - Budburst DOY")
abline(lm(BB_Frudoy~fFRstartm))
plot(fSENstartm, BB_Frudoy, xlab = "Senescence DOY", ylab = "Fruiting - Budburst DOY")
abline(lm(BB_Frudoy~fSENstartm))

Flo_Frudoy = fFRstartm - fFLstartm
plot(fLDstartm, Flo_Frudoy, xlab = "Budburst DOY", ylab = "Fruiting - Flowering DOY")
abline(lm(Flo_Frudoy~fLDstartm))
plot(fFLstartm, Flo_Frudoy, xlab = "Flowering DOY", ylab = "Fruiting - Flowering DOY")
abline(lm(Flo_Frudoy~fFLstartm))
plot(fFRstartm, Flo_Frudoy, xlab = "Fruiting DOY", ylab = "Fruiting - Flowering DOY")
abline(lm(Flo_Frudoy~fFRstartm))
plot(fSENstartm, Flo_Frudoy, xlab = "Senescence DOY", ylab = "Fruiting - Flowering DOY")
abline(lm(Flo_Frudoy~fSENstartm))

BB_SSdoy = fSENstartm - fLDstartm
plot(fLDstartm, BB_SSdoy, xlab = "Budburst DOY", ylab = "Senescence - Budburst DOY")
abline(lm(BB_SSdoy~fLDstartm))
plot(fFLstartm, BB_SSdoy, xlab = "Flowering DOY", ylab = "Senescence - Budburst DOY")
abline(lm(BB_SSdoy~fFLstartm))
plot(fFRstartm, BB_SSdoy, xlab = "Fruiting DOY", ylab = "Senescence - Budburst DOY")
abline(lm(BB_SSdoy~fFRstartm))
plot(fSENstartm, BB_SSdoy, xlab = "Senescence DOY", ylab = "Senescence - Budburst DOY")
abline(lm(BB_SSdoy~fSENstartm))

Flo_SSdoy = fSENstartm - fFLstartm 
plot(fLDstartm, Flo_SSdoy, xlab = "Budburst DOY", ylab = "Senescence - Flowering DOY")
abline(lm(Flo_SSdoy~fLDstartm))
plot(fFLstartm, Flo_SSdoy, xlab = "Flowering DOY", ylab = "Senescence - Flowering DOY")
abline(lm(Flo_SSdoy~fFLstartm))
plot(fFRstartm, Flo_SSdoy, xlab = "Fruiting DOY", ylab = "Senescence - Flowering DOY")
abline(lm(Flo_SSdoy~fFRstartm))
plot(fSENstartm, Flo_SSdoy, xlab = "Senescence DOY", ylab = "Senescence - Flowering DOY")
abline(lm(Flo_SSdoy~fSENstartm))

Fru_SSdoy = fSENstartm - fFRstartm
plot(fLDstartm, Fru_SSdoy, xlab = "Budburst DOY", ylab = "Senescence - Fruiting DOY")
abline(lm(Fru_SSdoy~fLDstartm))
plot(fFLstartm, Fru_SSdoy, xlab = "Flowering DOY", ylab = "Senescence - Fruiting DOY")
abline(lm(Fru_SSdoy~fFLstartm))
plot(fFRstartm, Fru_SSdoy, xlab = "Fruiting DOY", ylab = "Senescence - Fruiting DOY")
abline(lm(Fru_SSdoy~fFRstartm))
plot(fSENstartm, Fru_SSdoy, xlab = "Senescence DOY", ylab = "Senescence - Fruiting DOY")
abline(lm(Fru_SSdoy~fSENstartm))

f#10/5/16
#plot(fFRstartm, Flo_Frudoy, xlab = "Fruiting DOY", ylab = "Flowering - Fruiting DOY")
#plot(fFRstartm, Flo_Frudoy2, xlab = "Fruiting DOY", ylab = "Fruiting - Flowering DOY")
#plot(fFLstartm, Flo_Frudoy2, xlab = "Flowering DOY", ylab = "Fruiting - Flowering DOY")


quartz(height = 5, width = 7)
par(mai=c(1,3,.2,.1), omi=c(.7,.1,.2,.2))
plot(1,1,type="p", cex=.8,pch=21, col="white", bty="L", xlab="Flowering Doy - Leafout Doy",ylab=" ", ylim=c(1,25), yaxt='n',xlim=c(-10,80),las=1)
axis(side=2,at=c(seq(1:25)),labels=(paste(rev(names(fLDstartm)))),las=1)

y<-rev(seq(1:25))
for(i in 1:length(fspecies)){
  points(Flo_Leafdoy[i],y[i])
}

plot(1,1,type="p", cex=.8,pch=21, col="white", bty="L", xlab="Flowering Doy - Leafout Doy",ylab=" ", ylim=c(1,25), yaxt='n',xlim=c(-10,80),las=1)
axis(side=2,at=c(seq(1:25)),labels=(paste(rev(names(fLDstartm)))),las=1)

plot(fLDstartm, Flo_Leafdoy, xlab = "Budburst DOY", ylab = "Flowering - Budburst DOY")
plot(fFLstartm, Flo_Leafdoy, xlab = "Flowering DOY", ylab = "Flowering - Budburst DOY")
abline(lm(Flo_Leafdoy ~ fFLstartm), col = "red")
plot(fFRstartm, Flo_Frudoy, xlab = "Fruiting DOY", ylab = "Flowering - Fruiting DOY")
plot(fFRstartm, Flo_Frudoy2, xlab = "Fruiting DOY", ylab = "Fruiting - Flowering DOY")
plot(fFLstartm, Flo_Frudoy2, xlab = "Flowering DOY", ylab = "Fruiting - Flowering DOY")


### Addition 10/22/16
## After plotting all possible combinations of phenological phases, I(Sally) felt that there wasn't 
## a clear trend of the ends being constrained by weather conditions. So, I decided to plot the repoduction
## time from open flowering to appearance of fruit instead of end fruit ripe to see if there would be a clearer represenation.

##Plot by flowering date
quartz(height=10,width=10)#for pc you replace "quartz" with X11
par(mai=c(1,3,.2,.1), omi=c(.7,.1,.2,.2))
plot(10,10, type="p", cex=.8,pch=21, col="white", bty="L", xlab="Day of Year",ylab=" ", ylim=c(1,50), yaxt='n',xlim=c(110,340),las=1)
axis(side=2,at=c(seq(from =2,to=50, by=2)),labels=(paste(rev(names(fLDstartm)))),las=1)

fspecies<-names(fLDstartm)

## version 1
y<-rev(seq(from =2,to=50, by=2))
for(i in 1:length(fspecies)){
  lines(c(fLDstartm[i],fSENendm[i]),c(y[i],y[i]), col="seagreen",lwd=4)
  lines(c(fFLstartm[i],fFRstartm[i]),c(y[i]-0.8,y[i]-0.8),col="sandybrown", lwd=4)
  lines(c(fFLstartm[i],fFLendm[i]),c(y[i]-0.8,y[i]-0.8), col="violetred", lty = 3, lwd=3)
  lines(c(fLDstartm[i],fLDendm[i]),c(y[i],y[i]), col="palegreen", lty=3, lwd=3)
  lines(c(fFRstartm[i],fFRendm[i]),c(y[i]-0.8,y[i]-0.8), col="darkorchid", lty=3, lwd=3)
  lines(c(fSENstartm[i],fSENendm[i]),c(y[i],y[i]), col="yellow2", lty=3,lwd=3)
}

## version 2
y<-rev(seq(from =2,to=50, by=2))
for(i in 1:length(fspecies)){
  lines(c(fLDstartm[i],fSENstartm[i]),c(y[i],y[i]), col="seagreen",lwd=4)
  lines(c(fFLstartm[i],fFRstartm[i]),c(y[i]-0.8,y[i]-0.8),col="sandybrown", lwd=4)
  lines(c(fFLstartm[i],fFLendm[i]),c(y[i]-0.8,y[i]-0.8), col="violetred", lty = 3, lwd=3)
  lines(c(fLDstartm[i],fLDendm[i]),c(y[i],y[i]), col="palegreen", lty=3, lwd=3)
  lines(c(fFRstartm[i],fFRendm[i]),c(y[i]-0.8,y[i]-0.8), col="darkorchid", lty=3, lwd=3)
  lines(c(fSENstartm[i],fSENendm[i]),c(y[i],y[i]), col="yellow2", lty=3,lwd=3)
}

## version 3
y<-rev(seq(from =2,to=50, by=2))
for(i in 1:length(fspecies)){
  lines(c(fLDendm[i],fSENstartm[i]),c(y[i],y[i]), col="seagreen",lwd=4)
  lines(c(fFLstartm[i],fFRstartm[i]),c(y[i]-0.8,y[i]-0.8),col="sandybrown", lwd=4)
  lines(c(fFLstartm[i],fFLendm[i]),c(y[i]-0.8,y[i]-0.8), col="violetred", lty = 3, lwd=3)
  lines(c(fLDstartm[i],fLDendm[i]),c(y[i],y[i]), col="palegreen", lty=3, lwd=3)
  lines(c(fFRstartm[i],fFRendm[i]),c(y[i]-0.8,y[i]-0.8), col="darkorchid", lty=3, lwd=3)
  lines(c(fSENstartm[i],fSENendm[i]),c(y[i],y[i]), col="yellow2", lty=3,lwd=3)
}

### 10/23/16 Making plots for Leaf Out

LOstartm<-tapply(dat2$LOutStart_DOY,dat2$Species,mean, na.rm=T)
LOendm<-tapply(dat2$LOutEnd_DOY,dat2$Species,mean, na.rm=T)

fLOstartm<- LOstartm[names(fFLstartm)]
fLOendm<- LOendm[names(fFLstartm)]

BB_LOdoy = fLOstartm - fLDstartm
plot(fLDstartm, BB_LOdoy, xlab = "Budburst DOY", ylab = "Leaf Out - Budburst DOY")
abline(lm(BB_LOdoy~fLDstartm))
plot(fLOstartm, BB_LOdoy, xlab = "Leaf Out", ylab = "Leaf Out - Budburst DOY")
abline(lm(BB_LOdoy~fLOstartm))
plot(fFLstartm, BB_LOdoy, xlab = "Flowering DOY", ylab = "Leaf Out - Budburst DOY")
abline(lm(BB_LOdoy~fFLstartm))
plot(fFRstartm, BB_LOdoy, xlab = "Fruiting DOY", ylab = "Leaf Out - Budburst DOY")
abline(lm(BB_LOdoy~fFRstartm))
plot(fSENstartm, BB_LOdoy, xlab = "Senescence DOY", ylab = "Leaf Out - Budburst DOY")
abline(lm(BB_LOdoy~fSENstartm))

LO_FLdoy = fFLstartm - fLOstartm
plot(fLDstartm, LO_FLdoy, xlab = "Budburst DOY", ylab = "Flowering - Leaf Out DOY")
abline(lm(LO_FLdoy~fLDstartm))
plot(fLOstartm, LO_FLdoy, xlab = "Leaf Out DOY", ylab = "Flowering - Leaf Out DOY")
abline(lm(LO_FLdoy~fLOstartm))
plot(fFLstartm, LO_FLdoy, xlab = "Flowering DOY", ylab = "Flowering - Leaf Out DOY")
abline(lm(LO_FLdoy~fFLstartm))
plot(fFRstartm, LO_FLdoy, xlab = "Fruiting DOY", ylab = "Flowering - Leaf Out DOY")
abline(lm(LO_FLdoy~fFRstartm))
plot(fSENstartm, LO_FLdoy, xlab = "Senescence DOY", ylab = "Flowering - Leaf Out DOY")
abline(lm(LO_FLdoy~fSENstartm))

LO_FRdoy = fFRstartm - fLOstartm
plot(fLDstartm, LO_FRdoy, xlab = "Budburst DOY", ylab = "Fruiting - Leaf Out DOY")
abline(lm(LO_FRdoy~fLDstartm))
plot(fLOstartm, LO_FRdoy, xlab = "Leaf Out DOY", ylab = "Fruiting - Leaf Out DOY")
abline(lm(LO_FRdoy~fLOstartm))
plot(fFLstartm, LO_FRdoy, xlab = "Flowering DOY", ylab = "Fruiting - Leaf Out DOY")
abline(lm(LO_FRdoy~fFLstartm))
plot(fFRstartm, LO_FRdoy, xlab = "Fruiting DOY", ylab = "Fruiting - Leaf Out DOY")
abline(lm(LO_FRdoy~fFRstartm))
plot(fSENstartm, LO_FRdoy, xlab = "Senescence DOY", ylab = "Fruiting - Leaf Out DOY")
abline(lm(LO_FRdoy~fSENstartm))

LO_SSdoy = fSENstartm - fLOstartm
plot(fLDstartm, LO_SSdoy, xlab = "Budburst DOY", ylab = "Senescence - Leaf Out DOY")
abline(lm(LO_SSdoy~fLDstartm))
plot(fLOstartm, LO_SSdoy, xlab = "Leaf Out DOY", ylab = "Senescence - Leaf Out DOY")
abline(lm(LO_SSdoy~fLOstartm))
plot(fFLstartm, LO_SSdoy, xlab = "Flowering DOY", ylab = "Senescence - Leaf Out DOY")
abline(lm(LO_SSdoy~fFLstartm))
plot(fFRstartm, LO_SSdoy, xlab = "Fruiting DOY", ylab = "Senescence - Leaf Out DOY")
abline(lm(LO_SSdoy~fFRstartm))
plot(fSENstartm, LO_SSdoy, xlab = "Senescence DOY", ylab = "Senescence - Leaf Out DOY")
abline(lm(LO_SSdoy~fSENstartm))

plot(fLOstartm, BB_Flodoy, xlab = "Leaf Out DOY", ylab = "Flowering - Budburst DOY")
abline(lm(BB_Flodoy~fLOstartm))

plot(fLOstartm, BB_Frudoy, xlab = "Leaf Out DOY", ylab = "Fruiting - Budburst DOY")
abline(lm(BB_Frudoy~fLOstartm))

plot(fLOstartm, Flo_Frudoy, xlab = "Leaf Out DOY", ylab = "Fruiting - Flowering DOY")
abline(lm(Flo_Frudoy~fLOstartm))

plot(fLOstartm, BB_SSdoy, xlab = "Leaf Out DOY", ylab = "Senescence - Budburst DOY")
abline(lm(BB_SSdoy~fLOstartm))

plot(fLOstartm, Flo_SSdoy, xlab = "Leaf Out DOY", ylab = "Senescence - Flowering DOY")
abline(lm(Flo_SSdoy~fLOstartm))

plot(fLOstartm, Fru_SSdoy, xlab = "Leaf Out DOY", ylab = "Senescence - Fruiting DOY")
abline(lm(Fru_SSdoy~fLOstartm))


################################
################################
####### 11/1/16 correlations between stages and interperiod
##############

BB_Flodoy = fFLstartm - fLDstartm

summary(lm(BB_Flodoy~fLDstartm))
#R2 = -0.03894 ; p = 0.7541 DF = 23

summary(lm(BB_Flodoy~fFLstartm))
#R2 = 0.8692 ; p = <0.001 DF = 23

summary(lm(BB_Flodoy~fFRstartm))
#R2 = 0.3602 ; p = 0.0009 DF = 23

summary(lm(BB_Flodoy ~ fSENstartm))
#R2 = -0.01765 ; p = 0.4526 DF = 23


BB_Frudoy = fFRstartm - fLDstartm

summary(lm(BB_Frudoy~fLDstartm))
#R2 = 0.0304 ; p = 0.1986 ; DF = 23

summary(lm(BB_Frudoy~fFLstartm))
#R2 = 0.4686 ; p = <0.001 ; DF = 23

summary(lm(BB_Frudoy~fFRstartm))
#R2 = 0.9476 ; p = <0.0001 ; DF = 23

summary(lm(BB_Frudoy~fSENstartm))
#R2 = 0.103 ; p = 0.06499 ; DF = 23


Flo_Frudoy = fFRstartm - fFLstartm

summary(lm(Flo_Frudoy~fLDstartm))
#R2 = 0.04938 ; p = 0.1475 ; DF = 23

summary(lm(Flo_Frudoy~fFLstartm))
#r2 = -0.02903 ; p = 0.5753 ; DF = 23

summary(lm(Flo_Frudoy~fFRstartm))
#R2 = -.5577 ; p = <0.0001 ; DF 23

summary(lm(Flo_Frudoy~fSENstartm))
#R2 = 0.09442 ; p = 0.07405 ; DF = 23


BB_SSdoy = fSENstartm - fLDstartm

summary(lm(BB_SSdoy~fLDstartm))
#r2 = 0.0945 ; p = 0.07396

summary(lm(BB_SSdoy~fFLstartm))
#R2 = -0.04284 ; p = 0.9064 ; DF = 23

summary(lm(BB_SSdoy~fFRstartm))
#R2 = -0.03039 ; p = 0.594 ; DF = 23

summary(lm(BB_SSdoy~fSENstartm))
#R2 = 0.7248 ; p = <0.0001 ; DF = 23


Flo_SSdoy = fSENstartm - fFLstartm 

summary(lm(Flo_SSdoy~fLDstartm))
#r2 = 0.04225 ; p = 0.1648 ; DF = 23

summary(lm(Flo_SSdoy~fFLstartm))
#R2 = 0.6408 ; p = <0.0001 ; DF = 23

summary(lm(Flo_SSdoy~fFRstartm))
#R2 = 0..1745; p = 0.02162 ; DF = 23

summary(lm(Flo_SSdoy~fSENstartm))
#R2 = 0.1325; p = 0.04146 ; DF = 23


Fru_SSdoy = fSENstartm - fFRstartm

summary(lm(Fru_SSdoy~fLDstartm))
#R2 = 0.1565 ; p = 0.02862 ; DF = 23

summary(lm(Fru_SSdoy~fFLstartm))
#R2 = 0.4717 ; p = <0.0001 ; DF = 23

summary(lm(Fru_SSdoy~fFRstartm))
#R2 = 0.8147 ; p = <0.0001 ; DF = 23

summary(lm(Fru_SSdoy~fSENstartm))
#R2 = -0.04141 ; p = 0.8326 ; DF = 23


BB_LOdoy = fLOstartm - fLDstartm

summary(lm(BB_LOdoy~fLDstartm))
#R2 = 0.3484 ; p = 0.001126 ; DF = 23

summary(lm(BB_LOdoy~fLOstartm))
#R2 = -0.007418 ; p = 0.3736  DF = 23

summary(lm(BB_LOdoy~fFLstartm))
#R2 = -0.04231 ; p =0.8741 ; DF = 23

summary(lm(BB_LOdoy~fFRstartm))
#R2 = 0.001354 ; p = 0.3201 ; DF = 23

summary(lm(BB_LOdoy~fSENstartm))
#R2 = -0.03927 ; p = 0.7629


LO_FLdoy = fFLstartm - fLOstartm

summary(lm(LO_FLdoy~fLDstartm))
#R2 = 0.02576 ; p = 0.2138 ; DF = 23

summary(lm(LO_FLdoy~fLOstartm))
#r2 = 0.04986 ; p = 0.1464 ; DF = 23 

summary(lm(LO_FLdoy~fFLstartm))
#R2 = 0.9227 ; p = <0.0001 ; DF = 23

summary(lm(LO_FLdoy~fFRstartm))
#R2 = 0.4836 ; p = < 0.0001 ; DF = 23

summary(lm(LO_FLdoy~fSENstartm))
#R2 = -0.008455 ; p = 0.3807 ; DF = 23


LO_FRdoy = fFRstartm - fLOstartm

summary(lm(LO_FRdoy~fLDstartm))
#R2 = 0.01018 ; p = 0.06617 ; DF = 23

summary(lm(LO_FRdoy~fLOstartm))
#R2 = 0.00722 ; P = 0.2897 ; DF = 23

summary(lm(LO_FRdoy~fFLstartm))
#R2 = 0.4283 ; p = 0.0002314 ; DF = 23

summary(lm(LO_FRdoy~fFRstartm))
#R2 = 0.9653 ; p = < 0.0001 ; DF = 23

summary(lm(LO_FRdoy~fSENstartm))
#R2 = 0.1032 ; p = 0.06476 ; DF = 23


LO_SSdoy = fSENstartm - fLOstartm

summary(lm(LO_SSdoy~fLDstartm))
#R2 = -0.02761 ; p =0.557 ; DF =23

summary(lm(LO_SSdoy~fLOstartm))
#R2 = 0.04265 ; p = 0.1638 ; DF = 23

summary(lm(LO_SSdoy~fFLstartm))
#R2 = -0.04189 ; p = 0.8532 ; DF = 23

summary(lm(LO_SSdoy~fFRstartm))
#r2 = -0.002094 ; p = 0.3399 ; DF = 23

summary(lm(LO_SSdoy~fSENstartm))
#R2 = 0.8118 ; p = <0.0001 ; DF = 23


summary(lm(BB_Flodoy~fLOstartm))
#R2 = 0.079911 ; p = 0.09349 ; DF = 23

summary(lm(BB_Frudoy~fLOstartm))
#R2 = 0.02904 ; p = 0.2029 ; DF = 23

summary(lm(Flo_Frudoy~fLOstartm))
#r2 = -0.04062 ; p = 0.8037 ; DF = 23

summary(lm(BB_SSdoy~fLOstartm))
#r2 = 0.001014 ; p = 0.322; DF = 23

summary(lm(Flo_SSdoy~fLOstartm))
#R2 = 0.1423 ; p = 0.03563 ; DF = 23

summary(lm(Fru_SSdoy~fLOstartm))
#R2 = 0.09141 ; p = 0.07752 ; DF = 23


################################
################################
#### 11/1/16 Megaplot 2.0
####################

quartz(height=7, width=9)#this sets the dimensions of the plotting window
#par(mfrow=c(5,5))#this sets the number of rows and columns for the plots - in this case 5 rows and 5 columns
par(mfcol=c(5,5),mai=c(.1,.4,.1,.01), omi=c(.8,.01,.1,.01))#same thing but adding some measurements for margins within individual plots (may) and outside margins for the whole window (omi)

#1- commenting this panel out for now because budburst is earliest stage- can't predict it with previous stage
plot(BB_LOdoy, fLDstartm, ylab = "", xlab = "Leaf Out - Budburst DOY",pch=21,bg=cols[as.numeric(as.factor(fspecies))], xaxt='n')
abline(lm(fLDstartm~BB_LOdoy))
mtext("Budburst DOY", side=2, cex=.7, line=2)
plot(BB_LOdoy, fLOstartm, ylab = "", xlab = "",pch=21,bg=cols[as.numeric(as.factor(fspecies))], xaxt='n')
mtext("Leaf Out DOY", side=2, cex=.7, line=2)
plot(BB_LOdoy,fFLstartm, ylab = "", xlab = "",pch=21,bg=cols[as.numeric(as.factor(fspecies))], xaxt='n')
mtext("Flowering DOY", side=2, cex=.7, line=2)
plot(BB_LOdoy,fFRstartm,  ylab = "", xlab = "",pch=21,bg=cols[as.numeric(as.factor(fspecies))], xaxt='n')
mtext("Fruiting DOY", side=2, cex=.7, line=2)
plot(BB_LOdoy,fSENstartm, ylab = "", xlab = "Leaf Out - Budburst DOY",pch=21,bg=cols[as.numeric(as.factor(fspecies))])
mtext("Leaf Out - Budburst DOY", side=1, cex=.7, line=2)
mtext("Senescence DOY", side=2, cex=.7, line=2)

#2
plot(BB_Flodoy, fLDstartm, xlab = "", ylab = "",pch=21,bg=cols[as.numeric(as.factor(fspecies))], xaxt='n', yaxt='n')
plot(BB_Flodoy,fLOstartm, ylab = "", xlab = "",pch=21,bg=cols[as.numeric(as.factor(fspecies))], xaxt='n', yaxt='n')
abline(lm(fLOstartm~BB_Flodoy), lty=2)
plot(BB_Flodoy,fFLstartm, ylab = "", xlab = "",pch=21,bg=cols[as.numeric(as.factor(fspecies))], xaxt='n', yaxt='n')
abline(lm(fFLstartm~BB_Flodoy))
plot(BB_Flodoy,fFRstartm, ylab = "", xlab = "",pch=21,bg=cols[as.numeric(as.factor(fspecies))], xaxt='n', yaxt='n')
abline(lm(fFRstartm~BB_Flodoy))
plot(BB_Flodoy,fSENstartm, ylab = "", xlab = "Flowering - Budburst DOY",pch=21,bg=cols[as.numeric(as.factor(fspecies))], yaxt='n')
mtext("Flowering - Budburst DOY", side=1, cex=.7, line=2)

#3
plot(LO_FLdoy, fLDstartm, ylab = "", xlab = "",pch=21,bg=cols[as.numeric(as.factor(fspecies))], xaxt='n', yaxt='n')
plot(LO_FLdoy, fLOstartm, ylab = "", xlab = "",pch=21,bg=cols[as.numeric(as.factor(fspecies))], xaxt='n', yaxt='n')
plot(LO_FLdoy, fFLstartm, ylab = "", xlab = "",pch=21,bg=cols[as.numeric(as.factor(fspecies))], xaxt='n', yaxt='n')
abline(lm(fFLstartm~LO_FLdoy))
plot(LO_FLdoy, fFRstartm, ylab = "", xlab = "",pch=21,bg=cols[as.numeric(as.factor(fspecies))], xaxt='n', yaxt='n')
abline(lm(fFRstartm~LO_FLdoy))
plot(LO_FLdoy,fSENstartm, ylab = "", xlab = "Flowering - Leaf Out DOY",pch=21,bg=cols[as.numeric(as.factor(fspecies))],yaxt='n')
mtext("Flowering - Leaf Out DOY", side=1, cex=.7, line=2)

#4
plot(fLDstartm, Flo_Frudoy, xlab = "", ylab = "",pch=21,bg=cols[as.numeric(as.factor(fspecies))], xaxt='n', yaxt='n')
plot(Flo_Frudoy, fLOstartm, ylab = "", xlab = "",pch=21,bg=cols[as.numeric(as.factor(fspecies))], xaxt='n', yaxt='n')
plot(Flo_Frudoy, fFLstartm, ylab = "", xlab = "",pch=21,bg=cols[as.numeric(as.factor(fspecies))], xaxt='n', yaxt='n')
plot(Flo_Frudoy, fFRstartm, ylab = "", xlab = "",pch=21,bg=cols[as.numeric(as.factor(fspecies))], xaxt='n', yaxt='n')
abline(lm(fFRstartm~Flo_Frudoy))
plot(Flo_Frudoy, fSENstartm, ylab = "", xlab = "Fruiting - Flowering DOY",pch=21,bg=cols[as.numeric(as.factor(fspecies))], yaxt='n')
mtext("Fruiting - Flowering DOY", side=1, cex=.7, line=2)

#5
plot(Fru_SSdoy, fLDstartm, xlab = "", ylab = "",pch=21,bg=cols[as.numeric(as.factor(fspecies))], xaxt='n', yaxt='n')
abline(lm(fLDstartm~Fru_SSdoy))
plot(Fru_SSdoy, fLOstartm, xlab = "", ylab = "",pch=21,bg=cols[as.numeric(as.factor(fspecies))], xaxt='n', yaxt='n')
plot(Fru_SSdoy, fFLstartm, xlab = "", ylab = "",pch=21,bg=cols[as.numeric(as.factor(fspecies))], xaxt='n', yaxt='n')
abline(lm(fFLstartm~Fru_SSdoy))
plot(Fru_SSdoy, fFRstartm, xlab = "", ylab = "",pch=21,bg=cols[as.numeric(as.factor(fspecies))], xaxt='n', yaxt='n')
abline(lm(fFRstartm~Fru_SSdoy))
plot(Fru_SSdoy, fSENstartm, xlab = "", ylab = "Senescence - Fruiting DOY",pch=21,bg=cols[as.numeric(as.factor(fspecies))], xaxt='n', yaxt='n')
mtext("Senescence - Fruiting DOY", side=1, cex=.7, line=2)


##Now plot of later stage versus earlier stage
quartz(height=7, width=7)#this sets the dimensions of the plotting window
#par(mfrow=c(5,5))#this sets the number of rows and columns for the plots - in this case 5 rows and 5 columns
par(mfcol=c(4,4),mai=c(.2,.7,.2,.01), omi=c(.8,.01,.2,.2))#same thing but adding some measurements for margins within individual plots (may) and outside margins for the whole window (omi)

#1- commenting this panel out for now because budburst is earliest stage- can't predict it with previous stage
#plot(BB_LOdoy, fLDstartm, ylab = "Budburst DOY", xlab = "Leaf Out - Budburst DOY",bg=cols[as.numeric(as.factor(fspecies))])
#abline(lm(BB_LOdoy~fLDstartm))
plot(fLDstartm, fLOstartm, ylab = "Leaf Out", xlab = "")
abline(lm(fLOstartm~fLDstartm))
plot(fLDstartm,fFLstartm, ylab = "Flowering DOY", xlab = "")
abline(lm(fFLstartm~fLDstartm))
plot(fLDstartm,fFRstartm,  ylab = "Fruiting DOY", xlab = "")
abline(lm(fFRstartm~fLDstartm))
plot(fLDstartm,fSENstartm, ylab = "Senescence DOY", xlab = "Budburst DOY")
abline(lm(fSENstartm~fLDstartm), lty=2)

mtext("Budburst DOY", side=1, cex=.7, line=2)

#2
plot.new()
plot(fLOstartm,fFLstartm, ylab = "", xlab = "")
abline(lm(fFLstartm~fLOstartm))
plot(fLOstartm,fFRstartm, ylab = "", xlab = "")
abline(lm(fFRstartm~fLOstartm))
plot(fLOstartm,fSENstartm, ylab = "", xlab = "Leafout DOY")
mtext("Leafout DOY", side=1, cex=.7, line=2)

#3
plot.new()
plot.new()
plot(fFLstartm, fFRstartm, ylab = "", xlab = "")
abline(lm(fFRstartm~fFLstartm))
plot(fFLstartm,fSENstartm, ylab = "", xlab = "Flowering DOY")
mtext("Flowering DOY", side=1, cex=.7, line=2)

#4
plot.new()
plot.new()
plot.new()
plot(fFRstartm,fSENstartm, ylab = "", xlab = "Flowering DOY")
abline(lm(fSENstartm~fFRstartm), lty=2)
mtext("Fruiting DOY", side=1, cex=.7, line=2)


###Simulating data to make hypotheses figures
#Hyp1: previous stage (flow) constraints later stage (fruit); duration is random
flowstdoy<-c(1,2,3,4,5)
#flowdur<-c(5,5,5,5,5)
flowdur<-rnorm(5,mean=5, sd=1)
flowendoy<-flowstdoy+flowdur
frstdoy<-flowstdoy+flowdur
quartz(height=3, width=5)
par(mfrow=c(1,2))
plot(flowstdoy,frstdoy)
abline(lm(frstdoy~flowstdoy))
plot(flowdur,frstdoy)
abline(lm(frstdoy~flowdur))
#Hyp2: Interphase time (flow) constraints later stage (fruit); flow stard time is constant
flowstdoy<-c(1,1,1,1,1)
flowdur<-c(5,6,7,8,9)
flowendoy<-flowstdoy+flowdur
frstdoy<-flowstdoy+flowdur
plot(flowdur,frstdoy)
abline(lm(frstdoy~flowdur))
