
###Analyses and figures for Sally's Phenology data
#occur during the growing season at Arnold ARboretum, for Sally Gee's thesis data
#by Ailene and sally
#June 2017
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
# Set working directory: 
if(length(grep("ailene", getwd()))>0) {setwd("~/git/phenconstraints")}

#Read in data:
dat<-read.csv("data/growingseason_doy.csv", header=T)
#dat2 has additional phenophases in it; otherwise i think the two datasets are the same?
#plot(dat$FruStart_DOY,dat2$FruStart_DOY)
#plot(dat$LDStart_DOY,dat2$LDStart_DOY)

dat2<-read.csv("data/growingseason_doy2.csv", header = T)

#ailene added ripe fruit to dat2 on august 10, 2017
#dim(dat2)
#head(dat)
#names(dat)
#names(dat2)

library(RColorBrewer)
#Assign colors, using the RcolorBrewer package- each species gets a different color
#Assign colors using ramping
#cols <- rev(colorRampPalette(brewer.pal(9,"YlOrRd"))(25))#red to yellow with later flowering dates
cols <- colorRampPalette(brewer.pal(9,"YlOrRd"))(25)#yellow to red with later flowering dates

dat2$SenEnd_DOY
##data are currently by individual, but we want to plot them by species. 
## convert this to a summary by species (with mean start and end date across species for each phase)

LDstartm<-tapply(dat2$LDStart_DOY,dat2$Species,mean, na.rm=T)
LDendm<-tapply(dat2$LDEnd_DOY,dat2$Species,mean, na.rm=T)
LOstartm<-tapply(dat2$LOutStart_DOY,dat2$Species,mean, na.rm=T)
LOendm<-tapply(dat2$LOutEnd_DOY,dat2$Species,mean, na.rm=T)
FLstartm<-tapply(dat2$FloStart_DOY,dat2$Species,mean, na.rm=T)
FLendm<-tapply(dat2$FloEnd_DOY,dat2$Species,mean, na.rm=T)
FRstartm<-tapply(dat2$FruStart_DOY,dat2$Species,mean, na.rm=T)
FRendm<-tapply(dat2$FruEnd_DOY,dat2$Species,mean, na.rm=T)
SENstartm<-tapply(dat2$SenStart_DOY,dat2$Species,mean, na.rm=T)
SENendm<-tapply(dat2$SenEnd_DOY,dat2$Species,mean, na.rm=T)

RFRstartm<-tapply(dat2$RipeFruStart_DOY,dat2$Species,mean, na.rm=T)
RFRendm<-tapply(dat2$RipeFruEnd_DOY,dat2$Species,mean, na.rm=T)

#sort(dat2$FLstartm, decreasing = FALSE)
## finding the standard deviation
#LDstart.sd<-tapply(dat$LDStart_DOY,dat$Species, sd, na.rm=T)
#LDend.sd<-tapply(dat$LDEnd_DOY,dat$Species,sd, na.rm=T)
#FLstart.sd<-tapply(dat$FloStart_DOY,dat$Species,sd, na.rm=T)
#FLend.sd<-tapply(dat$FloEnd_DOY,dat$Species,sd, na.rm=T)
#FRstart.sd<-tapply(dat$FruStart_DOY,dat$Species,sd, na.rm=T)
#FRend.sd<-tapply(dat$FruEnd_DOY,dat$Species,sd, na.rm=T)
#SENstart.sd<-tapply(dat$SenStart_DOY,dat$Species,sd, na.rm=T)
#SENend.sd<-tapply(dat$SenEnd_DOY,dat$Species,sd, na.rm=T)

# tried length () to get sample size but that did not work. It said I was passing it two arguments.
#LDstart.n<-tapply(dat$LDStart_DOY,dat$Species,length(length(dat$Species[i])), na.rm=T)
#used table() instead and returns a vector

#samplesize <- table(dat$Species)

#standard error = standard deviation of DOY/sqrt of sample size of each species
#LDstart.se<- LDstart.sd/sqrt(samplesize)
#LDend.se<- LDend.sd/sqrt(samplesize)
#FLstart.se<-FLstart.sd/sqrt(samplesize)
#FLend.se<-FLend.sd/sqrt(samplesize)
#FRstart.se<-FRstart.sd/sqrt(samplesize)
#FRend.se<-FRend.sd/sqrt(samplesize)
#SENstart.se<-SENstart.sd/sqrt(samplesize)
#SENend.se<-SENend.sd/sqrt(samplesize)

### 8/8 sort values from earliest LD start date to latest 

lLDstartm<-sort(LDstartm, decreasing = FALSE)
lLDendm<-LDendm[names(LDstartm)]
lLOstartm<-sort(LDstartm, decreasing = FALSE)
lLOendm<-LOendm[names(LDstartm)]
lFLstartm<-LDstartm[names(LDstartm)]
lFLendm<-FLendm[names(LDstartm)]
lFRstartm<-FRstartm[names(LDstartm)]
lFRendm<-FRendm[names(LDstartm)]
lSENstartm<-SENstartm[names(LDstartm)]
lSENendm<-SENendm[names(LDstartm)]

#lLDstart.sd<-LDstart.sd[names(LDstartm)]
#lLDend.sd<-LDend.sd[names(LDstartm)]
#lFLstart.sd<-FLstart.sd[names(LDstartm)]
#lFLend.sd<-FLend.sd[names(LDstartm)]
#lFRstart.sd<-FRstart.sd[names(LDstartm)]
#lFRend.sd<-FRend.sd[names(LDstartm)]
#lSENstart.sd<-SENstart.sd[names(LDstartm)]
#lSENend.sd<-SENend.sd[names(LDstartm)]
#3
#lsamplesize <- samplesize[names(LDstartm)]

#lLDstart.se<- LDstart.se[names(LDstartm)]
#lLDend.se<- LDend.se[names(LDstartm)]
#lFLstart.se<-FLstart.se[names(LDstartm)]
#lFLend.se<-FLend.se[names(LDstartm)]
#lFRstart.se<-FRstart.se[names(LDstartm)]
#lFRend.se<-FRend.se[names(LDstartm)]
#lSENstart.se<-SENstart.se[names(LDstartm)]
#lSENend.se<-SENend.se[names(LDstartm)]

##sort by flowering time

fFLstartm<-sort(FLstartm, decreasing = FALSE)
fLDendm<-LDendm[names(fFLstartm)]
fLDstartm<-LDstartm[names(fFLstartm)]
fLOendm<-LOendm[names(fFLstartm)]
fLOstartm<-LOstartm[names(fFLstartm)]
fFLendm<-FLendm[names(fFLstartm)]
fFRstartm<-FRstartm[names(fFLstartm)]
fFRendm<-FRendm[names(fFLstartm)]
fRFRstartm<-RFRstartm[names(fFLstartm)]
fRFRendm<-RFRendm[names(fFLstartm)]

fSENstartm<-SENstartm[names(fFLstartm)]
fSENendm<-SENendm[names(fFLstartm)]

#fLDstart.sd<-LDstart.sd[names(fFLstartm)]
#fLDend.sd<-LDend.sd[names(fFLstartm)]
#fFLstart.sd<-FLstart.sd[names(fFLstartm)]
#fFLend.sd<-FLend.sd[names(fFLstartm)]
#fFRstart.sd<-FRstart.sd[names(fFLstartm)]
#fFRend.sd<-FRend.sd[names(fFLstartm)]
#fSENstart.sd<-SENstart.sd[names(fFLstartm)]
#fSENend.sd<-SENend.sd[names(fFLstartm)]


#fsamplesize <- samplesize[names(fFLstartm)]

#fLDstart.se<- LDstart.se[names(fFLstartm)]
#fLDend.se<- LDend.se[names(fFLstartm)]
#fFLstart.se<-FLstart.se[names(fFLstartm)]
#fFLend.se<-FLend.se[names(fFLstartm)]
#fFRstart.se<-FRstart.se[names(fFLstartm)]
#fFRend.se<-FRend.se[names(fFLstartm)]
#fSENstart.se<-SENstart.se[names(fFLstartm)]
#fSENend.se<-SENend.se[names(fFLstartm)]

##Now Plot
#figure out what max and min values are for all phases, so that i know what the x axis limits should be
#max(dat[,12:19],na.rm=T)
#min(dat[,12:19],na.rm=T)
quartz(height=8,width=8)#for pc you replace "quartz" with X11
par(mai=c(1,3,.1,.1), omi=c(1.2,.1,.1,.2))
plot(10,10, type="p", cex=.8,pch=21, col="white", bty="L", xlab="Day of Year",ylab=" ", ylim=c(1,50), yaxt='n',xlim=c(110,340),las=1)
axis(side=2,at=c(seq(from =2, to = 50, by = 2)),labels=(paste(rev(names(fLDstartm)))),las=1)

#Start with first to flower
species<-names(fFLstartm)


### 7/24/16
y<-rev(seq(from =2, to = 50, by = 2))
for(i in 1:length(species)){
  lines(c(fLOstartm[i],fSENendm[i]),c(y[i],y[i]), col="seagreen",lwd=3)
  lines(c(fFLstartm[i],fFRendm[i]),c(y[i]-0.4,y[i]-0.4),col="lightgray", lwd=4)
  lines(c(fFLstartm[i],fFRstartm[i]),c(y[i]-0.4,y[i]-0.4), col="orchid", lwd=3)
  lines(c(fLDstartm[i],fLDendm[i]),c(y[i],y[i]), col="palegreen", lwd=3)
  lines(c(fRFRstartm[i],fRFRendm[i]),c(y[i]-0.4,y[i]-0.4), col="darkorchid", lwd=3)
  lines(c(fSENstartm[i],fSENendm[i]),c(y[i],y[i]), col="yellow2",lwd=3)
}

legend(330,50, legend=c("budburst","leafout", "senescence","in flower","fruit developing", "ripe fruit"), lty=1,lwd=2,col=c("seagreen","palegreen","yellow","orchid","gray","darkorchid"))
# skiperror bars- too confusing
#error bars

#for(i in 1:length(species)){
 # arrows(LDstartm[i],y[i], LDstartm[i]-LDstart.se[i],y[i], length =0.02, angle=90, code=2, lty = 1,lwd = 1)
  #arrows(LDendm[i],y[i], LDendm[i]+LDend.se[i],y[i], length =0.02, angle=90, code=2, lty = 1,lwd = 1)
  #arrows(FLstartm[i],y[i]-0.4, FLstartm[i]-FLstart.se[i],y[i]-0.4, length =0.02, angle=90, code=2, lty = 1,lwd = 1)
  #arrows(FLendm[i],y[i]-0.4, FLendm[i]+FLend.se[i],y[i]-0.4, length =0.02, angle=90, code=2, col = "white",lty = 1,lwd = 1)
  #arrows(FRstartm[i],y[i]-0.4, FRstartm[i]-FRstart.se[i],y[i]-0.4, length =0.02, angle=90, code=2, col = "white", lty = 1,lwd = 1)
  #arrows(FRendm[i],y[i]-0.4, FRendm[i]+FRend.se[i],y[i]-0.4, length =0.02, angle=90, code=2, lty = 1,lwd = 1)
  #arrows(SENstartm[i],y[i], SENstartm[i]-SENstart.se[i],y[i], length =0.02, angle=90, code=2, lty = 1,lwd = 1)
  #arrows(SENendm[i],y[i], SENendm[i]+SENend.se[i],y[i], length =0.02, angle=90, code=2, lty = 1,lwd = 1)
#}
###########################################
# 10/5/16 & 10/18/16 plotting all possible combinations
# plotting the difference between flowering doy and leaf developement doy over the season

BB_Flodoy = fFLstartm - fLDstartm

#min(fFLstartm)
#max(fFLstartm)
#min(BB_Flodoy)
#max(BB_Flodoy)
#min(fLDstartm)
#max(fLDstartm)

Flo_Frudoy = fFLstartm - fRFRstartm #will get negative values
#min(Flo_Frudoy)
#max(Flo_Frudoy)
#min(fFRstartm)
#max(fFRstartm)

## Added best fit line 10/24
fspecies<-names(fLDstartm)

BB_Flodoy = fFLstartm - fLDstartm
#plot(fLDstartm, BB_Flodoy, xlab = "Budburst DOY", ylab = "Flowering - Budburst DOY",bg=cols[fspecies_num])
#abline(lm(BB_Flodoy~fLDstartm))
#plot(fFLstartm, BB_Flodoy, xlab = "Flowering DOY", ylab = "Flowering - Budburst DOY")
#abline(lm(BB_Flodoy~fFLstartm))
#plot(fFRstartm, BB_Flodoy, xlab = "Fruiting DOY", ylab = "Flowering - Budburst DOY")
#abline(lm(BB_Flodoy~fFRstartm))
#plot(fSENstartm, BB_Flodoy, xlab = "Senescence DOY", ylab = "Flowering - Budburst DOY")
#abline(lm(BB_Flodoy ~ fSENstartm))



BB_Frudoy = fRFRstartm - fLDstartm
#plot(fLDstartm, BB_Frudoy, xlab = "Budburst DOY", ylab = "Fruiting - Budburst DOY")
#abline(lm(BB_Frudoy~fLDstartm))
#plot(fFLstartm, BB_Frudoy, xlab = "Flowering DOY", ylab = "Fruiting - Budburst DOY")
#abline(lm(BB_Frudoy~fFLstartm))
#plot(fFRstartm, BB_Frudoy, xlab = "Fruiting DOY", ylab = "Fruiting - Budburst DOY")
#abline(lm(BB_Frudoy~fFRstartm))
#plot(fSENstartm, BB_Frudoy, xlab = "Senescence DOY", ylab = "Fruiting - Budburst DOY")
#abline(lm(BB_Frudoy~fSENstartm))

Flo_Frudoy = fRFRstartm - fFLstartm
#plot(fLDstartm, Flo_Frudoy, xlab = "Budburst DOY", ylab = "Fruiting - Flowering DOY")
#abline(lm(Flo_Frudoy~fLDstartm))
#plot(fFLstartm, Flo_Frudoy, xlab = "Flowering DOY", ylab = "Fruiting - Flowering DOY")
#abline(lm(Flo_Frudoy~fFLstartm))
#plot(fFRstartm, Flo_Frudoy, xlab = "Fruiting DOY", ylab = "Fruiting - Flowering DOY")
#abline(lm(Flo_Frudoy~fFRstartm))
#plot(fSENstartm, Flo_Frudoy, xlab = "Senescence DOY", ylab = "Fruiting - Flowering DOY")
#abline(lm(Flo_Frudoy~fSENstartm))

BB_SSdoy = fSENstartm - fLDstartm
#plot(fLDstartm, BB_SSdoy, xlab = "Budburst DOY", ylab = "Senescence - Budburst DOY")
#abline(lm(BB_SSdoy~fLDstartm))
#plot(fFLstartm, BB_SSdoy, xlab = "Flowering DOY", ylab = "Senescence - Budburst DOY")
#abline(lm(BB_SSdoy~fFLstartm))
#plot(fFRstartm, BB_SSdoy, xlab = "Fruiting DOY", ylab = "Senescence - Budburst DOY")
#abline(lm(BB_SSdoy~fFRstartm))
#plot(fSENstartm, BB_SSdoy, xlab = "Senescence DOY", ylab = "Senescence - Budburst DOY")
#abline(lm(BB_SSdoy~fSENstartm))

Flo_SSdoy = fSENstartm - fFLstartm 
#plot(fLDstartm, Flo_SSdoy, xlab = "Budburst DOY", ylab = "Senescence - Flowering DOY")
#abline(lm(Flo_SSdoy~fLDstartm))
#plot(fFLstartm, Flo_SSdoy, xlab = "Flowering DOY", ylab = "Senescence - Flowering DOY")
#abline(lm(Flo_SSdoy~fFLstartm))
#plot(fFRstartm, Flo_SSdoy, xlab = "Fruiting DOY", ylab = "Senescence - Flowering DOY")
#abline(lm(Flo_SSdoy~fFRstartm))
#plot(fSENstartm, Flo_SSdoy, xlab = "Senescence DOY", ylab = "Senescence - Flowering DOY")
#abline(lm(Flo_SSdoy~fSENstartm))

Fru_SSdoy = fSENstartm - fRFRstartm
#plot(fLDstartm, Fru_SSdoy, xlab = "Budburst DOY", ylab = "Senescence - Fruiting DOY")
#abline(lm(Fru_SSdoy~fLDstartm))
#plot(fFLstartm, Fru_SSdoy, xlab = "Flowering DOY", ylab = "Senescence - Fruiting DOY")
#abline(lm(Fru_SSdoy~fFLstartm))
#plot(fFRstartm, Fru_SSdoy, xlab = "Fruiting DOY", ylab = "Senescence - Fruiting DOY")
#abline(lm(Fru_SSdoy~fFRstartm))
#plot(fSENstartm, Fru_SSdoy, xlab = "Senescence DOY", ylab = "Senescence - Fruiting DOY")
#abline(lm(Fru_SSdoy~fSENstartm))

#10/5/16
#plot(fFRstartm, Flo_Frudoy, xlab = "Fruiting DOY", ylab = "Flowering - Fruiting DOY")
#plot(fFRstartm, Flo_Frudoy2, xlab = "Fruiting DOY", ylab = "Fruiting - Flowering DOY")
#plot(fFLstartm, Flo_Frudoy2, xlab = "Flowering DOY", ylab = "Fruiting - Flowering DOY")


#quartz(height = 5, width = 7)
#par(mai=c(1,3,.2,.1), omi=c(.7,.1,.2,.2))
#plot(1,1,type="p", cex=.8,pch=21, col="white", bty="L", xlab="Flowering Doy - Leafout Doy",ylab=" ", ylim=c(1,25), yaxt='n',xlim=c(-10,80),las=1)
#axis(side=2,at=c(seq(1:25)),labels=(paste(rev(names(fLDstartm)))),las=1)

#y<-rev(seq(1:25))
#for(i in 1:length(fspecies)){
#  points(Flo_Leafdoy[i],y[i])
#}

#plot(1,1,type="p", cex=.8,pch=21, col="white", bty="L", xlab="Flowering Doy - Leafout Doy",ylab=" ", ylim=c(1,25), yaxt='n',xlim=c(-10,80),las=1)
#axis(side=2,at=c(seq(1:25)),labels=(paste(rev(names(fLDstartm)))),las=1)

#plot(fLDstartm, Flo_Leafdoy, xlab = "Budburst DOY", ylab = "Flowering - Budburst DOY")
#plot(fFLstartm, Flo_Leafdoy, xlab = "Flowering DOY", ylab = "Flowering - Budburst DOY")
#abline(lm(Flo_Leafdoy ~ fFLstartm), col = "red")
#plot(fFRstartm, Flo_Frudoy, xlab = "Fruiting DOY", ylab = "Flowering - Fruiting DOY")
#plot(fFRstartm, Flo_Frudoy2, xlab = "Fruiting DOY", ylab = "Fruiting - Flowering DOY")
#plot(fFLstartm, Flo_Frudoy2, xlab = "Flowering DOY", ylab = "Fruiting - Flowering DOY")


### Addition 10/22/16
## After plotting all possible combinations of phenological phases, I(Sally) felt that there wasn't 
## a clear trend of the ends being constrained by weather conditions. So, I decided to plot the repoduction
## time from open flowering to appearance of fruit instead of end fruit ripe to see if there would be a clearer represenation.

##Plot by flowering date
#quartz(height=10,width=10)#for pc you replace "quartz" with X11
#par(mai=c(1,3,.2,.1), omi=c(.7,.1,.2,.2))
#plot(10,10, type="p", cex=.8,pch=21, col="white", bty="L", xlab="Day of Year",ylab=" ", ylim=c(1,50), yaxt='n',xlim=c(110,340),las=1)
#axis(side=2,at=c(seq(from =2,to=50, by=2)),labels=(paste(rev(names(fLDstartm)))),las=1)



### 10/23/16 Making plots for Leaf Out

LOstartm<-tapply(dat2$LOutStart_DOY,dat2$Species,mean, na.rm=T)
LOendm<-tapply(dat2$LOutEnd_DOY,dat2$Species,mean, na.rm=T)

fLOstartm<- LOstartm[names(fFLstartm)]
fLOendm<- LOendm[names(fFLstartm)]

BB_LOdoy = fLOstartm - fLDstartm
#plot(fLDstartm, BB_LOdoy, xlab = "Budburst DOY", ylab = "Leaf Out - Budburst DOY")
#abline(lm(BB_LOdoy~fLDstartm))
#plot(fLOstartm, BB_LOdoy, xlab = "Leaf Out", ylab = "Leaf Out - Budburst DOY")
#abline(lm(BB_LOdoy~fLOstartm))
#plot(fFLstartm, BB_LOdoy, xlab = "Flowering DOY", ylab = "Leaf Out - Budburst DOY")
#abline(lm(BB_LOdoy~fFLstartm))
#plot(fFRstartm, BB_LOdoy, xlab = "Fruiting DOY", ylab = "Leaf Out - Budburst DOY")
#abline(lm(BB_LOdoy~fFRstartm))
#plot(fSENstartm, BB_LOdoy, xlab = "Senescence DOY", ylab = "Leaf Out - Budburst DOY")
#abline(lm(BB_LOdoy~fSENstartm))

LO_FLdoy = fFLstartm - fLOstartm
#plot(fLDstartm, LO_FLdoy, xlab = "Budburst DOY", ylab = "Flowering - Leaf Out DOY")
#abline(lm(LO_FLdoy~fLDstartm))
#plot(fLOstartm, LO_FLdoy, xlab = "Leaf Out DOY", ylab = "Flowering - Leaf Out DOY")
#abline(lm(LO_FLdoy~fLOstartm))
#plot(fFLstartm, LO_FLdoy, xlab = "Flowering DOY", ylab = "Flowering - Leaf Out DOY")
#abline(lm(LO_FLdoy~fFLstartm))
#plot(fFRstartm, LO_FLdoy, xlab = "Fruiting DOY", ylab = "Flowering - Leaf Out DOY")
#abline(lm(LO_FLdoy~fFRstartm))
#plot(fSENstartm, LO_FLdoy, xlab = "Senescence DOY", ylab = "Flowering - Leaf Out DOY")
#abline(lm(LO_FLdoy~fSENstartm))

LO_FRdoy = fRFRstartm - fLOstartm
#plot(fLDstartm, LO_FRdoy, xlab = "Budburst DOY", ylab = "Fruiting - Leaf Out DOY")
#abline(lm(LO_FRdoy~fLDstartm))
#plot(fLOstartm, LO_FRdoy, xlab = "Leaf Out DOY", ylab = "Fruiting - Leaf Out DOY")
#abline(lm(LO_FRdoy~fLOstartm))
#plot(fFLstartm, LO_FRdoy, xlab = "Flowering DOY", ylab = "Fruiting - Leaf Out DOY")
#abline(lm(LO_FRdoy~fFLstartm))
#plot(fFRstartm, LO_FRdoy, xlab = "Fruiting DOY", ylab = "Fruiting - Leaf Out DOY")
#abline(lm(LO_FRdoy~fFRstartm))
#plot(fSENstartm, LO_FRdoy, xlab = "Senescence DOY", ylab = "Fruiting - Leaf Out DOY")
#abline(lm(LO_FRdoy~fSENstartm))

LO_SSdoy = fSENstartm - fLOstartm
#plot(fLDstartm, LO_SSdoy, xlab = "Budburst DOY", ylab = "Senescence - Leaf Out DOY")
#abline(lm(LO_SSdoy~fLDstartm))
#plot(fLOstartm, LO_SSdoy, xlab = "Leaf Out DOY", ylab = "Senescence - Leaf Out DOY")
#abline(lm(LO_SSdoy~fLOstartm))
#plot(fFLstartm, LO_SSdoy, xlab = "Flowering DOY", ylab = "Senescence - Leaf Out DOY")
#abline(lm(LO_SSdoy~fFLstartm))
#plot(fFRstartm, LO_SSdoy, xlab = "Fruiting DOY", ylab = "Senescence - Leaf Out DOY")
#abline(lm(LO_SSdoy~fFRstartm))
#plot(fSENstartm, LO_SSdoy, xlab = "Senescence DOY", ylab = "Senescence - Leaf Out DOY")
#abline(lm(LO_SSdoy~fSENstartm))

#plot(fLOstartm, BB_Flodoy, xlab = "Leaf Out DOY", ylab = "Flowering - Budburst DOY")
#abline(lm(BB_Flodoy~fLOstartm))

#plot(fLOstartm, BB_Frudoy, xlab = "Leaf Out DOY", ylab = "Fruiting - Budburst DOY")
#abline(lm(BB_Frudoy~fLOstartm))

#plot(fLOstartm, Flo_Frudoy, xlab = "Leaf Out DOY", ylab = "Fruiting - Flowering DOY")
#abline(lm(Flo_Frudoy~fLOstartm))

#plot(fLOstartm, BB_SSdoy, xlab = "Leaf Out DOY", ylab = "Senescence - Budburst DOY")
#abline(lm(BB_SSdoy~fLOstartm))

#plot(fLOstartm, Flo_SSdoy, xlab = "Leaf Out DOY", ylab = "Senescence - Flowering DOY")
#abline(lm(Flo_SSdoy~fLOstartm))

#plot(fLOstartm, Fru_SSdoy, xlab = "Leaf Out DOY", ylab = "Senescence - Fruiting DOY")
#abline(lm(Fru_SSdoy~fLOstartm))


################################
################################
####### 11/1/16 correlations between stages and interperiod
##############

BB_Flodoy = fFLstartm - fLDstartm

#summary(lm(BB_Flodoy~fLDstartm))
#R2 = -0.03894 ; p = 0.7541 DF = 23

#summary(lm(BB_Flodoy~fFLstartm))
#R2 = 0.8692 ; p = <0.001 DF = 23

#summary(lm(BB_Flodoy~fFRstartm))
#R2 = 0.3602 ; p = 0.0009 DF = 23

#summary(lm(BB_Flodoy ~ fSENstartm))
#R2 = -0.01765 ; p = 0.4526 DF = 23


BB_Frudoy = fRFRstartm - fLDstartm

#summary(lm(BB_Frudoy~fLDstartm))
#R2 = 0.0304 ; p = 0.1986 ; DF = 23

#summary(lm(BB_Frudoy~fFLstartm))
#R2 = 0.4686 ; p = <0.001 ; DF = 23

#summary(lm(BB_Frudoy~fFRstartm))
#R2 = 0.9476 ; p = <0.0001 ; DF = 23

#summary(lm(BB_Frudoy~fSENstartm))
#R2 = 0.103 ; p = 0.06499 ; DF = 23


Flo_Frudoy = fRFRstartm - fFLstartm

#summary(lm(Flo_Frudoy~fLDstartm))
#R2 = 0.04938 ; p = 0.1475 ; DF = 23

#summary(lm(Flo_Frudoy~fFLstartm))
#r2 = -0.02903 ; p = 0.5753 ; DF = 23

#summary(lm(Flo_Frudoy~fFRstartm))
#R2 = -.5577 ; p = <0.0001 ; DF 23

#summary(lm(Flo_Frudoy~fSENstartm))
#R2 = 0.09442 ; p = 0.07405 ; DF = 23


BB_SSdoy = fSENstartm - fLDstartm

#summary(lm(BB_SSdoy~fLDstartm))
#r2 = 0.0945 ; p = 0.07396

#summary(lm(BB_SSdoy~fFLstartm))
#R2 = -0.04284 ; p = 0.9064 ; DF = 23

#summary(lm(BB_SSdoy~fFRstartm))
#R2 = -0.03039 ; p = 0.594 ; DF = 23

#summary(lm(BB_SSdoy~fSENstartm))
#R2 = 0.7248 ; p = <0.0001 ; DF = 23


Flo_SSdoy = fSENstartm - fFLstartm 

#summary(lm(Flo_SSdoy~fLDstartm))
#r2 = 0.04225 ; p = 0.1648 ; DF = 23

#summary(lm(Flo_SSdoy~fFLstartm))
#R2 = 0.6408 ; p = <0.0001 ; DF = 23

#summary(lm(Flo_SSdoy~fFRstartm))
#R2 = 0..1745; p = 0.02162 ; DF = 23

#summary(lm(Flo_SSdoy~fSENstartm))
#R2 = 0.1325; p = 0.04146 ; DF = 23


Fru_SSdoy = fSENstartm - fRFRstartm

#summary(lm(Fru_SSdoy~fLDstartm))
#R2 = 0.1565 ; p = 0.02862 ; DF = 23

#summary(lm(Fru_SSdoy~fFLstartm))
#R2 = 0.4717 ; p = <0.0001 ; DF = 23

#summary(lm(Fru_SSdoy~fFRstartm))
#R2 = 0.8147 ; p = <0.0001 ; DF = 23

#summary(lm(Fru_SSdoy~fSENstartm))
#R2 = -0.04141 ; p = 0.8326 ; DF = 23


BB_LOdoy = fLOstartm - fLDstartm

#summary(lm(BB_LOdoy~fLDstartm))
#R2 = 0.3484 ; p = 0.001126 ; DF = 23

#summary(lm(BB_LOdoy~fLOstartm))
#R2 = -0.007418 ; p = 0.3736  DF = 23

#summary(lm(BB_LOdoy~fFLstartm))
#R2 = -0.04231 ; p =0.8741 ; DF = 23

#summary(lm(BB_LOdoy~fFRstartm))
#R2 = 0.001354 ; p = 0.3201 ; DF = 23

#summary(lm(BB_LOdoy~fSENstartm))
#R2 = -0.03927 ; p = 0.7629


LO_FLdoy = fFLstartm - fLOstartm

#summary(lm(LO_FLdoy~fLDstartm))
#R2 = 0.02576 ; p = 0.2138 ; DF = 23

#summary(lm(LO_FLdoy~fLOstartm))
#r2 = 0.04986 ; p = 0.1464 ; DF = 23 

#summary(lm(LO_FLdoy~fFLstartm))
#R2 = 0.9227 ; p = <0.0001 ; DF = 23

#summary(lm(LO_FLdoy~fFRstartm))
#R2 = 0.4836 ; p = < 0.0001 ; DF = 23

#summary(lm(LO_FLdoy~fSENstartm))
#R2 = -0.008455 ; p = 0.3807 ; DF = 23


LO_FRdoy = fRFRstartm - fLOstartm

#summary(lm(LO_FRdoy~fLDstartm))
#R2 = 0.01018 ; p = 0.06617 ; DF = 23

#summary(lm(LO_FRdoy~fLOstartm))
#R2 = 0.00722 ; P = 0.2897 ; DF = 23

#summary(lm(LO_FRdoy~fFLstartm))
#R2 = 0.4283 ; p = 0.0002314 ; DF = 23

#summary(lm(LO_FRdoy~fFRstartm))
#R2 = 0.9653 ; p = < 0.0001 ; DF = 23

#summary(lm(LO_FRdoy~fSENstartm))
#R2 = 0.1032 ; p = 0.06476 ; DF = 23


LO_SSdoy = fSENstartm - fLOstartm

#summary(lm(LO_SSdoy~fLDstartm))
#R2 = -0.02761 ; p =0.557 ; DF =23

#summary(lm(LO_SSdoy~fLOstartm))
#R2 = 0.04265 ; p = 0.1638 ; DF = 23

#summary(lm(LO_SSdoy~fFLstartm))
#R2 = -0.04189 ; p = 0.8532 ; DF = 23

#summary(lm(LO_SSdoy~fFRstartm))
#r2 = -0.002094 ; p = 0.3399 ; DF = 23

#summary(lm(LO_SSdoy~fSENstartm))
#R2 = 0.8118 ; p = <0.0001 ; DF = 23


#summary(lm(BB_Flodoy~fLOstartm))
#R2 = 0.079911 ; p = 0.09349 ; DF = 23

#summary(lm(BB_Frudoy~fLOstartm))
#R2 = 0.02904 ; p = 0.2029 ; DF = 23

#summary(lm(Flo_Frudoy~fLOstartm))
#r2 = -0.04062 ; p = 0.8037 ; DF = 23

#summary(lm(BB_SSdoy~fLOstartm))
#r2 = 0.001014 ; p = 0.322; DF = 23

#summary(lm(Flo_SSdoy~fLOstartm))
#R2 = 0.1423 ; p = 0.03563 ; DF = 23

#summary(lm(Fru_SSdoy~fLOstartm))
#R2 = 0.09141 ; p = 0.07752 ; DF = 23


################################
################################
#### 11/1/16 Megaplot 2.0
####################
fspecies<-names(fLDstartm)
fspecies_num<-c(1:25)
quartz(height=7, width=9)#this sets the dimensions of the plotting window
#par(mfrow=c(5,5))#this sets the number of rows and columns for the plots - in this case 5 rows and 5 columns
par(mfcol=c(5,5),mai=c(.2,.6,.2,.005), omi=c(.8,.01,.2,.2))#same thing but adding some measurements for margins within individual plots (may) and outside margins for the whole window (omi)

#1- commenting this panel out for now because budburst is earliest stage- can't predict it with previous stage
plot(BB_LOdoy, fLDstartm, ylab = "", xlab = "Leaf Out - Budburst DOY",pch=21,bg=cols[fspecies_num], xaxt='n', bty="l", ylim=c(min(fLDstartm-5),max(fLDstartm+5)))
abline(lm(fLDstartm~BB_LOdoy))
mtext(paste("=",round(summary(lm(fLDstartm~BB_LOdoy))$r.squared, digits=2),",  p=",round(summary(lm(fLDstartm~BB_LOdoy))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
mtext("Budburst DOY", side=2, cex=.7, line=2)
mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.4) # works

plot(BB_LOdoy, fLOstartm, ylab = "", xlab = "",pch=21,bg=cols[fspecies_num], xaxt='n', bty="l", ylim=c(min(fLOstartm-5),max(fLOstartm+5)))
mtext(paste("=",round(summary((lm(fLOstartm~BB_LOdoy)))$r.squared, digits=2),", p=",round(summary(lm(fLOstartm~BB_LOdoy))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.35) # works

mtext("Leaf Out DOY", side=2, cex=.7, line=2)

plot(BB_LOdoy,fFLstartm, ylab = "", xlab = "",pch=21,bg=cols[fspecies_num], xaxt='n', bty="l", ylim=c(min(fFLstartm-5),max(fFLstartm+5)))
mtext("Flowering DOY", side=2, cex=.7, line=2)
mtext(paste("=",round(summary((lm(fFLstartm~BB_LOdoy)))$r.squared, digits=2),", p=",round(summary(lm(fFLstartm~BB_LOdoy))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
mtext("Later phenological event", side=2, cex=.9, line=3, adj=.5)
mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.5) # works

plot(BB_LOdoy,fRFRstartm,  ylab = "", xlab = "",pch=21,bg=cols[fspecies_num], xaxt='n', bty="l", ylim=c(min(fRFRstartm-5),max(fRFRstartm+5)))
mtext("Fruiting DOY", side=2, cex=.7, line=2)
mtext(paste("=",round(summary((lm(fRFRstartm~BB_LOdoy)))$r.squared, digits=2),", p=",round(summary(lm(fRFRstartm~BB_LOdoy))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.4) # works

plot(BB_LOdoy,fSENstartm, ylab = "", xlab = "Leaf Out - Budburst DOY",pch=21,bg=cols[fspecies_num], bty="l", ylim=c(min(fSENstartm-5),max(fSENstartm+5)))
mtext("Leaf Out - Budburst DOY", side=1, cex=.7, line=2)
mtext("Senescence DOY", side=2, cex=.7, line=2)
mtext(paste("=",round(summary((lm(fSENstartm~BB_LOdoy)))$r.squared, digits=2),", p=",round(summary(lm(fSENstartm~BB_LOdoy))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.5) # works

#2
plot(BB_Flodoy, fLDstartm, xlab = "", ylab = "",pch=21,bg=cols[fspecies_num], xaxt='n', yaxt='n', bty="l", ylim=c(min(fLDstartm-5),max(fLDstartm+5)))
mtext(paste("=",round(summary((lm(fLDstartm~BB_Flodoy)))$r.squared, digits=2),", p=",round(summary(lm(fLDstartm~BB_Flodoy))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.5) # works

plot(BB_Flodoy,fLOstartm, ylab = "", xlab = "",pch=21,bg=cols[fspecies_num], xaxt='n', yaxt='n', bty="l", ylim=c(min(fLOstartm-5),max(fLOstartm+5)))
abline(lm(fLOstartm~BB_Flodoy), lty=2)
mtext(paste("=",round(summary((lm(fLOstartm~BB_Flodoy)))$r.squared, digits=2),", p=",round(summary(lm(fLOstartm~BB_Flodoy))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.35) # works

plot(BB_Flodoy,fFLstartm, ylab = "", xlab = "",pch=21,bg=cols[fspecies_num], xaxt='n', yaxt='n', bty="l", ylim=c(min(fFLstartm-5),max(fFLstartm+5)))
abline(lm(fFLstartm~BB_Flodoy))
mtext(paste("=",round(summary((lm(fFLstartm~BB_Flodoy)))$r.squared, digits=2),", p=",round(summary(lm(fFLstartm~BB_Flodoy))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.55) # works

plot(BB_Flodoy,fRFRstartm, ylab = "", xlab = "",pch=21,bg=cols[fspecies_num], xaxt='n', yaxt='n', bty="l", ylim=c(min(fRFRstartm-5),max(fRFRstartm+5)))
abline(lm(fRFRstartm~BB_Flodoy))
mtext(paste("=",round(summary((lm(fRFRstartm~BB_Flodoy)))$r.squared, digits=2),", p=",round(summary(lm(fRFRstartm~BB_Flodoy))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.35) # works

plot(BB_Flodoy,fSENstartm, ylab = "", xlab = "Flowering - Budburst DOY",pch=21,bg=cols[fspecies_num], yaxt='n', ylim=c(min(fSENstartm-5),max(fSENstartm+5)), bty="l")
mtext(paste("=",round(summary((lm(fSENstartm~BB_Flodoy)))$r.squared, digits=2),", p=",round(summary(lm(fSENstartm~BB_Flodoy))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.35) # works

mtext("Flowering - Budburst DOY", side=1, cex=.7, line=2)

#3
plot(LO_FLdoy, fLDstartm, ylab = "", xlab = "",pch=21,bg=cols[fspecies_num], xaxt='n', yaxt='n', bty="l", ylim=c(min(fLDstartm-5),max(fLDstartm+5)))
mtext(paste("=",round(summary((lm(fLDstartm~LO_FLdoy)))$r.squared, digits=2),", p=",round(summary(lm(fLDstartm~LO_FLdoy))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.35) # works

plot(LO_FLdoy, fLOstartm, ylab = "", xlab = "",pch=21,bg=cols[fspecies_num], xaxt='n', yaxt='n', bty="l", ylim=c(min(fLOstartm-5),max(fLOstartm+5)))
mtext(paste("=",round(summary((lm(fLOstartm~LO_FLdoy)))$r.squared, digits=2),", p=",round(summary(lm(fLOstartm~LO_FLdoy))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.35) # works

plot(LO_FLdoy, fFLstartm, ylab = "", xlab = "",pch=21,bg=cols[fspecies_num], xaxt='n', yaxt='n', bty="l", ylim=c(min(fFLstartm-5),max(fFLstartm+5)))
abline(lm(fFLstartm~LO_FLdoy))
mtext(paste("=",round(summary((lm(fFLstartm~LO_FLdoy)))$r.squared, digits=2),", p=",round(summary(lm(fFLstartm~LO_FLdoy))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.55) # works

plot(LO_FLdoy, fRFRstartm, ylab = "", xlab = "",pch=21,bg=cols[fspecies_num], xaxt='n', yaxt='n', bty="l", ylim=c(min(fRFRstartm-5),max(fRFRstartm+5)))
abline(lm(fRFRstartm~LO_FLdoy))
mtext(paste("=",round(summary((lm(fRFRstartm~LO_FLdoy)))$r.squared, digits=2),", p=",round(summary(lm(fRFRstartm~LO_FLdoy))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.55) # works

plot(LO_FLdoy,fSENstartm, ylab = "", xlab = "Flowering - Leaf Out DOY",pch=21,bg=cols[fspecies_num],yaxt='n', bty="l", ylim=c(min(fSENstartm-5),max(fSENstartm+5)))
mtext("Flowering - Leaf Out DOY", side=1, cex=.7, line=2)
mtext(paste("=",round(summary((lm(fSENstartm~LO_FLdoy)))$r.squared, digits=2),", p=",round(summary(lm(fSENstartm~LO_FLdoy))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
mtext("Inter-phenophase time", side=1, cex=.9, line=3, adj=.5)
mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.35) # works

#4
plot(Flo_Frudoy,fLDstartm,xlab = "", ylab = "",pch=21,bg=cols[fspecies_num], xaxt='n', yaxt='n', bty="l", ylim=c(min(fLDstartm-5),max(fLDstartm+5)))
mtext(paste("=",round(summary((lm(fLDstartm~Flo_Frudoy)))$r.squared, digits=2),", p=",round(summary(lm(fLDstartm~Flo_Frudoy))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.35) # works

plot(Flo_Frudoy, fLOstartm, ylab = "", xlab = "",pch=21,bg=cols[fspecies_num], xaxt='n', yaxt='n', bty="l", ylim=c(min(fLOstartm-5),max(fLOstartm+5)))
mtext(paste("=",round(summary((lm(fLOstartm~Flo_Frudoy)))$r.squared, digits=2),", p=",round(summary(lm(fLOstartm~Flo_Frudoy))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.5) # works

plot(Flo_Frudoy, fFLstartm, ylab = "", xlab = "",pch=21,bg=cols[fspecies_num], xaxt='n', yaxt='n', bty="l", ylim=c(min(fFLstartm-5),max(fFLstartm+5)))
mtext(paste("=",round(summary((lm(fFLstartm~Flo_Frudoy)))$r.squared, digits=2),", p=",round(summary(lm(fFLstartm~Flo_Frudoy))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.35) # works

plot(Flo_Frudoy, fRFRstartm, ylab = "", xlab = "",pch=21,bg=cols[fspecies_num], xaxt='n', yaxt='n', bty="l", ylim=c(min(fRFRstartm-5),max(fRFRstartm+5)))
abline(lm(fRFRstartm~Flo_Frudoy))
mtext(paste("=",round(summary((lm(fRFRstartm~Flo_Frudoy)))$r.squared, digits=2),", p=",round(summary(lm(fRFRstartm~Flo_Frudoy))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.6) # works

plot(Flo_Frudoy, fSENstartm, ylab = "", xlab = "Fruiting - Flowering DOY",pch=21,bg=cols[fspecies_num], yaxt='n', bty="l",ylim=c(min(fSENstartm-5),max(fSENstartm+5)))
mtext("Fruiting - Flowering DOY", side=1, cex=.7, line=2)
mtext(paste("=",round(summary((lm(fSENstartm~Flo_Frudoy)))$r.squared, digits=2),", p=",round(summary(lm(fSENstartm~Flo_Frudoy))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
abline(lm(fSENstartm~Flo_Frudoy), lty=2)
mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.4) # works

#5
plot(Fru_SSdoy, fLDstartm, xlab = "", ylab = "",pch=21,bg=cols[fspecies_num], xaxt='n', yaxt='n', bty="l",ylim=c(min(fLDstartm-5),max(fLDstartm+5)))
abline(lm(fLDstartm~Fru_SSdoy))
mtext(paste("=",round(summary((lm(fLDstartm~Fru_SSdoy)))$r.squared, digits=2),", p=",round(summary(lm(fLDstartm~Fru_SSdoy))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.35) # works

plot(Fru_SSdoy, fLOstartm, xlab = "", ylab = "",pch=21,bg=cols[fspecies_num], xaxt='n', yaxt='n', bty="l", ylim=c(min(fLOstartm-5),max(fLOstartm+5)))
mtext(paste("=",round(summary(lm(fLOstartm~Fru_SSdoy))$r.squared, digits=2),", p=",round(summary(lm(fLOstartm~Fru_SSdoy))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.35) # works

abline(lm(fLOstartm~Fru_SSdoy), lty=2)

plot(Fru_SSdoy, fFLstartm, xlab = "", ylab = "",pch=21,bg=cols[fspecies_num], xaxt='n', yaxt='n', bty="l", ylim=c(min(fFLstartm-5),max(fFLstartm+5)))
abline(lm(fFLstartm~Fru_SSdoy))
mtext(paste("=",round(summary((lm(fFLstartm~Fru_SSdoy)))$r.squared, digits=2),", p=",round(summary(lm(fFLstartm~Fru_SSdoy))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.6) # works

plot(Fru_SSdoy, fRFRstartm, xlab = "", ylab = "",pch=21,bg=cols[fspecies_num], xaxt='n', yaxt='n', bty="l", ylim=c(min(fRFRstartm-5),max(fRFRstartm+5)))
abline(lm(fRFRstartm~Fru_SSdoy))
mtext(paste("=",round(summary((lm(fRFRstartm~Fru_SSdoy)))$r.squared, digits=2),", p=",round(summary(lm(fRFRstartm~Fru_SSdoy))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.6) # works

plot(Fru_SSdoy, fSENstartm, xlab = "", ylab = "Senescence - Fruiting DOY",pch=21,bg=cols[fspecies_num], yaxt='n', bty="l", ylim=c(min(fSENstartm-5),max(fSENstartm+5)))
mtext("Senescence - Fruiting DOY", side=1, cex=.7, line=2)
mtext(paste("=",round(summary((lm(fSENstartm~Fru_SSdoy)))$r.squared, digits=2),", p=",round(summary(lm(fSENstartm~Fru_SSdoy))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.55) # works




##Now plot of later stage versus earlier stage
fspecies<-names(fLDstartm)
fspecies_num<-c(1:25)
quartz(height=7, width=7)#this sets the dimensions of the plotting window
#par(mfrow=c(5,5))#this sets the number of rows and columns for the plots - in this case 5 rows and 5 columns
par(mfcol=c(4,4),mai=c(.2,.7,.2,.01), omi=c(.8,.01,.2,.2))#same thing but adding some measurements for margins within individual plots (may) and outside margins for the whole window (omi)

#1- commenting this panel out for now because budburst is earliest stage- can't predict it with previous stage
#plot(BB_LOdoy, fLDstartm, ylab = "Budburst DOY", xlab = "Leaf Out - Budburst DOY",bg=cols[as.numeric(as.factor(fspecies))])
#abline(lm(BB_LOdoy~fLDstartm))

plot(fLDstartm, fLOstartm, ylab = "Leaf Out", xlab = "",pch=21,bg=cols[fspecies_num], bty="l")
abline(lm(fLOstartm~fLDstartm))
mtext(paste("=",round(summary((lm(fLOstartm~fLDstartm)))$r.squared, digits=2),", p=",round(summary(lm(fLOstartm~fLDstartm))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1)
mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.4) # works

plot(fLDstartm,fFLstartm, ylab = "Flowering DOY", xlab = "",pch=21,bg=cols[fspecies_num], bty="l")
abline(lm(fFLstartm~fLDstartm))
mtext(paste("=",round(summary((lm(fFLstartm~fLDstartm)))$r.squared, digits=2),", p=",round(summary(lm(fFLstartm~fLDstartm))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.2) # works

mtext("Later phenological event", side=2, cex=.9, line=4, adj=1)

plot(fLDstartm,fRFRstartm,  ylab = "Fruiting DOY", xlab = "",pch=21,bg=cols[fspecies_num], bty="l")
abline(lm(fRFRstartm~fLDstartm))
mtext(paste("=",round(summary((lm(fRFRstartm~fLDstartm)))$r.squared, digits=2),", p=",round(summary(lm(fRFRstartm~fLDstartm))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.2) # works

plot(fLDstartm,fSENstartm, ylab = "Senescence DOY", xlab = "",pch=21,bg=cols[fspecies_num], bty="l")
#abline(lm(fSENstartm~fLDstartm), lty=2)
mtext(paste("=",round(summary((lm(fSENstartm~fLDstartm)))$r.squared, digits=2),", p=",round(summary(lm(fSENstartm~fLDstartm))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.2) # works

mtext("Budburst DOY", side=1, cex=.7, line=2)

#2
plot.new()
plot(fLOstartm,fFLstartm, ylab = "", xlab = "",pch=21,bg=cols[fspecies_num], bty="l")
abline(lm(fFLstartm~fLOstartm))
mtext(paste("=",round(summary((lm(fFLstartm~fLOstartm)))$r.squared, digits=2),", p=",round(summary(lm(fFLstartm~fLOstartm))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.3) # works

plot(fLOstartm,fRFRstartm, ylab = "", xlab = "",pch=21,bg=cols[fspecies_num], bty="l")
abline(lm(fRFRstartm~fLOstartm))
mtext(paste("=",round(summary((lm(fRFRstartm~fLOstartm)))$r.squared, digits=2),", p=",round(summary(lm(fRFRstartm~fLOstartm))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.2) # works

plot(fLOstartm,fSENstartm, ylab = "", xlab = " ",pch=21,bg=cols[fspecies_num], bty="l")
mtext("Leafout DOY", side=1, cex=.7, line=2)
mtext(paste("=",round(summary((lm(fSENstartm~fLOstartm)))$r.squared, digits=2),", p=",round(summary(lm(fSENstartm~fLOstartm))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.2) # works

mtext("Previous phenological event", side=1, cex=.9, line=3, adj=0)

#3
plot.new()
plot.new()
plot(fFLstartm, fRFRstartm, ylab = "", xlab = "",pch=21,bg=cols, bty="l")
abline(lm(fRFRstartm~fFLstartm))
mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.4) # works
mtext(paste("=",round(summary((lm(fRFRstartm~fFLstartm)))$r.squared, digits=2),", p=",round(summary(lm(fRFRstartm~fFLstartm))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)

plot(fFLstartm,fSENstartm, ylab = "", xlab = "",pch=21,bg=cols[fspecies_num], bty="l")
mtext("Flowering DOY", side=1, cex=.7, line=2)
mtext(paste("=",round(summary((lm(fSENstartm~fFLstartm)))$r.squared, digits=2),", p=",round(summary(lm(fSENstartm~fFLstartm))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.2) # works

#4
par(xpd=NA)
plot(c(seq(1:25)),c(seq(1:25)), col="white",ylab = "", xlab = "",bty="n",xaxt='n', yaxt='n')
legend(-2,26,legend=fspecies,pch=21,pt.bg=cols[fspecies_num], bty="n")
#plot(c(seq(1:25)),c(seq(1:25)), col="white",ylab = "", xlab = "",bty="n",xaxt='n', yaxt='n')
#legend(-1,26,legend=fspecies[9:16],pch=21,pt.bg=cols[as.numeric(as.factor(fspecies))][9:16], bty="n")
#plot(c(seq(1:25)),c(seq(1:25)), col="white",ylab = "", xlab = "",bty="n",xaxt='n', yaxt='n')
#legend(-1,26,legend=fspecies[17:25],pch=21,pt.bg=cols[as.numeric(as.factor(fspecies))][17:25], bty="n")
#plot.new()
plot.new()
plot.new()
#for(i in 1:length(fspecies)){
#  mtext(paste(fspecies[i]),side=3, line=23-i, cex=.6, adj=0) 
#}
par(xpd=FALSE)
plot(fRFRstartm,fSENstartm, ylab = "", xlab = "",pch=21,bg=cols[fspecies_num], bty="l")
abline(lm(fSENstartm~fRFRstartm), lty=2)
mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.3) # works
mtext(paste("=",round(summary((lm(fSENstartm~fRFRstartm)))$r.squared, digits=2),", p=",round(summary(lm(fSENstartm~fRFRstartm))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)

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


#Look at Dan & Lizzie's data from growth chambers to see if it matches Sally's:
#FIRST, WITH CHILLING
d<-read.csv("data/leafoutdays_growthchamber.wlc1.csv", header=T)

cols2 <- colorRampPalette(brewer.pal(8,"Accent"))(28)

#First, later vs. earlier:
quartz(height=3, width=7)
par(mfrow=c(1,2))
plot(d$BB.WLC1.HF , d$LO.WLC1.HF, ylab = "Leafout", xlab = "Budburst",pch=21,bg=cols2[as.numeric(as.factor(d$X))], bty="l", main="Harvard Forest")
abline(lm(d$LO.WLC1.HF~d$BB.WLC1.HF))
mtext(paste("r2=",round(summary((lm(d$LO.WLC1.HF~d$BB.WLC1.HF)))$r.squared, digits=2),", p=",round(summary(lm(d$LO.WLC1.HF~d$BB.WLC1.HF))$coeff[2,4],digits=3)), side=1, line=-1, cex=.6, adj=1)

plot(d$BB.WLC1.SH , d$LO.WLC1.SH, ylab = "Leafout", xlab = "Budburst",pch=21,bg=cols2[as.numeric(as.factor(d$X))], bty="l", main="St. Hippolyte")
abline(lm(d$LO.WLC1.SH~d$BB.WLC1.SH))
mtext(paste("r2=",round(summary((lm(d$LO.WLC1.SH~d$BB.WLC1.SH)))$r.squared, digits=2),", p=",round(summary(lm(d$LO.WLC1.SH~d$BB.WLC1.SH))$coeff[2,4],digits=3)), side=1, line=-1, cex=.6, adj=1)

##Now , stages vs. interphenophase:
d$LO_BB.WLC1.HF<-d$LO.WLC1.HF-d$BB.WLC1.HF
d$LO_BB.WLC1.SH<-d$LO.WLC1.SH-d$BB.WLC1.SH

quartz(height=6, width=7)
par(mfrow=c(2,2))
plot(d$LO_BB.WLC1.HF,d$BB.WLC1.HF,ylab = "Budburst", xlab = "Leafout-Budburst",pch=21,bg=cols2[as.numeric(as.factor(d$X))], bty="l",main="Harvard Forest- Chill only")
mtext(paste("r2=",round(summary((lm(d$BB.WLC1.HF~d$LO_BB.WLC1.HF)))$r.squared, digits=2),", p=",round(summary(lm(d$BB.WLC1.HF~d$LO_BB.WLC1.HF))$coeff[2,4],digits=3)), side=1, line=-1, cex=.6, adj=1)
#abline(lm(d$BB.HF~d$LO_BB.HF))

plot(d$LO_BB.WLC1.SH, d$BB.WLC1.SH,ylab = "Budburst", xlab = "Leafout-Budburst",pch=21,bg=cols2[as.numeric(as.factor(d$X))], bty="l", main="St. Hippolyte- Chill only")
mtext(paste("r2=",round(summary((lm(d$BB.WLC1.SH~d$LO_BB.WLC1.SH)))$r.squared, digits=2),", p=",round(summary(lm(d$BB.WLC1.SH~d$LO_BB.WLC1.SH))$coeff[2,4],digits=3)), side=1, line=-1, cex=.6, adj=1)
#abline(lm(d$BB.WLC1.SH~d$LO_BB.WLC1.SH))

plot( d$LO_BB.WLC1.HF,d$LO.WLC1.HF,ylab = "Leafout", xlab = "Leafout-Budburst",pch=21,bg=cols2[as.numeric(as.factor(d$X))], bty="l")
mtext(paste("r2=",round(summary((lm(d$LO.WLC1.HF~d$LO_BB.WLC1.HF)))$r.squared, digits=2),", p=",round(summary(lm(d$LO.WLC1.HF~d$LO_BB.WLC1.HF))$coeff[2,4],digits=3)), side=1, line=-1, cex=.6, adj=1)
#abline(lm(d$LO.WLC1.HF~d$LO_BB.WLC1.HF))

plot(d$LO_BB.WLC1.SH,d$LO.WLC1.SH, ylab = "Leafout", xlab = "Leafout-Budburst",pch=21,bg=cols2[as.numeric(as.factor(d$X))], bty="l")
mtext(paste("r2=",round(summary((lm(d$LO.WLC1.SH~d$LO_BB.WLC1.SH)))$r.squared, digits=2),", p=",round(summary(lm(d$LO.WLC1.SH~d$LO_BB.WLC1.SH))$coeff[2,4],digits=3)), side=1, line=-1, cex=.6, adj=1)
#abline(lm(d$LO.WLC1.SH~d$LO_BB.WLC1.SH), lty=1, col="black")

#NOW WITH ALL EXPERIMENTAL TREATMENTS:
d<-read.csv("data/leafoutdays_growthchamber.csv", header=T)

cols2 <- colorRampPalette(brewer.pal(8,"Accent"))(28)
#First, later vs. earlier:
quartz(height=3, width=7)
par(mfrow=c(1,2))
plot(d$BB.HF, d$LO.HF, ylab = "Leafout", xlab = "Budburst",pch=21,bg=cols2[as.numeric(as.factor(d$X))], bty="l", main="Harvard Forest")
abline(lm(d$LO.HF~d$BB.HF))
mtext(paste("r2=",round(summary((lm(d$LO.HF~d$BB.HF)))$r.squared, digits=2),", p=",round(summary(lm(d$LO.HF~d$BB.HF))$coeff[2,4],digits=3)), side=1, line=-1, cex=.6, adj=1)

plot(d$BB.SH , d$LO.SH, ylab = "Leafout", xlab = "Budburst",pch=21,bg=cols2[as.numeric(as.factor(d$X))], bty="l", main="St. Hippolyte")
abline(lm(d$LO.SH~d$BB.SH))
mtext(paste("r2=",round(summary((lm(d$LO.SH~d$BB.SH)))$r.squared, digits=2),", p=",round(summary(lm(d$LO.SH~d$BB.SH))$coeff[2,4],digits=3)), side=1, line=-1, cex=.6, adj=1)

##Now , stages vs. interphenophase:
d$LO_BB.HF<-d$LO.HF-d$BB.HF
d$LO_BB.SH<-d$LO.SH-d$BB.SH

quartz(height=6, width=7)
par(mfrow=c(2,2))
plot(d$LO_BB.HF,d$BB.HF,ylab = "Budburst", xlab = "Leafout-Budburst",pch=21,bg=cols2[as.numeric(as.factor(d$X))], bty="l",main="Harvard Forest")
mtext(paste("r2=",round(summary((lm(d$BB.HF~d$LO_BB.HF)))$r.squared, digits=2),", p=",round(summary(lm(d$BB.HF~d$LO_BB.HF))$coeff[2,4],digits=3)), side=1, line=-1, cex=.6, adj=1)
#abline(lm(d$BB.HF~d$LO_BB.HF))

plot(d$LO_BB.SH, d$BB.SH,ylab = "Budburst", xlab = "Leafout-Budburst",pch=21,bg=cols2[as.numeric(as.factor(d$X))], bty="l", main="St. Hippolyte")
mtext(paste("r2=",round(summary((lm(d$BB.SH~d$LO_BB.SH)))$r.squared, digits=2),", p=",round(summary(lm(d$BB.SH~d$LO_BB.SH))$coeff[2,4],digits=3)), side=1, line=-1, cex=.6, adj=1)
#abline(lm(d$BB.SH~d$LO_BB.SH))

plot( d$LO_BB.HF,d$LO.HF,ylab = "Leafout", xlab = "Leafout-Budburst",pch=21,bg=cols2[as.numeric(as.factor(d$X))], bty="l")
mtext(paste("r2=",round(summary((lm(d$LO.HF~d$LO_BB.HF)))$r.squared, digits=2),", p=",round(summary(lm(d$LO.HF~d$LO_BB.HF))$coeff[2,4],digits=3)), side=1, line=-1, cex=.6, adj=1)
#abline(lm(d$LO.HF~d$LO_BB.HF))

plot(d$LO_BB.SH,d$LO.SH, ylab = "Leafout", xlab = "Leafout-Budburst",pch=21,bg=cols2[as.numeric(as.factor(d$X))], bty="l")
mtext(paste("r2=",round(summary((lm(d$LO.SH~d$LO_BB.SH)))$r.squared, digits=2),", p=",round(summary(lm(d$LO.SH~d$LO_BB.SH))$coeff[2,4],digits=3)), side=1, line=-1, cex=.6, adj=1)
abline(lm(d$LO.SH~d$LO_BB.SH), lty=1, col="black")


