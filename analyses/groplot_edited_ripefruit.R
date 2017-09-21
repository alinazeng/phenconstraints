
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
#dat<-read.csv("data/growingseason_doy.csv", header=T)
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

#dat2$SenEnd_DOY
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

##Now Plot
#figure out what max and min values are for all phases, so that i know what the x axis limits should be
#max(dat[,12:19],na.rm=T)
#min(dat[,12:19],na.rm=T)
quartz(height=8,width=10)#for pc you replace "quartz" with X11
par(mai=c(1,3,.1,.1), omi=c(1.2,.1,.1,.2))
plot(8,10, type="p", cex=.8,pch=21, col="white", bty="L", xlab="Day of Year",ylab=" ", ylim=c(1,50), yaxt='n',xlim=c(110,385),las=1)
axis(side=2,at=c(seq(from =2, to = 50, by = 2)),labels=(paste(rev(names(fLDstartm)))),las=1, font=3)

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

legend(325,50, legend=c("budburst","leafout", "senescence","in flower","fruit developing", "ripe fruit"), lty=1,lwd=2,col=c("palegreen","seagreen","yellow","orchid","gray","darkorchid"), cex=.85)

#Plot same figure, but for individual instead of species:
#sort by species-level mean flowering date
nameorder<-data.frame(cbind(names(fFLstartm),c(1:25)))
colnames(nameorder)<-c("Species","Order")
dat3<-left_join(dat2,nameorder,by=c("Species"), copy=TRUE)

inddat<-dat3[order(dat3$Order),]

quartz(height=12,width=12)#for pc you replace "quartz" with X11
par(mai=c(1,1.5,.1,.1), omi=c(1.5,.1,.1,.2))
plot(10,15, type="p", cex=.8,pch=21, col="white", bty="L", xlab="Day of Year",ylab=" ", ylim=c(2,236), yaxt='n',xlim=c(110,385),las=1)
#Make new label for y-axis that is has species name only once
names<- c("Populus deltoides"," "," "," "," ",
          "Quercus alba"," "," "," "," " ,
          "Quercus glandulifera"," "," "," ",
          "Carya ovata"," "," "," "," ",
          "Fraxinus chinensis"," "," "," "," ",
          "Liquidambar styraciflua"," "," "," "," ",
          "Platanus occidentalis","","","","",
          "Carya glabra",""," "," "," ",
          "Crataegus crus-galli"," "," "," "," ",                 
          "Phellodendron amurense"," ","  "," ",
          "Liriodendron tulipifera"," "," "," "," ", 
          "Betula alleghaniensis"," "," "," "," ", 
          "Gleditsia triacanthos"," "," "," "," ",            
          "Tilia japonica"," "," "," "," ", 
          "Catalpa speciosa"," "," "," "," ", 
          "Tilia americana"," "," "," "," ",                      
          "Kalopanax septemlobus"," "," ",
          "Styphnolobium japonicum"," "," "," "," ",
          "Betula nigra", " "," "," "," ", 
          "Pyrus ussuriensis"," "," "," "," ",  
          "Fagus engleriana"," "," "," ",
          "Quercus rubra", " "," "," "," ", 
          "Aesculus flava"," "," "," "," ", 
          "Fagus grandifolia"," "," "," "," ", 
          "Pyrus calleryana var. dimorphophylla"," "," ")
axis(side=2,at=c(seq(from =2, to = 236, by = 2)),labels=(paste(rev(names))),las=1, cex.axis=0.5, font=3, tick=FALSE)


y<-rev(seq(from =2, to = 236, by = 2))
for(i in 1:length(inddat$Species)){
  lines(c(inddat$LOutStart_DOY[i],inddat$LOutEnd_DOY[i]),c(y[i],y[i]), col="seagreen",lwd=2)
  lines(c(inddat$LDStart_DOY[i],inddat$SenEnd_DOY[i]),c(y[i],y[i]), col="seagreen",lwd=2)
  lines(c(inddat$FloStart_DOY[i],inddat$RipeFruEnd_DOY[i]),c(y[i]-0.4,y[i]-0.4),col="lightgray", lwd=2)
  lines(c(inddat$FloStart_DOY[i],inddat$FruStart_DOY[i]),c(y[i]-0.4,y[i]-0.4), col="orchid", lwd=2)
  lines(c(inddat$LDStart_DOY[i],inddat$LDEnd_DOY[i]),c(y[i],y[i]), col="palegreen", lwd=2)
  lines(c(inddat$RipeFruStart_DOY[i],inddat$RipeFruEnd_DOY[i]),c(y[i]-0.4,y[i]-0.4), col="darkorchid", lwd=2)
  lines(c(inddat$SenStart_DOY[i],inddat$SenEnd_DOY[i]),c(y[i],y[i]), col="yellow2",lwd=2)
}

legend(340,50, legend=c("budburst","leafout", "senescence","in flower","fruit developing", "ripe fruit"), lty=1,lwd=2,col=c("palegreen","seagreen","yellow","orchid","gray","darkorchid"))


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

BB_Frudoy = fRFRstartm - fLDstartm

Flo_Frudoy = fRFRstartm - fFLstartm

BB_SSdoy = fSENstartm - fLDstartm

Flo_SSdoy = fSENstartm - fFLstartm 

Fru_SSdoy = fSENstartm - fRFRstartm

LOstartm<-tapply(dat2$LOutStart_DOY,dat2$Species,mean, na.rm=T)
LOendm<-tapply(dat2$LOutEnd_DOY,dat2$Species,mean, na.rm=T)

fLOstartm<- LOstartm[names(fFLstartm)]
fLOendm<- LOendm[names(fFLstartm)]

BB_LOdoy = fLOstartm - fLDstartm

LO_FLdoy = fFLstartm - fLOstartm
LO_FRdoy = fRFRstartm - fLOstartm
LO_SSdoy = fSENstartm - fLOstartm

BB_Flodoy = fFLstartm - fLDstartm
BB_Frudoy = fRFRstartm - fLDstartm
Flo_Frudoy = fRFRstartm - fFLstartm
BB_SSdoy = fSENstartm - fLDstartm
Flo_SSdoy = fSENstartm - fFLstartm 
Fru_SSdoy = fSENstartm - fRFRstartm
BB_LOdoy = fLOstartm - fLDstartm
LO_FLdoy = fFLstartm - fLOstartm
LO_FRdoy = fRFRstartm - fLOstartm
LO_SSdoy = fSENstartm - fLOstartm
################################
################################
#### 11/1/16 Megaplot 2.0
####################
#Modified August 18, 2017 by Ailene to only shower lower triangle (only earlier interphenophases can predict later phases)
fspecies<-names(fFLstartm)
fspecies_num<-c(1:25)
quartz(height=7, width=9)#this sets the dimensions of the plotting window
#par(mfrow=c(5,5))#this sets the number of rows and columns for the plots - in this case 5 rows and 5 columns
par(mfcol=c(4,4),mai=c(.2,.6,.2,.005), omi=c(.8,.01,.2,.2))#same thing but adding some measurements for margins within individual plots (may) and outside margins for the whole window (omi)

#1- 
#plot(BB_LOdoy, fLDstartm, ylab = "", xlab = "Leaf Out - Budburst DOY",pch=21,bg=cols[fspecies_num], xaxt='n', bty="l", ylim=c(min(fLDstartm-5),max(fLDstartm+5)))
#abline(lm(fLDstartm~BB_LOdoy))
#mtext(paste("=",round(summary(lm(fLDstartm~BB_LOdoy))$r.squared, digits=2),",  p=",round(summary(lm(fLDstartm~BB_LOdoy))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
#mtext("Budburst DOY", side=2, cex=.7, line=2)
#mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.4) # works

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

#2-Remove this column and focus on leafout
#plot(BB_Flodoy, fLDstartm, xlab = "", ylab = "",pch=21,bg=cols[fspecies_num], xaxt='n', yaxt='n', bty="l", ylim=c(min(fLDstartm-5),max(fLDstartm+5)))
#mtext(paste("=",round(summary((lm(fLDstartm~BB_Flodoy)))$r.squared, digits=2),", p=",round(summary(lm(fLDstartm~BB_Flodoy))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
#mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.5) # works

#plot(BB_Flodoy,fLOstartm, ylab = "", xlab = "",pch=21,bg=cols[fspecies_num], xaxt='n', yaxt='n', bty="l", ylim=c(min(fLOstartm-5),max(fLOstartm+5)))
#abline(lm(fLOstartm~BB_Flodoy), lty=2)
#mtext(paste("=",round(summary((lm(fLOstartm~BB_Flodoy)))$r.squared, digits=2),", p=",round(summary(lm(fLOstartm~BB_Flodoy))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
#mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.35) # works

#plot(BB_Flodoy,fFLstartm, ylab = "", xlab = "",pch=21,bg=cols[fspecies_num], xaxt='n', yaxt='n', bty="l", ylim=c(min(fFLstartm-5),max(fFLstartm+5)))
#abline(lm(fFLstartm~BB_Flodoy))
#mtext(paste("=",round(summary((lm(fFLstartm~BB_Flodoy)))$r.squared, digits=2),", p=",round(summary(lm(fFLstartm~BB_Flodoy))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
#mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.55) # works

#plot(BB_Flodoy,fRFRstartm, ylab = "", xlab = "",pch=21,bg=cols[fspecies_num], xaxt='n', yaxt='n', bty="l", ylim=c(min(fRFRstartm-5),max(fRFRstartm+5)))
#abline(lm(fRFRstartm~BB_Flodoy))
#mtext(paste("=",round(summary((lm(fRFRstartm~BB_Flodoy)))$r.squared, digits=2),", p=",round(summary(lm(fRFRstartm~BB_Flodoy))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
#mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.35) # works

#plot(BB_Flodoy,fSENstartm, ylab = "", xlab = "Flowering - Budburst DOY",pch=21,bg=cols[fspecies_num], yaxt='n', ylim=c(min(fSENstartm-5),max(fSENstartm+5)), bty="l")
#mtext(paste("=",round(summary((lm(fSENstartm~BB_Flodoy)))$r.squared, digits=2),", p=",round(summary(lm(fSENstartm~BB_Flodoy))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
#mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.35) # works

#mtext("Flowering - Budburst DOY", side=1, cex=.7, line=2)

#3
#plot(LO_FLdoy, fLDstartm, ylab = "", xlab = "",pch=21,bg=cols[fspecies_num], xaxt='n', yaxt='n', bty="l", ylim=c(min(fLDstartm-5),max(fLDstartm+5)))
#mtext(paste("=",round(summary((lm(fLDstartm~LO_FLdoy)))$r.squared, digits=2),", p=",round(summary(lm(fLDstartm~LO_FLdoy))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
#mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.35) # works
plot.new()
#plot(LO_FLdoy, fLOstartm, ylab = "", xlab = "",pch=21,bg=cols[fspecies_num], xaxt='n', yaxt='n', bty="l", ylim=c(min(fLOstartm-5),max(fLOstartm+5)))
#mtext(paste("=",round(summary((lm(fLOstartm~LO_FLdoy)))$r.squared, digits=2),", p=",round(summary(lm(fLOstartm~LO_FLdoy))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
#mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.35) # works

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
#plot(Flo_Frudoy,fLDstartm,xlab = "", ylab = "",pch=21,bg=cols[fspecies_num], xaxt='n', yaxt='n', bty="l", ylim=c(min(fLDstartm-5),max(fLDstartm+5)))
#mtext(paste("=",round(summary((lm(fLDstartm~Flo_Frudoy)))$r.squared, digits=2),", p=",round(summary(lm(fLDstartm~Flo_Frudoy))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
#mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.35) # works

#plot(Flo_Frudoy, fLOstartm, ylab = "", xlab = "",pch=21,bg=cols[fspecies_num], xaxt='n', yaxt='n', bty="l", ylim=c(min(fLOstartm-5),max(fLOstartm+5)))
#mtext(paste("=",round(summary((lm(fLOstartm~Flo_Frudoy)))$r.squared, digits=2),", p=",round(summary(lm(fLOstartm~Flo_Frudoy))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
#mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.5) # works
plot.new()
plot.new()


#plot(Flo_Frudoy, fFLstartm, ylab = "", xlab = "",pch=21,bg=cols[fspecies_num], xaxt='n', yaxt='n', bty="l", ylim=c(min(fFLstartm-5),max(fFLstartm+5)))
#mtext(paste("=",round(summary((lm(fFLstartm~Flo_Frudoy)))$r.squared, digits=2),", p=",round(summary(lm(fFLstartm~Flo_Frudoy))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
#mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.35) # works

plot(Flo_Frudoy, fRFRstartm, ylab = "", xlab = "",pch=21,bg=cols[fspecies_num], xaxt='n', yaxt='n', bty="l", ylim=c(min(fRFRstartm-5),max(fRFRstartm+5)))
abline(lm(fRFRstartm~Flo_Frudoy))
mtext(paste("=",round(summary((lm(fRFRstartm~Flo_Frudoy)))$r.squared, digits=2),", p=",round(summary(lm(fRFRstartm~Flo_Frudoy))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.6) # works

plot(Flo_Frudoy, fSENstartm, ylab = "", xlab = "Fruiting - Flowering DOY",pch=21,bg=cols[fspecies_num], yaxt='n', bty="l",ylim=c(min(fSENstartm-5),max(fSENstartm+5)))
mtext("Fruiting - Flowering DOY", side=1, cex=.7, line=2)
mtext(paste("=",round(summary((lm(fSENstartm~Flo_Frudoy)))$r.squared, digits=2),", p=",round(summary(lm(fSENstartm~Flo_Frudoy))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
abline(lm(fSENstartm~Flo_Frudoy))
mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.4) # works

#5
#plot(Fru_SSdoy, fLDstartm, xlab = "", ylab = "",pch=21,bg=cols[fspecies_num], xaxt='n', yaxt='n', bty="l",ylim=c(min(fLDstartm-5),max(fLDstartm+5)))
#abline(lm(fLDstartm~Fru_SSdoy))
#mtext(paste("=",round(summary((lm(fLDstartm~Fru_SSdoy)))$r.squared, digits=2),", p=",round(summary(lm(fLDstartm~Fru_SSdoy))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
#mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.35) # works

#plot(Fru_SSdoy, fLOstartm, xlab = "", ylab = "",pch=21,bg=cols[fspecies_num], xaxt='n', yaxt='n', bty="l", ylim=c(min(fLOstartm-5),max(fLOstartm+5)))
#mtext(paste("=",round(summary(lm(fLOstartm~Fru_SSdoy))$r.squared, digits=2),", p=",round(summary(lm(fLOstartm~Fru_SSdoy))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
#mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.35) # works
#abline(lm(fLOstartm~Fru_SSdoy), lty=2)

#plot(Fru_SSdoy, fFLstartm, xlab = "", ylab = "",pch=21,bg=cols[fspecies_num], xaxt='n', yaxt='n', bty="l", ylim=c(min(fFLstartm-5),max(fFLstartm+5)))
#abline(lm(fFLstartm~Fru_SSdoy))
#mtext(paste("=",round(summary((lm(fFLstartm~Fru_SSdoy)))$r.squared, digits=2),", p=",round(summary(lm(fFLstartm~Fru_SSdoy))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
#mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.6) # works

#plot(Fru_SSdoy, fRFRstartm, xlab = "", ylab = "",pch=21,bg=cols[fspecies_num], xaxt='n', yaxt='n', bty="l", ylim=c(min(fRFRstartm-5),max(fRFRstartm+5)))
#abline(lm(fRFRstartm~Fru_SSdoy))
#mtext(paste("=",round(summary((lm(fRFRstartm~Fru_SSdoy)))$r.squared, digits=2),", p=",round(summary(lm(fRFRstartm~Fru_SSdoy))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
#mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.6) # works
plot.new()
plot.new()
plot.new()

plot(Fru_SSdoy, fSENstartm, xlab = "", ylab = "",pch=21,bg=cols[fspecies_num], yaxt='n', bty="l", ylim=c(min(fSENstartm-5),max(fSENstartm+5)))
mtext("Senescence - Fruiting DOY", side=1, cex=.7, line=2)
mtext(paste("=",round(summary((lm(fSENstartm~Fru_SSdoy)))$r.squared, digits=2),", p=",round(summary(lm(fSENstartm~Fru_SSdoy))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.55) # works




##Now plot of later stage versus earlier stage
fspecies<-names(fFLstartm)
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
abline(lm(fRFRstartm~fLOstartm), lty=2)
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
abline(lm(fSENstartm~fRFRstartm), lty=1)
mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.3) # works
mtext(paste("=",round(summary((lm(fSENstartm~fRFRstartm)))$r.squared, digits=2),", p=",round(summary(lm(fSENstartm~fRFRstartm))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)

mtext("Fruiting DOY", side=1, cex=.7, line=2)


##Write csv with all the species-level data in it for Lizzie to use:
sp<-cbind(fspecies,fLDstartm,fLOstartm,fFLstartm,fRFRstartm,fSENstartm,fSENendm,BB_LOdoy, LO_FLdoy, Flo_Frudoy, Fru_SSdoy)
colnames(sp)<-
write.csv(sp, rownames=FALSE)






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


