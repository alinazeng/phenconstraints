#Analyses and figures for Sally's Phenology data
#data collection occured during the 2015 growing season at Arnold ARboretum, for Sally Gee's thesis data
#by Sally (Started 2016) and added to by Ailene (beginning in Feb 2017)
#June 2017
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
# Set working directory: 
if(length(grep("ailene", getwd()))>0) {setwd("~/git/phenconstraints")}

#load libraries
library(RColorBrewer)

#Read in data:
#dat<-read.csv("data/growingseason_doy.csv", header=T)
#dat2 has additional phenophases in it; otherwise i think the two datasets are the same?
dat2<-read.csv("analyses/output/growingseason_doy2.csv", header = T)

#calculate start of each phase and interphase durations:
source("analyses/source/phase_start_and_inter_species.R")

#Make "Phenograms": Plots when different phenophases happened

#Each species gets a different color, using ramping
#cols <- rev(colorRampPalette(brewer.pal(9,"YlOrRd"))(25))#red to yellow with later flowering dates
cols <- colorRampPalette(brewer.pal(9,"YlOrRd"))(25)#yellow to red with later flowering dates

quartz(height=8,width=10)
par(mai=c(1,3,.1,.1), omi=c(1.2,.1,.1,.2))
plot(8,10, type="p", cex=.8,pch=21, col="white", bty="L", xlab="Day of Year",ylab=" ", ylim=c(1,50), yaxt='n',xlim=c(110,385),las=1)
axis(side=2,at=c(seq(from =2, to = 50, by = 2)),labels=(paste(rev(names(fLDstartm)))),las=1, font=3)

#Start with first to flower
species<-names(fFLstartm)
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


#####################################################
##Make plot of Later Event vs. Interphase duration:##
#####################################################

fspecies<-names(fFLstartm)
fspecies_num<-c(1:25)
quartz(height=7, width=9)#this sets the dimensions of the plotting window
par(mfcol=c(4,4),mai=c(.2,.6,.2,.005), omi=c(.8,.01,.2,.2))#same thing but adding some measurements for margins within individual plots (may) and outside margins for the whole window (omi)

#Col. 1: Phenophases vs. Leafout-Budburst Interphase
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

#Col. 2:Phenophases vs. Flowering-Leafout Interphase
plot.new()

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

#Col. 3: Phenophases vs. Fruiting-Flowering Interphase
plot.new()
plot.new()

plot(Flo_Frudoy, fRFRstartm, ylab = "", xlab = "",pch=21,bg=cols[fspecies_num], xaxt='n', yaxt='n', bty="l", ylim=c(min(fRFRstartm-5),max(fRFRstartm+5)))
abline(lm(fRFRstartm~Flo_Frudoy))
mtext(paste("=",round(summary((lm(fRFRstartm~Flo_Frudoy)))$r.squared, digits=2),", p=",round(summary(lm(fRFRstartm~Flo_Frudoy))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.6) # works

plot(Flo_Frudoy, fSENstartm, ylab = "", xlab = "Fruiting - Flowering DOY",pch=21,bg=cols[fspecies_num], yaxt='n', bty="l",ylim=c(min(fSENstartm-5),max(fSENstartm+5)))
mtext("Fruiting - Flowering DOY", side=1, cex=.7, line=2)
mtext(paste("=",round(summary((lm(fSENstartm~Flo_Frudoy)))$r.squared, digits=2),", p=",round(summary(lm(fSENstartm~Flo_Frudoy))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
abline(lm(fSENstartm~Flo_Frudoy))
mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.4) # works

#Col 4:Phenophases vs. Senescence-Fruiting Interphase
plot.new()
plot.new()
plot.new()

plot(Fru_SSdoy, fSENstartm, xlab = "", ylab = "",pch=21,bg=cols[fspecies_num], yaxt='n', bty="l", ylim=c(min(fSENstartm-5),max(fSENstartm+5)))
mtext("Senescence - Fruiting DOY", side=1, cex=.7, line=2)
mtext(paste("=",round(summary((lm(fSENstartm~Fru_SSdoy)))$r.squared, digits=2),", p=",round(summary(lm(fSENstartm~Fru_SSdoy))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.55) # works

##########################################
##Make plot of Later vs. Earlier phases:##
##########################################

fspecies<-names(fFLstartm)
fspecies_num<-c(1:25)

quartz(height=7, width=7)#this sets the dimensions of the plotting window
par(mfcol=c(4,4),mai=c(.2,.7,.2,.01), omi=c(.8,.01,.2,.2))#same thing but adding some measurements for margins within individual plots (may) and outside margins for the whole window (omi)

#Col. 1: Phenophases vs. Budburst 
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

#Col. 2: Phenophases vs. Leafout
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

#Col. 3: Phenophases vs. Flowering
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

#Col. 4: Phenophases vs. Flowering
par(xpd=NA)
plot(c(seq(1:25)),c(seq(1:25)), col="white",ylab = "", xlab = "",bty="n",xaxt='n', yaxt='n')
legend(-2,26,legend=fspecies,pch=21,pt.bg=cols[fspecies_num], bty="n")
plot.new()
plot.new()
#Col. 4: Phenophases vs. Fruiting
par(xpd=FALSE)
plot(fRFRstartm,fSENstartm, ylab = "", xlab = "",pch=21,bg=cols[fspecies_num], bty="l")
abline(lm(fSENstartm~fRFRstartm), lty=1)
mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.3) # works
mtext(paste("=",round(summary((lm(fSENstartm~fRFRstartm)))$r.squared, digits=2),", p=",round(summary(lm(fSENstartm~fRFRstartm))$coeff[2,4],digits=3)), side=3, line=-.5, cex=.6, adj=1.2)
mtext("Fruiting DOY", side=1, cex=.7, line=2)

##Write csv with all the species-level data in it for Lizzie to use:
sp<-cbind(fspecies,fLDstartm,fLOstartm,fFLstartm,fRFRstartm,fSENstartm,BB_LOdoy, LO_FLdoy, Flo_Frudoy, Fru_SSdoy)
colnames(sp)<-c("species","bb","lo","fl", "fr", "sen","bb_lo","lo_fl","fl_fr","fr_sen")
write.csv(sp,"analyses/output/phenomeans.csv", row.names=FALSE)
