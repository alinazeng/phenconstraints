#Analyses and figures for Sally's Phenology data
#forced slope models (Hyp1) and resampling approach (Hyp2) suggested by Jonathan
#code started october 5, 2017 based on lizzie and jonathan's resampling code
#data collection occured during the 2015 growing season at Arnold ARboretum, for Sally Gee's thesis data

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
# Set working directory: 
if(length(grep("ailene", getwd()))>0) {setwd("~/git/phenconstraints")}

#load libraries
library(RColorBrewer)

#Read in data:
dat2<-read.csv("analyses/output/growingseason_doy2.csv", header = T)

#calculate start of each phase and interphase durations:
source("analyses/source/phase_start_and_inter_species.R")

#Fit models with a forced slope of 1 through the data (only the intercept can move)
#With forced slope models, we have to calculate R2 by hand, so write
bblo_mod <- lm(fLOstartm ~ 1 + offset(1*fLDstartm)) # Force a slope of 1 through the data
summary(bblo_mod)
#looks like I have to do the R2 by hand!
bblo_ss_tot <- sum((fLOstartm - mean(fLOstartm))^2)
bblo_ss_res <- sum(summary(bblo_mod)$residuals)
bblo_r2 <- 1 - ss_res / ss_tot
bblo_r2

#Write a loop to fit model, get interepts, r2, and p vals for all 
#make vectors of all prev phases (x variables) and later phases (y variables)
phases=cbind(LDstartm,LOstartm,FLstartm,RFRstartm,SENstartm)
colnames(phases)<-c("LDstartm","LOstartm","FLstartm","RFRstartm","SENstartm")
prev_phase_col=c(1,2,3,4)
late_phase_col=c(2,3,4,5)

#make blank dataframe for model results
prephase<-latephase<-int<-rse<-c()

#fit 10 models:
for (i in 1:length(late_phase)){
  y=phases[,late_phase_col[i]]
  for(j in 1:i){
  x=phases[,j]
  pmod <- lm(y ~ 1 + offset(1*x)) # Force a slope of 1 through the data
  yhat <- predict(mod)
  ss_tot <- sum((y - mean(y))^2)
  ss_res <- sum((y - yhat)^2)
  #Rsq <- 1 - ss_res / ss_tot
  latephase<-c(latephase,colnames(phases)[late_phase_col[i]])
  prephase<-c(prephase,colnames(phases)[prev_phase_col[j]])
  plot(x,y)
  abline(a=mod,b=1)
  int<-c(int,summary(pmod)$coef[1])
  rse<-c(rse,summary(pmod)$sigma)
  #rsq<-c(rsq,Rsq)
  #pval<-c(pval,summary(mod)$coef[4])
  }
}


##########################################
##Make plots of Later vs. Earlier phases##
########with forced slope model R2s #######
##########################################

quartz(height=7, width=7)#this sets the dimensions of the plotting window
par(mfcol=c(4,4),mai=c(.2,.7,.2,.01), omi=c(.8,.01,.2,.2))#same thing but adding some measurements for margins within individual plots (may) and outside margins for the whole window (omi)

#Col. 1: Phenophases vs. Budburst 
plot(fLDstartm, fLOstartm, ylab = "Leaf Out", xlab = "",pch=21,bg=cols[fspecies_num], bty="l")
abline(lm(fLOstartm~fLDstartm))
mtext(paste("=",round(bblo_r2, digits=2),", p=",round(summary(bblo_mod)$coeff[4],digits=3), side=3, line=-.5, cex=.6, adj=1)
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
