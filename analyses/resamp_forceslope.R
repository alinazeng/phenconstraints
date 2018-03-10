#Analyses and figures for Sally's Phenology data
#forced slope models (Hyp1) and resampling approach (Hyp2) suggested by Jonathan
#code started october 5, 2017 based on lizzie and jonathan's resampling code
#data collection occured during the 2015 growing season at Arnold ARboretum, for Sally Gee's thesis data

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
# Set working directory: 

if(length(grep("Lizzie", getwd())>0)) {setwd("~/Documents/git/projects/misc/phenconstraints")} else if
(length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/phenconstraints")
}
#load libraries
library(RColorBrewer)

#Read in data:
dat2<-read.csv("analyses/output/growingseason_doy2.csv", header = T)

#calculate start of each phase and interphase durations:
source("analyses/source/phase_start_and_inter_species.R")

#########################################
####Hypothesis 1: forced slope models####
#########################################
#Fit models with a forced slope of 1 through the data (only the intercept can move)
#Write a loop to fit model, get interepts, r2, and p vals for all 
#make vectors of all prev phases (x variables) and later phases (y variables)
phases=as.data.frame(cbind(fLDstartm,fLOstartm,fFLstartm,fRFRstartm,fSENstartm))
colnames(phases)<-c("LDstartm","LOstartm","FLstartm","RFRstartm","SENstartm")
prev_phase_col=c(1,2,3,4)
late_phase_col=c(2,3,4,5)
prevphaselab<-c("Budburst DOY","Leafout DOY","Flowering DOY","Fruiting DOY")
latephaselab<-c("Leafout DOY","Flowering DOY","Fruiting DOY","Senescence DOY")

##Fit 10 models and Make plots of Later vs. Earlier phases, with forced slope model
#Rsquared values are weird in some cases becuase model fit is worse than mean (intercept only) model...some are negative (e.g. -6.11)
#could use AIC instead to compare fit...or ????
cols <- colorRampPalette(brewer.pal(9,"YlOrRd"))(25)#yellow to red with later flowering dates
fspecies<-names(fFLstartm)
fspecies_num<-c(1:25)
#make blank dataframe for model results
prephase<-latephase<-int<-rsq.fs<-rsq.reg<-c()

quartz(height=7, width=7)#this sets the dimensions of the plotting window
par(mfrow=c(4,4),mai=c(.2,.2,.2,.01), omi=c(.6,.6,.2,.2))#same thing but adding some measurements for margins within individual plots (may) and outside margins for the whole window (omi)
  ylims=rbind(c(110,160), c(116,201),c(154,322),c(154,322))
  xlims=rbind(c(48,216),c(74,242), c(74,242),c(154,322))
for (i in 1:length(late_phase_col)){
  y=phases[,late_phase_col[i]]
  for(j in 1:i){
  x=phases[,j]
  forceBmod <- lm(y ~ 1 + offset(1*x)) # Force a slope of 1 through the data
  #Intmod <- lm(y ~ 1 ) # fit only intercept
  #Bmod <- lm(y ~ -1+x ) # fit only slope
  regmod <- lm(y ~ x ) # fit standard regression
  yhat <- predict(forceBmod)
  ss_tot <- sum((y - mean(y))^2)
  ss_res <- sum((y - yhat)^2)
  Rsq.fs <- 1 - (ss_res / ss_tot)
  latephase<-c(latephase,colnames(phases)[late_phase_col[i]])
  prephase<-c(prephase,colnames(phases)[prev_phase_col[j]])
  xlim=c(xlims[j,])
  ylim=c(ylims[i,])
  if(i==1 & j==1){xlim=c(110,160)}
  if(i==2 & j==1){xlim=c(100,180)}
  if(i==2 & j==2){xlim=c(100,180)}

  if(i==4 & j<3){xlim=c(105,175); ylim=c(240,310)}
  if(i==4 & j==3){xlim=c(110,200);ylim=c(230,320)}
  
  plot(x,y, ylab = paste(latephaselab[i]), xlab = paste(prevphaselab[j]),pch=21,bg=cols[fspecies_num], bty="l", xaxt='n', yaxt='n', xlim=xlim, ylim=ylim)
  axis(side=1,labels=TRUE) 
  axis(side=2,labels=TRUE) 
  abline(a=forceBmod$coef,b=1, lty=2, col="Red")
  rsq.regmod<-round(summary(regmod)$r.squared, digits=3)
  rsq.fsmod<-round(Rsq.fs, digits=3)
  if(rsq.fsmod>0.10){mtext(paste("fs =",rsq.fsmod), side=3, line=-.5, cex=.6, adj=.2, col="Red")
  mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=0.05, col="Red")}
  
  p.regmod<-summary(regmod)$coef[2,4]
  if(rsq.regmod>0.10){
    abline(a=regmod$coef[1],b=regmod$coef[2], lty=1)
    mtext(paste("reg =",rsq.regmod), side=3, line=-1.5, cex=.6, adj=.2)
    mtext(expression( "r" ^ italic("2")), side=3, line=-1.5, cex=.6, adj=0.05)
    }
  if(i==1 & j==1){
    axis(side=2,labels=TRUE) 
    mtext("Leafout DOY", side=2, cex=.7, line=2, adj=.5)
    plot.new();plot.new();plot.new();
    legend("top",legend=fspecies,pch=21,pt.bg=cols[fspecies_num], bty="n", cex=1.1)
    }
  if(i==2 & j==1){axis(side=2,labels=TRUE) 
    mtext("Flowering DOY", side=2, cex=.7, line=2, adj=.5)
    mtext("Later phenological event", side=2, cex=.9, line=4, adj=.5)
    }
  if(i==2 & j==2){plot.new();plot.new()}
  if(i==3 & j==1){axis(side=2,labels=TRUE) 
  mtext("Fruiting DOY", side=2, cex=.7, line=2, adj=.5)}
  if(i==3 & j==3){plot(c(seq(1:25)),c(seq(1:25)), col="white",ylab = "", xlab = "",bty="n",xaxt='n', yaxt='n')}
  if(i==4 & j==1){
    axis(side=2,labels=TRUE) 
    mtext("Senescence DOY", side=2, cex=.7, line=2, adj=.5)
    mtext("Budburst DOY", side=1, cex=.7, line=2, adj=.5)
    axis(side=1,labels=TRUE) 
  }
  if(i==4 & j==2){
    axis(side=1,labels=TRUE) 
    mtext("Leafout DOY", side=1, cex=.7, line=2, adj=.5)
    mtext("Previous phenological event", side=1, cex=.9, line=3, adj=.5)
    }
  if(i==4 & j==3){ 
    axis(side=1,labels=TRUE) 
    mtext("Flowering DOY", side=1, cex=.7, line=2, adj=.5)}
  if(i==4 & j==4){axis(side=1,labels=TRUE) 

    mtext("Fruiting DOY", side=1, cex=.7, line=2, adj=.5)}
  
  #int<-c(int,summary(forceBmod)$coef[1])
  #rse<-c(rse,summary(forceBmod)$sigma)
  rsq.fs.<-c(rsq.fs,rsq.fsmod)
  rsq.reg<-c(rsq.reg,rsq.regmod)
  print(colnames(phases)[late_phase_col[i]]); print(colnames(phases)[prev_phase_col[j]])
  print(max(y));print(min(y));print(max(y)-min(y))
  print(max(x));print(min(x));print(max(x)-min(x))
  print(AIC(forceBmod,regmod))
  }
}

  
  
#######################################################################
####Hypothesis 2: Interphase predicts Later phase: Using Resampling####
#######################################################################
interph<-as.data.frame(cbind(BB_LOdoy,LO_FLdoy,Flo_Frudoy, Fru_SSdoy)) #interphases for adjacent phases for now:budburst to leafout interphase, leafout to flowering interphase,flowering to fruiting interphase, fruiting to senescence interphase

# Function to plot everything and fit line to resampled data f(x)
plothyp2 <- function(postphase,interpheno,prephase, extraphase,xlab,ylab){
  plot(postphase~interpheno, type="n", bty="l", xlab=paste(xlab), ylab=paste(ylab))
  axis(side=1,labels=FALSE) 
  axis(side=2,labels=FALSE) 
  for (i in 1:999){
    interpheno.resamp <- sample(interpheno, 25, replace=TRUE)
    postphase.resamp <- prephase+interpheno.resamp+extraphase
    mod<-lm(postphase.resamp~interpheno.resamp)
    abline(mod, col="gray")
  }
  mod<-lm(postphase~interpheno)
  rsq<-summary(mod)$r.squared
  p<-summary(mod)$coeff[2,4]
  if(p<0.05){
    abline(a=mod$coef[1],b=mod$coef[2], lty=1)
    mtext(paste("=",round(summary(mod)$r.squared, digits=2),digits=3), side=3, line=-.5, cex=.6, adj=1.2)
    mtext(expression( "r" ^ italic("2")), side=3, line=-.5, cex=.6, adj=.2) # works
    abline(mod, col="black", lwd=2)
    }
  points(postphase~interpheno, pch=21,bg=cols[fspecies_num])
}

# adjacent phases only
quartz(height=7, width=3)#this sets the dimensions of the plotting window
par(mfrow=c(4,1),mai=c(.5,.6,.1,.01), omi=c(.6,.6,.2,.2))#same thing but adding some measurements for margins within individual plots (may) and outside margins for the whole window (omi)
plothyp2(phases$LOstartm, interph$BB_LOdoy, phases$LDstartm, rep(0, 25),"Leaf Out - Budburst DOY", "Leafout DOY")
plothyp2(phases$FLstartm, interph$LO_FLdoy, phases$LOstartm, rep(0, 25),"Flowering - Leaf Out DOY", "Flowering DOY")
plothyp2(phases$RFRstartm, interph$Flo_Frudoy, phases$FLstartm, rep(0, 25), "Fruiting - Flowering DOY","Fruiting DOY")
mtext("Later phenological event DOY", side=2, cex=.9, line=4, adj=0)
plothyp2(phases$SENstartm, interph$Fru_SSdoy, phases$RFRstartm, rep(0, 25), "Senescence - Fruiting DOY","Senescence DOY")
mtext("Interphase duration (days)", side=1, cex=.9, line=4, adj=.5)

#all phases
interph2<-as.data.frame(cbind(BB_LOdoy,BB_Flodoy,LO_FLdoy,BB_Frudoy,LO_Frudoy,Flo_Frudoy, BB_SSdoy,LO_SSdoy,Flo_SSdoy,Fru_SSdoy)) #interphases for adjacent phases for now:budburst to leafout interphase, leafout to flowering interphase,flowering to fruiting interphase, fruiting to senescence interphase
quartz(height=7, width=7)#this sets the dimensions of the plotting window
par(mfrow=c(4,4),mai=c(.2,.2,.2,.01), omi=c(.6,.6,.2,.2))#same thing but adding some measurements for margins within individual plots (may) and outside margins for the whole window (omi)
#leafout
plothyp2(phases$LOstartm, interph2$BB_LOdoy, phases$LDstartm, rep(0, 25),"Leafout - Budburst DOY", "Leafout DOY")
axis(side=2,labels=TRUE) 
mtext("Leafout", side=2, cex=.7, line=2, adj=.5)
plot.new();plot.new(); plot.new();
#flowering
plothyp2(phases$FLstartm, interph2$BB_Flodoy, phases$LDstartm, rep(0, 25),"Flowering - Budburst DOY", "Flowering DOY")
axis(side=2,labels=TRUE) 
mtext("Flowering", side=2, cex=.7, line=2, adj=.5)
mtext("Later phenological event DOY", side=2, cex=.9, line=3, adj=.5)

plothyp2(phases$FLstartm, interph2$LO_FLdoy, phases$LOstartm, rep(0, 25),"Flowering - Leafout DOY", "")
plot.new();plot.new();
legend("top",legend=fspecies,pch=21,pt.bg=cols[fspecies_num], bty="n", cex=1.1)}

#fruiting
plothyp2(phases$RFRstartm, interph2$BB_Frudoy, phases$LDstartm, rep(0, 25), "Fruiting - Budburst DOY","Fruiting DOY")
axis(side=2,labels=TRUE) 
mtext("Fruiting", side=2, cex=.7, line=2, adj=.5)
plothyp2(phases$RFRstartm, interph2$LO_Frudoy, phases$LOstartm, rep(0, 25), "Fruiting - Leafout DOY","Fruiting DOY")
plothyp2(phases$RFRstartm, interph2$Flo_Frudoy, phases$FLstartm, rep(0, 25), "Fruiting - Flowering DOY","Fruiting DOY")
plot.new(); 
#senescence
plothyp2(phases$SENstartm, interph2$BB_SSdoy, phases$LDstartm, rep(0, 25), "Senescence - Budburst DOY","Senescence DOY")
axis(side=2,labels=TRUE) 
mtext("Senescence", side=2, cex=.7, line=2, adj=.5)
axis(side=1,labels=TRUE) 
mtext("Later phase - Budburst DOY", side=1, cex=.7, line=2, adj=.5)
plothyp2(phases$SENstartm, interph2$LO_SSdoy, phases$LOstartm, rep(0, 25), "Senescence - Leafout DOY","Senescence DOY")
axis(side=1,labels=TRUE) 
mtext("Later phase - Leafout DOY", side=1, cex=.7, line=2, adj=.5)
mtext("Interphase duration (days)", side=1, cex=.9, line=3, adj=.5)

plothyp2(phases$SENstartm, interph2$Flo_SSdoy, phases$FLstartm, rep(0, 25), "Senescence - Flowering DOY","Senescence DOY")
axis(side=1,labels=TRUE) 
mtext("Later phase - Flowering DOY", side=1, cex=.7, line=2, adj=.5)
plothyp2(phases$SENstartm, interph2$Fru_SSdoy, phases$RFRstartm, rep(0, 25), "Senescence - Fruiting DOY","Senescence DOY")
axis(side=1,labels=TRUE) 
mtext("Senescence - Fruiting DOY", side=1, cex=.7, line=2, adj=.5)
