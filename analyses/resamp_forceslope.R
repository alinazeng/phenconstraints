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
prephase<-latephase<-int<-rse<-rsq<-c()

quartz(height=7, width=7)#this sets the dimensions of the plotting window
par(mfrow=c(4,4),mai=c(.2,.7,.2,.01), omi=c(.8,.01,.2,.2))#same thing but adding some measurements for margins within individual plots (may) and outside margins for the whole window (omi)

for (i in 1:length(late_phase_col)){
  y=phases[,late_phase_col[i]]
  for(j in 1:i){
  x=phases[,j]
  forceBmod <- lm(y ~ 1 + offset(1*x)) # Force a slope of 1 through the data
  Intmod <- lm(y ~ 1 ) # fit only intercept
  Bmod <- lm(y ~ -1+x ) # fit only slope
  regmod <- lm(y ~ x ) # fit standard regression
  yhat <- predict(forceBmod)
  ss_tot <- sum((y - mean(y))^2)
  ss_res <- sum((y - yhat)^2)
  Rsq <- 1 - (ss_res / ss_tot)
  latephase<-c(latephase,colnames(phases)[late_phase_col[i]])
  prephase<-c(prephase,colnames(phases)[prev_phase_col[j]])
  plot(x,y, ylab = paste(latephaselab[i]), xlab = paste(prevphaselab[j]),pch=21,bg=cols[fspecies_num], bty="l")
  abline(a=forceBmod$coef,b=1, lty=1)
  abline(a=regmod$coef[1],b=regmod$coef[2], lty=2, col="Red")
  if(i==1 & j==1){plot.new();plot.new(); plot.new()}
  if(i==2 & j==2){plot.new();plot.new()}
  if(i==3 & j==3){plot(c(seq(1:25)),c(seq(1:25)), col="white",ylab = "", xlab = "",bty="n",xaxt='n', yaxt='n')
    legend("top",legend=fspecies,pch=21,pt.bg=cols[fspecies_num], bty="n", cex=1.1)}
  int<-c(int,summary(forceBmod)$coef[1])
  rse<-c(rse,summary(forceBmod)$sigma)
  rsq<-c(rsq,Rsq)
  print(colnames(phases)[late_phase_col[i]]); print(colnames(phases)[prev_phase_col[j]])
  print(AIC(forceBmod,Intmod,Bmod,regmod)); print(Rsq)
  }
}

#######################################################################
####Hypothesis 2: Interphase predicts Later phase: Using Resampling####
#######################################################################
interph<-as.data.frame(cbind(BB_LOdoy,LO_FLdoy,Flo_Frudoy, Fru_SSdoy)) #interphases for adjacent phases for now:budburst to leafout interphase, leafout to flowering interphase,flowering to fruiting interphase, fruiting to senescence interphase

# Function to plot everything and fit line to resampled data f(x)
plothyp2 <- function(postphase,interpheno,prephase, extraphase,xlab,ylab){
  plot(postphase~interpheno, type="n", bty="l", xlab=paste(xlab), ylab=paste(ylab))
  for (i in 1:999){
    interpheno.resamp <- sample(interpheno, 25, replace=TRUE)
    postphase.resamp <- prephase+interpheno.resamp+extraphase
    abline(lm(postphase.resamp~interpheno.resamp), col="gray")
  }
  abline(lm(postphase~interpheno), col="black", lwd=2)
  points(postphase~interpheno, pch=21,bg=cols[fspecies_num])
}

# adj phases
quartz(height=7, width=2.5)#this sets the dimensions of the plotting window
par(mfrow=c(4,1),mai=c(.5,.7,.2,.01), omi=c(.8,.01,.2,.2))#same thing but adding some measurements for margins within individual plots (may) and outside margins for the whole window (omi)
plothyp2(phases$LOstartm, interph$BB_LOdoy, phases$LDstartm, rep(0, 25),"Leaf Out - Budburst DOY", "Leafout DOY")
#plot.new();plot.new(); plot.new();
plothyp2(phases$FLstartm, interph$LO_FLdoy, phases$LOstartm, rep(0, 25),"Flowering - Leaf Out DOY", "Flowering DOY")
plothyp2(phases$RFRstartm, interph$Flo_Frudoy, phases$FLstartm, rep(0, 25), "Fruiting - Flowering DOY","Fruiting DOY")
plothyp2(phases$SENstartm, interph$Fru_SSdoy, phases$RFRstartm, rep(0, 25), "Senescence - Fruiting DOY","Senescence DOY")

#all phases
interph2<-as.data.frame(cbind(BB_LOdoy,BB_Flodoy,LO_FLdoy,BB_Frudoy,LO_Frudoy,Flo_Frudoy, BB_SSdoy,LO_SSdoy,Flo_SSdoy,Fru_SSdoy)) #interphases for adjacent phases for now:budburst to leafout interphase, leafout to flowering interphase,flowering to fruiting interphase, fruiting to senescence interphase
quartz(height=7, width=7)#this sets the dimensions of the plotting window
par(mfrow=c(4,4),mai=c(.5,.7,.2,.01), omi=c(.8,.01,.2,.2))#same thing but adding some measurements for margins within individual plots (may) and outside margins for the whole window (omi)
#leafout
plothyp2(phases$LOstartm, interph2$BB_LOdoy, phases$LDstartm, rep(0, 25),"Leafout - Budburst DOY", "Leafout DOY")
plot.new();plot.new(); plot.new();
#flowering
plothyp2(phases$FLstartm, interph2$BB_Flodoy, phases$LDstartm, rep(0, 25),"Flowering - Budburst DOY", "Flowering DOY")
plothyp2(phases$FLstartm, interph2$LO_FLdoy, phases$LOstartm, rep(0, 25),"Flowering - Leafout DOY", "Flowering DOY")
plot.new();plot.new(); 
#fruiting
plothyp2(phases$RFRstartm, interph2$BB_Frudoy, phases$LDstartm, rep(0, 25), "Fruiting - Budburst DOY","Fruiting DOY")
plothyp2(phases$RFRstartm, interph2$LO_Frudoy, phases$LOstartm, rep(0, 25), "Fruiting - Leafout DOY","Fruiting DOY")
plothyp2(phases$RFRstartm, interph2$Flo_Frudoy, phases$FLstartm, rep(0, 25), "Fruiting - Flowering DOY","Fruiting DOY")
plot.new(); 
#senescence
plothyp2(phases$SENstartm, interph2$BB_SSdoy, phases$LDstartm, rep(0, 25), "Senescence - Budburst DOY","Senescence DOY")
plothyp2(phases$SENstartm, interph2$LO_SSdoy, phases$LOstartm, rep(0, 25), "Senescence - Leafout DOY","Senescence DOY")
plothyp2(phases$SENstartm, interph2$Flo_SSdoy, phases$FLstartm, rep(0, 25), "Senescence - Flowering DOY","Senescence DOY")
plothyp2(phases$SENstartm, interph2$Fru_SSdoy, phases$RFRstartm, rep(0, 25), "Senescence - Fruiting DOY","Senescence DOY")
