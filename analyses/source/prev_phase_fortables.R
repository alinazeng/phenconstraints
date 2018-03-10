##Analyses for previous stage models
options(stringsAsFactors = FALSE)

library(xtable)
require(dplyr)
require(xtable)

dat2<-read.csv("../analyses/output/growingseason_doy2.csv", header = T)

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
fLOstartm<- LOstartm[names(fFLstartm)]
fLOendm<- LOendm[names(fFLstartm)]

#inter-phenophase times:
BB_LOdoy = fLOstartm - fLDstartm
LO_FLdoy = fFLstartm - fLOstartm
Flo_Frudoy = fRFRstartm - fFLstartm
Fru_SSdoy = fSENstartm - fRFRstartm

#Fit the previous stage models
LO_BBmod<-lm(fLOstartm~fLDstartm)
LO_BBfmod <- lm(fLOstartm ~ 1 + offset(1*fLDstartm)) # Force a slope of 1 through the data

#anova(LO_BBmod, LO_BBfmod)#test if the models are different

library(car)
#linearHypothesis(LO_BBmod, "fLDstartm=1")
FL_BBmod<-lm(fFLstartm~fLDstartm)
FL_BBfmod<-lm(fFLstartm~1 + offset(1*fLDstartm))

RFR_BBmod<-lm(fRFRstartm~fLDstartm)
RFR_BBfmod <- lm(fRFRstartm ~ 1 + offset(1*fLDstartm)) # Force a slope of 1 through the data
#anova(FR_BBmod, FR_BBfmod)#test if the models are different

SEN_BBmod<-lm(fSENstartm~fLDstartm)
SEN_BBfmod<-lm(fSENstartm~1+offset(1*fLDstartm))

FL_LOmod<-lm(fFLstartm~fLOstartm)
FL_LOfmod<-lm(fFLstartm~1+offset(1*fLOstartm))

RFR_LOmod<-lm(fRFRstartm~fLOstartm)
RFR_LOfmod<-lm(fRFRstartm~1+offset(1*fLOstartm))

SEN_LOmod<-lm(fSENstartm~fLOstartm)

SEN_LOfmod<-lm(fSENstartm~1+offset(1*fLOstartm))



RFR_FLmod<-lm(fRFRstartm~fFLstartm)
RFR_FLfmod<-lm(fRFRstartm~1+offset(1*fFLstartm))
#anova(FR_FLmod, FR_FLfmod)#test if the models are different

SEN_FLmod<-lm(fSENstartm~fFLstartm)
SEN_FLfmod<-lm(fSENstartm~1+offset(1*fFLstartm))

SEN_RFRmod<-lm(fSENstartm~fRFRstartm)
SEN_RFRfmod<-lm(fSENstartm~1+offset(1*FRstartm))

