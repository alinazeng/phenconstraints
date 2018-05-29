#Calculate start dates for phenophases and interphase durations with Sally's phenology data

##data are currently by individual, but we also want to plot them by species. 
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


##################################
##Calculate Interphase duration:##
##################################

#budburst to leafout interphase
BB_LOdoy = fLOstartm - fLDendm
#budburst to flowering interphase
BB_Flodoy = fFLstartm - fLDendm

#budburst to fruiting interphase
BB_Frudoy = fRFRstartm - fLDendm
#budburst to senesence interphase
BB_SSdoy = fSENstartm - fLDendm

#leafout to flowering interphase
LO_FLdoy = fFLstartm - fLOendm
#leafout to fruiting interphase
LO_Frudoy = fRFRstartm - fLOendm
#leafout to senesence interphase
LO_SSdoy = fSENstartm - fLOendm

#flowering to fruiting interphase
Flo_Frudoy = fRFRstartm - fFLendm
#flowering to senescence interphase
Flo_SSdoy = fSENstartm - fFLendm 

#fruiting to senescence interphase
Fru_SSdoy = fSENstartm - fRFRendm
