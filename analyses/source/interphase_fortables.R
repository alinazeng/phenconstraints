
#Fit the inter-phenophase models
LOBB_LOmod<-lm(fLOstartm~BB_LOdoy)
FLBB_FLmod<-lm(fFLstartm~BB_Flodoy)
FRBB_FRmod<-lm(fRFRstartm~BB_Frudoy)
SENBB_SSmod<-lm(fSENstartm~BB_SSdoy)

FLLO_FLmod<-lm(fFLstartm~LO_FLdoy)
FRLO_FRmod<-lm(fRFRstartm~LO_Frudoy)
SENLO_SSmod<-lm(fSENstartm~LO_SSdoy)

#FRFlo_FRmod<-lm(fRFRstartm~Flo_Frudoy)
FRFlo_FRmod<-lm(fRFRstartm~Flo_Frudoy)
SENFlo_SSmod<-lm(fSENstartm~Flo_SSdoy)
SENFru_SSmod<-lm(fSENstartm~Fru_SSdoy)

#Fit reampled models and save slopes, intercepts
# Function to plot everything and fit line to resampled data f(x)
phases=as.data.frame(cbind(fLDstartm,fLOstartm,fFLstartm,fRFRstartm,fSENstartm))
interph2<-as.data.frame(cbind(BB_LOdoy,BB_Flodoy,LO_FLdoy,BB_Frudoy,LO_Frudoy,Flo_Frudoy, BB_SSdoy,LO_SSdoy,Flo_SSdoy,Fru_SSdoy)) #interphases for adjacent phases for now:budburst to leafout interphase, leafout to flowering interphase,flowering to fruiting interphase, fruiting to senescence interphase
resamps <- function(postphase,interpheno,prephase, extraphase){
  slopes<-c()
  ints<-c()
  for (i in 1:999){
    interpheno.resamp <- sample(interpheno, 25, replace=TRUE)
    postphase.resamp <- prephase+interpheno.resamp+extraphase
    mod<-lm(postphase.resamp~interpheno.resamp)
    slopes[i]<-coef(mod)[2]
    ints[i]<-coef(mod)[1]
    }
  resamps<-cbind(ints,slopes)
  return(resamps)
}
LOBBresamps<-as.data.frame(resamps(phases$fLOstartm, interph2$BB_LOdoy, phases$fLDstartm, rep(0, 25)))
FLLOresamps<-as.data.frame(resamps(phases$fFLstartm, interph2$LO_FLdoy, phases$fLOstartm, rep(0, 25)))
FLBBresamps<-as.data.frame(resamps(phases$fFLstartm, interph2$BB_Flodoy, phases$fLDstartm, rep(0, 25)))
FRBBresamps<-as.data.frame(resamps(phases$fRFRstartm, interph2$BB_Frudoy, phases$fLDstartm, rep(0, 25)))
FRLOresamps<-as.data.frame(resamps(phases$fRFRstartm, interph2$LO_Frudoy, phases$fLOstartm, rep(0, 25)))
FRFLresamps<-as.data.frame(resamps(phases$fRFRstartm, interph2$Flo_Frudoy, phases$fFLstartm, rep(0, 25)))
SENBBresamps<-as.data.frame(resamps(phases$fSENstartm, interph2$BB_SSdoy, phases$fLDstartm, rep(0, 25)))
SENLOresamps<-as.data.frame(resamps(phases$fSENstartm, interph2$LO_SSdoy, phases$fLOstartm, rep(0, 25)))
SENFLresamps<-as.data.frame(resamps(phases$fSENstartm, interph2$Flo_SSdoy, phases$fFLstartm, rep(0, 25)))
SENFRresamps<-as.data.frame(resamps(phases$fSENstartm, interph2$Fru_SSdoy, phases$fRFRstartm, rep(0, 25)))

LOBB.CI<-round(quantile(LOBBresamps$slopes,c(0.025, 0.975)),digits=2)
FLLO.CI<-round(quantile(FLLOresamps$slopes,c(0.025, 0.975)),digits=2)
FLBB.CI<-round(quantile(FLBBresamps$slopes,c(0.025, 0.975)),digits=2)
FRBB.CI<-round(quantile(FRBBresamps$slopes,c(0.025, 0.975)),digits=2)
FRLO.CI<-round(quantile(FRLOresamps$slopes,c(0.025, 0.975)),digits=2)
FRFL.CI<-round(quantile(FRFLresamps$slopes,c(0.025, 0.975)),digits=2)
SENBB.CI<-round(quantile(SENBBresamps$slopes,c(0.025, 0.975)),digits=2)
SENLO.CI<-round(quantile(SENLOresamps$slopes,c(0.025, 0.975)),digits=2)
SENFL.CI<-round(quantile(SENFLresamps$slopes,c(0.025, 0.975)),digits=2)
SENFR.CI<-round(quantile(SENFRresamps$slopes,c(0.025, 0.975)),digits=2)
cis<-as.data.frame(rbind(LOBB.CI,FLBB.CI,FRBB.CI,SENBB.CI,FLLO.CI,FRLO.CI,SENLO.CI,FRFL.CI,SENFL.CI,SENFR.CI))
colnames(cis)<-c("perc2.5","perc97.5")
