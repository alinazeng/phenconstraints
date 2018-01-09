#Fit the inter-phenophase models
LOBB_LOmod<-lm(fLOstartm~BB_LOdoy)
FLBB_LOmod<-lm(fFLstartm~BB_LOdoy)
FRBB_LOmod<-lm(fRFRstartm~BB_LOdoy)
SENBB_LOmod<-lm(fSENstartm~BB_LOdoy)

FLLO_FLmod<-lm(fFLstartm~LO_FLdoy)
FRLO_FLmod<-lm(fRFRstartm~LO_FLdoy)
SENLO_FLmod<-lm(fSENstartm~LO_FLdoy)

FRFlo_Frmod<-lm(fRFRstartm~Flo_Frudoy)
SENFlo_Frmod<-lm(fSENstartm~Flo_Frudoy)
SENFru_SSmod<-lm(fSENstartm~Fru_SSdoy)
