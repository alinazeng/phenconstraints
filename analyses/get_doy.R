### Sally Growing Season Partition Data
### 1/31/2015

# Leaf Developement: Start = Breaking leaf buds >3, End = Increasing Leaf Size >95%
# Flowering: Start = 5-24% Open Flowers, End >95% Open Flowers
# Fruiting: Start = 3-10 Fruits, End >95% Ripe Fruits
# Senescence: Start = 5-24% Color Change, End = >95% Color Chnage

## Addition 10/23/16
## Leaf Out: Start = Leaf Out Percent 5-24%, End >95% Leaf Out Percent
## Created a new csv file grosingeason_doy2

#Addition 08/10/2017
# Ripe Fruit: Start = 5-24% or more Ripe Fruits, End >95% Ripe Fruits
rm(list=ls()) 
options(stringsAsFactors = FALSE)
# Set working directory: 
if(length(grep("ailene", getwd()))>0) {setwd("~/git/phenconstraints")}
pheno=read.csv("data/Phenology2015_R.csv",header=T)
#head(pheno)
#colnames(pheno)#see what all the column names are
#dim(pheno)#see how many rows and columns there are (118 rwos, 365 columns)

pheno.long = reshape(pheno, varying = list(names(pheno)[3:35], names(pheno)[36:68], names(pheno)[69:101], names(pheno)[102:134], names(pheno)[135:167], 
                                           names(pheno)[168:200], names(pheno)[201:233], names(pheno)[234:266], names(pheno)[267:299],
                                           names(pheno)[300:332], names(pheno)[333:365]),direction = "long", 
                     v.names = c("Date","BB_No","CC_Perc", "FExpL_No","FB_No","F_No","IncLS_Perc","LDrop_Perc","LOut_Perc",
                                 "OFlows_Perc","RipeF_Perc"))

#dim(pheno.long)
#colnames(pheno.long)

## Leaf Development
#unique(pheno.long$BB_No)
pheno.long$LeafDevStart=NA #create a new column
pheno.long[which(pheno.long$BB_No == "<3"),]$LeafDevStart="N"
pheno.long[which(pheno.long$BB_No == "10-Mar"| pheno.long$BB_No == "11-100"| pheno.long$BB_No == "101-1000"| 
                   pheno.long$BB_No == "1001-10000"| pheno.long$BB_No == ">10000"),]$LeafDevStart="Y"
#unique(pheno.long$LeafDevStart)

#unique(pheno.long$IncLS_Perc)
pheno.long$LeafDevEnd=NA #create a new column
pheno.long[which(pheno.long$IncLS_Perc == ">95"),]$LeafDevEnd="Y"
pheno.long[which(pheno.long$IncLS_Perc == "<5"| pheno.long$IncLS == "24-May"| pheno.long$IncLS_Perc == "<25"| 
                   pheno.long$IncLS_Perc == "25-49"| pheno.long$IncLS_Perc == "25-94"| pheno.long$IncLS_Perc == "50-74"|
                   pheno.long$IncLS_Perc == "75-95"),]$LeafDevEnd="N"
#unique(pheno.long$LeafDevEnd)

### Leaf Out Addition 10/23/16 for new phenology interphase graphs
#unique(pheno.long$LOut_Perc)
pheno.long$LeafOutStart = NA
pheno.long[which(pheno.long$LOut_Perc == "<5"|pheno.long$LOut_Perc == "<25"),]$LeafOutStart="N"
pheno.long[which(pheno.long$LOut_Perc == "24-May"| pheno.long$LOut_Perc == "25-49"| pheno.long$LOut_Perc == "74-94%"|
                   pheno.long$LOut_Perc == "75-95"| pheno.long$LOut_Perc == "75-95%"| pheno.long$LOut_Perc == "50-74"|
                   pheno.long$LOut_Perc == ">95"| pheno.long$LOut_Perc == ">96"| pheno.long$LOut_Perc == ">97"),]$LeafOutStart="Y"
#unique(pheno.long$LeafOutStart)

pheno.long$LeafOutEnd = NA
pheno.long[which(pheno.long$LOut_Perc == ">95"|pheno.long$LOut_Perc == ">96"|pheno.long$LOut_Perc == ">97"),]$LeafOutEnd="Y"
pheno.long[which(pheno.long$LOut_Perc == "<5"|pheno.long$LOut_Perc == "<25"|pheno.long$LOut_Perc == "24-May"| pheno.long$LOut_Perc == "25-49"| 
                   pheno.long$LOut_Perc == "74-94%"| pheno.long$LOut_Perc == "75-95"| pheno.long$LOut_Perc == "75-95%"| 
                   pheno.long$LOut_Perc == "50-74"),]$LeafOutEnd="N"
#unique(pheno.long$LeafOutEnd)

## Flowering
#unique(pheno.long$OFlows_Perc)
pheno.long$FloStart=NA #create a Start column
pheno.long[which(pheno.long$OFlows_Perc == "<5"),]$FloStart="N"
pheno.long[which(pheno.long$OFlows_Perc == "101-1000"| pheno.long$OFlows_Perc == "24-May"| pheno.long$OFlows_Perc == "25-49"|
                   pheno.long$OFlows_Perc == "50-74"|pheno.long$OFlows_Perc == "75-95"| pheno.long$OFlows_Perc == ">95" | 
                   pheno.long$OFlows_Perc == "95"),]$FloStart="Y"
#unique(pheno.long$FloStart)

pheno.long$FloEnd=NA #create a new column
pheno.long[which(pheno.long$OFlows_Perc == ">95" | pheno.long$OFlows_Perc == "95"),]$FloEnd="Y"
pheno.long[which(pheno.long$OFlows_Perc == "101-1000"| pheno.long$OFlows_Perc == "24-May"| pheno.long$OFlows_Perc == "25-49"|
                   pheno.long$OFlows_Perc == "50-74"|pheno.long$OFlows_Perc == "75-95"| pheno.long$OFlows_Perc == "<5"),]$FloEnd="N"
#unique(pheno.long$FloEnd)

## Fruiting
#unique(pheno.long$F_No)
pheno.long$FruitStart=NA
pheno.long[which(pheno.long$F_No == "<3"),]$FruitStart="N"
pheno.long[which(pheno.long$F_No == "10-Mar" | pheno.long$F_No == "1001-10000"| pheno.long$F_No == "101-1000"| pheno.long$F_No == "11-100"|
                   pheno.long$F_No == "1001-1000" | pheno.long$F_No == "101-10000" | pheno.long$F_No == ">10000"),]$FruitStart="Y"
#unique(pheno.long$FruitStart)

#unique(pheno.long$RipeF_Perc)
pheno.long$FruitEnd=NA
pheno.long[which(pheno.long$RipeF_Perc == ">95"),]$FruitEnd="Y"
pheno.long[which(pheno.long$RipeF_Perc == "<3"| pheno.long$RipeF_Perc == "<5"| pheno.long$RipeF_Perc == "24-May"| pheno.long$RipeF_Perc == "2-May"| 
                   pheno.long$RipeF_Perc == "25-49"| pheno.long$RipeF_Perc == "50-74"| pheno.long$RipeF_Perc == "75-95"),]$FruitEnd="N"
#unique(pheno.long$FruitEnd)

## Ripe Fruiting
#unique(pheno.long$RipeF_Perc)
pheno.long$RipeFruitStart=NA
pheno.long[which(pheno.long$RipeF_Perc == "<3"| pheno.long$RipeF_Perc == "<5"),]$RipeFruitStart="N"
pheno.long[which(pheno.long$RipeF_Perc == "24-May"| pheno.long$RipeF_Perc == "2-May"| pheno.long$RipeF_Perc == "25-49"| pheno.long$RipeF_Perc == "50-74"|pheno.long$RipeF_Perc == "75-95"|pheno.long$RipeF_Perc == ">95"),]$RipeFruitStart="Y"
#unique(pheno.long$RipeFruitStart)

pheno.long$RipeFruitEnd=NA
pheno.long[which(pheno.long$RipeF_Perc == ">95"),]$RipeFruitEnd="Y"
pheno.long[which(pheno.long$RipeF_Perc == "<3"| pheno.long$RipeF_Perc == "<5"| pheno.long$RipeF_Perc == "24-May"| pheno.long$RipeF_Perc == "2-May"| 
                   pheno.long$RipeF_Perc == "25-49"| pheno.long$RipeF_Perc == "50-74"| pheno.long$RipeF_Perc == "75-95"),]$RipeFruitEnd="N"
#unique(pheno.long$RipeFruitEnd)


## Senescence
#unique(pheno.long$CC_Perc)
pheno.long$SenStart=NA
pheno.long[which(pheno.long$CC_Perc == "<5"),]$SenStart="N"
pheno.long[which(pheno.long$CC_Perc == "24-May"| pheno.long$CC_Perc == "25-49"| pheno.long$CC_Perc == "50-74"|
                   pheno.long$CC_Perc == "75-95"| pheno.long$CC_Perc == ">95"),]$SenStart="Y"
unique(pheno.long$SenStart)

pheno.long$SenEnd=NA
pheno.long[which(pheno.long$CC_Perc == ">95"),]$SenEnd="Y"
pheno.long[which(pheno.long$CC_Perc == "24-May"| pheno.long$CC_Perc == "25-49"| pheno.long$CC_Perc == "50-74"|
                   pheno.long$CC_Perc == "75-95"| pheno.long$CC_Perc == "<5"),]$SenEnd="N"
#unique(pheno.long$SenEnd)


######## Get Dates out of "Y"
indivs<-unique(pheno.long$ACC_NUM)
ldevstart=c(rep(NA,times=length(indivs)))
ldevend=c(rep(NA,times=length(indivs)))
loutstart=c(rep(NA,times=length(indivs)))
loutend=c(rep(NA,times=length(indivs)))
flostart=c(rep(NA,times=length(indivs)))
floend=c(rep(NA,times=length(indivs)))
fruitstart=c(rep(NA,times=length(indivs)))
fruitend=c(rep(NA,times=length(indivs)))
ripefruitstart=c(rep(NA,times=length(indivs)))
ripefruitend=c(rep(NA,times=length(indivs)))
senstart=c(rep(NA,times=length(indivs)))
senend=c(rep(NA,times=length(indivs)))
species=c(rep(NA,times=length(indivs)))
for(i in 1:length(indivs)){
  indivdat=pheno.long[pheno.long$ACC_NUM==indivs[i],]
  ldevstart[i]=as.character(indivdat[which(indivdat$LeafDevStart=="Y"),]$Date[1])
  ldevend[i]=as.character(indivdat[which(indivdat$LeafDevEnd=="Y"),]$Date[1])
  loutstart[i]=as.character(indivdat[which(indivdat$LeafOutStart=="Y"),]$Date[1])
  loutend[i]=as.character(indivdat[which(indivdat$LeafOutEnd=="Y"),]$Date[1])
  flostart[i]=as.character(indivdat[which(indivdat$FloStart=="Y"),]$Date[1])
  floend[i]=as.character(indivdat[which(indivdat$FloEnd=="Y"),]$Date[1])
  fruitstart[i]=as.character(indivdat[which(indivdat$FruitStart=="Y"),]$Date[1])
  fruitend[i]=as.character(indivdat[which(indivdat$FruitEnd=="Y"),]$Date[1])
  ripefruitstart[i]=as.character(indivdat[which(indivdat$RipeFruitStart=="Y"),]$Date[1])
  ripefruitend[i]=as.character(indivdat[which(indivdat$RipeFruitEnd=="Y"),]$Date[1])
  senstart[i]=as.character(indivdat[which(indivdat$SenStart=="Y"),]$Date[1])
  senend[i]=as.character(indivdat[which(indivdat$SenEnd=="Y"),]$Date[1])
  species[i]=as.character(indivdat$Species[1])  
}

alldata=cbind(as.character(indivs),species,ldevstart,ldevend,loutstart,loutend,flostart,floend,fruitstart,fruitend,ripefruitstart,ripefruitend,senstart,senend)
colnames(alldata)=c("Accession","Species","LDevStart","LDevEnd","LOutStart","LOutEnd","FloStart","FloEnd","FruitStart","FruitEnd","RipeFruitStart","RipeFruitEnd","SenStart","SenEnd")
#head(alldata)

## Convert to DOY
LDStart_DOY=strptime(alldata[,3], "%m/%d/%Y")$yday+1
LDEnd_DOY=strptime(alldata[,4], "%m/%d/%Y")$yday+1
LOutStart_DOY=strptime(alldata[,5], "%m/%d/%Y")$yday+1
LOutEnd_DOY=strptime(alldata[,6], "%m/%d/%Y")$yday+1
FloStart_DOY=strptime(alldata[,7], "%m/%d/%Y")$yday+1
FloEnd_DOY=strptime(alldata[,8], "%m/%d/%Y")$yday+1
FruStart_DOY=strptime(alldata[,9], "%m/%d/%Y")$yday+1
FruEnd_DOY=strptime(alldata[,10], "%m/%d/%Y")$yday+1
RipeFruStart_DOY=strptime(alldata[,11], "%m/%d/%Y")$yday+1
RipeFruEnd_DOY=strptime(alldata[,12], "%m/%d/%Y")$yday+1
SenStart_DOY=strptime(alldata[,13], "%m/%d/%Y")$yday+1
SenEnd_DOY=strptime(alldata[,14], "%m/%d/%Y")$yday+1

alldata2=cbind(alldata,LDStart_DOY,LDEnd_DOY,LOutStart_DOY,LOutEnd_DOY,FloStart_DOY,FloEnd_DOY,FruStart_DOY,FruEnd_DOY,RipeFruStart_DOY,RipeFruEnd_DOY,SenStart_DOY,SenEnd_DOY)
head(alldata2)
write.csv(alldata2,"analyses/output/growingseason_doy2.csv")
alldata2<-as.data.frame(alldata2)

###create CSV that just has the doy (no dates)
alldata3<-subset(alldata2, select=c(Accession, Species,LDStart_DOY,LDEnd_DOY,LOutStart_DOY,LOutEnd_DOY,FloStart_DOY,FloEnd_DOY,FruStart_DOY,FruEnd_DOY,RipeFruStart_DOY,RipeFruEnd_DOY, SenStart_DOY,SenEnd_DOY))
write.csv(alldata3,"analyses/output/gee2018.csv")
#Getting the numbers that sally had in her thesis (first paragraph in results)
min(as.numeric(alldata2$LDStart_DOY))#110
max(as.numeric(alldata2$LDStart_DOY))#142
min(as.numeric(alldata2$LOutStart_DOY))#117
max(as.numeric(alldata2$LOutStart_DOY))#147
min(as.numeric(alldata2$FloStart_DOY), na.rm=T)#111
max(as.numeric(alldata2$FloStart_DOY), na.rm=T)#252
min(as.numeric(alldata2$FruStart), na.rm=T)#132
max(as.numeric(alldata2$FruStart), na.rm=T)#315
min(as.numeric(alldata2$RipeFruStart), na.rm=T)#154
max(as.numeric(alldata2$RipeFruStart), na.rm=T)#329
min(as.numeric(alldata2$SenStart_DOY), na.rm=T)#245
max(as.numeric(alldata2$SenStart_DOY), na.rm=T)#301
