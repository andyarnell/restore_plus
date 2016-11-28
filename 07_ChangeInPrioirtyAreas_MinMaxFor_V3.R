##########################Land use change in Priority areas##########################
#note this code trys to look at min and max inpacts. Assumes that in 2010 forest and prioirty areas are 
#evenly disrtirbuted irrespectively of the other. e.g. proportion of forest in prioirty areas is 
#proportion of forest in colrow

########### 1. Set data sources (needs to be adjused by user) ##################
# Please set the inputpath to the csv files
inputpath = "C:/R/Brazil1611/Inputs"
outputpath = "C:/R/Brazil1611/Outputs"
scriptpath = "C:/R/Brazil1611"

### 2. Input data ############
setwd(inputpath)
#Data on biodiversity priority areas
#load priority areas overlap with colrows.

#If loading raw data from GIS calculate proportional overlap of Prioirty area
#gridcells (remove #'s to run)
#PriAr_Overlap<-read.csv("PriAr_Overlap.csv")
#CR_area<-aggregate(Area~Colrow,data=PriAr_Overlap,FUN=sum)
#PriAr_Overlap2<-merge(CR_area,PriAr_Overlap,by="Colrow",suffixe=c(".CR",""))
#PriAr_Overlap2$prop<-PriAr_Overlap2$Area/PriAr_Overlap2$Area.CR
#names(PriAr_Overlap2)
#PriAr_Overlap<-PriAr_Overlap2[c("Colrow","PriAr","PA","prop")]

PriAr_Overlap<-read.csv("PriAr_Overlap2.csv")


#select non protected areas priority areas 
names(PriAr_Overlap)
PriArNonPA<-PriAr_Overlap[PriAr_Overlap$PA=="N",]
names(PriArNonPA)

#select "Extremamente Alta" priority areas
PriArNonPaImp1<-PriArNonPA[PriArNonPA$PriAr=="EA",]
PriArNonPaImp<- aggregate(prop~Colrow, FUN="sum", data= PriArNonPaImp1)
max(PriArNonPaImp$prop)

#Inputting GLOBIOM data
setwd(outputpath)
Land_IDCbrazil<-read.csv("IDCbrazil.csv")
names(Land_IDCbrazil)
Land_FC<-read.csv("FC.csv")
Land_FCnoSFA<-read.csv("FCnoSFA.csv")
Land_FCnoCRA<-read.csv("FCnoCRA.csv")
Land_FCnoCRAnoSFA<-read.csv("FCnoCRAnoSFA.csv")
colrow<-read.csv("colrow.csv")
######4. Carry out calculations - IDCbrazil ##############
#join GLOBIOM output and Prioirty area overlap
PriA_ForMinMax_IDCbrazil <- merge(Land_IDCbrazil,PriArNonPaImp,by.x="ColRow30",by.y="Colrow")
PriA_ForMinMax_IDCbrazil<- merge(colrow,PriA_ForMinMax_IDCbrazil, by.x="colrow", by.y="ColRow30")
names(PriA_ForMinMax_IDCbrazil)
PriA_ForMinMax_IDCbrazil$HaPriArea <-(PriA_ForMinMax_IDCbrazil$prop*PriA_ForMinMax_IDCbrazil$CRArea)
#calculating inital forest area
PriA_ForMinMax_IDCbrazil$HaPriAPriFor2010<-PriA_ForMinMax_IDCbrazil$LU_PriFor_2010*PriA_ForMinMax_IDCbrazil$prop
#calculating change in forest area assuming even, max and min distributions
PriA_ForMinMax_IDCbrazil$PriForEven10_50<-(ifelse(PriA_ForMinMax_IDCbrazil$HaPriAPriFor2010==0,0,
  (((PriA_ForMinMax_IDCbrazil$LU_PriFor_2050-PriA_ForMinMax_IDCbrazil$LU_PriFor_2010)/
    PriA_ForMinMax_IDCbrazil$LU_PriFor_2010)*PriA_ForMinMax_IDCbrazil$HaPriAPriFor2010)))

PriA_ForMinMax_IDCbrazil$PriForMax10_50<-ifelse(PriA_ForMinMax_IDCbrazil$HaPriAPriFor2010==0,0,
  ifelse(((PriA_ForMinMax_IDCbrazil$LU_PriFor_2010-
        PriA_ForMinMax_IDCbrazil$LU_PriFor_2050)>PriA_ForMinMax_IDCbrazil$HaPriAPriFor2010),
    (-PriA_ForMinMax_IDCbrazil$HaPriAPriFor2010),
    (PriA_ForMinMax_IDCbrazil$LU_PriFor_2050-PriA_ForMinMax_IDCbrazil$LU_PriFor_2010)
    ))
PriA_ForMinMax_IDCbrazil$PriForMin10_50<-ifelse(PriA_ForMinMax_IDCbrazil$HaPriAPriFor2010==0,0,
  ifelse((PriA_ForMinMax_IDCbrazil$LU_PriFor_2010-PriA_ForMinMax_IDCbrazil$LU_PriFor_2050)>
             (PriA_ForMinMax_IDCbrazil$LU_PriFor_2010-PriA_ForMinMax_IDCbrazil$HaPriAPriFor2010),
          (PriA_ForMinMax_IDCbrazil$LU_PriFor_2050-PriA_ForMinMax_IDCbrazil$HaPriAPriFor2010),0))
#create new table of combinded database
names(PriA_ForMinMax_IDCbrazil)
setwd(outputpath)
PriAFMM_IDCbrazil<-PriA_ForMinMax_IDCbrazil[c("colrow","HaPriArea","HaPriAPriFor2010","PriForEven10_50",
                                       "PriForMax10_50","PriForMin10_50" )]
write.csv(PriAFMM_IDCbrazil, file="PriAFMM_IDCbrazilv3.csv", 
          row.names=FALSE, quote=FALSE)
###################4. Carry out calculations_FC#####
#join GLOBIOM output and Prioirty area overlap
PriA_ForMinMax_FC <- merge(Land_FC,PriArNonPaImp,by.x="ColRow30",by.y="Colrow")
PriA_ForMinMax_FC<- merge(colrow,PriA_ForMinMax_FC, by.x="colrow", by.y="ColRow30")
names(PriA_ForMinMax_FC)
PriA_ForMinMax_FC$HaPriArea <-(PriA_ForMinMax_FC$prop*PriA_ForMinMax_FC$CRArea)
#calculating inital forest area
PriA_ForMinMax_FC$HaPriAPriFor2010<-PriA_ForMinMax_FC$LU_PriFor_2010*
  PriA_ForMinMax_FC$prop
#calculating change in forest area assuming even, max and min distributions
PriA_ForMinMax_FC$PriForEven10_50<-(ifelse(PriA_ForMinMax_FC$HaPriAPriFor2010==0,0,
                                              (((PriA_ForMinMax_FC$LU_PriFor_2050-PriA_ForMinMax_FC$LU_PriFor_2010)/
                                                  PriA_ForMinMax_FC$LU_PriFor_2010)*PriA_ForMinMax_FC$HaPriAPriFor2010)))
PriA_ForMinMax_FC$PriForMax10_50<-ifelse(PriA_ForMinMax_FC$HaPriAPriFor2010==0,0,
                                            ifelse(((PriA_ForMinMax_FC$LU_PriFor_2010-
                                                       PriA_ForMinMax_FC$LU_PriFor_2050)>PriA_ForMinMax_FC$HaPriAPriFor2010),
                                                   (-PriA_ForMinMax_FC$HaPriAPriFor2010),
                                                   (PriA_ForMinMax_FC$LU_PriFor_2050-PriA_ForMinMax_FC$LU_PriFor_2010)
                                            ))
PriA_ForMinMax_FC$PriForMin10_50<-ifelse(PriA_ForMinMax_FC$HaPriAPriFor2010==0,0,
                                            ifelse((PriA_ForMinMax_FC$LU_PriFor_2010-PriA_ForMinMax_FC$LU_PriFor_2050)>
                                                     (PriA_ForMinMax_FC$LU_PriFor_2010-PriA_ForMinMax_FC$HaPriAPriFor2010),
                                                   (PriA_ForMinMax_FC$LU_PriFor_2050-PriA_ForMinMax_FC$HaPriAPriFor2010),0))
#create new table of combinded database
names(PriA_ForMinMax_FC)
setwd(outputpath)
PriAFMM_FC<-PriA_ForMinMax_FC[c("colrow","HaPriArea","HaPriAPriFor2010","PriForEven10_50",
                                      "PriForMax10_50","PriForMin10_50" )]
write.csv(PriAFMM_FC, file="PriAFMM_FCv3.csv", 
          row.names=FALSE, quote=FALSE)
                                      
###################4. Carry out calculations_FCnoSFA#####
#join GLOBIOM output and Prioirty area overlap
PriA_ForMinMax_FCnoSFA <- merge(Land_FCnoSFA,PriArNonPaImp,by.x="ColRow30",by.y="Colrow")
PriA_ForMinMax_FCnoSFA<- merge(colrow,PriA_ForMinMax_FCnoSFA, by.x="colrow", by.y="ColRow30")
names(PriA_ForMinMax_FCnoSFA)
PriA_ForMinMax_FCnoSFA$HaPriArea <-(PriA_ForMinMax_FCnoSFA$prop*PriA_ForMinMax_FCnoSFA$CRArea)
#calculating inital forest area
PriA_ForMinMax_FCnoSFA$HaPriAPriFor2010<-PriA_ForMinMax_FCnoSFA$LU_PriFor_2010*
  PriA_ForMinMax_FCnoSFA$prop
#calculating change in forest area assuming even, max and min distributions
PriA_ForMinMax_FCnoSFA$PriForEven10_50<-(ifelse(PriA_ForMinMax_FCnoSFA$HaPriAPriFor2010==0,0,
                                           (((PriA_ForMinMax_FCnoSFA$LU_PriFor_2050-PriA_ForMinMax_FCnoSFA$LU_PriFor_2010)/
                                               PriA_ForMinMax_FCnoSFA$LU_PriFor_2010)*PriA_ForMinMax_FCnoSFA$HaPriAPriFor2010)))
PriA_ForMinMax_FCnoSFA$PriForMax10_50<-ifelse(PriA_ForMinMax_FCnoSFA$HaPriAPriFor2010==0,0,
                                         ifelse(((PriA_ForMinMax_FCnoSFA$LU_PriFor_2010-
                                                    PriA_ForMinMax_FCnoSFA$LU_PriFor_2050)>PriA_ForMinMax_FCnoSFA$HaPriAPriFor2010),
                                                (-PriA_ForMinMax_FCnoSFA$HaPriAPriFor2010),
                                                (PriA_ForMinMax_FCnoSFA$LU_PriFor_2050-PriA_ForMinMax_FCnoSFA$LU_PriFor_2010)
                                         ))
PriA_ForMinMax_FCnoSFA$PriForMin10_50<-ifelse(PriA_ForMinMax_FCnoSFA$HaPriAPriFor2010==0,0,
                                         ifelse((PriA_ForMinMax_FCnoSFA$LU_PriFor_2010-PriA_ForMinMax_FCnoSFA$LU_PriFor_2050)>
                                                  (PriA_ForMinMax_FCnoSFA$LU_PriFor_2010-PriA_ForMinMax_FCnoSFA$HaPriAPriFor2010),
                                                (PriA_ForMinMax_FCnoSFA$LU_PriFor_2050-PriA_ForMinMax_FCnoSFA$HaPriAPriFor2010),0))
#create new table of combinded database
names(PriA_ForMinMax_FCnoSFA)
setwd(outputpath)
PriAFMM_FCnoSFA<-PriA_ForMinMax_FCnoSFA[c("colrow","HaPriArea","HaPriAPriFor2010","PriForEven10_50",
                                "PriForMax10_50","PriForMin10_50" )]
write.csv(PriAFMM_FCnoSFA, file="PriAFMM_FCnoSFAv3.csv", 
          row.names=FALSE, quote=FALSE)
###################4. Carry out calculations_FCnoCRA#####
#join GLOBIOM output and Prioirty area overlap
PriA_ForMinMax_FCnoCRA <- merge(Land_FCnoCRA,PriArNonPaImp,by.x="ColRow30",by.y="Colrow")
PriA_ForMinMax_FCnoCRA<- merge(colrow,PriA_ForMinMax_FCnoCRA, by.x="colrow", by.y="ColRow30")
names(PriA_ForMinMax_FCnoCRA)
PriA_ForMinMax_FCnoCRA$HaPriArea <-(PriA_ForMinMax_FCnoCRA$prop*PriA_ForMinMax_FCnoCRA$CRArea)
#calculating inital forest area
PriA_ForMinMax_FCnoCRA$HaPriAPriFor2010<-PriA_ForMinMax_FCnoCRA$LU_PriFor_2010*
  PriA_ForMinMax_FCnoCRA$prop
#calculating change in forest area assuming even, max and min distributions
PriA_ForMinMax_FCnoCRA$PriForEven10_50<-(ifelse(PriA_ForMinMax_FCnoCRA$HaPriAPriFor2010==0,0,
                                               (((PriA_ForMinMax_FCnoCRA$LU_PriFor_2050-PriA_ForMinMax_FCnoCRA$LU_PriFor_2010)/
                                                   PriA_ForMinMax_FCnoCRA$LU_PriFor_2010)*PriA_ForMinMax_FCnoCRA$HaPriAPriFor2010)))
PriA_ForMinMax_FCnoCRA$PriForMax10_50<-ifelse(PriA_ForMinMax_FCnoCRA$HaPriAPriFor2010==0,0,
                                             ifelse(((PriA_ForMinMax_FCnoCRA$LU_PriFor_2010-
                                                        PriA_ForMinMax_FCnoCRA$LU_PriFor_2050)>PriA_ForMinMax_FCnoCRA$HaPriAPriFor2010),
                                                    (-PriA_ForMinMax_FCnoCRA$HaPriAPriFor2010),
                                                    (PriA_ForMinMax_FCnoCRA$LU_PriFor_2050-PriA_ForMinMax_FCnoCRA$LU_PriFor_2010)
                                             ))
PriA_ForMinMax_FCnoCRA$PriForMin10_50<-ifelse(PriA_ForMinMax_FCnoCRA$HaPriAPriFor2010==0,0,
                                             ifelse((PriA_ForMinMax_FCnoCRA$LU_PriFor_2010-PriA_ForMinMax_FCnoCRA$LU_PriFor_2050)>
                                                      (PriA_ForMinMax_FCnoCRA$LU_PriFor_2010-PriA_ForMinMax_FCnoCRA$HaPriAPriFor2010),
                                                    (PriA_ForMinMax_FCnoCRA$LU_PriFor_2050-PriA_ForMinMax_FCnoCRA$HaPriAPriFor2010),0))
#create new table of combinded database
names(PriA_ForMinMax_FCnoCRA)
setwd(outputpath)
PriAFMM_FCnoCRA<-PriA_ForMinMax_FCnoCRA[c("colrow","HaPriArea","HaPriAPriFor2010","PriForEven10_50",
                                        "PriForMax10_50","PriForMin10_50" )]
write.csv(PriAFMM_FCnoCRA, file="PriAFMM_FCnoCRAv3.csv", 
          row.names=FALSE, quote=FALSE)
###################4. Carry out calculations_FCnoCRAnoSFA#####
#join GLOBIOM output and Prioirty area overlap
PriA_ForMinMax_FCnoCRAnoSFA <- merge(Land_FCnoCRAnoSFA,PriArNonPaImp,by.x="ColRow30",by.y="Colrow")
PriA_ForMinMax_FCnoCRAnoSFA<- merge(colrow,PriA_ForMinMax_FCnoCRAnoSFA, by.x="colrow", by.y="ColRow30")
names(PriA_ForMinMax_FCnoCRAnoSFA)
PriA_ForMinMax_FCnoCRAnoSFA$HaPriArea <-(PriA_ForMinMax_FCnoCRAnoSFA$prop*PriA_ForMinMax_FCnoCRAnoSFA$CRArea)
#calculating inital forest area
PriA_ForMinMax_FCnoCRAnoSFA$HaPriAPriFor2010<-PriA_ForMinMax_FCnoCRAnoSFA$LU_PriFor_2010*
  PriA_ForMinMax_FCnoCRAnoSFA$prop
#calculating change in forest area assuming even, max and min distributions
PriA_ForMinMax_FCnoCRAnoSFA$PriForEven10_50<-(ifelse(PriA_ForMinMax_FCnoCRAnoSFA$HaPriAPriFor2010==0,0,
                                               (((PriA_ForMinMax_FCnoCRAnoSFA$LU_PriFor_2050-PriA_ForMinMax_FCnoCRAnoSFA$LU_PriFor_2010)/
                                                   PriA_ForMinMax_FCnoCRAnoSFA$LU_PriFor_2010)*PriA_ForMinMax_FCnoCRAnoSFA$HaPriAPriFor2010)))
PriA_ForMinMax_FCnoCRAnoSFA$PriForMax10_50<-ifelse(PriA_ForMinMax_FCnoCRAnoSFA$HaPriAPriFor2010==0,0,
                                             ifelse(((PriA_ForMinMax_FCnoCRAnoSFA$LU_PriFor_2010-
                                                        PriA_ForMinMax_FCnoCRAnoSFA$LU_PriFor_2050)>PriA_ForMinMax_FCnoCRAnoSFA$HaPriAPriFor2010),
                                                    (-PriA_ForMinMax_FCnoCRAnoSFA$HaPriAPriFor2010),
                                                    (PriA_ForMinMax_FCnoCRAnoSFA$LU_PriFor_2050-PriA_ForMinMax_FCnoCRAnoSFA$LU_PriFor_2010)
                                             ))
PriA_ForMinMax_FCnoCRAnoSFA$PriForMin10_50<-ifelse(PriA_ForMinMax_FCnoCRAnoSFA$HaPriAPriFor2010==0,0,
                                             ifelse((PriA_ForMinMax_FCnoCRAnoSFA$LU_PriFor_2010-PriA_ForMinMax_FCnoCRAnoSFA$LU_PriFor_2050)>
                                                      (PriA_ForMinMax_FCnoCRAnoSFA$LU_PriFor_2010-PriA_ForMinMax_FCnoCRAnoSFA$HaPriAPriFor2010),
                                                    (PriA_ForMinMax_FCnoCRAnoSFA$LU_PriFor_2050-PriA_ForMinMax_FCnoCRAnoSFA$HaPriAPriFor2010),0))
#create new table of combinded database
names(PriA_ForMinMax_FCnoCRAnoSFA)
setwd(outputpath)
PriAFMM_FCnoCRAnoSFA<-PriA_ForMinMax_FCnoCRAnoSFA[c("colrow","HaPriArea","HaPriAPriFor2010","PriForEven10_50",
                                        "PriForMax10_50","PriForMin10_50" )]
write.csv(PriAFMM_FCnoCRAnoSFA, file="PriAFMM_FCnoCRAnoSFAv3.csv", 
          row.names=FALSE, quote=FALSE)
#######summary####
names(PriAFMM_IDCbrazil)
PriAFMM__all<-merge(PriAFMM_IDCbrazil,PriAFMM_FC,by="colrow",
                       suffixes=c(".IDCbrazil",".FC"))
PriAFMM__all<-merge(PriAFMM__all,PriAFMM_FCnoSFA,by="colrow")
PriAFMM__all<-merge(PriAFMM__all,PriAFMM_FCnoCRA,by="colrow",
                       suffixes=c(".FCnoSFA",".FCnoCRA"))
names(PriAFMM_FCnoCRAnoSFA)<-paste(names(PriAFMM_FCnoCRAnoSFA),".FCnoCRAnoSFA",sep="")
PriAFMM__all<-merge(PriAFMM__all,PriAFMM_FCnoCRAnoSFA,by.x="colrow",by.y="colrow.FCnoCRAnoSFA")
names(PriAFMM__all)
setwd(inputpath)
biomes<-read.csv("Biomes.csv")
names(biomes)
PriAFMM__all<-merge(PriAFMM__all,biomes,by.x="colrow",by.y="Colrow")
names(PriAFMM__all)

PriAForMinMax_allB<- aggregate(
  cbind(HaPriArea.IDCbrazil,HaPriAPriFor2010.IDCbrazil,PriForEven10_50.IDCbrazil,PriForMax10_50.IDCbrazil,PriForMin10_50.IDCbrazil,
        PriForEven10_50.FC,PriForMax10_50.FC,PriForMin10_50.FC,PriForEven10_50.FCnoSFA,
        PriForMax10_50.FCnoSFA,PriForMin10_50.FCnoSFA,PriForEven10_50.FCnoCRA,PriForMax10_50.FCnoCRA,
        PriForMin10_50.FCnoCRA,PriForEven10_50.FCnoCRAnoSFA,PriForMax10_50.FCnoCRAnoSFA,
        PriForMin10_50.FCnoCRAnoSFA)~Biome, data=PriAFMM__all,FUN="sum")
View(PriAForMinMax_allB)
setwd(outputpath)
write.csv(PriAForMinMax_allB, file="PriAForMinMax_allBv3.csv", 
          row.names=FALSE, quote=FALSE)
write.csv(PriAFMM__all, file="PriAForMinMax_allv3.csv")
names(PriAFMM__all)          