##########################Land use change in Priority areas##########################
#note this code assumes even distribution of land uses and land use change
#with this assumption in calculates the amount of land use change within biodiversity priority areas

### 1. Set data sources (needs to be adjused by user) ##################
# Please set the inputpath to the csv files
inputpath = "C:/R/Brazil1611/Inputs"
outputpath = "C:/R/Brazil1611/Outputs"
scriptpath = "C:/R/Brazil1611"

### 2. Input data ############
setwd(inputpath)
#Data on biodiversity priority areas####
#load priority areas overlap with colrows.

#if loading raw data from GIS calculate proportional overlap of Prioirty area
#gridcells (remove #'s to run)
#PriAr_Overlap<-read.csv("PriAr_Overlap.csv")
#View(PriAr_Overlap)
#CR_area<-aggregate(Area~Colrow,data=PriAr_Overlap,FUN=sum)
#PriAr_Overlap2<-merge(CR_area,PriAr_Overlap,
#                      by="Colrow",suffixe=c(".CR",".polygon"))
#mean(PriAr_Overlap2$Area.CR)
#PriAr_Overlap2$prop<-PriAr_Overlap2$Area.polygon/PriAr_Overlap2$Area.CR
#names(PriAr_Overlap2)
#PriAr_Overlap<-PriAr_Overlap2[c("Colrow","PriAr","PA","prop")]#

PriAr_Overlap<-read.csv("PriAr_Overlap2.csv")
View(PriAr_Overlap[PriAr_Overlap$Colrow=="CR267202",])

#select non protected areas priority areas 
names(PriAr_Overlap)
PriArNonPA<-PriAr_Overlap[PriAr_Overlap$PA=="N",]
names(PriArNonPA)

#select "Extremamente Alta" priority areas
PriArNonPaImp1<-PriArNonPA[PriArNonPA$PriAr=="EA",]
PriArNonPaImp<- aggregate(prop~Colrow, FUN="sum", data= PriArNonPaImp1)
max(PriArNonPaImp$prop)
#Inputting GLOBIOM data####
setwd(outputpath)
Land_IDCbrazil<-read.csv("IDCbrazil.csv")
names(Land_IDCbrazil)
Land_IDCno<-read.csv("IDCno.csv")
Land_IDCamazon<-read.csv("IDCamazon.csv")
Land_FC<-read.csv("FC.csv")
Land_FCnoSFA<-read.csv("FCnoSFA.csv")
Land_FCnoCRAnoSFA<-read.csv("FCnoCRAnoSFA.csv")
Land_FCnoCRA<-read.csv("FCnoCRA.csv")

### 3. Carry out calculations_IDCno_LC3##############
#join GLOBIOM output, Prioirty area overlap and Colrow area
colrow<-read.csv("colrow.csv")
Result_PriA_IDCno<- merge(Land_IDCno,PriArNonPaImp,by.x="ColRow30",by.y="Colrow")
Result_PriA_IDCno <- merge(colrow,Result_PriA_IDCno, by.x="colrow", by.y="ColRow30")
names(Result_PriA_IDCno)

#####make intermediate variables needed in analysis 
Result_PriA_IDCno$HaPriArea <-(Result_PriA_IDCno$prop*Result_PriA_IDCno$CRArea)
names(Result_PriA_IDCno)
Result_PriA_IDCno$ChangePriFor<-((Result_PriA_IDCno$LU_PriFor_2050-
                                        Result_PriA_IDCno$LU_PriFor_2010)*Result_PriA_IDCno$prop)
Result_PriA_IDCno$ChangeNat<-((Result_PriA_IDCno$LU_NatLnd_2050-
                                     Result_PriA_IDCno$LU_NatLnd_2010)*Result_PriA_IDCno$prop)
Result_PriA_IDCno$ChangeCrp<-((Result_PriA_IDCno$LU_CrpLnd_2050-
                                     Result_PriA_IDCno$LU_CrpLnd_2010)*Result_PriA_IDCno$prop)
Result_PriA_IDCno$ChangeGrs<-((Result_PriA_IDCno$LU_GrsLnd_2050-
                                     Result_PriA_IDCno$LU_GrsLnd_2010)*Result_PriA_IDCno$prop)
Result_PriA_IDCno$ChangePlt<-((Result_PriA_IDCno$LU_PltFor_2050-
                                     Result_PriA_IDCno$LU_PltFor_2010)*Result_PriA_IDCno$prop)
Result_PriA_IDCno$ChangeForReg<-0
names(Result_PriA_IDCno)
Result_PriA_IDCnoResults<-Result_PriA_IDCno[c("colrow","prop","HaPriArea",
                                                      "ChangePriFor","ChangeNat",
                                                      "ChangeForReg","ChangePlt", "ChangeGrs",
                                                      "ChangeCrp")]

#create new table of combinded database
setwd(outputpath)
PriAResults_IDCno<- Result_PriA_IDCno [c("colrow","ChangePriFor","ChangeNat", 
                                                 "ChangeForReg","ChangePlt", "ChangeGrs",
                                                 "ChangeCrp")]
write.csv(PriAResults_IDCno, file="PriAResults_IDCno.csv", 
          row.names=FALSE, quote=FALSE) 
### 3. Carry out calculations_IDCamazon_LC3##############
#join GLOBIOM output, Prioirty area overlap and Colrow area
colrow<-read.csv("colrow.csv")
Result_PriA_IDCamazon<- merge(Land_IDCamazon,PriArNonPaImp,by.x="ColRow30",by.y="Colrow")
Result_PriA_IDCamazon <- merge(colrow,Result_PriA_IDCamazon, by.x="colrow", by.y="ColRow30")
names(Result_PriA_IDCamazon)

#####make intermediate variables needed in analysis 
Result_PriA_IDCamazon$HaPriArea <-(Result_PriA_IDCamazon$prop*Result_PriA_IDCamazon$CRArea)
names(Result_PriA_IDCamazon)
Result_PriA_IDCamazon$ChangePriFor<-((Result_PriA_IDCamazon$LU_PriFor_2050-
                                        Result_PriA_IDCamazon$LU_PriFor_2010)*Result_PriA_IDCamazon$prop)
Result_PriA_IDCamazon$ChangeNat<-((Result_PriA_IDCamazon$LU_NatLnd_2050-
                                     Result_PriA_IDCamazon$LU_NatLnd_2010)*Result_PriA_IDCamazon$prop)
Result_PriA_IDCamazon$ChangeCrp<-((Result_PriA_IDCamazon$LU_CrpLnd_2050-
                                     Result_PriA_IDCamazon$LU_CrpLnd_2010)*Result_PriA_IDCamazon$prop)
Result_PriA_IDCamazon$ChangeGrs<-((Result_PriA_IDCamazon$LU_GrsLnd_2050-
                                     Result_PriA_IDCamazon$LU_GrsLnd_2010)*Result_PriA_IDCamazon$prop)
Result_PriA_IDCamazon$ChangePlt<-((Result_PriA_IDCamazon$LU_PltFor_2050-
                                     Result_PriA_IDCamazon$LU_PltFor_2010)*Result_PriA_IDCamazon$prop)
Result_PriA_IDCamazon$ChangeForReg<-0
names(Result_PriA_IDCamazon)
Result_PriA_IDCamazonResults<-Result_PriA_IDCamazon[c("colrow","prop","HaPriArea",
                                                      "ChangePriFor","ChangeNat",
                                                      "ChangeForReg","ChangePlt", "ChangeGrs",
                                                      "ChangeCrp")]

#create new table of combinded database
setwd(outputpath)
PriAResults_IDCamazon<- Result_PriA_IDCamazon [c("colrow","ChangePriFor","ChangeNat", 
                                                 "ChangeForReg","ChangePlt", "ChangeGrs",
                                                 "ChangeCrp")]
write.csv(PriAResults_IDCamazon, file="PriAResults_IDCamazon.csv", 
          row.names=FALSE, quote=FALSE) 
### 3. Carry out calculations_IDCbrazil_LC3##############
#join GLOBIOM output, Prioirty area overlap and Colrow area
colrow<-read.csv("colrow.csv")
Result_PriA_IDCbrazil<- merge(Land_IDCbrazil,PriArNonPaImp,by.x="ColRow30",by.y="Colrow")
Result_PriA_IDCbrazil <- merge(colrow,Result_PriA_IDCbrazil, by.x="colrow", by.y="ColRow30")
names(Result_PriA_IDCbrazil)

#####make intermediate variables needed in analysis 
Result_PriA_IDCbrazil$HaPriArea <-(Result_PriA_IDCbrazil$prop*Result_PriA_IDCbrazil$CRArea)
names(Result_PriA_IDCbrazil)
Result_PriA_IDCbrazil$ChangePriFor<-((Result_PriA_IDCbrazil$LU_PriFor_2050-
                                        Result_PriA_IDCbrazil$LU_PriFor_2010)*Result_PriA_IDCbrazil$prop)
Result_PriA_IDCbrazil$ChangeNat<-((Result_PriA_IDCbrazil$LU_NatLnd_2050-
                                    Result_PriA_IDCbrazil$LU_NatLnd_2010)*Result_PriA_IDCbrazil$prop)
Result_PriA_IDCbrazil$ChangeCrp<-((Result_PriA_IDCbrazil$LU_CrpLnd_2050-
                                   Result_PriA_IDCbrazil$LU_CrpLnd_2010)*Result_PriA_IDCbrazil$prop)
Result_PriA_IDCbrazil$ChangeGrs<-((Result_PriA_IDCbrazil$LU_GrsLnd_2050-
                                   Result_PriA_IDCbrazil$LU_GrsLnd_2010)*Result_PriA_IDCbrazil$prop)
Result_PriA_IDCbrazil$ChangePlt<-((Result_PriA_IDCbrazil$LU_PltFor_2050-
                                   Result_PriA_IDCbrazil$LU_PltFor_2010)*Result_PriA_IDCbrazil$prop)
Result_PriA_IDCbrazil$ChangeForReg<-0
names(Result_PriA_IDCbrazil)
Result_PriA_IDCbrazilResults<-Result_PriA_IDCbrazil[c("colrow","prop","HaPriArea",
                                              "ChangePriFor","ChangeNat",
                                              "ChangeForReg","ChangePlt", "ChangeGrs",
                                          "ChangeCrp")]

#create new table of combinded database
setwd(outputpath)
PriAResults_IDCbrazil<- Result_PriA_IDCbrazil [c("colrow","ChangePriFor","ChangeNat", 
                                         "ChangeForReg","ChangePlt", "ChangeGrs",
                                     "ChangeCrp")]
write.csv(PriAResults_IDCbrazil, file="PriAResults_IDCbrazil.csv", 
          row.names=FALSE, quote=FALSE) 
### 4. Carry out calculations_FC_LC3##############
names(Land_FC)

Result_PriA_FC<- merge(Land_FC,PriArNonPaImp,by.x="ColRow30",by.y="Colrow")
Result_PriA_FC <- merge(colrow,Result_PriA_FC, by.x="colrow", by.y="ColRow30")
names(Result_PriA_FC)

#####make intermediate variables needed in analysis 
Result_PriA_FC$HaPriArea <-(Result_PriA_FC$prop*Result_PriA_FC$CRArea)
names(Result_PriA_FC)
Result_PriA_FC$ChangePriFor<-((Result_PriA_FC$LU_PriFor_2050-
                                        Result_PriA_FC$LU_PriFor_2010)*Result_PriA_FC$prop)
Result_PriA_FC$ChangeNat<-((Result_PriA_FC$LU_NatLnd_2050-
                                     Result_PriA_FC$LU_NatLnd_2010)*Result_PriA_FC$prop)
Result_PriA_FC$ChangeForReg<-((Result_PriA_FC$LU_ForReg_2050)*Result_PriA_FC$prop)
Result_PriA_FC$ChangeGrs<-((Result_PriA_FC$LU_GrsLnd_2050-
                                  Result_PriA_FC$LU_GrsLnd_2010)*Result_PriA_FC$prop)
Result_PriA_FC$ChangeCrp<-((Result_PriA_FC$LU_CrpLnd_2050-
                                  Result_PriA_FC$LU_CrpLnd_2010)*Result_PriA_FC$prop)
Result_PriA_FC$ChangePlt<-((Result_PriA_FC$LU_PltFor_2050-
                                  Result_PriA_FC$LU_PltFor_2010)*Result_PriA_FC$prop)
Result_PriA_FCResults<-Result_PriA_FC[c("colrow","prop","HaPriArea",
                                              "HaPriArea","ChangePriFor","ChangeNat",
                                              "ChangeForReg","ChangePlt", "ChangeGrs",
                                        "ChangeCrp" )]

#create new table of combinded database
setwd(outputpath)
PriAResults_FC<- Result_PriA_FC [c("colrow","ChangePriFor","ChangeNat", 
                                         "ChangeForReg","ChangePlt", "ChangeGrs",
                                   "ChangeCrp")]
write.csv(PriAResults_FC, file="PriAResults_FC.csv", 
          row.names=FALSE, quote=FALSE) 
### 4. Carry out calculations_FCnoSFA_LC3##############
names(Land_FCnoSFA)

Result_PriA_FCnoSFA<- merge(Land_FCnoSFA,PriArNonPaImp,by.x="ColRow30",by.y="Colrow")
Result_PriA_FCnoSFA <- merge(colrow,Result_PriA_FCnoSFA, by.x="colrow", by.y="ColRow30")
names(Result_PriA_FCnoSFA)

#####make intermediate variables needed in analysis 
Result_PriA_FCnoSFA$HaPriArea <-(Result_PriA_FCnoSFA$prop*Result_PriA_FCnoSFA$CRArea)
names(Result_PriA_FCnoSFA)
Result_PriA_FCnoSFA$ChangePriFor<-((Result_PriA_FCnoSFA$LU_PriFor_2050-
                                     Result_PriA_FCnoSFA$LU_PriFor_2010)*Result_PriA_FCnoSFA$prop)
Result_PriA_FCnoSFA$ChangeNat<-((Result_PriA_FCnoSFA$LU_NatLnd_2050-
                                  Result_PriA_FCnoSFA$LU_NatLnd_2010)*Result_PriA_FCnoSFA$prop)
Result_PriA_FCnoSFA$ChangeForReg<-((Result_PriA_FCnoSFA$LU_ForReg_2050)*Result_PriA_FCnoSFA$prop)
Result_PriA_FCnoSFA$ChangeGrs<-((Result_PriA_FCnoSFA$LU_GrsLnd_2050-
                                  Result_PriA_FCnoSFA$LU_GrsLnd_2010)*Result_PriA_FCnoSFA$prop)
Result_PriA_FCnoSFA$ChangeCrp<-((Result_PriA_FCnoSFA$LU_CrpLnd_2050-
                                  Result_PriA_FCnoSFA$LU_CrpLnd_2010)*Result_PriA_FCnoSFA$prop)
Result_PriA_FCnoSFA$ChangePlt<-((Result_PriA_FCnoSFA$LU_PltFor_2050-
                                  Result_PriA_FCnoSFA$LU_PltFor_2010)*Result_PriA_FCnoSFA$prop)
Result_PriA_FCnoSFAResults<-Result_PriA_FCnoSFA[c("colrow","prop","HaPriArea",
                                        "HaPriArea","ChangePriFor","ChangeNat",
                                        "ChangeForReg","ChangePlt", "ChangeGrs",
                                        "ChangeCrp" )]

#create new table of combinded database
setwd(outputpath)
PriAResults_FCnoSFA<- Result_PriA_FCnoSFA [c("colrow","ChangePriFor","ChangeNat", 
                                   "ChangeForReg","ChangePlt", "ChangeGrs",
                                   "ChangeCrp")]
write.csv(PriAResults_FCnoSFA, file="PriAResults_FCnoSFA.csv", 
          row.names=FALSE, quote=FALSE) 
### 4. Carry out calculations_FCnoCRAnoSFA_LC3##############
names(Land_FCnoCRAnoSFA)

Result_PriA_FCnoCRAnoSFA<- merge(Land_FCnoCRAnoSFA,PriArNonPaImp,by.x="ColRow30",by.y="Colrow")
Result_PriA_FCnoCRAnoSFA <- merge(colrow,Result_PriA_FCnoCRAnoSFA, by.x="colrow", by.y="ColRow30")
names(Result_PriA_FCnoCRAnoSFA)

#####make intermediate variables needed in analysis 
Result_PriA_FCnoCRAnoSFA$HaPriArea <-(Result_PriA_FCnoCRAnoSFA$prop*Result_PriA_FCnoCRAnoSFA$CRArea)
names(Result_PriA_FCnoCRAnoSFA)
Result_PriA_FCnoCRAnoSFA$ChangePriFor<-((Result_PriA_FCnoCRAnoSFA$LU_PriFor_2050-
                                         Result_PriA_FCnoCRAnoSFA$LU_PriFor_2010)*Result_PriA_FCnoCRAnoSFA$prop)
Result_PriA_FCnoCRAnoSFA$ChangeNat<-((Result_PriA_FCnoCRAnoSFA$LU_NatLnd_2050-
                                      Result_PriA_FCnoCRAnoSFA$LU_NatLnd_2010)*Result_PriA_FCnoCRAnoSFA$prop)
Result_PriA_FCnoCRAnoSFA$ChangeForReg<-((Result_PriA_FCnoCRAnoSFA$LU_ForReg_2050)*Result_PriA_FCnoCRAnoSFA$prop)
Result_PriA_FCnoCRAnoSFA$ChangeGrs<-((Result_PriA_FCnoCRAnoSFA$LU_GrsLnd_2050-
                                      Result_PriA_FCnoCRAnoSFA$LU_GrsLnd_2010)*Result_PriA_FCnoCRAnoSFA$prop)
Result_PriA_FCnoCRAnoSFA$ChangeCrp<-((Result_PriA_FCnoCRAnoSFA$LU_CrpLnd_2050-
                                      Result_PriA_FCnoCRAnoSFA$LU_CrpLnd_2010)*Result_PriA_FCnoCRAnoSFA$prop)
Result_PriA_FCnoCRAnoSFA$ChangePlt<-((Result_PriA_FCnoCRAnoSFA$LU_PltFor_2050-
                                      Result_PriA_FCnoCRAnoSFA$LU_PltFor_2010)*Result_PriA_FCnoCRAnoSFA$prop)
Result_PriA_FCnoCRAnoSFAResults<-Result_PriA_FCnoCRAnoSFA[c("colrow","prop","HaPriArea",
                                                "HaPriArea","ChangePriFor","ChangeNat",
                                                "ChangeForReg","ChangePlt", "ChangeGrs",
                                                "ChangeCrp" )]

#create new table of combinded database
setwd(outputpath)
PriAResults_FCnoCRAnoSFA<- Result_PriA_FCnoCRAnoSFA [c("colrow","ChangePriFor","ChangeNat", 
                                           "ChangeForReg","ChangePlt", "ChangeGrs",
                                           "ChangeCrp")]
write.csv(PriAResults_FCnoCRAnoSFA, file="PriAResults_FCnoCRAnoSFA.csv", 
          row.names=FALSE, quote=FALSE) 
### 4. Carry out calculations_FCnoCRA_LC3##############
names(Land_FCnoCRA)

Result_PriA_FCnoCRA<- merge(Land_FCnoCRA,PriArNonPaImp,by.x="ColRow30",by.y="Colrow")
Result_PriA_FCnoCRA <- merge(colrow,Result_PriA_FCnoCRA, by.x="colrow", by.y="ColRow30")
names(Result_PriA_FCnoCRA)

#####make intermediate variables needed in analysis 
Result_PriA_FCnoCRA$HaPriArea <-(Result_PriA_FCnoCRA$prop*Result_PriA_FCnoCRA$CRArea)
Result_PriA_FCnoCRA$PriFor2010<-Result_PriA_FCnoCRA$LU_PriFor_2010*Result_PriA_FCnoCRA$prop
Result_PriA_FCnoCRA$Nat2010<-Result_PriA_FCnoCRA$LU_NatLnd_2010*Result_PriA_FCnoCRA$prop
names(Result_PriA_FCnoCRA)
Result_PriA_FCnoCRA$ChangePriFor.FCnoCRA<-((Result_PriA_FCnoCRA$LU_PriFor_2050-
                                         Result_PriA_FCnoCRA$LU_PriFor_2010)*Result_PriA_FCnoCRA$prop)
Result_PriA_FCnoCRA$ChangeNat.FCnoCRA<-((Result_PriA_FCnoCRA$LU_NatLnd_2050-
                                      Result_PriA_FCnoCRA$LU_NatLnd_2010)*Result_PriA_FCnoCRA$prop)
Result_PriA_FCnoCRA$ChangeForReg.FCnoCRA<-((Result_PriA_FCnoCRA$LU_ForReg_2050)*Result_PriA_FCnoCRA$prop)
Result_PriA_FCnoCRA$ChangeGrs.FCnoCRA<-((Result_PriA_FCnoCRA$LU_GrsLnd_2050-
                                      Result_PriA_FCnoCRA$LU_GrsLnd_2010)*Result_PriA_FCnoCRA$prop)
Result_PriA_FCnoCRA$ChangeCrp.FCnoCRA<-((Result_PriA_FCnoCRA$LU_CrpLnd_2050-
                                      Result_PriA_FCnoCRA$LU_CrpLnd_2010)*Result_PriA_FCnoCRA$prop)
Result_PriA_FCnoCRA$ChangePlt.FCnoCRA<-((Result_PriA_FCnoCRA$LU_PltFor_2050-
                                      Result_PriA_FCnoCRA$LU_PltFor_2010)*Result_PriA_FCnoCRA$prop)
Result_PriA_FCnoCRAResults<-Result_PriA_FCnoCRA[c("colrow","prop","HaPriArea",
                                                "ChangePriFor.FCnoCRA","ChangeNat.FCnoCRA",
                                                "ChangeForReg.FCnoCRA","ChangePlt.FCnoCRA", "ChangeGrs.FCnoCRA",
                                                "ChangeCrp.FCnoCRA" )]

#create new table of combinded database
setwd(outputpath)
PriAResults_FCnoCRA<- Result_PriA_FCnoCRA [c("colrow","ChangePriFor.FCnoCRA","ChangeNat.FCnoCRA", 
                                           "ChangeForReg.FCnoCRA","ChangePlt.FCnoCRA", "ChangeGrs.FCnoCRA",
                                           "ChangeCrp.FCnoCRA","HaPriArea","PriFor2010","Nat2010")]
write.csv(PriAResults_FCnoCRA, file="PriAResults_FCnoCRA.csv", 
          row.names=FALSE, quote=FALSE) 

### 5. Creating summary tables####
PriAResults_all<-merge(PriAResults_IDCno,PriAResults_IDCbrazil,by="colrow",
                       suffixes = c(".IDCno",".IDCbrazil"))
PriAResults_all<-merge(PriAResults_all,PriAResults_IDCamazon,by="colrow")
PriAResults_all<-merge(PriAResults_all,PriAResults_FC,by="colrow",
                       suffixes = c(".IDCamazon",".FC"))
PriAResults_all<-merge(PriAResults_all,PriAResults_FCnoSFA,by="colrow")
PriAResults_all<-merge(PriAResults_all,PriAResults_FCnoCRAnoSFA,by="colrow",
                       suffixes = c(".FCnoSFA",".FCnoCRAnoSFA"))
PriAResults_all<-merge(PriAResults_all,PriAResults_FCnoCRA,by="colrow")
names(PriAResults_all)
setwd(inputpath)
biomes<-read.csv("Biomes.csv")
names(biomes)
PriAResults_all<-merge(PriAResults_all,biomes,by.x="colrow",by.y="Colrow")
names(PriAResults_all)
View(PriAResults_all)
PriAResults_allB<- aggregate(
  cbind(ChangePriFor.IDCbrazil,ChangeNat.IDCbrazil,ChangeForReg.IDCbrazil,ChangePlt.IDCbrazil,
        ChangeGrs.IDCbrazil,ChangeCrp.IDCbrazil,ChangePriFor.IDCamazon,ChangeNat.IDCamazon,
        ChangeForReg.IDCamazon,ChangePlt.IDCamazon,ChangeGrs.IDCamazon,ChangeCrp.IDCamazon,
        ChangePriFor.IDCno,ChangeNat.IDCno,ChangeForReg.IDCno,ChangePlt.IDCno,ChangeGrs.IDCno,
        ChangeCrp.IDCno,
        ChangePriFor.FC,ChangeNat.FC,ChangeForReg.FC,ChangePlt.FC,ChangeGrs.FC,ChangeCrp.FC,
        ChangePriFor.FCnoSFA,ChangeNat.FCnoSFA,ChangeForReg.FCnoSFA,ChangePlt.FCnoSFA,ChangeGrs.FCnoSFA,
        ChangeCrp.FCnoSFA,ChangePriFor.FCnoCRAnoSFA,ChangeNat.FCnoCRAnoSFA,ChangeForReg.FCnoCRAnoSFA,ChangePlt.FCnoCRAnoSFA,
        ChangeGrs.FCnoCRAnoSFA,ChangeCrp.FCnoCRAnoSFA,ChangePriFor.FCnoCRA,ChangeNat.FCnoCRA,ChangeForReg.FCnoCRA,
        ChangePlt.FCnoCRA,ChangeGrs.FCnoCRA,ChangeCrp.FCnoCRA,HaPriArea,PriFor2010,Nat2010)~Biome, 
  data=PriAResults_all, FUN="sum")
View(PriAResults_allB)
setwd(outputpath)
write.csv(PriAResults_allB, file="PriAResults_allB.csv", 
          row.names=FALSE, quote=FALSE)
PriAResults_allB<-read.csv("PriAResults_allB.csv")

###################### Fig for graph####
setwd(outputpath)
PriAResults_all<-read.csv(file="PriAResults_all.csv")
sum(PriAResults_all$ChangePriFor.IDCbrazil)/sum(PriAResults_all$PriFor2010)
sum(PriAResults_all$ChangePriFor.FC)/sum(PriAResults_all$PriFor2010)
sum(PriAResults_all$ChangePriFor.FCnoSFA)/sum(PriAResults_all$PriFor2010)
sum(PriAResults_all$ChangePriFor.FCnoCRA)/sum(PriAResults_all$PriFor2010)

-sum(PriAResults_all$ChangeNat.IDCbrazil)/sum(PriAResults_all$Nat2010)
-sum(PriAResults_all$ChangeNat.FC)/sum(PriAResults_all$Nat2010)
-sum(PriAResults_all$ChangeNat.FCnoSFA)/sum(PriAResults_all$Nat2010)
-sum(PriAResults_all$ChangeNat.FCnoCRA)/sum(PriAResults_all$Nat2010)

###################### 6.Visulising outputs####################################
  ##install spatial packages for handling polygons etc. - useful for linking results to shapefiles
  #install.packages("maptools")
  #install.packages("rgdal")
  #install.packages("classInt")
  #install.packages("RColorBrewer")
  library(maptools)
  library(rgdal)
  library(classInt)
  library(RColorBrewer)
  
  mappath = "C:/R/Brazil/Maps"
  setwd(mappath)
  CR_B_shape<-readShapePoly("Colrow_Brazil.shp")
  ### if coming back to the work need to reload saved datasets
  #load("Result_RemCheck140711.RData", .GlobalEnv)
  
  # Create copy of shape file 
  outMap_PriA<-CR_B_shape
  nrow(outMap_PriA@data)
  #join outputs 
  #use all.x and all.y to control inner or outer join etc.
  outMap_PriA@data<- merge(outMap_PriA@data, PriAResults_reduced, by.x="Colrow", 
                           by.y="ColRow30", all.x=TRUE,all.y=FALSE, sort=F)
  #view structure = str(ooutMap_PriA@)
  #plot of outline to test shapefile is ok
  plot(outMap_PriA)
  spplot(outMap_PriA, "ChangePriMax",at=c(-307,-100,-50,-0.1,0.1,50,100,307), 
         col.regions=brewer.pal(7,"RdBu"), col="black",
         main = list(label="ChangePriMax"))

####
names(Land_IDCbrazil)
names(biomes)
Land_IDCbrazil_biomes<-merge(Land_IDCbrazil,biomes,by.x="ColRow30", by.y="Colrow")
names(Land_IDCbrazil_biomes)
View(Land_IDCbrazil_biomes[Land_IDCbrazil_biomes$Biome=="Pampa",]

                         