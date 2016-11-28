# Total change #####
#Calculating the total amount of change in different land covers in different scenarios. 

########## 1. Set data sources (needs to be adjused by user) ##################
# Please set the inputpath to the csv files
inputpath = "C:/R/Brazil1611/Inputs"
outputpath = "C:/R/Brazil1611/Outputs"
scriptpath = "C:/R/Brazil1611"

########### 2. Input globiom data (calculated in PreparingGlobiomOutputs)################
setwd(outputpath)
Land_IDCno<-read.csv("Land_IDCno.csv")
Land_IDCamazon<-read.csv("Land_IDCamazon.csv")
Land_IDCbrazil<-read.csv("Land_IDCbrazil.csv")
names(Land_IDCno)
Land_FC<-read.csv("Land_FC.csv")
Land_FCnoSFA<-read.csv("Land_FCnoSFA.csv")
Land_FCnoCRA<-read.csv("Land_FCnoCRA.csv")
Land_FCnoCRAnoSFA<-read.csv("Land_FCnoCRAnoSFA.csv")

############################ 3. Change 10_50 ######################
Land_IDCno$PriFor10_50<-Land_IDCno$LU_PriFor_2050-Land_IDCno$LU_PriFor_2010
Land_IDCno$MngFor10_50<-Land_IDCno$LU_MngFor_2050-Land_IDCno$LU_MngFor_2010
Land_IDCno$PltFor10_50<-Land_IDCno$LU_PltFor_2050-Land_IDCno$LU_PltFor_2010
Land_IDCno$ForReg10_50<-0
Land_IDCno$NatLnd10_50<-Land_IDCno$LU_NatLnd_2050-Land_IDCno$LU_NatLnd_2010
Change_IDCno<-Land_IDCno[c("ColRow30","LCI_SimUarea", "PA_Forest","PA_OthNatLnd", "LU_MngFor_2010", "LU_PriFor_2010","LU_NatLnd_2010",
                       "PriFor10_50","MngFor10_50","PltFor10_50", "ForReg10_50","NatLnd10_50")]
head(Change_IDCno)
Land_IDCamazon$PriFor10_50<-Land_IDCamazon$LU_PriFor_2050-Land_IDCamazon$LU_PriFor_2010
Land_IDCamazon$MngFor10_50<-Land_IDCamazon$LU_MngFor_2050-Land_IDCamazon$LU_MngFor_2010
Land_IDCamazon$PltFor10_50<-Land_IDCamazon$LU_PltFor_2050-Land_IDCamazon$LU_PltFor_2010
Land_IDCamazon$ForReg10_50<-0
Land_IDCamazon$NatLnd10_50<-Land_IDCamazon$LU_NatLnd_2050-Land_IDCamazon$LU_NatLnd_2010
Change_IDCamazon<-Land_IDCamazon[c("ColRow30","LCI_SimUarea", "PA_Forest","PA_OthNatLnd", "LU_MngFor_2010", "LU_PriFor_2010","LU_NatLnd_2010",
                           "PriFor10_50","MngFor10_50","PltFor10_50", "ForReg10_50","NatLnd10_50")]
head(Change_IDCamazon)
Land_IDCbrazil$PriFor10_50<-Land_IDCbrazil$LU_PriFor_2050-Land_IDCbrazil$LU_PriFor_2010
Land_IDCbrazil$MngFor10_50<-Land_IDCbrazil$LU_MngFor_2050-Land_IDCbrazil$LU_MngFor_2010
Land_IDCbrazil$PltFor10_50<-Land_IDCbrazil$LU_PltFor_2050-Land_IDCbrazil$LU_PltFor_2010
Land_IDCbrazil$ForReg10_50<-0
Land_IDCbrazil$NatLnd10_50<-Land_IDCbrazil$LU_NatLnd_2050-Land_IDCbrazil$LU_NatLnd_2010
Change_IDCbrazil<-Land_IDCbrazil[c("ColRow30","LCI_SimUarea", "PA_Forest","PA_OthNatLnd", "LU_MngFor_2010", "LU_PriFor_2010","LU_NatLnd_2010",
                           "PriFor10_50","MngFor10_50","PltFor10_50", "ForReg10_50","NatLnd10_50")]
head(Change_IDCbrazil)
Land_FC$PriFor10_50<-Land_FC$LU_PriFor_2050-Land_FC$LU_PriFor_2010
Land_FC$MngFor10_50<-Land_FC$LU_MngFor_2050-Land_FC$LU_MngFor_2010
Land_FC$PltFor10_50<-Land_FC$LU_PltFor_2050-Land_FC$LU_PltFor_2010
Land_FC$ForReg10_50<-Land_FC$LU_ForReg_2050
Land_FC$NatLnd10_50<-Land_FC$LU_NatLnd_2050-Land_FC$LU_NatLnd_2010
Change_FC<-Land_FC[c("ColRow30","PriFor10_50","MngFor10_50","PltFor10_50",
                           "ForReg10_50","NatLnd10_50")]

Land_FCnoSFA$PriFor10_50<-Land_FCnoSFA$LU_PriFor_2050-Land_FCnoSFA$LU_PriFor_2010
Land_FCnoSFA$MngFor10_50<-Land_FCnoSFA$LU_MngFor_2050-Land_FCnoSFA$LU_MngFor_2010
Land_FCnoSFA$PltFor10_50<-Land_FCnoSFA$LU_PltFor_2050-Land_FCnoSFA$LU_PltFor_2010
Land_FCnoSFA$ForReg10_50<-Land_FCnoSFA$LU_ForReg_2050
Land_FCnoSFA$NatLnd10_50<-Land_FCnoSFA$LU_NatLnd_2050-Land_FCnoSFA$LU_NatLnd_2010
Change_FCnoSFA<-Land_FCnoSFA[c("ColRow30","PriFor10_50","MngFor10_50","PltFor10_50",
                           "ForReg10_50","NatLnd10_50")]
Land_FCnoCRA$PriFor10_50<-Land_FCnoCRA$LU_PriFor_2050-Land_FCnoCRA$LU_PriFor_2010
Land_FCnoCRA$MngFor10_50<-Land_FCnoCRA$LU_MngFor_2050-Land_FCnoCRA$LU_MngFor_2010
Land_FCnoCRA$PltFor10_50<-Land_FCnoCRA$LU_PltFor_2050-Land_FCnoCRA$LU_PltFor_2010
Land_FCnoCRA$ForReg10_50<-Land_FCnoCRA$LU_ForReg_2050
Land_FCnoCRA$NatLnd10_50<-Land_FCnoCRA$LU_NatLnd_2050-Land_FCnoCRA$LU_NatLnd_2010
Change_FCnoCRA<-Land_FCnoCRA[c("ColRow30","PriFor10_50","MngFor10_50","PltFor10_50",
                               "ForReg10_50","NatLnd10_50")]
Land_FCnoCRAnoSFA$PriFor10_50<-Land_FCnoCRAnoSFA$LU_PriFor_2050-Land_FCnoCRAnoSFA$LU_PriFor_2010
Land_FCnoCRAnoSFA$MngFor10_50<-Land_FCnoCRAnoSFA$LU_MngFor_2050-Land_FCnoCRAnoSFA$LU_MngFor_2010
Land_FCnoCRAnoSFA$PltFor10_50<-Land_FCnoCRAnoSFA$LU_PltFor_2050-Land_FCnoCRAnoSFA$LU_PltFor_2010
Land_FCnoCRAnoSFA$ForReg10_50<-Land_FCnoCRAnoSFA$LU_ForReg_2050
Land_FCnoCRAnoSFA$NatLnd10_50<-Land_FCnoCRAnoSFA$LU_NatLnd_2050-Land_FCnoCRAnoSFA$LU_NatLnd_2010
Change_FCnoCRAnoSFA<-Land_FCnoCRAnoSFA[c("ColRow30","PriFor10_50","MngFor10_50","PltFor10_50",
                               "ForReg10_50","NatLnd10_50")]


Change_all<-merge(Change_IDCno,Change_IDCamazon,by="ColRow30", suffixes=c(".IDCno",".IDCamazon"))
Change_all<-merge(Change_all,Change_IDCbrazil,by="ColRow30")
Change_all<-merge(Change_all,Change_FC,by="ColRow30", suffixes=c(".IDCbrazil",".FC"))
Change_all<-merge(Change_all,Change_FCnoSFA,by="ColRow30")
Change_all<-merge(Change_all,Change_FCnoCRA,by="ColRow30", suffixes=c(".FCnoSFA",".FCnoCRA"))
Change_all<-merge(Change_all,Change_FCnoCRAnoSFA,by="ColRow30")
names(Change_all)
names(Change_all)[names(Change_all)=="MngFor10_50"] <- "MngFor10_50.FCnoCRAnoSFA"
names(Change_all)[names(Change_all)=="PriFor10_50"] <- "PriFor10_50.FCnoCRAnoSFA"
names(Change_all)[names(Change_all)=="PltFor10_50"] <- "PltFor10_50.FCnoCRAnoSFA"
names(Change_all)[names(Change_all)=="ForReg10_50"] <- "ForReg10_50.FCnoCRAnoSFA"
names(Change_all)[names(Change_all)=="NatLnd10_50"] <- "NatLnd10_50.FCnoCRAnoSFA"
names(Change_all)
setwd(inputpath)
biomes<-read.csv("Biomes.csv")
names(biomes)
Change_all<-merge(Change_all,biomes,by.x="ColRow30",by.y="Colrow")
names(Change_all)
Change_allB<- aggregate(
  cbind(LCI_SimUarea,PA_Forest,LU_MngFor_2010,LU_PriFor_2010, PriFor10_50.IDCno,MngFor10_50.IDCno,
        PltFor10_50.IDCno,ForReg10_50.IDCno,NatLnd10_50.IDCno,PriFor10_50.IDCamazon,MngFor10_50.IDCamazon,
        PltFor10_50.IDCamazon,ForReg10_50.IDCamazon,NatLnd10_50.IDCamazon,PriFor10_50.IDCbrazil,
        MngFor10_50.IDCbrazil,PltFor10_50.IDCbrazil,ForReg10_50.IDCbrazil,NatLnd10_50.IDCbrazil,
        PriFor10_50.FC,MngFor10_50.FC,PltFor10_50.FC,ForReg10_50.FC,NatLnd10_50.FC,PriFor10_50.FCnoSFA,
        MngFor10_50.FCnoSFA,PltFor10_50.FCnoSFA,ForReg10_50.FCnoSFA,NatLnd10_50.FCnoSFA,
        PriFor10_50.FCnoCRA,MngFor10_50.FCnoCRA,PltFor10_50.FCnoCRA,ForReg10_50.FCnoCRA,
        NatLnd10_50.FCnoCRA, PriFor10_50.FCnoCRAnoSFA,MngFor10_50.FCnoCRAnoSFA,PltFor10_50.FCnoCRAnoSFA,
        ForReg10_50.FCnoCRAnoSFA,NatLnd10_50.FCnoCRAnoSFA)~Biome, data=Change_all, FUN="sum")
View(Change_allB)
setwd(outputpath)
write.csv(Change_allB, file="Change_allB.csv", 
          row.names=FALSE, quote=FALSE)
write.csv(Change_all, file="Change_all.csv")
############################ 3. Change 10_50 data for graph  of change######################
Change_all<-read.csv(file="Change_all.csv")

sum(Change_all$LU_MngFor_2010)+sum(Change_all$LU_PriFor_2010)+sum(Change_all$PA_Forest)

sum(Land_IDCno$PA_Forest)
sum(Land_IDCno$PA_OthNatLnd)
sum(Land_IDCno$LU_PriFor_2010)
sum(Land_IDCno$LU_MngFor_2010)
sum(Land_IDCno$LU_PltFor_2010)
sum(Land_IDCno$LU_NatLnd_2010)
sum(Land_IDCno$LU_ForReg_2010)

sum(Land_IDCamazon$PA_Forest)
sum(Land_IDCamazon$PA_OthNatLnd)
sum(Land_IDCamazon$LU_PriFor_2010)
sum(Land_IDCamazon$LU_MngFor_2010)
sum(Land_IDCamazon$LU_PltFor_2010)
sum(Land_IDCamazon$LU_NatLnd_2010)
sum(Land_IDCamazon$LU_ForReg_2010)

sum(Land_IDCbrazil$PA_Forest)
sum(Land_IDCbrazil$PA_OthNatLnd)
sum(Land_IDCbrazil$LU_PriFor_2010)
sum(Land_IDCbrazil$LU_MngFor_2010)
sum(Land_IDCbrazil$LU_PltFor_2010)
sum(Land_IDCbrazil$LU_NatLnd_2010)
sum(Land_IDCbrazil$LU_ForReg_2010)

sum(Land_FC$LU_PriFor_2010)
sum(Land_FC$LU_MngFor_2010)
sum(Land_FC$LU_PltFor_2010)
sum(Land_FC$LU_NatLnd_2010)
sum(Land_FC$LU_ForReg_2010)

sum(Land_FCnoSFA$LU_PriFor_2010)
sum(Land_FCnoSFA$LU_MngFor_2010)
sum(Land_FCnoSFA$LU_PltFor_2010)
sum(Land_FCnoSFA$LU_NatLnd_2010)
sum(Land_FCnoSFA$LU_ForReg_2010)

sum(Land_FCnoCRA$LU_PriFor_2010)
sum(Land_FCnoCRA$LU_MngFor_2010)
sum(Land_FCnoCRA$LU_PltFor_2010)
sum(Land_FCnoCRA$LU_NatLnd_2010)
sum(Land_FCnoCRA$LU_ForReg_2010)

sum(Land_IDCno$LU_PriFor_2050)
sum(Land_IDCno$LU_MngFor_2050)
sum(Land_IDCno$LU_PltFor_2050)
sum(Land_IDCno$LU_NatLnd_2050)
sum(Land_IDCno$LU_ForReg_2050)

sum(Land_IDCamazon$LU_PriFor_2050)
sum(Land_IDCamazon$LU_MngFor_2050)
sum(Land_IDCamazon$LU_PltFor_2050)
sum(Land_IDCamazon$LU_NatLnd_2050)
sum(Land_IDCamazon$LU_ForReg_2050)

sum(Land_IDCbrazil$LU_PriFor_2050)
sum(Land_IDCbrazil$LU_MngFor_2050)
sum(Land_IDCbrazil$LU_PltFor_2050)
sum(Land_IDCbrazil$LU_NatLnd_2050)
sum(Land_IDCbrazil$LU_ForReg_2050)

sum(Land_FC$LU_PriFor_2050)
sum(Land_FC$LU_MngFor_2050)
sum(Land_FC$LU_PltFor_2050)
sum(Land_FC$LU_NatLnd_2050)
sum(Land_FC$LU_ForReg_2050)

sum(Land_FCnoSFA$LU_PriFor_2050)
sum(Land_FCnoSFA$LU_MngFor_2050)
sum(Land_FCnoSFA$LU_PltFor_2050)
sum(Land_FCnoSFA$LU_NatLnd_2050)
sum(Land_FCnoSFA$LU_ForReg_2050)

sum(Land_FCnoCRA$LU_PriFor_2050)
sum(Land_FCnoCRA$LU_MngFor_2050)
sum(Land_FCnoCRA$LU_PltFor_2050)
sum(Land_FCnoCRA$LU_NatLnd_2050)
sum(Land_FCnoCRA$LU_ForReg_2050)
############################ 3. Change 10_50 data for graph  of proportions lost######################
Change_all<-read.csv(file="Change_all.csv")

sum(Change_all$PriFor10_50.IDCno)/(sum(Change_all$LU_PriFor_2010)+sum(Change_all$PA_Forest))
sum(Change_all$PriFor10_50.FC)/(sum(Change_all$LU_PriFor_2010)+sum(Change_all$PA_Forest))
sum(Change_all$PriFor10_50.FCnoSFA)/(sum(Change_all$LU_PriFor_2010)+sum(Change_all$PA_Forest))
sum(Change_all$PriFor10_50.FCnoCRA)/(sum(Change_all$LU_PriFor_2010)+sum(Change_all$PA_Forest))

sum(Change_all$PriFor10_50.IDCno)/sum(Change_all$LU_PriFor_2010)
sum(Change_all$PriFor10_50.FC)/sum(Change_all$LU_PriFor_2010)
sum(Change_all$PriFor10_50.FCnoSFA)/sum(Change_all$LU_PriFor_2010)
sum(Change_all$PriFor10_50.FCnoCRA)/sum(Change_all$LU_PriFor_2010)
############################ 3. Change 00_10 ######################
Land_IDCno$PriFor00_10<-Land_IDCno$LU_PriFor_2010-Land_IDCno$LU_PriFor_2000
Land_IDCno$MngFor00_10<-Land_IDCno$LU_MngFor_2010-Land_IDCno$LU_MngFor_2000
Land_IDCno$PltFor00_10<-Land_IDCno$LU_PltFor_2010-Land_IDCno$LU_PltFor_2000
Land_IDCno$ForReg00_10<-0
Land_IDCno$NatLnd00_10<-Land_IDCno$LU_NatLnd_2010-Land_IDCno$LU_NatLnd_2000
Change_IDCno00_10<-Land_IDCno[c("ColRow30","LCI_SimUarea", "PA_Forest","PA_OthNatLnd", "LU_MngFor_2000", "LU_PriFor_2000","LU_NatLnd_2000",
                       "PriFor00_10","MngFor00_10","PltFor00_10", "ForReg00_10","NatLnd00_10")]
head(Change_IDCno)

setwd(inputpath)
biomes<-read.csv("Biomes.csv")
names(biomes)

Change_IDCno00_10<-merge(Change_IDCno,biomes,by.x="ColRow30",by.y="Colrow")

Change_IDCnob<- aggregate(
  cbind(LCI_SimUarea,PA_Forest,LU_MngFor_2000,LU_PriFor_2000, PriFor00_10,MngFor00_10,
        PltFor00_10,ForReg00_10,NatLnd00_10)~Biome, 
  data=Change_IDCno00_10, FUN="sum")
View(Change_IDCnob)
############################ 3. Development of forest over time. ################
setwd(outputpath)
Land_IDCno<-read.csv("Land_IDCno.csv")
names(Land_IDCno)
Land_FC<-read.csv("Land_FC.csv")

sum(Land_IDCno$PA_Forest)
sum(Land_IDCno$LU_PriFor_2010)
sum(Land_IDCno$LU_MngFor_2010)
sum(Land_IDCno$LU_ForReg_2010)
sum(Land_IDCno$LU_PriFor_2020)
sum(Land_IDCno$LU_MngFor_2020)
sum(Land_IDCno$LU_ForReg_2020)
sum(Land_IDCno$LU_PriFor_2030)
sum(Land_IDCno$LU_MngFor_2030)
sum(Land_IDCno$LU_ForReg_2030)
sum(Land_IDCno$LU_PriFor_2040)
sum(Land_IDCno$LU_MngFor_2040)
sum(Land_IDCno$LU_ForReg_2040)
sum(Land_IDCno$LU_PriFor_2050)
sum(Land_IDCno$LU_MngFor_2050)
sum(Land_IDCno$LU_ForReg_2050)

sum(Land_FC$PA_Forest)
sum(Land_FC$LU_PriFor_2010)
sum(Land_FC$LU_MngFor_2010)
sum(Land_FC$LU_ForReg_2010)
sum(Land_FC$LU_PriFor_2020)
sum(Land_FC$LU_MngFor_2020)
sum(Land_FC$LU_ForReg_2020)
sum(Land_FC$LU_PriFor_2030)
sum(Land_FC$LU_MngFor_2030)
sum(Land_FC$LU_ForReg_2030)
sum(Land_FC$LU_PriFor_2040)
sum(Land_FC$LU_MngFor_2040)
sum(Land_FC$LU_ForReg_2040)
sum(Land_FC$LU_PriFor_2050)
sum(Land_FC$LU_MngFor_2050)
sum(Land_FC$LU_ForReg_2050)