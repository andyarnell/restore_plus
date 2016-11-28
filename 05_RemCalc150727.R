########Remnants test##########
# This code compares the distibution of forest and other natural land in 2010 in the GLOBIOM outputs
# and the remnants databse from paper
#distribution shows that in most areas there is more remnant vegetation than forest - showing that some
#of the other natural land category of GLOBIOM are likely to be remnants of natural land

########### 1. Set data sources (needs to be adjused by user) ##################
# Please set the inputpath to the csv files
inputpath = "C:/R/Brazil1611/Inputs"
outputpath = "C:/R/Brazil1611/Outputs"
scriptpath = "C:/R/Brazil1611"
mappath = "C:/R/Brazil1611/Maps"

########### 2. Input globiom data (calculated in PrepareGlobiomOutputs)################
setwd(outputpath)
Land_IDCno<-read.csv("Land_IDCno.csv")
names(Land_IDCno)
Land_FC<-read.csv("Land_FC.csv")

########################### 2. Enter remnants dataset #######################
### need to set file name of remnantes database to use ###
#Add remnants information and check loaded
# Remnants database is the mean coverage per 10000 ha in each Colrow, 
# (currently from the ICMBio data as overlap with high resolution remnants data crashed and no time to re-run) 
# but visually, v. similar distribution 
# however, some part col rows at the very edge of the country are missing data.

setwd(inputpath)
Input_Remnants = "RemnantCover.csv"

Remnants<-read.csv(Input_Remnants,
                     header=TRUE)
names(Remnants) 

#############2.b create new table of combinded database#####
######convert Remnants datbase to percentage of colrow which is a remnant to creat variable percRemSF

result_Rem_IDCno<- merge(Land_IDCno,Remnants,by.x="ColRow30",by.y="Colrow", 
                         all.x=TRUE,incomparables==0)
names(result_Rem_IDCno)
result_Rem_IDCno[is.na(result_Rem_IDCno)] = 0

result_Rem_FC<- merge(Land_FC,Remnants,by.x="ColRow30",by.y="Colrow", 
                         all.x=TRUE,incomparables==0)
names(result_Rem_FC)
result_Rem_FC[is.na(result_Rem_FC)] = 0

# Store
setwd(outputpath)
save(result_Rem_IDCno, file="result_Rem_IDCno.RData")
write.csv(result_Rem_IDCno, file="Rem_Land_IDCno.csv", row.names=FALSE, quote=FALSE)

############################ 3. comparing natural land in ColRow and GLOBIOM inputs/data #################
RemCheck<-data.frame(result_Rem_IDCno)

RemCheck$RemCovSF<-(RemCheck$percRemSF*RemCheck$LCI_SimUarea)
min(RemCheck$RemCovSF, na.rm=TRUE)
max(RemCheck$RemCovSF,na.rm=TRUE)
RemCheck$RemCovSF[is.na(RemCheck$RemCovSF)] = 0

RemCheck$RemCovMMA<-(RemCheck$perRemMMA*RemCheck$LCI_SimUarea)
min(RemCheck$RemCovMMA, na.rm=TRUE)
max(RemCheck$RemCovMMA,na.rm=TRUE)
RemCheck$RemCovMMA[is.na(RemCheck$RemCovMMA)] = 0

######### calculate diferencce between area of remnants and GLOBIOm land cats
#DifRemNonPro<-((result_Rem_IDCno$LCI_NotRel
 #               +result_Rem_IDCno$LCI_WetLnd
#               +result_Rem_IDCno$LU_PriFor_2010
#               +result_Rem_IDCno$LU_NatLnd_2010
#               +result_Rem_IDCno$PA_Forest
#               +result_Rem_IDCno$PA_NotRel
#               +result_Rem_IDCno$PA_OthAgri
#               +result_Rem_IDCno$PA_OthNatLnd
#               +result_Rem_IDCno$PA_WetLnd)
##               -(result_Rem_IDCno$percRemSF*result_Rem_IDCno$LCI_SimUarea))
names(RemCheck)
RemCheck$DifRemSFForA<-((RemCheck$LU_PriFor_2010+RemCheck$LU_MngFor_2010+RemCheck$PA_Forest)-
    (RemCheck$percRemSF*RemCheck$LCI_SimUarea))
RemCheck$ForAinREm<-ifelse((RemCheck$LU_PriFor_2010+RemCheck$LU_MngFor_2010+RemCheck$PA_Forest)>
                                (RemCheck$percRemSF*RemCheck$LCI_SimUarea),
                              (RemCheck$percRemSF*RemCheck$LCI_SimUarea),
                              (RemCheck$LU_PriFor_2010+RemCheck$LU_MngFor_2010+RemCheck$PA_Forest))
RemCheck$DifRemSFForA2<-((RemCheck$LU_PriFor_2010+RemCheck$LU_MngFor_2010+RemCheck$PA_Forest)
                          -(RemCheck$percRemSF*(RemCheck$PA_SimUarea+RemCheck$LU_CrpLnd_2010+
                                                  RemCheck$LU_GrsLnd_2010+RemCheck$LU_MngFor_2010+
                                                  RemCheck$LU_NatLnd_2010+RemCheck$LU_PltFor_2010+
                                                  RemCheck$LU_PriFor_2010)))
RemCheck$DifRemSFPriFor2<-((RemCheck$LU_PriFor_2010+RemCheck$PA_Forest)
            -(RemCheck$percRemSF*(RemCheck$PA_SimUarea+RemCheck$LU_CrpLnd_2010+RemCheck$LU_GrsLnd_2010+
                                    RemCheck$LU_MngFor_2010+RemCheck$LU_NatLnd_2010+
                                    RemCheck$LU_PltFor_2010+RemCheck$LU_PriFor_2010)))
RemCheck$ForA<-(RemCheck$LU_PriFor_2010+RemCheck$LU_MngFor_2010+RemCheck$PA_Forest)
min(RemCheck$DifRemSFForA, na.rm=TRUE)
max(RemCheck$DifRemSFForA, na.rm=TRUE)
mean(RemCheck$DifRemSFForA, na.rm=TRUE)
min(RemCheck$DifRemSFForA2, na.rm=TRUE)
max(RemCheck$DifRemSFForA2, na.rm=TRUE)
mean(RemCheck$DifRemSFForA2, na.rm=TRUE)
hist(RemCheck$DifRemSFForA2,breaks = c(-350,-200,-100,-10,10,100,200,350), freq=TRUE)
# calculate diferencce between area of forest (all land in PAs and PriFor) in a cell and area of remnants.
RemCheck$DifRemMMAForA<-((RemCheck$LU_PriFor_2010+RemCheck$LU_MngFor_2010+RemCheck$PA_Forest)-
                          (RemCheck$perRemMMA*RemCheck$LCI_SimUarea))
RemCheck$DifRemMMAForA2<-((RemCheck$LU_PriFor_2010+RemCheck$LU_MngFor_2010+RemCheck$PA_Forest)
                         -(RemCheck$perRemMMA*(RemCheck$PA_SimUarea+RemCheck$LU_CrpLnd_2010+
                                                 RemCheck$LU_GrsLnd_2010+RemCheck$LU_MngFor_2010+
                                                 RemCheck$LU_NatLnd_2010+RemCheck$LU_PltFor_2010+
                                                 RemCheck$LU_PriFor_2010)))
RemCheck$DifRemMMAPriFor2<-((RemCheck$LU_PriFor_2010+RemCheck$PA_Forest)
                           -(RemCheck$perRemMMA*(RemCheck$PA_SimUarea+RemCheck$LU_CrpLnd_2010+RemCheck$LU_GrsLnd_2010+
                                                   RemCheck$LU_MngFor_2010+RemCheck$LU_NatLnd_2010+
                                                   RemCheck$LU_PltFor_2010+RemCheck$LU_PriFor_2010)))


min(RemCheck$DifRemMMAForA2)
max(RemCheck$DifRemMMAForA2)
mean(RemCheck$DifRemMMAForA2)
# Store
setwd(outputpath)
write.csv(RemCheck, file="RemCheck.csv", row.names=FALSE, quote=FALSE)

setwd(inputpath)
biomes<-read.csv("Biomes.csv")
names(biomes)
RemCheck<-merge(RemCheck,biomes,by.x="ColRow30",by.y="Colrow")
names(RemCheck)
RemCheckB<- aggregate(
  cbind(DifRemSFForA,DifRemSFForA2,DifRemSFPriFor2,RemCovSF,ForAinREm,ForA)~Biome, 
  data=RemCheck, FUN="sum")
RemCheckB$perc<-RemCheckB$ForAinREm/RemCheckB$RemCovSF
View(RemCheckB)


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
names(RemCheck)
mappath = "C:/R/Brazil/Maps"
setwd(mappath)
CR_B_shape<-readShapePoly("SIMU_BRAZ_GRIDCELLS.shp")
### if coming back to the work need to reload saved datasets
#load("Result_RemCheck140711.RData", .GlobalEnv)

# Create copy of shape file 
outMap_PriA<-CR_B_shape
nrow(outMap_PriA@data)
#join outputs 
#use all.x and all.y to control inner or outer join etc.
outMap_PriA@data<- merge(outMap_PriA@data, RemCheck, by.x="Colrow", 
                         by.y="ColRow30", all.x=TRUE,all.y=FALSE, sort=F)
#view structure = str(ooutMap_PriA@)
#plot of outline to test shapefile is ok
plot(outMap_PriA)
spplot(outMap_PriA, "DifRemSFFor",at=c(-307,-100,-50,-0.1,0.1,50,100,307), 
       col.regions=brewer.pal(7,"RdBu"), col="black",
       main = list(label="DifRemSFFor"))

setwd(mappath)
CR_B_shape<-readShapePoly("SIMU_BRAZ_GRIDCELLS.shp")
### if coming back to the work need to reload saved datasets
#load("Result_RemCheck140711.RData", .GlobalEnv)

# Create copy of shape file 
outMap_RemCheck<-CR_B_shape
nrow(outMap_RemCheck@data)
names(outMap_RemCheck@data)
#join outputs 
#use all.x and all.y to control inner or outer join etc.
outMap_RemCheck@data<- merge(outMap_RemCheck@data, RemCheck, by.x="Colrow", 
                         by.y="ColRow30", all.x=TRUE,all.y=FALSE, sort=F)
#view structure = str(ooutMap_PriA@)
#plot of outline to test shapefile is ok
plot(outMap_RemCheck)
spplot(outMap_RemCheck, "DifRemSFFor",at=c(-307,-100,-50,-0.1,0.1,50,100,307), 
       col.regions=brewer.pal(7,"RdBu"), col="black",
       main = list(label="DifRemSFFor"))
