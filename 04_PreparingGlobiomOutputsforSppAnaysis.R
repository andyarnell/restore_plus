##########################PreparingGlobiomData##########################
#Preparing globiom outputs for the species analysis 
# creates files "Land_IDCno_2010.csv", "Land_IDCno_2050.csv" etc. 
#has one row per land use per colrow

###### 1. Set data sources (needs to be adjused by user) ##################
# Please set the inputpath to the csv files
inputpath = "C:/R/Brazil1611/Inputs"
outputpath = "C:/R/Brazil1611/Outputs"
########### 2. Input globiom data 
###### 2.a Setting files ####
setwd(inputpath)
# Filenames of GLOBIOM output files
filename_IDCno_LC3 = "IDC_no_Land_Compare3.csv"
filename_IDCamazon_LC3 = "IDC_amazon_Land_Compare3.csv"
filename_IDCbrazil_LC3 = "IDC_brazil_Land_Compare3.csv"
filename_FC_LC3 = "FC_Land_Compare3.csv"
filename_FCnoSFA_LC3 = "FCnoSFA_Land_Compare3.csv"
filename_FCnoCRAnoSFA_LC3 = "FCnoCRAnoSFA_Land_Compare3.csv"
filename_FCnoCRA_LC3 = "FCnoCRA_Land_Compare3.csv"
filename_PAS_MMA = "ProtectedAreas_Brazil.CSV"
filename_LANDCOVER_INIT = "LANDCOVER_INIT_PA.CSV"
###### 2.b Getting PA and INIT  GLOBIO data in right format #####
# Load the GLOBIOM output files one by one and give them column names

input_PAS = read.csv(filename_PAS_MMA, header=FALSE)
names(input_PAS) = c("country","colrow","altitude","slope","soil","aezclass","palandcover","value")
input_LANDCOVER_INIT = read.csv(filename_LANDCOVER_INIT, header=FALSE)
head(input_LANDCOVER_INIT)
names(input_LANDCOVER_INIT) = c("country","colrow","altitude","slope","soil",
                                "lcialandcover","value")
levels(input_LANDCOVER_INIT$lcialandcover)
### Data checks ###
head(input_PAS)

### Aggregating the CSV tables ###
# We will aggregate "value"
# The differences in "aezclass" are IRrelevant, so we leave it out!
# The differences in "colrow" and "palandcover" are relevant
# The differences in "country", "altitude", "slope" and "soil" might be relevant, 
# but in my files they are simply constant
aggregated_PAS= aggregate(value~colrow+palandcover, 
                          data=input_PAS, FUN="sum")
aggregated_LANDCOVER_INIT = aggregate(value~colrow+lcialandcover, 
                                      data=input_LANDCOVER_INIT, FUN="sum")

### Land Cover values from PAS_MMA ###
result_PAS= data.frame(unique(aggregated_PAS$colrow))
names(result_PAS) = "colrow"
counter = 0
for (landcover in unique(aggregated_PAS$palandcover)){
  print(paste("Going through land use class", landcover))
  colname = paste("PA",landcover, sep="_")
  print(colname)
  condition = (aggregated_PAS$palandcover==landcover)
  col = aggregated_PAS[condition,c("colrow","value")]
  names(col)=c("colrow",colname)
  print(head(col))
  if (nrow(col)>0) result_PAS= merge(result_PAS, col, by.x="colrow", by.y="colrow", all=TRUE)
  counter = counter+1
  #if (counter>5) break()
}
result_PAS[is.na(result_PAS)] = 0
if(length(unique(input_PAS$colrow)) != nrow(result_PAS)) stop("Problem with transforming the GAMS output of 'PAS_MMA_Brazil_LCTYPESagg.csv' into table format. Too few ColRows!")
# store
setwd(outputpath)
save(result_PAS, file="result_PAS.RData")
names(result_PAS)
nrow(result_PAS)
dif<-(result_PAS$PA_SimUarea-(result_PAS$PA_Forest+result_PAS$PA_NotRel+result_PAS$PA_OthNatLnd+
                                result_PAS$PA_WetLnd))

### Land Cover values from LANDCOVER_INIT.CSV    ###
result_LANDCOVER_INIT = data.frame(unique(aggregated_LANDCOVER_INIT$colrow))
names(result_LANDCOVER_INIT) = "colrow"
counter = 0
for (landcover in unique(aggregated_LANDCOVER_INIT$lcialandcover)){
  print(paste("Going through land use class", landcover))
  colname = paste("LCI",landcover, sep="_")
  print(colname)
  condition = (aggregated_LANDCOVER_INIT$lcialandcover==landcover)
  col = aggregated_LANDCOVER_INIT[condition,c("colrow","value")]
  names(col)=c("colrow",colname)
  print(head(col))
  if (nrow(col)>0) result_LANDCOVER_INIT = merge(result_LANDCOVER_INIT, col, by="colrow", all=TRUE)
  counter = counter+1
  #if (counter>5) break()
}
result_LANDCOVER_INIT[is.na(result_LANDCOVER_INIT)] = 0

if(length(unique(input_LANDCOVER_INIT$colrow)) != nrow(result_LANDCOVER_INIT))  stop("Problem with transforming the GAMS output of 'LANDCOVER_INITagg.csv' into table format. Too few ColRows!")

# store
setwd(outputpath)
save(result_LANDCOVER_INIT, file="result_LANDCOVER_INIT.RData")
###### 2.c Land Use values from IDCno_LC3.CSV ####
setwd(inputpath)
IDCno_LC3 = read.csv(filename_IDCno_LC3, header=FALSE)
names(IDCno_LC3) = c("country","colrow","altitude","slope","soil","aezclass",
                     "landuselu","noidea1","noidea2","scenario","year","value")
View(IDCno_LC3)
levels(IDCno_LC3$landuselu)
aggregated_IDCno_LC3 = aggregate(value~colrow+landuselu+year,
                                 data=IDCno_LC3, FUN=sum)

result_IDCno_LC3 = data.frame(unique(aggregated_IDCno_LC3$colrow))
names(result_IDCno_LC3) = "colrow"
counter = 0
for (landuse in unique(aggregated_IDCno_LC3$landuselu)){
  print(paste("Going through land use class", landuse))
  for (year in c(2000,2010,2020,2030,2040,2050)){
    print(paste("Going through year", year))
    colname = paste("LU",landuse,year, sep="_")
    print(colname)
    condition = (aggregated_IDCno_LC3$landuselu==landuse) & (aggregated_IDCno_LC3$year==year)
    col = aggregated_IDCno_LC3[condition,c("colrow","value")]
    names(col)=c("colrow",colname)
    print(head(col))
    if (nrow(col)>0) result_IDCno_LC3 = merge(result_IDCno_LC3, col, by="colrow", all=TRUE)
    counter = counter+1
    #if (counter>5) break()
  }
  #if (counter>5) break()
}
result_IDCno_LC3[is.na(result_IDCno_LC3)] = 0

if(length(unique(IDCno_LC3$colrow)) != nrow(result_IDCno_LC3)) stop("Problem with transforming the GAMS output of 'IDCno_LC3.csv' into table format. Too few ColRows!")

# store
setwd(outputpath)
save(result_IDCno_LC3, file="result_IDCno_LC3.RData")

### Combine land uses/covers ###
# We use the ColRows of the Table_Land. If other tables have more ColRows,
# they are discarded. If other tables have less ColRows, they gaps are filled with zeros.
result_Land_IDCno = data.frame(unique(result_IDCno_LC3$colrow))
names(result_Land_IDCno) = "colrow"
result_Land_IDCno = merge(result_Land_IDCno, result_PAS, by="colrow", all=TRUE)
result_Land_IDCno = merge(result_Land_IDCno, result_LANDCOVER_INIT, by="colrow", all=TRUE)
result_Land_IDCno = merge(result_Land_IDCno, result_IDCno_LC3, by="colrow", all=TRUE)
#ok

names(result_Land_IDCno)

# Sort the rows
result_Land_IDCno=result_Land_IDCno[order(result_Land_IDCno$colrow),]

# Replace NA by zeros
#There is NAs because the different files have different numbers of ColRows
nrow(result_PAS) # [1] 2105
nrow(result_IDCno_LC3) # [1] 2934
nrow(result_Land_IDCno) # [1] 3001
result_Land_IDCno[is.na(result_Land_IDCno)] = 0

result_Land_IDCno$SimUArea<-ifelse((result_Land_IDCno$LCI_SimUarea-(result_Land_IDCno$PA_SimUarea+
                                                                      result_Land_IDCno$LU_CrpLnd_2000+result_Land_IDCno$LU_GrsLnd_2000+result_Land_IDCno$LU_MngFor_2000+
                                                                      result_Land_IDCno$LU_NatLnd_2000+result_Land_IDCno$LU_PltFor_2000+
                                                                      result_Land_IDCno$LU_PriFor_2000))>=0,result_Land_IDCno$LCI_SimUarea,
                                   (result_Land_IDCno$LU_CrpLnd_2000+result_Land_IDCno$LU_GrsLnd_2000+result_Land_IDCno$LU_MngFor_2000+
                                      result_Land_IDCno$LU_NatLnd_2000+result_Land_IDCno$LU_PltFor_2000+result_Land_IDCno$LU_PriFor_2000+
                                      result_Land_IDCno$PA_SimUarea))

result_Land_IDCno$NotRel<-ifelse(
  (result_Land_IDCno$LCI_SimUarea-(result_Land_IDCno$PA_SimUarea+result_Land_IDCno$LU_CrpLnd_2000+
                                     result_Land_IDCno$LU_GrsLnd_2000+result_Land_IDCno$LU_MngFor_2000+
                                     result_Land_IDCno$LU_NatLnd_2000+result_Land_IDCno$LU_PltFor_2000+
                                     result_Land_IDCno$LU_PriFor_2000))<=0,0,
  ifelse(
    result_Land_IDCno$LCI_SimUarea-(result_Land_IDCno$LU_CrpLnd_2000+result_Land_IDCno$LU_GrsLnd_2000+
                                      result_Land_IDCno$LU_MngFor_2000+result_Land_IDCno$LU_NatLnd_2000+
                                      result_Land_IDCno$LU_PltFor_2000+result_Land_IDCno$LU_PriFor_2000+
                                      result_Land_IDCno$PA_SimUarea)>result_Land_IDCno$LCI_NotRel,
    result_Land_IDCno$LCI_NotRel,
    result_Land_IDCno$LCI_SimUarea-(result_Land_IDCno$LU_CrpLnd_2000+result_Land_IDCno$LU_GrsLnd_2000+
                                      result_Land_IDCno$LU_MngFor_2000+result_Land_IDCno$LU_NatLnd_2000+
                                      result_Land_IDCno$LU_PltFor_2000+result_Land_IDCno$LU_PriFor_2000+
                                      result_Land_IDCno$PA_SimUarea)))

result_Land_IDCno$WetLnd<-ifelse(
  (result_Land_IDCno$LCI_SimUarea-(result_Land_IDCno$PA_SimUarea+result_Land_IDCno$LU_CrpLnd_2000+
                                     result_Land_IDCno$LU_GrsLnd_2000+result_Land_IDCno$LU_MngFor_2000+
                                     result_Land_IDCno$LU_NatLnd_2000+result_Land_IDCno$LU_PltFor_2000+
                                     result_Land_IDCno$LU_PriFor_2000+ result_Land_IDCno$NotRel))<=0,0,
  ifelse(
    result_Land_IDCno$LCI_SimUarea-(result_Land_IDCno$LU_CrpLnd_2000+result_Land_IDCno$LU_GrsLnd_2000+
                                      result_Land_IDCno$LU_MngFor_2000+result_Land_IDCno$LU_NatLnd_2000+
                                      result_Land_IDCno$LU_PltFor_2000+result_Land_IDCno$LU_PriFor_2000+
                                      result_Land_IDCno$PA_SimUarea+
                                      result_Land_IDCno$NotRel)>result_Land_IDCno$LCI_WetLnd,
    result_Land_IDCno$LCI_WetLnd,
    result_Land_IDCno$LCI_SimUarea-(result_Land_IDCno$LU_CrpLnd_2000+result_Land_IDCno$LU_GrsLnd_2000+
                                      result_Land_IDCno$LU_MngFor_2000+result_Land_IDCno$LU_NatLnd_2000+
                                      result_Land_IDCno$LU_PltFor_2000+result_Land_IDCno$LU_PriFor_2000+
                                      result_Land_IDCno$PA_SimUarea+result_Land_IDCno$NotRel)))
result_Land_IDCno$OthAgri<-ifelse(
  (result_Land_IDCno$LCI_SimUarea-(result_Land_IDCno$PA_SimUarea+result_Land_IDCno$LU_CrpLnd_2000+
                                     result_Land_IDCno$LU_GrsLnd_2000+result_Land_IDCno$LU_MngFor_2000+
                                     result_Land_IDCno$LU_NatLnd_2000+result_Land_IDCno$LU_PltFor_2000+
                                     result_Land_IDCno$LU_PriFor_2000+ result_Land_IDCno$NotRel+
                                     result_Land_IDCno$WetLnd))<=0,0,
  ifelse(
    result_Land_IDCno$LCI_SimUarea-(result_Land_IDCno$LU_CrpLnd_2000+result_Land_IDCno$LU_GrsLnd_2000+
                                      result_Land_IDCno$LU_MngFor_2000+result_Land_IDCno$LU_NatLnd_2000+
                                      result_Land_IDCno$LU_PltFor_2000+result_Land_IDCno$LU_PriFor_2000+
                                      result_Land_IDCno$PA_SimUarea+result_Land_IDCno$NotRel+
                                      result_Land_IDCno$WetLnd)>result_Land_IDCno$LCI_OthAgri,
    result_Land_IDCno$LCI_OthAgri,
    result_Land_IDCno$LCI_SimUarea-(result_Land_IDCno$LU_CrpLnd_2000+result_Land_IDCno$LU_GrsLnd_2000+
                                      result_Land_IDCno$LU_MngFor_2000+result_Land_IDCno$LU_NatLnd_2000+
                                      result_Land_IDCno$LU_PltFor_2000+result_Land_IDCno$LU_PriFor_2000+
                                      result_Land_IDCno$PA_SimUarea+result_Land_IDCno$NotRel+
                                      result_Land_IDCno$WetLnd)))

# Store
names(result_Land_IDCno)
land_IDCno<-result_Land_IDCno[c("colrow","PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd",
                                "LU_CrpLnd_2000", "LU_CrpLnd_2010","LU_CrpLnd_2050",
                                "LU_GrsLnd_2000","LU_GrsLnd_2010" ,"LU_GrsLnd_2050",
                                "LU_MngFor_2000", "LU_MngFor_2010","LU_MngFor_2050",
                                "LU_NatLnd_2000", "LU_NatLnd_2010", "LU_NatLnd_2050",
                                "LU_PltFor_2000", "LU_PltFor_2010" , "LU_PltFor_2050",
                                "LU_PriFor_2000", "LU_PriFor_2010", "LU_PriFor_2050",
                                "SimUArea","NotRel","WetLnd","OthAgri")]

names(land_IDCno)
land_IDCno_2010<-result_Land_IDCno[c("colrow","PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd",
                                     "LU_CrpLnd_2010","LU_GrsLnd_2010" ,"LU_MngFor_2010","LU_NatLnd_2010", 
                                     "LU_PltFor_2010" , "LU_PriFor_2010",
                                     "SimUArea","NotRel","WetLnd","OthAgri")]
names(land_IDCno_2010)<-c("colrow","PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd",
                          "LU_CrpLnd","LU_GrsLnd" ,"LU_MngFor","LU_NatLnd", 
                          "LU_PltFor" , "LU_PriFor",
                          "SimUArea","NotRel","WetLnd","OthAgri")

Land_IDCno_2010<-reshape(land_IDCno_2010, 
                         varying = c("PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd","LU_CrpLnd",
                                     "LU_GrsLnd","LU_MngFor","LU_NatLnd","LU_PltFor","LU_PriFor","NotRel",
                                     "WetLnd","OthAgri"), 
                         v.names = "lc_area",
                         timevar = "lulc", 
                         times = c("PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd","LU_CrpLnd",
                                   "LU_GrsLnd","LU_MngFor","LU_NatLnd","LU_PltFor","LU_PriFor","NotRel",
                                   "WetLnd","OthAgri"),
                         direction = "long")
View(Land_IDCno_2010)
land_IDCno_2050<-result_Land_IDCno[c("colrow","PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd",
                                     "LU_CrpLnd_2050","LU_GrsLnd_2050" ,"LU_MngFor_2050","LU_NatLnd_2050", 
                                     "LU_PltFor_2050" , "LU_PriFor_2050","NotRel","WetLnd","OthAgri")]
names(land_IDCno_2050)<-c("colrow","PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd",
                          "LU_CrpLnd","LU_GrsLnd" ,"LU_MngFor","LU_NatLnd", 
                          "LU_PltFor" , "LU_PriFor","NotRel","WetLnd","OthAgri")
Land_IDCno_2050<-reshape(land_IDCno_2050, 
                         varying = c("PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd","LU_CrpLnd",
                                     "LU_GrsLnd","LU_MngFor","LU_NatLnd","LU_PltFor","LU_PriFor",
                                     "NotRel", "WetLnd","OthAgri"), 
                         v.names = "lc_area",
                         timevar = "lulc", 
                         times = c("PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd","LU_CrpLnd",
                                   "LU_GrsLnd","LU_MngFor","LU_NatLnd","LU_PltFor","LU_PriFor",
                                   "NotRel","WetLnd","OthAgri"),
                         direction = "long")
View(Land_IDCno_2050)

colrow<-result_Land_IDCno[c("colrow","SimUArea")]
names(colrow)<-c("colrow","CRArea")
names(colrow)
names(land_IDCno)


setwd(outputpath)
write.csv(colrow, file="colrow.csv", row.names=FALSE, quote=FALSE)
write.csv(Land_IDCno_2010, file="Land_IDCno_2010.csv", row.names=FALSE, quote=FALSE)
write.csv(Land_IDCno_2050, file="Land_IDCno_2050.csv", row.names=FALSE, quote=FALSE)
Land_IDCno_2010<-read.csv("Land_IDCno_2010.csv")
###### 2.c Land Use values from IDCamazon_LC3.CSV ####
setwd(inputpath)
IDCamazon_LC3 = read.csv(filename_IDCamazon_LC3, header=FALSE)
names(IDCamazon_LC3) = c("country","colrow","altitude","slope","soil","aezclass",
                     "landuselu","noidea1","noidea2","scenario","year","value")
View(IDCamazon_LC3)
levels(IDCamazon_LC3$landuselu)
aggregated_IDCamazon_LC3 = aggregate(value~colrow+landuselu+year,
                                 data=IDCamazon_LC3, FUN=sum)

result_IDCamazon_LC3 = data.frame(unique(aggregated_IDCamazon_LC3$colrow))
names(result_IDCamazon_LC3) = "colrow"
counter = 0
for (landuse in unique(aggregated_IDCamazon_LC3$landuselu)){
  print(paste("Going through land use class", landuse))
  for (year in c(2000,2010,2020,2030,2040,2050)){
    print(paste("Going through year", year))
    colname = paste("LU",landuse,year, sep="_")
    print(colname)
    condition = (aggregated_IDCamazon_LC3$landuselu==landuse) & (aggregated_IDCamazon_LC3$year==year)
    col = aggregated_IDCamazon_LC3[condition,c("colrow","value")]
    names(col)=c("colrow",colname)
    print(head(col))
    if (nrow(col)>0) result_IDCamazon_LC3 = merge(result_IDCamazon_LC3, col, by="colrow", all=TRUE)
    counter = counter+1
    #if (counter>5) break()
  }
  #if (counter>5) break()
}
result_IDCamazon_LC3[is.na(result_IDCamazon_LC3)] = 0

if(length(unique(IDCamazon_LC3$colrow)) != nrow(result_IDCamazon_LC3)) stop("Problem with transforming the GAMS output of 'IDCamazon_LC3.csv' into table format. Too few ColRows!")

# store
setwd(outputpath)
save(result_IDCamazon_LC3, file="result_IDCamazon_LC3.RData")

### Combine land uses/covers ###
# We use the ColRows of the Table_Land. If other tables have more ColRows,
# they are discarded. If other tables have less ColRows, they gaps are filled with zeros.
result_Land_IDCamazon = data.frame(unique(result_IDCamazon_LC3$colrow))
names(result_Land_IDCamazon) = "colrow"
result_Land_IDCamazon = merge(result_Land_IDCamazon, result_PAS, by="colrow", all=TRUE)
result_Land_IDCamazon = merge(result_Land_IDCamazon, result_LANDCOVER_INIT, by="colrow", all=TRUE)
result_Land_IDCamazon = merge(result_Land_IDCamazon, result_IDCamazon_LC3, by="colrow", all=TRUE)
#ok

names(result_Land_IDCamazon)

# Sort the rows
result_Land_IDCamazon=result_Land_IDCamazon[order(result_Land_IDCamazon$colrow),]

# Replace NA by zeros
#There is NAs because the different files have different numbers of ColRows
nrow(result_PAS) # [1] 2105
nrow(result_IDCamazon_LC3) # [1] 2934
nrow(result_Land_IDCamazon) # [1] 3001
result_Land_IDCamazon[is.na(result_Land_IDCamazon)] = 0

result_Land_IDCamazon$SimUArea<-ifelse((result_Land_IDCamazon$LCI_SimUarea-(result_Land_IDCamazon$PA_SimUarea+
                                                                      result_Land_IDCamazon$LU_CrpLnd_2000+result_Land_IDCamazon$LU_GrsLnd_2000+result_Land_IDCamazon$LU_MngFor_2000+
                                                                      result_Land_IDCamazon$LU_NatLnd_2000+result_Land_IDCamazon$LU_PltFor_2000+
                                                                      result_Land_IDCamazon$LU_PriFor_2000))>=0,result_Land_IDCamazon$LCI_SimUarea,
                                   (result_Land_IDCamazon$LU_CrpLnd_2000+result_Land_IDCamazon$LU_GrsLnd_2000+result_Land_IDCamazon$LU_MngFor_2000+
                                      result_Land_IDCamazon$LU_NatLnd_2000+result_Land_IDCamazon$LU_PltFor_2000+result_Land_IDCamazon$LU_PriFor_2000+
                                      result_Land_IDCamazon$PA_SimUarea))

result_Land_IDCamazon$NotRel<-ifelse(
  (result_Land_IDCamazon$LCI_SimUarea-(result_Land_IDCamazon$PA_SimUarea+result_Land_IDCamazon$LU_CrpLnd_2000+
                                     result_Land_IDCamazon$LU_GrsLnd_2000+result_Land_IDCamazon$LU_MngFor_2000+
                                     result_Land_IDCamazon$LU_NatLnd_2000+result_Land_IDCamazon$LU_PltFor_2000+
                                     result_Land_IDCamazon$LU_PriFor_2000))<=0,0,
  ifelse(
    result_Land_IDCamazon$LCI_SimUarea-(result_Land_IDCamazon$LU_CrpLnd_2000+result_Land_IDCamazon$LU_GrsLnd_2000+
                                      result_Land_IDCamazon$LU_MngFor_2000+result_Land_IDCamazon$LU_NatLnd_2000+
                                      result_Land_IDCamazon$LU_PltFor_2000+result_Land_IDCamazon$LU_PriFor_2000+
                                      result_Land_IDCamazon$PA_SimUarea)>result_Land_IDCamazon$LCI_NotRel,
    result_Land_IDCamazon$LCI_NotRel,
    result_Land_IDCamazon$LCI_SimUarea-(result_Land_IDCamazon$LU_CrpLnd_2000+result_Land_IDCamazon$LU_GrsLnd_2000+
                                      result_Land_IDCamazon$LU_MngFor_2000+result_Land_IDCamazon$LU_NatLnd_2000+
                                      result_Land_IDCamazon$LU_PltFor_2000+result_Land_IDCamazon$LU_PriFor_2000+
                                      result_Land_IDCamazon$PA_SimUarea)))

result_Land_IDCamazon$WetLnd<-ifelse(
  (result_Land_IDCamazon$LCI_SimUarea-(result_Land_IDCamazon$PA_SimUarea+result_Land_IDCamazon$LU_CrpLnd_2000+
                                     result_Land_IDCamazon$LU_GrsLnd_2000+result_Land_IDCamazon$LU_MngFor_2000+
                                     result_Land_IDCamazon$LU_NatLnd_2000+result_Land_IDCamazon$LU_PltFor_2000+
                                     result_Land_IDCamazon$LU_PriFor_2000+ result_Land_IDCamazon$NotRel))<=0,0,
  ifelse(
    result_Land_IDCamazon$LCI_SimUarea-(result_Land_IDCamazon$LU_CrpLnd_2000+result_Land_IDCamazon$LU_GrsLnd_2000+
                                      result_Land_IDCamazon$LU_MngFor_2000+result_Land_IDCamazon$LU_NatLnd_2000+
                                      result_Land_IDCamazon$LU_PltFor_2000+result_Land_IDCamazon$LU_PriFor_2000+
                                      result_Land_IDCamazon$PA_SimUarea+
                                      result_Land_IDCamazon$NotRel)>result_Land_IDCamazon$LCI_WetLnd,
    result_Land_IDCamazon$LCI_WetLnd,
    result_Land_IDCamazon$LCI_SimUarea-(result_Land_IDCamazon$LU_CrpLnd_2000+result_Land_IDCamazon$LU_GrsLnd_2000+
                                      result_Land_IDCamazon$LU_MngFor_2000+result_Land_IDCamazon$LU_NatLnd_2000+
                                      result_Land_IDCamazon$LU_PltFor_2000+result_Land_IDCamazon$LU_PriFor_2000+
                                      result_Land_IDCamazon$PA_SimUarea+result_Land_IDCamazon$NotRel)))
result_Land_IDCamazon$OthAgri<-ifelse(
  (result_Land_IDCamazon$LCI_SimUarea-(result_Land_IDCamazon$PA_SimUarea+result_Land_IDCamazon$LU_CrpLnd_2000+
                                     result_Land_IDCamazon$LU_GrsLnd_2000+result_Land_IDCamazon$LU_MngFor_2000+
                                     result_Land_IDCamazon$LU_NatLnd_2000+result_Land_IDCamazon$LU_PltFor_2000+
                                     result_Land_IDCamazon$LU_PriFor_2000+ result_Land_IDCamazon$NotRel+
                                     result_Land_IDCamazon$WetLnd))<=0,0,
  ifelse(
    result_Land_IDCamazon$LCI_SimUarea-(result_Land_IDCamazon$LU_CrpLnd_2000+result_Land_IDCamazon$LU_GrsLnd_2000+
                                      result_Land_IDCamazon$LU_MngFor_2000+result_Land_IDCamazon$LU_NatLnd_2000+
                                      result_Land_IDCamazon$LU_PltFor_2000+result_Land_IDCamazon$LU_PriFor_2000+
                                      result_Land_IDCamazon$PA_SimUarea+result_Land_IDCamazon$NotRel+
                                      result_Land_IDCamazon$WetLnd)>result_Land_IDCamazon$LCI_OthAgri,
    result_Land_IDCamazon$LCI_OthAgri,
    result_Land_IDCamazon$LCI_SimUarea-(result_Land_IDCamazon$LU_CrpLnd_2000+result_Land_IDCamazon$LU_GrsLnd_2000+
                                      result_Land_IDCamazon$LU_MngFor_2000+result_Land_IDCamazon$LU_NatLnd_2000+
                                      result_Land_IDCamazon$LU_PltFor_2000+result_Land_IDCamazon$LU_PriFor_2000+
                                      result_Land_IDCamazon$PA_SimUarea+result_Land_IDCamazon$NotRel+
                                      result_Land_IDCamazon$WetLnd)))

# Store
names(result_Land_IDCamazon)
land_IDCamazon<-result_Land_IDCamazon[c("colrow","PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd",
                                "LU_CrpLnd_2000", "LU_CrpLnd_2010","LU_CrpLnd_2050",
                                "LU_GrsLnd_2000","LU_GrsLnd_2010" ,"LU_GrsLnd_2050",
                                "LU_MngFor_2000", "LU_MngFor_2010","LU_MngFor_2050",
                                "LU_NatLnd_2000", "LU_NatLnd_2010", "LU_NatLnd_2050",
                                "LU_PltFor_2000", "LU_PltFor_2010" , "LU_PltFor_2050",
                                "LU_PriFor_2000", "LU_PriFor_2010", "LU_PriFor_2050",
                                "SimUArea","NotRel","WetLnd","OthAgri")]

names(land_IDCamazon)
land_IDCamazon_2010<-result_Land_IDCamazon[c("colrow","PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd",
                                     "LU_CrpLnd_2010","LU_GrsLnd_2010" ,"LU_MngFor_2010","LU_NatLnd_2010", 
                                     "LU_PltFor_2010" , "LU_PriFor_2010",
                                     "SimUArea","NotRel","WetLnd","OthAgri")]
names(land_IDCamazon_2010)<-c("colrow","PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd",
                          "LU_CrpLnd","LU_GrsLnd" ,"LU_MngFor","LU_NatLnd", 
                          "LU_PltFor" , "LU_PriFor",
                          "SimUArea","NotRel","WetLnd","OthAgri")

Land_IDCamazon_2010<-reshape(land_IDCamazon_2010, 
                         varying = c("PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd","LU_CrpLnd",
                                     "LU_GrsLnd","LU_MngFor","LU_NatLnd","LU_PltFor","LU_PriFor","NotRel",
                                     "WetLnd","OthAgri"), 
                         v.names = "lc_area",
                         timevar = "lulc", 
                         times = c("PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd","LU_CrpLnd",
                                   "LU_GrsLnd","LU_MngFor","LU_NatLnd","LU_PltFor","LU_PriFor","NotRel",
                                   "WetLnd","OthAgri"),
                         direction = "long")
View(Land_IDCamazon_2010)
land_IDCamazon_2050<-result_Land_IDCamazon[c("colrow","PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd",
                                     "LU_CrpLnd_2050","LU_GrsLnd_2050" ,"LU_MngFor_2050","LU_NatLnd_2050", 
                                     "LU_PltFor_2050" , "LU_PriFor_2050","NotRel","WetLnd","OthAgri")]
names(land_IDCamazon_2050)<-c("colrow","PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd",
                          "LU_CrpLnd","LU_GrsLnd" ,"LU_MngFor","LU_NatLnd", 
                          "LU_PltFor" , "LU_PriFor","NotRel","WetLnd","OthAgri")
Land_IDCamazon_2050<-reshape(land_IDCamazon_2050, 
                         varying = c("PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd","LU_CrpLnd",
                                     "LU_GrsLnd","LU_MngFor","LU_NatLnd","LU_PltFor","LU_PriFor",
                                     "NotRel", "WetLnd","OthAgri"), 
                         v.names = "lc_area",
                         timevar = "lulc", 
                         times = c("PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd","LU_CrpLnd",
                                   "LU_GrsLnd","LU_MngFor","LU_NatLnd","LU_PltFor","LU_PriFor",
                                   "NotRel","WetLnd","OthAgri"),
                         direction = "long")
View(Land_IDCamazon_2050)

colrow<-result_Land_IDCamazon[c("colrow","SimUArea")]
names(colrow)<-c("colrow","CRArea")
names(colrow)
names(land_IDCamazon)


setwd(outputpath)
write.csv(colrow, file="colrow.csv", row.names=FALSE, quote=FALSE)
write.csv(Land_IDCamazon_2010, file="Land_IDCamazon_2010.csv", row.names=FALSE, quote=FALSE)
write.csv(Land_IDCamazon_2050, file="Land_IDCamazon_2050.csv", row.names=FALSE, quote=FALSE)
Land_IDCamazon_2010<-read.csv("Land_IDCamazon_2010.csv")
###### 2.c Land Use values from IDCbrazil_LC3.CSV ####
setwd(inputpath)
IDCbrazil_LC3 = read.csv(filename_IDCbrazil_LC3, header=FALSE)
names(IDCbrazil_LC3) = c("country","colrow","altitude","slope","soil","aezclass",
                     "landuselu","noidea1","noidea2","scenario","year","value")
View(IDCbrazil_LC3)
levels(IDCbrazil_LC3$landuselu)
aggregated_IDCbrazil_LC3 = aggregate(value~colrow+landuselu+year,
                                 data=IDCbrazil_LC3, FUN=sum)

result_IDCbrazil_LC3 = data.frame(unique(aggregated_IDCbrazil_LC3$colrow))
names(result_IDCbrazil_LC3) = "colrow"
counter = 0
for (landuse in unique(aggregated_IDCbrazil_LC3$landuselu)){
  print(paste("Going through land use class", landuse))
  for (year in c(2000,2010,2020,2030,2040,2050)){
    print(paste("Going through year", year))
    colname = paste("LU",landuse,year, sep="_")
    print(colname)
    condition = (aggregated_IDCbrazil_LC3$landuselu==landuse) & (aggregated_IDCbrazil_LC3$year==year)
    col = aggregated_IDCbrazil_LC3[condition,c("colrow","value")]
    names(col)=c("colrow",colname)
    print(head(col))
    if (nrow(col)>0) result_IDCbrazil_LC3 = merge(result_IDCbrazil_LC3, col, by="colrow", all=TRUE)
    counter = counter+1
    #if (counter>5) break()
  }
  #if (counter>5) break()
}
result_IDCbrazil_LC3[is.na(result_IDCbrazil_LC3)] = 0

if(length(unique(IDCbrazil_LC3$colrow)) != nrow(result_IDCbrazil_LC3)) stop("Problem with transforming the GAMS output of 'IDCbrazil_LC3.csv' into table format. Too few ColRows!")

# store
setwd(outputpath)
save(result_IDCbrazil_LC3, file="result_IDCbrazil_LC3.RData")

### Combine land uses/covers ###
# We use the ColRows of the Table_Land. If other tables have more ColRows,
# they are discarded. If other tables have less ColRows, they gaps are filled with zeros.
result_Land_IDCbrazil = data.frame(unique(result_IDCbrazil_LC3$colrow))
names(result_Land_IDCbrazil) = "colrow"
result_Land_IDCbrazil = merge(result_Land_IDCbrazil, result_PAS, by="colrow", all=TRUE)
result_Land_IDCbrazil = merge(result_Land_IDCbrazil, result_LANDCOVER_INIT, by="colrow", all=TRUE)
result_Land_IDCbrazil = merge(result_Land_IDCbrazil, result_IDCbrazil_LC3, by="colrow", all=TRUE)
#ok

names(result_Land_IDCbrazil)

# Sort the rows
result_Land_IDCbrazil=result_Land_IDCbrazil[order(result_Land_IDCbrazil$colrow),]

# Replace NA by zeros
#There is NAs because the different files have different numbers of ColRows
nrow(result_PAS) # [1] 2105
nrow(result_IDCbrazil_LC3) # [1] 2934
nrow(result_Land_IDCbrazil) # [1] 3001
result_Land_IDCbrazil[is.na(result_Land_IDCbrazil)] = 0

result_Land_IDCbrazil$SimUArea<-ifelse((result_Land_IDCbrazil$LCI_SimUarea-(result_Land_IDCbrazil$PA_SimUarea+
                                                                      result_Land_IDCbrazil$LU_CrpLnd_2000+result_Land_IDCbrazil$LU_GrsLnd_2000+result_Land_IDCbrazil$LU_MngFor_2000+
                                                                      result_Land_IDCbrazil$LU_NatLnd_2000+result_Land_IDCbrazil$LU_PltFor_2000+
                                                                      result_Land_IDCbrazil$LU_PriFor_2000))>=0,result_Land_IDCbrazil$LCI_SimUarea,
                                   (result_Land_IDCbrazil$LU_CrpLnd_2000+result_Land_IDCbrazil$LU_GrsLnd_2000+result_Land_IDCbrazil$LU_MngFor_2000+
                                      result_Land_IDCbrazil$LU_NatLnd_2000+result_Land_IDCbrazil$LU_PltFor_2000+result_Land_IDCbrazil$LU_PriFor_2000+
                                      result_Land_IDCbrazil$PA_SimUarea))

result_Land_IDCbrazil$NotRel<-ifelse(
  (result_Land_IDCbrazil$LCI_SimUarea-(result_Land_IDCbrazil$PA_SimUarea+result_Land_IDCbrazil$LU_CrpLnd_2000+
                                     result_Land_IDCbrazil$LU_GrsLnd_2000+result_Land_IDCbrazil$LU_MngFor_2000+
                                     result_Land_IDCbrazil$LU_NatLnd_2000+result_Land_IDCbrazil$LU_PltFor_2000+
                                     result_Land_IDCbrazil$LU_PriFor_2000))<=0,0,
  ifelse(
    result_Land_IDCbrazil$LCI_SimUarea-(result_Land_IDCbrazil$LU_CrpLnd_2000+result_Land_IDCbrazil$LU_GrsLnd_2000+
                                      result_Land_IDCbrazil$LU_MngFor_2000+result_Land_IDCbrazil$LU_NatLnd_2000+
                                      result_Land_IDCbrazil$LU_PltFor_2000+result_Land_IDCbrazil$LU_PriFor_2000+
                                      result_Land_IDCbrazil$PA_SimUarea)>result_Land_IDCbrazil$LCI_NotRel,
    result_Land_IDCbrazil$LCI_NotRel,
    result_Land_IDCbrazil$LCI_SimUarea-(result_Land_IDCbrazil$LU_CrpLnd_2000+result_Land_IDCbrazil$LU_GrsLnd_2000+
                                      result_Land_IDCbrazil$LU_MngFor_2000+result_Land_IDCbrazil$LU_NatLnd_2000+
                                      result_Land_IDCbrazil$LU_PltFor_2000+result_Land_IDCbrazil$LU_PriFor_2000+
                                      result_Land_IDCbrazil$PA_SimUarea)))

result_Land_IDCbrazil$WetLnd<-ifelse(
  (result_Land_IDCbrazil$LCI_SimUarea-(result_Land_IDCbrazil$PA_SimUarea+result_Land_IDCbrazil$LU_CrpLnd_2000+
                                     result_Land_IDCbrazil$LU_GrsLnd_2000+result_Land_IDCbrazil$LU_MngFor_2000+
                                     result_Land_IDCbrazil$LU_NatLnd_2000+result_Land_IDCbrazil$LU_PltFor_2000+
                                     result_Land_IDCbrazil$LU_PriFor_2000+ result_Land_IDCbrazil$NotRel))<=0,0,
  ifelse(
    result_Land_IDCbrazil$LCI_SimUarea-(result_Land_IDCbrazil$LU_CrpLnd_2000+result_Land_IDCbrazil$LU_GrsLnd_2000+
                                      result_Land_IDCbrazil$LU_MngFor_2000+result_Land_IDCbrazil$LU_NatLnd_2000+
                                      result_Land_IDCbrazil$LU_PltFor_2000+result_Land_IDCbrazil$LU_PriFor_2000+
                                      result_Land_IDCbrazil$PA_SimUarea+
                                      result_Land_IDCbrazil$NotRel)>result_Land_IDCbrazil$LCI_WetLnd,
    result_Land_IDCbrazil$LCI_WetLnd,
    result_Land_IDCbrazil$LCI_SimUarea-(result_Land_IDCbrazil$LU_CrpLnd_2000+result_Land_IDCbrazil$LU_GrsLnd_2000+
                                      result_Land_IDCbrazil$LU_MngFor_2000+result_Land_IDCbrazil$LU_NatLnd_2000+
                                      result_Land_IDCbrazil$LU_PltFor_2000+result_Land_IDCbrazil$LU_PriFor_2000+
                                      result_Land_IDCbrazil$PA_SimUarea+result_Land_IDCbrazil$NotRel)))
result_Land_IDCbrazil$OthAgri<-ifelse(
  (result_Land_IDCbrazil$LCI_SimUarea-(result_Land_IDCbrazil$PA_SimUarea+result_Land_IDCbrazil$LU_CrpLnd_2000+
                                     result_Land_IDCbrazil$LU_GrsLnd_2000+result_Land_IDCbrazil$LU_MngFor_2000+
                                     result_Land_IDCbrazil$LU_NatLnd_2000+result_Land_IDCbrazil$LU_PltFor_2000+
                                     result_Land_IDCbrazil$LU_PriFor_2000+ result_Land_IDCbrazil$NotRel+
                                     result_Land_IDCbrazil$WetLnd))<=0,0,
  ifelse(
    result_Land_IDCbrazil$LCI_SimUarea-(result_Land_IDCbrazil$LU_CrpLnd_2000+result_Land_IDCbrazil$LU_GrsLnd_2000+
                                      result_Land_IDCbrazil$LU_MngFor_2000+result_Land_IDCbrazil$LU_NatLnd_2000+
                                      result_Land_IDCbrazil$LU_PltFor_2000+result_Land_IDCbrazil$LU_PriFor_2000+
                                      result_Land_IDCbrazil$PA_SimUarea+result_Land_IDCbrazil$NotRel+
                                      result_Land_IDCbrazil$WetLnd)>result_Land_IDCbrazil$LCI_OthAgri,
    result_Land_IDCbrazil$LCI_OthAgri,
    result_Land_IDCbrazil$LCI_SimUarea-(result_Land_IDCbrazil$LU_CrpLnd_2000+result_Land_IDCbrazil$LU_GrsLnd_2000+
                                      result_Land_IDCbrazil$LU_MngFor_2000+result_Land_IDCbrazil$LU_NatLnd_2000+
                                      result_Land_IDCbrazil$LU_PltFor_2000+result_Land_IDCbrazil$LU_PriFor_2000+
                                      result_Land_IDCbrazil$PA_SimUarea+result_Land_IDCbrazil$NotRel+
                                      result_Land_IDCbrazil$WetLnd)))

# Store
names(result_Land_IDCbrazil)
land_IDCbrazil<-result_Land_IDCbrazil[c("colrow","PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd",
                                "LU_CrpLnd_2000", "LU_CrpLnd_2010","LU_CrpLnd_2050",
                                "LU_GrsLnd_2000","LU_GrsLnd_2010" ,"LU_GrsLnd_2050",
                                "LU_MngFor_2000", "LU_MngFor_2010","LU_MngFor_2050",
                                "LU_NatLnd_2000", "LU_NatLnd_2010", "LU_NatLnd_2050",
                                "LU_PltFor_2000", "LU_PltFor_2010" , "LU_PltFor_2050",
                                "LU_PriFor_2000", "LU_PriFor_2010", "LU_PriFor_2050",
                                "SimUArea","NotRel","WetLnd","OthAgri")]

names(land_IDCbrazil)
land_IDCbrazil_2010<-result_Land_IDCbrazil[c("colrow","PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd",
                                     "LU_CrpLnd_2010","LU_GrsLnd_2010" ,"LU_MngFor_2010","LU_NatLnd_2010", 
                                     "LU_PltFor_2010" , "LU_PriFor_2010",
                                     "SimUArea","NotRel","WetLnd","OthAgri")]
names(land_IDCbrazil_2010)<-c("colrow","PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd",
                          "LU_CrpLnd","LU_GrsLnd" ,"LU_MngFor","LU_NatLnd", 
                          "LU_PltFor" , "LU_PriFor",
                          "SimUArea","NotRel","WetLnd","OthAgri")

Land_IDCbrazil_2010<-reshape(land_IDCbrazil_2010, 
                         varying = c("PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd","LU_CrpLnd",
                                     "LU_GrsLnd","LU_MngFor","LU_NatLnd","LU_PltFor","LU_PriFor","NotRel",
                                     "WetLnd","OthAgri"), 
                         v.names = "lc_area",
                         timevar = "lulc", 
                         times = c("PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd","LU_CrpLnd",
                                   "LU_GrsLnd","LU_MngFor","LU_NatLnd","LU_PltFor","LU_PriFor","NotRel",
                                   "WetLnd","OthAgri"),
                         direction = "long")
View(Land_IDCbrazil_2010)
land_IDCbrazil_2050<-result_Land_IDCbrazil[c("colrow","PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd",
                                     "LU_CrpLnd_2050","LU_GrsLnd_2050" ,"LU_MngFor_2050","LU_NatLnd_2050", 
                                     "LU_PltFor_2050" , "LU_PriFor_2050","NotRel","WetLnd","OthAgri")]
names(land_IDCbrazil_2050)<-c("colrow","PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd",
                          "LU_CrpLnd","LU_GrsLnd" ,"LU_MngFor","LU_NatLnd", 
                          "LU_PltFor" , "LU_PriFor","NotRel","WetLnd","OthAgri")
Land_IDCbrazil_2050<-reshape(land_IDCbrazil_2050, 
                         varying = c("PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd","LU_CrpLnd",
                                     "LU_GrsLnd","LU_MngFor","LU_NatLnd","LU_PltFor","LU_PriFor",
                                     "NotRel", "WetLnd","OthAgri"), 
                         v.names = "lc_area",
                         timevar = "lulc", 
                         times = c("PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd","LU_CrpLnd",
                                   "LU_GrsLnd","LU_MngFor","LU_NatLnd","LU_PltFor","LU_PriFor",
                                   "NotRel","WetLnd","OthAgri"),
                         direction = "long")
View(Land_IDCbrazil_2050)

colrow<-result_Land_IDCbrazil[c("colrow","SimUArea")]
names(colrow)<-c("colrow","CRArea")
names(colrow)
names(land_IDCbrazil)


setwd(outputpath)
write.csv(colrow, file="colrow.csv", row.names=FALSE, quote=FALSE)
write.csv(Land_IDCbrazil_2010, file="Land_IDCbrazil_2010.csv", row.names=FALSE, quote=FALSE)
write.csv(Land_IDCbrazil_2050, file="Land_IDCbrazil_2050.csv", row.names=FALSE, quote=FALSE)
Land_IDCbrazil_2010<-read.csv("Land_IDCbrazil_2010.csv")
###### 2.d Land Use values from FC_LC3.CSV #####
setwd(inputpath)
FC_LC3 = read.csv(filename_FC_LC3, header=FALSE)
names(FC_LC3) = c("country","colrow","altitude","slope","soil","aezclass",
                     "landuselu","noidea1","noidea2","scenario","year","value")
View(FC_LC3)
levels(FC_LC3$landuselu)
aggregated_FC_LC3 = aggregate(value~colrow+landuselu+year,
                                 data=FC_LC3, FUN=sum)

result_FC_LC3 = data.frame(unique(aggregated_FC_LC3$colrow))
names(result_FC_LC3) = "colrow"
counter = 0
for (landuse in unique(aggregated_FC_LC3$landuselu)){
  print(paste("Going through land use class", landuse))
  for (year in c(2000,2010,2020,2030,2040,2050)){
    print(paste("Going through year", year))
    colname = paste("LU",landuse,year, sep="_")
    print(colname)
    condition = (aggregated_FC_LC3$landuselu==landuse) & (aggregated_FC_LC3$year==year)
    col = aggregated_FC_LC3[condition,c("colrow","value")]
    names(col)=c("colrow",colname)
    print(head(col))
    if (nrow(col)>0) result_FC_LC3 = merge(result_FC_LC3, col, by="colrow", all=TRUE)
    counter = counter+1
    #if (counter>5) break()
  }
  #if (counter>5) break()
}
result_FC_LC3[is.na(result_FC_LC3)] = 0

if(length(unique(FC_LC3$colrow)) != nrow(result_FC_LC3)) stop("Problem with transforming the GAMS output of 'FC_LC3.csv' into table format. Too few ColRows!")

# store
setwd(outputpath)
save(result_FC_LC3, file="result_FC_LC3.RData")

### Combine land uses/covers ###
# We use the ColRows of the Table_Land. If other tables have more ColRows,
# they are discarded. If other tables have less ColRows, they gaps are filled with zeros.
result_Land_FC = data.frame(unique(result_FC_LC3$colrow))
names(result_Land_FC) = "colrow"
result_Land_FC = merge(result_Land_FC, result_PAS, by="colrow", all=TRUE)
result_Land_FC = merge(result_Land_FC, result_LANDCOVER_INIT, by="colrow", all=TRUE)
result_Land_FC = merge(result_Land_FC, result_FC_LC3, by="colrow", all=TRUE)
#ok

names(result_Land_FC)

# Sort the rows
result_Land_FC=result_Land_FC[order(result_Land_FC$colrow),]

# Replace NA by zeros
#There is NAs because the different files have different numbers of ColRows
nrow(result_PAS) # [1] 2105
nrow(result_FC_LC3) # [1] 2934
nrow(result_Land_FC) # [1] 3001
result_Land_FC[is.na(result_Land_FC)] = 0

result_Land_FC$SimUArea<-ifelse((result_Land_FC$LCI_SimUarea-(result_Land_FC$PA_SimUarea+
                                                                      result_Land_FC$LU_CrpLnd_2000+result_Land_FC$LU_GrsLnd_2000+result_Land_FC$LU_MngFor_2000+
                                                                      result_Land_FC$LU_NatLnd_2000+result_Land_FC$LU_PltFor_2000+
                                                                      result_Land_FC$LU_PriFor_2000))>=0,result_Land_FC$LCI_SimUarea,
                                   (result_Land_FC$LU_CrpLnd_2000+result_Land_FC$LU_GrsLnd_2000+result_Land_FC$LU_MngFor_2000+
                                      result_Land_FC$LU_NatLnd_2000+result_Land_FC$LU_PltFor_2000+result_Land_FC$LU_PriFor_2000+
                                      result_Land_FC$PA_SimUarea))

result_Land_FC$NotRel<-ifelse(
  (result_Land_FC$LCI_SimUarea-(result_Land_FC$PA_SimUarea+result_Land_FC$LU_CrpLnd_2000+
                                     result_Land_FC$LU_GrsLnd_2000+result_Land_FC$LU_MngFor_2000+
                                     result_Land_FC$LU_NatLnd_2000+result_Land_FC$LU_PltFor_2000+
                                     result_Land_FC$LU_PriFor_2000))<=0,0,
  ifelse(
    result_Land_FC$LCI_SimUarea-(result_Land_FC$LU_CrpLnd_2000+result_Land_FC$LU_GrsLnd_2000+
                                      result_Land_FC$LU_MngFor_2000+result_Land_FC$LU_NatLnd_2000+
                                      result_Land_FC$LU_PltFor_2000+result_Land_FC$LU_PriFor_2000+
                                      result_Land_FC$PA_SimUarea)>result_Land_FC$LCI_NotRel,
    result_Land_FC$LCI_NotRel,
    result_Land_FC$LCI_SimUarea-(result_Land_FC$LU_CrpLnd_2000+result_Land_FC$LU_GrsLnd_2000+
                                      result_Land_FC$LU_MngFor_2000+result_Land_FC$LU_NatLnd_2000+
                                      result_Land_FC$LU_PltFor_2000+result_Land_FC$LU_PriFor_2000+
                                      result_Land_FC$PA_SimUarea)))

result_Land_FC$WetLnd<-ifelse(
  (result_Land_FC$LCI_SimUarea-(result_Land_FC$PA_SimUarea+result_Land_FC$LU_CrpLnd_2000+
                                     result_Land_FC$LU_GrsLnd_2000+result_Land_FC$LU_MngFor_2000+
                                     result_Land_FC$LU_NatLnd_2000+result_Land_FC$LU_PltFor_2000+
                                     result_Land_FC$LU_PriFor_2000+ result_Land_FC$NotRel))<=0,0,
  ifelse(
    result_Land_FC$LCI_SimUarea-(result_Land_FC$LU_CrpLnd_2000+result_Land_FC$LU_GrsLnd_2000+
                                      result_Land_FC$LU_MngFor_2000+result_Land_FC$LU_NatLnd_2000+
                                      result_Land_FC$LU_PltFor_2000+result_Land_FC$LU_PriFor_2000+
                                      result_Land_FC$PA_SimUarea+
                                      result_Land_FC$NotRel)>result_Land_FC$LCI_WetLnd,
    result_Land_FC$LCI_WetLnd,
    result_Land_FC$LCI_SimUarea-(result_Land_FC$LU_CrpLnd_2000+result_Land_FC$LU_GrsLnd_2000+
                                      result_Land_FC$LU_MngFor_2000+result_Land_FC$LU_NatLnd_2000+
                                      result_Land_FC$LU_PltFor_2000+result_Land_FC$LU_PriFor_2000+
                                      result_Land_FC$PA_SimUarea+result_Land_FC$NotRel)))
result_Land_FC$OthAgri<-ifelse(
  (result_Land_FC$LCI_SimUarea-(result_Land_FC$PA_SimUarea+result_Land_FC$LU_CrpLnd_2000+
                                     result_Land_FC$LU_GrsLnd_2000+result_Land_FC$LU_MngFor_2000+
                                     result_Land_FC$LU_NatLnd_2000+result_Land_FC$LU_PltFor_2000+
                                     result_Land_FC$LU_PriFor_2000+ result_Land_FC$NotRel+
                                     result_Land_FC$WetLnd))<=0,0,
  ifelse(
    result_Land_FC$LCI_SimUarea-(result_Land_FC$LU_CrpLnd_2000+result_Land_FC$LU_GrsLnd_2000+
                                      result_Land_FC$LU_MngFor_2000+result_Land_FC$LU_NatLnd_2000+
                                      result_Land_FC$LU_PltFor_2000+result_Land_FC$LU_PriFor_2000+
                                      result_Land_FC$PA_SimUarea+result_Land_FC$NotRel+
                                      result_Land_FC$WetLnd)>result_Land_FC$LCI_OthAgri,
    result_Land_FC$LCI_OthAgri,
    result_Land_FC$LCI_SimUarea-(result_Land_FC$LU_CrpLnd_2000+result_Land_FC$LU_GrsLnd_2000+
                                      result_Land_FC$LU_MngFor_2000+result_Land_FC$LU_NatLnd_2000+
                                      result_Land_FC$LU_PltFor_2000+result_Land_FC$LU_PriFor_2000+
                                      result_Land_FC$PA_SimUarea+result_Land_FC$NotRel+
                                      result_Land_FC$WetLnd)))

# Store
names(result_Land_FC)
land_FC<-result_Land_FC[c("colrow","PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd",
                                "LU_CrpLnd_2000", "LU_CrpLnd_2010","LU_CrpLnd_2050",
                                "LU_GrsLnd_2000","LU_GrsLnd_2010" ,"LU_GrsLnd_2050",
                                "LU_MngFor_2000", "LU_MngFor_2010","LU_MngFor_2050",
                                "LU_NatLnd_2000", "LU_NatLnd_2010", "LU_NatLnd_2050",
                                "LU_PltFor_2000", "LU_PltFor_2010" , "LU_PltFor_2050",
                                "LU_PriFor_2000", "LU_PriFor_2010", "LU_PriFor_2050","LU_ForReg_2050",
                                "SimUArea","NotRel","WetLnd","OthAgri")]

names(land_FC)
land_FC_2010<-result_Land_FC[c("colrow","PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd",
                                     "LU_CrpLnd_2010","LU_GrsLnd_2010" ,"LU_MngFor_2010","LU_NatLnd_2010", 
                                     "LU_PltFor_2010" , "LU_PriFor_2010",
                                     "SimUArea","NotRel","WetLnd","OthAgri")]
names(land_FC_2010)<-c("colrow","PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd",
                          "LU_CrpLnd","LU_GrsLnd" ,"LU_MngFor","LU_NatLnd", 
                          "LU_PltFor" , "LU_PriFor",
                          "SimUArea","NotRel","WetLnd","OthAgri")

Land_FC_2010<-reshape(land_FC_2010, 
                         varying = c("PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd","LU_CrpLnd",
                                     "LU_GrsLnd","LU_MngFor","LU_NatLnd","LU_PltFor","LU_PriFor","NotRel",
                                     "WetLnd","OthAgri"), 
                         v.names = "lc_area",
                         timevar = "lulc", 
                         times = c("PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd","LU_CrpLnd",
                                   "LU_GrsLnd","LU_MngFor","LU_NatLnd","LU_PltFor","LU_PriFor","NotRel",
                                   "WetLnd","OthAgri"),
                         direction = "long")
View(Land_FC_2010)
names(result_Land_FC)
land_FC_2050<-result_Land_FC[c("colrow","PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd",
                                     "LU_CrpLnd_2050","LU_GrsLnd_2050" ,"LU_MngFor_2050","LU_NatLnd_2050", 
                                     "LU_PltFor_2050" , "LU_PriFor_2050","LU_ForReg_2050",
                               "NotRel","WetLnd","OthAgri")]
names(land_FC_2050)<-c("colrow","PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd",
                          "LU_CrpLnd","LU_GrsLnd" ,"LU_MngFor","LU_NatLnd", 
                          "LU_PltFor" , "LU_PriFor","LU_ForReg","NotRel","WetLnd","OthAgri")
Land_FC_2050<-reshape(land_FC_2050, 
                         varying = c("PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd","LU_CrpLnd",
                                     "LU_GrsLnd","LU_MngFor","LU_NatLnd","LU_PltFor","LU_PriFor","LU_ForReg",
                                     "NotRel", "WetLnd","OthAgri"), 
                         v.names = "lc_area",
                         timevar = "lulc", 
                         times = c("PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd","LU_CrpLnd",
                                   "LU_GrsLnd","LU_MngFor","LU_NatLnd","LU_PltFor","LU_PriFor","LU_ForReg",
                                   "NotRel","WetLnd","OthAgri"),
                         direction = "long")
View(Land_FC_2050)

colrow<-result_Land_FC[c("colrow","SimUArea")]
names(colrow)<-c("colrow","CRArea")
names(colrow)
names(land_FC)


setwd(outputpath)
write.csv(colrow, file="colrow.csv", row.names=FALSE, quote=FALSE)
write.csv(Land_FC_2010, file="Land_FC_2010.csv", row.names=FALSE, quote=FALSE)
write.csv(Land_FC_2050, file="Land_FC_2050.csv", row.names=FALSE, quote=FALSE)
Land_FC_2010<-read.csv("Land_FC_2010.csv")
###### 2.d Land Use values from FCnoSFA_LC3.CSV #####
setwd(inputpath)
FCnoSFA_LC3 = read.csv(filename_FCnoSFA_LC3, header=FALSE)
names(FCnoSFA_LC3) = c("country","colrow","altitude","slope","soil","aezclass",
                      "landuselu","noidea1","noidea2","scenario","year","value")
View(FCnoSFA_LC3)
levels(FCnoSFA_LC3$landuselu)
aggregated_FCnoSFA_LC3 = aggregate(value~colrow+landuselu+year,
                                  data=FCnoSFA_LC3, FUN=sum)

result_FCnoSFA_LC3 = data.frame(unique(aggregated_FCnoSFA_LC3$colrow))
names(result_FCnoSFA_LC3) = "colrow"
counter = 0
for (landuse in unique(aggregated_FCnoSFA_LC3$landuselu)){
  print(paste("Going through land use class", landuse))
  for (year in c(2000,2010,2020,2030,2040,2050)){
    print(paste("Going through year", year))
    colname = paste("LU",landuse,year, sep="_")
    print(colname)
    condition = (aggregated_FCnoSFA_LC3$landuselu==landuse) & (aggregated_FCnoSFA_LC3$year==year)
    col = aggregated_FCnoSFA_LC3[condition,c("colrow","value")]
    names(col)=c("colrow",colname)
    print(head(col))
    if (nrow(col)>0) result_FCnoSFA_LC3 = merge(result_FCnoSFA_LC3, col, by="colrow", all=TRUE)
    counter = counter+1
    #if (counter>5) break()
  }
  #if (counter>5) break()
}
result_FCnoSFA_LC3[is.na(result_FCnoSFA_LC3)] = 0

if(length(unique(FCnoSFA_LC3$colrow)) != nrow(result_FCnoSFA_LC3)) stop("Problem with transforming the GAMS output of 'FCnoSFA_LC3.csv' into table format. Too few ColRows!")

# store
setwd(outputpath)
save(result_FCnoSFA_LC3, file="result_FCnoSFA_LC3.RData")

### Combine land uses/covers ###
# We use the ColRows of the Table_Land. If other tables have more ColRows,
# they are discarded. If other tables have less ColRows, they gaps are filled with zeros.
result_Land_FCnoSFA = data.frame(unique(result_FCnoSFA_LC3$colrow))
names(result_Land_FCnoSFA) = "colrow"
result_Land_FCnoSFA = merge(result_Land_FCnoSFA, result_PAS, by="colrow", all=TRUE)
result_Land_FCnoSFA = merge(result_Land_FCnoSFA, result_LANDCOVER_INIT, by="colrow", all=TRUE)
result_Land_FCnoSFA = merge(result_Land_FCnoSFA, result_FCnoSFA_LC3, by="colrow", all=TRUE)
#ok

names(result_Land_FCnoSFA)

# Sort the rows
result_Land_FCnoSFA=result_Land_FCnoSFA[order(result_Land_FCnoSFA$colrow),]

# Replace NA by zeros
#There is NAs because the different files have different numbers of ColRows
nrow(result_PAS) # [1] 2105
nrow(result_FCnoSFA_LC3) # [1] 2934
nrow(result_Land_FCnoSFA) # [1] 3001
result_Land_FCnoSFA[is.na(result_Land_FCnoSFA)] = 0

result_Land_FCnoSFA$SimUArea<-ifelse((result_Land_FCnoSFA$LCI_SimUarea-(result_Land_FCnoSFA$PA_SimUarea+
                                                                        result_Land_FCnoSFA$LU_CrpLnd_2000+result_Land_FCnoSFA$LU_GrsLnd_2000+result_Land_FCnoSFA$LU_MngFor_2000+
                                                                        result_Land_FCnoSFA$LU_NatLnd_2000+result_Land_FCnoSFA$LU_PltFor_2000+
                                                                        result_Land_FCnoSFA$LU_PriFor_2000))>=0,result_Land_FCnoSFA$LCI_SimUarea,
                                    (result_Land_FCnoSFA$LU_CrpLnd_2000+result_Land_FCnoSFA$LU_GrsLnd_2000+result_Land_FCnoSFA$LU_MngFor_2000+
                                       result_Land_FCnoSFA$LU_NatLnd_2000+result_Land_FCnoSFA$LU_PltFor_2000+result_Land_FCnoSFA$LU_PriFor_2000+
                                       result_Land_FCnoSFA$PA_SimUarea))

result_Land_FCnoSFA$NotRel<-ifelse(
  (result_Land_FCnoSFA$LCI_SimUarea-(result_Land_FCnoSFA$PA_SimUarea+result_Land_FCnoSFA$LU_CrpLnd_2000+
                                      result_Land_FCnoSFA$LU_GrsLnd_2000+result_Land_FCnoSFA$LU_MngFor_2000+
                                      result_Land_FCnoSFA$LU_NatLnd_2000+result_Land_FCnoSFA$LU_PltFor_2000+
                                      result_Land_FCnoSFA$LU_PriFor_2000))<=0,0,
  ifelse(
    result_Land_FCnoSFA$LCI_SimUarea-(result_Land_FCnoSFA$LU_CrpLnd_2000+result_Land_FCnoSFA$LU_GrsLnd_2000+
                                       result_Land_FCnoSFA$LU_MngFor_2000+result_Land_FCnoSFA$LU_NatLnd_2000+
                                       result_Land_FCnoSFA$LU_PltFor_2000+result_Land_FCnoSFA$LU_PriFor_2000+
                                       result_Land_FCnoSFA$PA_SimUarea)>result_Land_FCnoSFA$LCI_NotRel,
    result_Land_FCnoSFA$LCI_NotRel,
    result_Land_FCnoSFA$LCI_SimUarea-(result_Land_FCnoSFA$LU_CrpLnd_2000+result_Land_FCnoSFA$LU_GrsLnd_2000+
                                       result_Land_FCnoSFA$LU_MngFor_2000+result_Land_FCnoSFA$LU_NatLnd_2000+
                                       result_Land_FCnoSFA$LU_PltFor_2000+result_Land_FCnoSFA$LU_PriFor_2000+
                                       result_Land_FCnoSFA$PA_SimUarea)))

result_Land_FCnoSFA$WetLnd<-ifelse(
  (result_Land_FCnoSFA$LCI_SimUarea-(result_Land_FCnoSFA$PA_SimUarea+result_Land_FCnoSFA$LU_CrpLnd_2000+
                                      result_Land_FCnoSFA$LU_GrsLnd_2000+result_Land_FCnoSFA$LU_MngFor_2000+
                                      result_Land_FCnoSFA$LU_NatLnd_2000+result_Land_FCnoSFA$LU_PltFor_2000+
                                      result_Land_FCnoSFA$LU_PriFor_2000+ result_Land_FCnoSFA$NotRel))<=0,0,
  ifelse(
    result_Land_FCnoSFA$LCI_SimUarea-(result_Land_FCnoSFA$LU_CrpLnd_2000+result_Land_FCnoSFA$LU_GrsLnd_2000+
                                       result_Land_FCnoSFA$LU_MngFor_2000+result_Land_FCnoSFA$LU_NatLnd_2000+
                                       result_Land_FCnoSFA$LU_PltFor_2000+result_Land_FCnoSFA$LU_PriFor_2000+
                                       result_Land_FCnoSFA$PA_SimUarea+
                                       result_Land_FCnoSFA$NotRel)>result_Land_FCnoSFA$LCI_WetLnd,
    result_Land_FCnoSFA$LCI_WetLnd,
    result_Land_FCnoSFA$LCI_SimUarea-(result_Land_FCnoSFA$LU_CrpLnd_2000+result_Land_FCnoSFA$LU_GrsLnd_2000+
                                       result_Land_FCnoSFA$LU_MngFor_2000+result_Land_FCnoSFA$LU_NatLnd_2000+
                                       result_Land_FCnoSFA$LU_PltFor_2000+result_Land_FCnoSFA$LU_PriFor_2000+
                                       result_Land_FCnoSFA$PA_SimUarea+result_Land_FCnoSFA$NotRel)))
result_Land_FCnoSFA$OthAgri<-ifelse(
  (result_Land_FCnoSFA$LCI_SimUarea-(result_Land_FCnoSFA$PA_SimUarea+result_Land_FCnoSFA$LU_CrpLnd_2000+
                                      result_Land_FCnoSFA$LU_GrsLnd_2000+result_Land_FCnoSFA$LU_MngFor_2000+
                                      result_Land_FCnoSFA$LU_NatLnd_2000+result_Land_FCnoSFA$LU_PltFor_2000+
                                      result_Land_FCnoSFA$LU_PriFor_2000+ result_Land_FCnoSFA$NotRel+
                                      result_Land_FCnoSFA$WetLnd))<=0,0,
  ifelse(
    result_Land_FCnoSFA$LCI_SimUarea-(result_Land_FCnoSFA$LU_CrpLnd_2000+result_Land_FCnoSFA$LU_GrsLnd_2000+
                                       result_Land_FCnoSFA$LU_MngFor_2000+result_Land_FCnoSFA$LU_NatLnd_2000+
                                       result_Land_FCnoSFA$LU_PltFor_2000+result_Land_FCnoSFA$LU_PriFor_2000+
                                       result_Land_FCnoSFA$PA_SimUarea+result_Land_FCnoSFA$NotRel+
                                       result_Land_FCnoSFA$WetLnd)>result_Land_FCnoSFA$LCI_OthAgri,
    result_Land_FCnoSFA$LCI_OthAgri,
    result_Land_FCnoSFA$LCI_SimUarea-(result_Land_FCnoSFA$LU_CrpLnd_2000+result_Land_FCnoSFA$LU_GrsLnd_2000+
                                       result_Land_FCnoSFA$LU_MngFor_2000+result_Land_FCnoSFA$LU_NatLnd_2000+
                                       result_Land_FCnoSFA$LU_PltFor_2000+result_Land_FCnoSFA$LU_PriFor_2000+
                                       result_Land_FCnoSFA$PA_SimUarea+result_Land_FCnoSFA$NotRel+
                                       result_Land_FCnoSFA$WetLnd)))

# Store
names(result_Land_FCnoSFA)
land_FCnoSFA<-result_Land_FCnoSFA[c("colrow","PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd",
                                  "LU_CrpLnd_2000", "LU_CrpLnd_2010","LU_CrpLnd_2050",
                                  "LU_GrsLnd_2000","LU_GrsLnd_2010" ,"LU_GrsLnd_2050",
                                  "LU_MngFor_2000", "LU_MngFor_2010","LU_MngFor_2050",
                                  "LU_NatLnd_2000", "LU_NatLnd_2010", "LU_NatLnd_2050",
                                  "LU_PltFor_2000", "LU_PltFor_2010" , "LU_PltFor_2050",
                                  "LU_PriFor_2000", "LU_PriFor_2010", "LU_PriFor_2050","LU_ForReg_2050",
                                  "SimUArea","NotRel","WetLnd","OthAgri")]

names(land_FCnoSFA)
land_FCnoSFA_2010<-result_Land_FCnoSFA[c("colrow","PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd",
                                       "LU_CrpLnd_2010","LU_GrsLnd_2010" ,"LU_MngFor_2010","LU_NatLnd_2010", 
                                       "LU_PltFor_2010" , "LU_PriFor_2010",
                                       "SimUArea","NotRel","WetLnd","OthAgri")]
names(land_FCnoSFA_2010)<-c("colrow","PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd",
                           "LU_CrpLnd","LU_GrsLnd" ,"LU_MngFor","LU_NatLnd", 
                           "LU_PltFor" , "LU_PriFor",
                           "SimUArea","NotRel","WetLnd","OthAgri")

Land_FCnoSFA_2010<-reshape(land_FCnoSFA_2010, 
                          varying = c("PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd","LU_CrpLnd",
                                      "LU_GrsLnd","LU_MngFor","LU_NatLnd","LU_PltFor","LU_PriFor","NotRel",
                                      "WetLnd","OthAgri"), 
                          v.names = "lc_area",
                          timevar = "lulc", 
                          times = c("PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd","LU_CrpLnd",
                                    "LU_GrsLnd","LU_MngFor","LU_NatLnd","LU_PltFor","LU_PriFor","NotRel",
                                    "WetLnd","OthAgri"),
                          direction = "long")
View(Land_FCnoSFA_2010)
names(result_Land_FCnoSFA)
land_FCnoSFA_2050<-result_Land_FCnoSFA[c("colrow","PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd",
                                       "LU_CrpLnd_2050","LU_GrsLnd_2050" ,"LU_MngFor_2050","LU_NatLnd_2050", 
                                       "LU_PltFor_2050" , "LU_PriFor_2050","LU_ForReg_2050",
                                       "NotRel","WetLnd","OthAgri")]
names(land_FCnoSFA_2050)<-c("colrow","PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd",
                           "LU_CrpLnd","LU_GrsLnd" ,"LU_MngFor","LU_NatLnd", 
                           "LU_PltFor" , "LU_PriFor","LU_ForReg","NotRel","WetLnd","OthAgri")
Land_FCnoSFA_2050<-reshape(land_FCnoSFA_2050, 
                          varying = c("PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd","LU_CrpLnd",
                                      "LU_GrsLnd","LU_MngFor","LU_NatLnd","LU_PltFor","LU_PriFor","LU_ForReg",
                                      "NotRel", "WetLnd","OthAgri"), 
                          v.names = "lc_area",
                          timevar = "lulc", 
                          times = c("PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd","LU_CrpLnd",
                                    "LU_GrsLnd","LU_MngFor","LU_NatLnd","LU_PltFor","LU_PriFor","LU_ForReg",
                                    "NotRel","WetLnd","OthAgri"),
                          direction = "long")
View(Land_FCnoSFA_2050)

colrow<-result_Land_FCnoSFA[c("colrow","SimUArea")]
names(colrow)<-c("colrow","CRArea")
names(colrow)
names(land_FCnoSFA)


setwd(outputpath)
write.csv(colrow, file="colrow.csv", row.names=FALSE, quote=FALSE)
write.csv(Land_FCnoSFA_2010, file="Land_FCnoSFA_2010.csv", row.names=FALSE, quote=FALSE)
write.csv(Land_FCnoSFA_2050, file="Land_FCnoSFA_2050.csv", row.names=FALSE, quote=FALSE)
Land_FCnoSFA_2010<-read.csv("Land_FCnoSFA_2010.csv")
###### 2.d Land Use values from FCnoCRA_LC3.CSV #####
setwd(inputpath)
FCnoCRA_LC3 = read.csv(filename_FCnoCRA_LC3, header=FALSE)
names(FCnoCRA_LC3) = c("country","colrow","altitude","slope","soil","aezclass",
                        "landuselu","noidea1","noidea2","scenario","year","value")
View(FCnoCRA_LC3)
levels(FCnoCRA_LC3$landuselu)
aggregated_FCnoCRA_LC3 = aggregate(value~colrow+landuselu+year,
                                    data=FCnoCRA_LC3, FUN=sum)

result_FCnoCRA_LC3 = data.frame(unique(aggregated_FCnoCRA_LC3$colrow))
names(result_FCnoCRA_LC3) = "colrow"
counter = 0
for (landuse in unique(aggregated_FCnoCRA_LC3$landuselu)){
  print(paste("Going through land use class", landuse))
  for (year in c(2000,2010,2020,2030,2040,2050)){
    print(paste("Going through year", year))
    colname = paste("LU",landuse,year, sep="_")
    print(colname)
    condition = (aggregated_FCnoCRA_LC3$landuselu==landuse) & (aggregated_FCnoCRA_LC3$year==year)
    col = aggregated_FCnoCRA_LC3[condition,c("colrow","value")]
    names(col)=c("colrow",colname)
    print(head(col))
    if (nrow(col)>0) result_FCnoCRA_LC3 = merge(result_FCnoCRA_LC3, col, by="colrow", all=TRUE)
    counter = counter+1
    #if (counter>5) break()
  }
  #if (counter>5) break()
}
result_FCnoCRA_LC3[is.na(result_FCnoCRA_LC3)] = 0

if(length(unique(FCnoCRA_LC3$colrow)) != nrow(result_FCnoCRA_LC3)) stop("Problem with transforming the GAMS output of 'FCnoCRA_LC3.csv' into table format. Too few ColRows!")

# store
setwd(outputpath)
save(result_FCnoCRA_LC3, file="result_FCnoCRA_LC3.RData")

### Combine land uses/covers ###
# We use the ColRows of the Table_Land. If other tables have more ColRows,
# they are discarded. If other tables have less ColRows, they gaps are filled with zeros.
result_Land_FCnoCRA = data.frame(unique(result_FCnoCRA_LC3$colrow))
names(result_Land_FCnoCRA) = "colrow"
result_Land_FCnoCRA = merge(result_Land_FCnoCRA, result_PAS, by="colrow", all=TRUE)
result_Land_FCnoCRA = merge(result_Land_FCnoCRA, result_LANDCOVER_INIT, by="colrow", all=TRUE)
result_Land_FCnoCRA = merge(result_Land_FCnoCRA, result_FCnoCRA_LC3, by="colrow", all=TRUE)
#ok

names(result_Land_FCnoCRA)

# Sort the rows
result_Land_FCnoCRA=result_Land_FCnoCRA[order(result_Land_FCnoCRA$colrow),]

# Replace NA by zeros
#There is NAs because the different files have different numbers of ColRows
nrow(result_PAS) # [1] 2105
nrow(result_FCnoCRA_LC3) # [1] 2934
nrow(result_Land_FCnoCRA) # [1] 3001
result_Land_FCnoCRA[is.na(result_Land_FCnoCRA)] = 0

result_Land_FCnoCRA$SimUArea<-ifelse((result_Land_FCnoCRA$LCI_SimUarea-(result_Land_FCnoCRA$PA_SimUarea+
                                                                            result_Land_FCnoCRA$LU_CrpLnd_2000+result_Land_FCnoCRA$LU_GrsLnd_2000+result_Land_FCnoCRA$LU_MngFor_2000+
                                                                            result_Land_FCnoCRA$LU_NatLnd_2000+result_Land_FCnoCRA$LU_PltFor_2000+
                                                                            result_Land_FCnoCRA$LU_PriFor_2000))>=0,result_Land_FCnoCRA$LCI_SimUarea,
                                      (result_Land_FCnoCRA$LU_CrpLnd_2000+result_Land_FCnoCRA$LU_GrsLnd_2000+result_Land_FCnoCRA$LU_MngFor_2000+
                                         result_Land_FCnoCRA$LU_NatLnd_2000+result_Land_FCnoCRA$LU_PltFor_2000+result_Land_FCnoCRA$LU_PriFor_2000+
                                         result_Land_FCnoCRA$PA_SimUarea))

result_Land_FCnoCRA$NotRel<-ifelse(
  (result_Land_FCnoCRA$LCI_SimUarea-(result_Land_FCnoCRA$PA_SimUarea+result_Land_FCnoCRA$LU_CrpLnd_2000+
                                        result_Land_FCnoCRA$LU_GrsLnd_2000+result_Land_FCnoCRA$LU_MngFor_2000+
                                        result_Land_FCnoCRA$LU_NatLnd_2000+result_Land_FCnoCRA$LU_PltFor_2000+
                                        result_Land_FCnoCRA$LU_PriFor_2000))<=0,0,
  ifelse(
    result_Land_FCnoCRA$LCI_SimUarea-(result_Land_FCnoCRA$LU_CrpLnd_2000+result_Land_FCnoCRA$LU_GrsLnd_2000+
                                         result_Land_FCnoCRA$LU_MngFor_2000+result_Land_FCnoCRA$LU_NatLnd_2000+
                                         result_Land_FCnoCRA$LU_PltFor_2000+result_Land_FCnoCRA$LU_PriFor_2000+
                                         result_Land_FCnoCRA$PA_SimUarea)>result_Land_FCnoCRA$LCI_NotRel,
    result_Land_FCnoCRA$LCI_NotRel,
    result_Land_FCnoCRA$LCI_SimUarea-(result_Land_FCnoCRA$LU_CrpLnd_2000+result_Land_FCnoCRA$LU_GrsLnd_2000+
                                         result_Land_FCnoCRA$LU_MngFor_2000+result_Land_FCnoCRA$LU_NatLnd_2000+
                                         result_Land_FCnoCRA$LU_PltFor_2000+result_Land_FCnoCRA$LU_PriFor_2000+
                                         result_Land_FCnoCRA$PA_SimUarea)))

result_Land_FCnoCRA$WetLnd<-ifelse(
  (result_Land_FCnoCRA$LCI_SimUarea-(result_Land_FCnoCRA$PA_SimUarea+result_Land_FCnoCRA$LU_CrpLnd_2000+
                                        result_Land_FCnoCRA$LU_GrsLnd_2000+result_Land_FCnoCRA$LU_MngFor_2000+
                                        result_Land_FCnoCRA$LU_NatLnd_2000+result_Land_FCnoCRA$LU_PltFor_2000+
                                        result_Land_FCnoCRA$LU_PriFor_2000+ result_Land_FCnoCRA$NotRel))<=0,0,
  ifelse(
    result_Land_FCnoCRA$LCI_SimUarea-(result_Land_FCnoCRA$LU_CrpLnd_2000+result_Land_FCnoCRA$LU_GrsLnd_2000+
                                         result_Land_FCnoCRA$LU_MngFor_2000+result_Land_FCnoCRA$LU_NatLnd_2000+
                                         result_Land_FCnoCRA$LU_PltFor_2000+result_Land_FCnoCRA$LU_PriFor_2000+
                                         result_Land_FCnoCRA$PA_SimUarea+
                                         result_Land_FCnoCRA$NotRel)>result_Land_FCnoCRA$LCI_WetLnd,
    result_Land_FCnoCRA$LCI_WetLnd,
    result_Land_FCnoCRA$LCI_SimUarea-(result_Land_FCnoCRA$LU_CrpLnd_2000+result_Land_FCnoCRA$LU_GrsLnd_2000+
                                         result_Land_FCnoCRA$LU_MngFor_2000+result_Land_FCnoCRA$LU_NatLnd_2000+
                                         result_Land_FCnoCRA$LU_PltFor_2000+result_Land_FCnoCRA$LU_PriFor_2000+
                                         result_Land_FCnoCRA$PA_SimUarea+result_Land_FCnoCRA$NotRel)))
result_Land_FCnoCRA$OthAgri<-ifelse(
  (result_Land_FCnoCRA$LCI_SimUarea-(result_Land_FCnoCRA$PA_SimUarea+result_Land_FCnoCRA$LU_CrpLnd_2000+
                                        result_Land_FCnoCRA$LU_GrsLnd_2000+result_Land_FCnoCRA$LU_MngFor_2000+
                                        result_Land_FCnoCRA$LU_NatLnd_2000+result_Land_FCnoCRA$LU_PltFor_2000+
                                        result_Land_FCnoCRA$LU_PriFor_2000+ result_Land_FCnoCRA$NotRel+
                                        result_Land_FCnoCRA$WetLnd))<=0,0,
  ifelse(
    result_Land_FCnoCRA$LCI_SimUarea-(result_Land_FCnoCRA$LU_CrpLnd_2000+result_Land_FCnoCRA$LU_GrsLnd_2000+
                                         result_Land_FCnoCRA$LU_MngFor_2000+result_Land_FCnoCRA$LU_NatLnd_2000+
                                         result_Land_FCnoCRA$LU_PltFor_2000+result_Land_FCnoCRA$LU_PriFor_2000+
                                         result_Land_FCnoCRA$PA_SimUarea+result_Land_FCnoCRA$NotRel+
                                         result_Land_FCnoCRA$WetLnd)>result_Land_FCnoCRA$LCI_OthAgri,
    result_Land_FCnoCRA$LCI_OthAgri,
    result_Land_FCnoCRA$LCI_SimUarea-(result_Land_FCnoCRA$LU_CrpLnd_2000+result_Land_FCnoCRA$LU_GrsLnd_2000+
                                         result_Land_FCnoCRA$LU_MngFor_2000+result_Land_FCnoCRA$LU_NatLnd_2000+
                                         result_Land_FCnoCRA$LU_PltFor_2000+result_Land_FCnoCRA$LU_PriFor_2000+
                                         result_Land_FCnoCRA$PA_SimUarea+result_Land_FCnoCRA$NotRel+
                                         result_Land_FCnoCRA$WetLnd)))

# Store
names(result_Land_FCnoCRA)
land_FCnoCRA<-result_Land_FCnoCRA[c("colrow","PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd",
                                      "LU_CrpLnd_2000", "LU_CrpLnd_2010","LU_CrpLnd_2050",
                                      "LU_GrsLnd_2000","LU_GrsLnd_2010" ,"LU_GrsLnd_2050",
                                      "LU_MngFor_2000", "LU_MngFor_2010","LU_MngFor_2050",
                                      "LU_NatLnd_2000", "LU_NatLnd_2010", "LU_NatLnd_2050",
                                      "LU_PltFor_2000", "LU_PltFor_2010" , "LU_PltFor_2050",
                                      "LU_PriFor_2000", "LU_PriFor_2010", "LU_PriFor_2050","LU_ForReg_2050",
                                      "SimUArea","NotRel","WetLnd","OthAgri")]

names(land_FCnoCRA)
land_FCnoCRA_2010<-result_Land_FCnoCRA[c("colrow","PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd",
                                           "LU_CrpLnd_2010","LU_GrsLnd_2010" ,"LU_MngFor_2010","LU_NatLnd_2010", 
                                           "LU_PltFor_2010" , "LU_PriFor_2010",
                                           "SimUArea","NotRel","WetLnd","OthAgri")]
names(land_FCnoCRA_2010)<-c("colrow","PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd",
                             "LU_CrpLnd","LU_GrsLnd" ,"LU_MngFor","LU_NatLnd", 
                             "LU_PltFor" , "LU_PriFor",
                             "SimUArea","NotRel","WetLnd","OthAgri")

Land_FCnoCRA_2010<-reshape(land_FCnoCRA_2010, 
                            varying = c("PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd","LU_CrpLnd",
                                        "LU_GrsLnd","LU_MngFor","LU_NatLnd","LU_PltFor","LU_PriFor","NotRel",
                                        "WetLnd","OthAgri"), 
                            v.names = "lc_area",
                            timevar = "lulc", 
                            times = c("PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd","LU_CrpLnd",
                                      "LU_GrsLnd","LU_MngFor","LU_NatLnd","LU_PltFor","LU_PriFor","NotRel",
                                      "WetLnd","OthAgri"),
                            direction = "long")
View(Land_FCnoCRA_2010)
names(result_Land_FCnoCRA)
land_FCnoCRA_2050<-result_Land_FCnoCRA[c("colrow","PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd",
                                           "LU_CrpLnd_2050","LU_GrsLnd_2050" ,"LU_MngFor_2050","LU_NatLnd_2050", 
                                           "LU_PltFor_2050" , "LU_PriFor_2050","LU_ForReg_2050",
                                           "NotRel","WetLnd","OthAgri")]
names(land_FCnoCRA_2050)<-c("colrow","PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd",
                             "LU_CrpLnd","LU_GrsLnd" ,"LU_MngFor","LU_NatLnd", 
                             "LU_PltFor" , "LU_PriFor","LU_ForReg","NotRel","WetLnd","OthAgri")
Land_FCnoCRA_2050<-reshape(land_FCnoCRA_2050, 
                            varying = c("PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd","LU_CrpLnd",
                                        "LU_GrsLnd","LU_MngFor","LU_NatLnd","LU_PltFor","LU_PriFor","LU_ForReg",
                                        "NotRel", "WetLnd","OthAgri"), 
                            v.names = "lc_area",
                            timevar = "lulc", 
                            times = c("PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd","LU_CrpLnd",
                                      "LU_GrsLnd","LU_MngFor","LU_NatLnd","LU_PltFor","LU_PriFor","LU_ForReg",
                                      "NotRel","WetLnd","OthAgri"),
                            direction = "long")
View(Land_FCnoCRA_2050)

colrow<-result_Land_FCnoCRA[c("colrow","SimUArea")]
names(colrow)<-c("colrow","CRArea")
names(colrow)
names(land_FCnoCRA)


setwd(outputpath)
write.csv(colrow, file="colrow.csv", row.names=FALSE, quote=FALSE)
write.csv(Land_FCnoCRA_2010, file="Land_FCnoCRA_2010.csv", row.names=FALSE, quote=FALSE)
write.csv(Land_FCnoCRA_2050, file="Land_FCnoCRA_2050.csv", row.names=FALSE, quote=FALSE)
Land_FCnoCRA_2010<-read.csv("Land_FCnoCRA_2010.csv")
###### 2.d Land Use values from FCnoCRAnoSFA_LC3.CSV #####
setwd(inputpath)
FCnoCRAnoSFA_LC3 = read.csv(filename_FCnoCRAnoSFA_LC3, header=FALSE)
names(FCnoCRAnoSFA_LC3) = c("country","colrow","altitude","slope","soil","aezclass",
                      "landuselu","noidea1","noidea2","scenario","year","value")
View(FCnoCRAnoSFA_LC3)
levels(FCnoCRAnoSFA_LC3$landuselu)
aggregated_FCnoCRAnoSFA_LC3 = aggregate(value~colrow+landuselu+year,
                                  data=FCnoCRAnoSFA_LC3, FUN=sum)

result_FCnoCRAnoSFA_LC3 = data.frame(unique(aggregated_FCnoCRAnoSFA_LC3$colrow))
names(result_FCnoCRAnoSFA_LC3) = "colrow"
counter = 0
for (landuse in unique(aggregated_FCnoCRAnoSFA_LC3$landuselu)){
  print(paste("Going through land use class", landuse))
  for (year in c(2000,2010,2020,2030,2040,2050)){
    print(paste("Going through year", year))
    colname = paste("LU",landuse,year, sep="_")
    print(colname)
    condition = (aggregated_FCnoCRAnoSFA_LC3$landuselu==landuse) & (aggregated_FCnoCRAnoSFA_LC3$year==year)
    col = aggregated_FCnoCRAnoSFA_LC3[condition,c("colrow","value")]
    names(col)=c("colrow",colname)
    print(head(col))
    if (nrow(col)>0) result_FCnoCRAnoSFA_LC3 = merge(result_FCnoCRAnoSFA_LC3, col, by="colrow", all=TRUE)
    counter = counter+1
    #if (counter>5) break()
  }
  #if (counter>5) break()
}
result_FCnoCRAnoSFA_LC3[is.na(result_FCnoCRAnoSFA_LC3)] = 0

if(length(unique(FCnoCRAnoSFA_LC3$colrow)) != nrow(result_FCnoCRAnoSFA_LC3)) stop("Problem with transforming the GAMS output of 'FCnoCRAnoSFA_LC3.csv' into table format. Too few ColRows!")

# store
setwd(outputpath)
save(result_FCnoCRAnoSFA_LC3, file="result_FCnoCRAnoSFA_LC3.RData")

### Combine land uses/covers ###
# We use the ColRows of the Table_Land. If other tables have more ColRows,
# they are discarded. If other tables have less ColRows, they gaps are filled with zeros.
result_Land_FCnoCRAnoSFA = data.frame(unique(result_FCnoCRAnoSFA_LC3$colrow))
names(result_Land_FCnoCRAnoSFA) = "colrow"
result_Land_FCnoCRAnoSFA = merge(result_Land_FCnoCRAnoSFA, result_PAS, by="colrow", all=TRUE)
result_Land_FCnoCRAnoSFA = merge(result_Land_FCnoCRAnoSFA, result_LANDCOVER_INIT, by="colrow", all=TRUE)
result_Land_FCnoCRAnoSFA = merge(result_Land_FCnoCRAnoSFA, result_FCnoCRAnoSFA_LC3, by="colrow", all=TRUE)
#ok

names(result_Land_FCnoCRAnoSFA)

# Sort the rows
result_Land_FCnoCRAnoSFA=result_Land_FCnoCRAnoSFA[order(result_Land_FCnoCRAnoSFA$colrow),]

# Replace NA by zeros
#There is NAs because the different files have different numbers of ColRows
nrow(result_PAS) # [1] 2105
nrow(result_FCnoCRAnoSFA_LC3) # [1] 2934
nrow(result_Land_FCnoCRAnoSFA) # [1] 3001
result_Land_FCnoCRAnoSFA[is.na(result_Land_FCnoCRAnoSFA)] = 0

result_Land_FCnoCRAnoSFA$SimUArea<-ifelse((result_Land_FCnoCRAnoSFA$LCI_SimUarea-(result_Land_FCnoCRAnoSFA$PA_SimUarea+
                                                                        result_Land_FCnoCRAnoSFA$LU_CrpLnd_2000+result_Land_FCnoCRAnoSFA$LU_GrsLnd_2000+result_Land_FCnoCRAnoSFA$LU_MngFor_2000+
                                                                        result_Land_FCnoCRAnoSFA$LU_NatLnd_2000+result_Land_FCnoCRAnoSFA$LU_PltFor_2000+
                                                                        result_Land_FCnoCRAnoSFA$LU_PriFor_2000))>=0,result_Land_FCnoCRAnoSFA$LCI_SimUarea,
                                    (result_Land_FCnoCRAnoSFA$LU_CrpLnd_2000+result_Land_FCnoCRAnoSFA$LU_GrsLnd_2000+result_Land_FCnoCRAnoSFA$LU_MngFor_2000+
                                       result_Land_FCnoCRAnoSFA$LU_NatLnd_2000+result_Land_FCnoCRAnoSFA$LU_PltFor_2000+result_Land_FCnoCRAnoSFA$LU_PriFor_2000+
                                       result_Land_FCnoCRAnoSFA$PA_SimUarea))

result_Land_FCnoCRAnoSFA$NotRel<-ifelse(
  (result_Land_FCnoCRAnoSFA$LCI_SimUarea-(result_Land_FCnoCRAnoSFA$PA_SimUarea+result_Land_FCnoCRAnoSFA$LU_CrpLnd_2000+
                                      result_Land_FCnoCRAnoSFA$LU_GrsLnd_2000+result_Land_FCnoCRAnoSFA$LU_MngFor_2000+
                                      result_Land_FCnoCRAnoSFA$LU_NatLnd_2000+result_Land_FCnoCRAnoSFA$LU_PltFor_2000+
                                      result_Land_FCnoCRAnoSFA$LU_PriFor_2000))<=0,0,
  ifelse(
    result_Land_FCnoCRAnoSFA$LCI_SimUarea-(result_Land_FCnoCRAnoSFA$LU_CrpLnd_2000+result_Land_FCnoCRAnoSFA$LU_GrsLnd_2000+
                                       result_Land_FCnoCRAnoSFA$LU_MngFor_2000+result_Land_FCnoCRAnoSFA$LU_NatLnd_2000+
                                       result_Land_FCnoCRAnoSFA$LU_PltFor_2000+result_Land_FCnoCRAnoSFA$LU_PriFor_2000+
                                       result_Land_FCnoCRAnoSFA$PA_SimUarea)>result_Land_FCnoCRAnoSFA$LCI_NotRel,
    result_Land_FCnoCRAnoSFA$LCI_NotRel,
    result_Land_FCnoCRAnoSFA$LCI_SimUarea-(result_Land_FCnoCRAnoSFA$LU_CrpLnd_2000+result_Land_FCnoCRAnoSFA$LU_GrsLnd_2000+
                                       result_Land_FCnoCRAnoSFA$LU_MngFor_2000+result_Land_FCnoCRAnoSFA$LU_NatLnd_2000+
                                       result_Land_FCnoCRAnoSFA$LU_PltFor_2000+result_Land_FCnoCRAnoSFA$LU_PriFor_2000+
                                       result_Land_FCnoCRAnoSFA$PA_SimUarea)))

result_Land_FCnoCRAnoSFA$WetLnd<-ifelse(
  (result_Land_FCnoCRAnoSFA$LCI_SimUarea-(result_Land_FCnoCRAnoSFA$PA_SimUarea+result_Land_FCnoCRAnoSFA$LU_CrpLnd_2000+
                                      result_Land_FCnoCRAnoSFA$LU_GrsLnd_2000+result_Land_FCnoCRAnoSFA$LU_MngFor_2000+
                                      result_Land_FCnoCRAnoSFA$LU_NatLnd_2000+result_Land_FCnoCRAnoSFA$LU_PltFor_2000+
                                      result_Land_FCnoCRAnoSFA$LU_PriFor_2000+ result_Land_FCnoCRAnoSFA$NotRel))<=0,0,
  ifelse(
    result_Land_FCnoCRAnoSFA$LCI_SimUarea-(result_Land_FCnoCRAnoSFA$LU_CrpLnd_2000+result_Land_FCnoCRAnoSFA$LU_GrsLnd_2000+
                                       result_Land_FCnoCRAnoSFA$LU_MngFor_2000+result_Land_FCnoCRAnoSFA$LU_NatLnd_2000+
                                       result_Land_FCnoCRAnoSFA$LU_PltFor_2000+result_Land_FCnoCRAnoSFA$LU_PriFor_2000+
                                       result_Land_FCnoCRAnoSFA$PA_SimUarea+
                                       result_Land_FCnoCRAnoSFA$NotRel)>result_Land_FCnoCRAnoSFA$LCI_WetLnd,
    result_Land_FCnoCRAnoSFA$LCI_WetLnd,
    result_Land_FCnoCRAnoSFA$LCI_SimUarea-(result_Land_FCnoCRAnoSFA$LU_CrpLnd_2000+result_Land_FCnoCRAnoSFA$LU_GrsLnd_2000+
                                       result_Land_FCnoCRAnoSFA$LU_MngFor_2000+result_Land_FCnoCRAnoSFA$LU_NatLnd_2000+
                                       result_Land_FCnoCRAnoSFA$LU_PltFor_2000+result_Land_FCnoCRAnoSFA$LU_PriFor_2000+
                                       result_Land_FCnoCRAnoSFA$PA_SimUarea+result_Land_FCnoCRAnoSFA$NotRel)))
result_Land_FCnoCRAnoSFA$OthAgri<-ifelse(
  (result_Land_FCnoCRAnoSFA$LCI_SimUarea-(result_Land_FCnoCRAnoSFA$PA_SimUarea+result_Land_FCnoCRAnoSFA$LU_CrpLnd_2000+
                                      result_Land_FCnoCRAnoSFA$LU_GrsLnd_2000+result_Land_FCnoCRAnoSFA$LU_MngFor_2000+
                                      result_Land_FCnoCRAnoSFA$LU_NatLnd_2000+result_Land_FCnoCRAnoSFA$LU_PltFor_2000+
                                      result_Land_FCnoCRAnoSFA$LU_PriFor_2000+ result_Land_FCnoCRAnoSFA$NotRel+
                                      result_Land_FCnoCRAnoSFA$WetLnd))<=0,0,
  ifelse(
    result_Land_FCnoCRAnoSFA$LCI_SimUarea-(result_Land_FCnoCRAnoSFA$LU_CrpLnd_2000+result_Land_FCnoCRAnoSFA$LU_GrsLnd_2000+
                                       result_Land_FCnoCRAnoSFA$LU_MngFor_2000+result_Land_FCnoCRAnoSFA$LU_NatLnd_2000+
                                       result_Land_FCnoCRAnoSFA$LU_PltFor_2000+result_Land_FCnoCRAnoSFA$LU_PriFor_2000+
                                       result_Land_FCnoCRAnoSFA$PA_SimUarea+result_Land_FCnoCRAnoSFA$NotRel+
                                       result_Land_FCnoCRAnoSFA$WetLnd)>result_Land_FCnoCRAnoSFA$LCI_OthAgri,
    result_Land_FCnoCRAnoSFA$LCI_OthAgri,
    result_Land_FCnoCRAnoSFA$LCI_SimUarea-(result_Land_FCnoCRAnoSFA$LU_CrpLnd_2000+result_Land_FCnoCRAnoSFA$LU_GrsLnd_2000+
                                       result_Land_FCnoCRAnoSFA$LU_MngFor_2000+result_Land_FCnoCRAnoSFA$LU_NatLnd_2000+
                                       result_Land_FCnoCRAnoSFA$LU_PltFor_2000+result_Land_FCnoCRAnoSFA$LU_PriFor_2000+
                                       result_Land_FCnoCRAnoSFA$PA_SimUarea+result_Land_FCnoCRAnoSFA$NotRel+
                                       result_Land_FCnoCRAnoSFA$WetLnd)))

# Store
names(result_Land_FCnoCRAnoSFA)
land_FCnoCRAnoSFA<-result_Land_FCnoCRAnoSFA[c("colrow","PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd",
                                  "LU_CrpLnd_2000", "LU_CrpLnd_2010","LU_CrpLnd_2050",
                                  "LU_GrsLnd_2000","LU_GrsLnd_2010" ,"LU_GrsLnd_2050",
                                  "LU_MngFor_2000", "LU_MngFor_2010","LU_MngFor_2050",
                                  "LU_NatLnd_2000", "LU_NatLnd_2010", "LU_NatLnd_2050",
                                  "LU_PltFor_2000", "LU_PltFor_2010" , "LU_PltFor_2050",
                                  "LU_PriFor_2000", "LU_PriFor_2010", "LU_PriFor_2050","LU_ForReg_2050",
                                  "SimUArea","NotRel","WetLnd","OthAgri")]

names(land_FCnoCRAnoSFA)
land_FCnoCRAnoSFA_2010<-result_Land_FCnoCRAnoSFA[c("colrow","PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd",
                                       "LU_CrpLnd_2010","LU_GrsLnd_2010" ,"LU_MngFor_2010","LU_NatLnd_2010", 
                                       "LU_PltFor_2010" , "LU_PriFor_2010",
                                       "SimUArea","NotRel","WetLnd","OthAgri")]
names(land_FCnoCRAnoSFA_2010)<-c("colrow","PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd",
                           "LU_CrpLnd","LU_GrsLnd" ,"LU_MngFor","LU_NatLnd", 
                           "LU_PltFor" , "LU_PriFor",
                           "SimUArea","NotRel","WetLnd","OthAgri")

Land_FCnoCRAnoSFA_2010<-reshape(land_FCnoCRAnoSFA_2010, 
                          varying = c("PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd","LU_CrpLnd",
                                      "LU_GrsLnd","LU_MngFor","LU_NatLnd","LU_PltFor","LU_PriFor","NotRel",
                                      "WetLnd","OthAgri"), 
                          v.names = "lc_area",
                          timevar = "lulc", 
                          times = c("PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd","LU_CrpLnd",
                                    "LU_GrsLnd","LU_MngFor","LU_NatLnd","LU_PltFor","LU_PriFor","NotRel",
                                    "WetLnd","OthAgri"),
                          direction = "long")
View(Land_FCnoCRAnoSFA_2010)
names(result_Land_FCnoCRAnoSFA)
land_FCnoCRAnoSFA_2050<-result_Land_FCnoCRAnoSFA[c("colrow","PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd",
                                       "LU_CrpLnd_2050","LU_GrsLnd_2050" ,"LU_MngFor_2050","LU_NatLnd_2050", 
                                       "LU_PltFor_2050" , "LU_PriFor_2050","LU_ForReg_2050",
                                       "NotRel","WetLnd","OthAgri")]
names(land_FCnoCRAnoSFA_2050)<-c("colrow","PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd",
                           "LU_CrpLnd","LU_GrsLnd" ,"LU_MngFor","LU_NatLnd", 
                           "LU_PltFor" , "LU_PriFor","LU_ForReg","NotRel","WetLnd","OthAgri")
Land_FCnoCRAnoSFA_2050<-reshape(land_FCnoCRAnoSFA_2050, 
                          varying = c("PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd","LU_CrpLnd",
                                      "LU_GrsLnd","LU_MngFor","LU_NatLnd","LU_PltFor","LU_PriFor","LU_ForReg",
                                      "NotRel", "WetLnd","OthAgri"), 
                          v.names = "lc_area",
                          timevar = "lulc", 
                          times = c("PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd","LU_CrpLnd",
                                    "LU_GrsLnd","LU_MngFor","LU_NatLnd","LU_PltFor","LU_PriFor","LU_ForReg",
                                    "NotRel","WetLnd","OthAgri"),
                          direction = "long")
View(Land_FCnoCRAnoSFA_2050)

colrow<-result_Land_FCnoCRAnoSFA[c("colrow","SimUArea")]
names(colrow)<-c("colrow","CRArea")
names(colrow)
names(land_FCnoCRAnoSFA)


setwd(outputpath)
write.csv(colrow, file="colrow.csv", row.names=FALSE, quote=FALSE)
write.csv(Land_FCnoCRAnoSFA_2010, file="Land_FCnoCRAnoSFA_2010.csv", row.names=FALSE, quote=FALSE)
write.csv(Land_FCnoCRAnoSFA_2050, file="Land_FCnoCRAnoSFA_2050.csv", row.names=FALSE, quote=FALSE)
Land_FCnoCRAnoSFA_2010<-read.csv("Land_FCnoCRAnoSFA_2010.csv")


######View(Test)####
setwd(outputpath)
write.csv(Test,"SimUareaTest.csv")
library(maptools)
library(rgdal)
library(classInt)
library(RColorBrewer)

mappath = "C:/R/Brazil/Maps"
setwd(mappath)
CR_B_shape<-readShapePoly("SIMU_BRAZ_GRIDCELLS.shp")