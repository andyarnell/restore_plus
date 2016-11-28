##########################PreparingGlobiomData##########################
#creating a file with total colrow area based on the GLOBIOM ouputs to ensure it is consistent with them
# and avoid issues with inconsitencies. 
###### 1. Set data sources (needs to be adjused by user) ##################
# Please set the inputpath to the csv files
inputpath = "C:/R/Brazil1611/Inputs"
outputpath = "C:/R/Brazil1611/Outputs"
scriptpath = "C:/R/Brazil1611"
spp.wd<-"C:/R/Brazil1611/SpeciesAnalysis"
########### 2. Input globiom data 
###### 2.a Setting files ####
setwd(inputpath)
# Filenames of GLOBIOM output files
filename_IDCno_LC3 = "IDC_NO_Land_Compare3.csv"
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
# calculating the simUArea

result_Land_IDCno$SimUArea<-ifelse((result_Land_IDCno$LCI_SimUarea-(result_Land_IDCno$PA_SimUarea+
  result_Land_IDCno$LU_CrpLnd_2000+result_Land_IDCno$LU_GrsLnd_2000+result_Land_IDCno$LU_MngFor_2000+
  result_Land_IDCno$LU_NatLnd_2000+result_Land_IDCno$LU_PltFor_2000+
    result_Land_IDCno$LU_PriFor_2000))>=0,result_Land_IDCno$LCI_SimUarea,
  (result_Land_IDCno$LU_CrpLnd_2000+result_Land_IDCno$LU_GrsLnd_2000+result_Land_IDCno$LU_MngFor_2000+
    result_Land_IDCno$LU_NatLnd_2000+result_Land_IDCno$LU_PltFor_2000+result_Land_IDCno$LU_PriFor_2000+
     result_Land_IDCno$PA_SimUarea))
names(result_Land_IDCno)
colrow<-result_Land_IDCno[c("colrow","SimUArea")]
names(colrow)<-c("colrow","CRArea")
names(colrow)

setwd(outputpath)
write.csv(colrow, file="colrow.csv", row.names=FALSE, quote=FALSE)
