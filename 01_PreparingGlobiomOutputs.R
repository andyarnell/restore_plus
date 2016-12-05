##########################PreparingGlobiomData##########################
#this script converts the GLOBIOM output files into the format needed for the biodiveresity analysis. 
#creates tables with a row per each colrow and a column for each land use in different years saved as
#"Land_IDCno.csv", "Land_FC.csv", "Land_FCnoSFA.csv", "Land_FCnoCRA.csv", "Land_FCnoCRAnoSFA.csv"
# files IDCno.csv etc (i.e. without the Land_ start of the file name) are reduced files with only 2010 and 2050
# land use columns
#install/load packages
library(reshape2)
########### 1. Set data sources (needs to be adjused by user) ##################
# Please set the inputpath to the csv files
inputpath = "C:/Data/restore_plus/raw/past_data/Brazil1611/Inputs"
outputpath = "C:/Data/restore_plus/raw/past_data/Brazil1611/Outputs"
scriptpath = "C:/Data/restore_plus/scripts"
########### 2. Input globiom data ################
######2.a Setting files ####
setwd(inputpath)
# Filenames of GLOBIOM output files
filename_IDCno_LC3 = "IDC_NO_Land_Compare3.csv"
filename_IDCamazon_LC3 = "IDC_AMAZON_Land_Compare3.csv"
filename_IDCbrazil_LC3 = "IDC_BRAZIL_Land_Compare3.csv"
filename_FC_LC3 = "FC_Land_Compare3.csv"
filename_FCnoSFA_LC3 = "FCnoSFA_Land_Compare3.csv"
filename_FCnoCRA_LC3 = "FCnoCRA_Land_Compare3.csv"
filename_FCnoCRAnoSFA_LC3 = "FCnoCRAnoSFA_Land_Compare3.csv"
filename_PAS_MMA = "ProtectedAreas_Brazil.CSV"
filename_LANDCOVER_INIT = "LANDCOVER_INIT_PA.CSV"
###########2b.Getting PA and INIT  GLOBIO data in right format #####
# Load the GLOBIOM output files one by one and give them column names

input_PAS = read.csv(filename_PAS_MMA, header=FALSE)
names(input_PAS) = c("country","ColRow30","altitude","slope","soil","aezclass","palandcover","value")
input_LANDCOVER_INIT = read.csv(filename_LANDCOVER_INIT, header=FALSE)
head(input_LANDCOVER_INIT)
names(input_LANDCOVER_INIT) = c("country","ColRow30","altitude","slope","soil",
                                "lcialandcover","value")
levels(input_LANDCOVER_INIT$lcialandcover)
### Data checks ###
head(input_PAS)

### Aggregating the CSV tables ###
# We will aggregate "value"
# The differences in "aezclass" are IRrelevant, so we leave it out!
# The differences in "ColRow30" and "palandcover" are relevant
# The differences in "country", "altitude", "slope" and "soil" might be relevant, 
# but in my files they are simply constant
aggregated_PAS= aggregate(value~ColRow30+palandcover, 
                          data=input_PAS, FUN="sum")
aggregated_LANDCOVER_INIT = aggregate(value~ColRow30+lcialandcover, 
                                      data=input_LANDCOVER_INIT, FUN=sum)
### Land Cover values from PAS_MMA ####
result_PAS= data.frame(unique(aggregated_PAS$ColRow30))
names(result_PAS) = "ColRow30"
counter = 0
for (landcover in unique(aggregated_PAS$palandcover)){
  print(paste("Going through land use class", landcover))
  colname = paste("PA",landcover, sep="_")
  print(colname)
  condition = (aggregated_PAS$palandcover==landcover)
  col = aggregated_PAS[condition,c("ColRow30","value")]
  names(col)=c("ColRow30",colname)
  print(head(col))
  if (nrow(col)>0) result_PAS= merge(result_PAS, col, by.x="ColRow30", by.y="ColRow30", all=TRUE)
  counter = counter+1
  #if (counter>5) break()
}


result_PAS[is.na(result_PAS)] = 0
if(length(unique(input_PAS$ColRow30)) != nrow(result_PAS)) stop("Problem with transforming the GAMS output of 'PAS_MMA_Brazil_LCTYPESagg.csv' into table format. Too few ColRows!")
# store
setwd(outputpath)
dif<-(result_PAS$PA_SimUarea-(result_PAS$PA_Forest+result_PAS$PA_NotRel+result_PAS$PA_OthNatLnd+
                                result_PAS$PA_WetLnd))
### Land Cover values from LANDCOVER_INIT.CSV    ####
result_LANDCOVER_INIT = data.frame(unique(aggregated_LANDCOVER_INIT$ColRow30))
names(result_LANDCOVER_INIT) = "ColRow30"
counter = 0
for (landcover in unique(aggregated_LANDCOVER_INIT$lcialandcover)){
  print(paste("Going through land use class", landcover))
  colname = paste("LCI",landcover, sep="_")
  print(colname)
  condition = (aggregated_LANDCOVER_INIT$lcialandcover==landcover)
  col = aggregated_LANDCOVER_INIT[condition,c("ColRow30","value")]
  names(col)=c("ColRow30",colname)
  print(head(col))
  if (nrow(col)>0) result_LANDCOVER_INIT = merge(result_LANDCOVER_INIT, col, by="ColRow30", all=TRUE)
  counter = counter+1
  #if (counter>5) break()
}


result_LANDCOVER_INIT[is.na(result_LANDCOVER_INIT)] = 0

if(length(unique(input_LANDCOVER_INIT$ColRow30)) != nrow(result_LANDCOVER_INIT))  stop("Problem with transforming the GAMS output of 'LANDCOVER_INITagg.csv' into table format. Too few ColRows!")

####### 2.c Land Use values from IDCno_LC3.CSV, create Land_IDCno.csv #####
setwd(inputpath)
IDCno_LC3 = read.csv(filename_IDCno_LC3, header=FALSE)
names(IDCno_LC3) = c("country","ColRow30","altitude","slope","soil","aezclass",
                     "landuselu","noidea1","noidea2","noidea3","year","value")
levels(IDCno_LC3$landuselu)
aggregated_IDCno_LC3 = aggregate(value~ColRow30+landuselu+year,
                                 data=IDCno_LC3, FUN=sum)
result_IDCno_LC3 = data.frame(unique(aggregated_IDCno_LC3$ColRow30))
names(result_IDCno_LC3) = "ColRow30"
counter = 0
for (landuse in unique(aggregated_IDCno_LC3$landuselu)){
  print(paste("Going through land use class", landuse))
  for (year in c(2000,2010,2020,2030,2040,2050)){
    print(paste("Going through year", year))
    colname = paste("LU",landuse,year, sep="_")
    print(colname)
    condition = (aggregated_IDCno_LC3$landuselu==landuse) & (aggregated_IDCno_LC3$year==year)
    col = aggregated_IDCno_LC3[condition,c("ColRow30","value")]
    names(col)=c("ColRow30",colname)
    print(head(col))
    if (nrow(col)>0) result_IDCno_LC3 = merge(result_IDCno_LC3, col, by="ColRow30", all=TRUE)
    counter = counter+1
    #if (counter>5) break()
  }
  #if (counter>5) break()
}
result_IDCno_LC3[is.na(result_IDCno_LC3)] = 0

if(length(unique(IDCno_LC3$ColRow30)) != nrow(result_IDCno_LC3)) stop("Problem with transforming the GAMS output of 'IDCno_LC3.csv' into table format. Too few ColRows!")

### Combine land uses/covers ###
# We use the ColRows of the Table_Land. If other tables have more ColRows,
# they are discarded. If other tables have less ColRows, they gaps are filled with zeros.
result_Land_IDCno = data.frame(unique(result_IDCno_LC3$ColRow30))
names(result_Land_IDCno) = "ColRow30"
result_Land_IDCno = merge(result_Land_IDCno, result_PAS, by="ColRow30", all=TRUE)
result_Land_IDCno = merge(result_Land_IDCno, result_LANDCOVER_INIT, by="ColRow30", all=TRUE)
result_Land_IDCno = merge(result_Land_IDCno, result_IDCno_LC3, by="ColRow30", all=TRUE)
#ok

names(result_Land_IDCno)

# Sort the rows
result_Land_IDCno=result_Land_IDCno[order(result_Land_IDCno$ColRow30),]

# Replace NA by zeros
#There is NAs because the different files have different numbers of ColRows
nrow(result_PAS) # [1] 2105
nrow(result_IDCno_LC3) # [1] 2934
nrow(result_Land_IDCno) # [1] 3001
result_Land_IDCno[is.na(result_Land_IDCno)] = 0

# Store
setwd(outputpath)
write.csv(result_Land_IDCno, file="Land_IDCno.csv", row.names=FALSE, quote=FALSE)
#table<-read.csv("Land_IDCno.csv")
####### 2.c Land Use values from IDCamazon_LC3.CSV, create Land_IDCamazon.csv #####
setwd(inputpath)
IDCamazon_LC3 = read.csv(filename_IDCamazon_LC3, header=FALSE)
names(IDCamazon_LC3) = c("country","ColRow30","altitude","slope","soil","aezclass",
                     "landuselu","noidea1","noidea2","noidea3","year","value")
levels(IDCamazon_LC3$landuselu)
aggregated_IDCamazon_LC3 = aggregate(value~ColRow30+landuselu+year,
                                 data=IDCamazon_LC3, FUN=sum)
result_IDCamazon_LC3 = data.frame(unique(aggregated_IDCamazon_LC3$ColRow30))
names(result_IDCamazon_LC3) = "ColRow30"
counter = 0
for (landuse in unique(aggregated_IDCamazon_LC3$landuselu)){
  print(paste("Going through land use class", landuse))
  for (year in c(2000,2010,2020,2030,2040,2050)){
    print(paste("Going through year", year))
    colname = paste("LU",landuse,year, sep="_")
    print(colname)
    condition = (aggregated_IDCamazon_LC3$landuselu==landuse) & (aggregated_IDCamazon_LC3$year==year)
    col = aggregated_IDCamazon_LC3[condition,c("ColRow30","value")]
    names(col)=c("ColRow30",colname)
    print(head(col))
    if (nrow(col)>0) result_IDCamazon_LC3 = merge(result_IDCamazon_LC3, col, by="ColRow30", all=TRUE)
    counter = counter+1
    #if (counter>5) break()
  }
  #if (counter>5) break()
}
result_IDCamazon_LC3[is.na(result_IDCamazon_LC3)] = 0

if(length(unique(IDCamazon_LC3$ColRow30)) != nrow(result_IDCamazon_LC3)) stop("Problem with transforming the GAMS output of 'IDCamazon_LC3.csv' into table format. Too few ColRows!")

### Combine land uses/covers ###
# We use the ColRows of the Table_Land. If other tables have more ColRows,
# they are discarded. If other tables have less ColRows, they gaps are filled with zeros.
result_Land_IDCamazon = data.frame(unique(result_IDCamazon_LC3$ColRow30))
names(result_Land_IDCamazon) = "ColRow30"
result_Land_IDCamazon = merge(result_Land_IDCamazon, result_PAS, by="ColRow30", all=TRUE)
result_Land_IDCamazon = merge(result_Land_IDCamazon, result_LANDCOVER_INIT, by="ColRow30", all=TRUE)
result_Land_IDCamazon = merge(result_Land_IDCamazon, result_IDCamazon_LC3, by="ColRow30", all=TRUE)
#ok

names(result_Land_IDCamazon)

# Sort the rows
result_Land_IDCamazon=result_Land_IDCamazon[order(result_Land_IDCamazon$ColRow30),]

# Replace NA by zeros
#There is NAs because the different files have different numbers of ColRows
nrow(result_PAS) # [1] 2105
nrow(result_IDCamazon_LC3) # [1] 2934
nrow(result_Land_IDCamazon) # [1] 3001
result_Land_IDCamazon[is.na(result_Land_IDCamazon)] = 0

# Store
setwd(outputpath)
write.csv(result_Land_IDCamazon, file="Land_IDCamazon.csv", row.names=FALSE, quote=FALSE)
#table<-read.csv("Land_IDCamazon.csv")
####### 2.c Land Use values from IDCbrazil_LC3.CSV, create Land_IDCbrazil.csv #####
setwd(inputpath)
IDCbrazil_LC3 = read.csv(filename_IDCbrazil_LC3, header=FALSE)
names(IDCbrazil_LC3) = c("country","ColRow30","altitude","slope","soil","aezclass",
                     "landuselu","noidea1","noidea2","noidea3","year","value")
levels(IDCbrazil_LC3$landuselu)
aggregated_IDCbrazil_LC3 = aggregate(value~ColRow30+landuselu+year,
                                 data=IDCbrazil_LC3, FUN=sum)
result_IDCbrazil_LC3 = data.frame(unique(aggregated_IDCbrazil_LC3$ColRow30))
names(result_IDCbrazil_LC3) = "ColRow30"
counter = 0
for (landuse in unique(aggregated_IDCbrazil_LC3$landuselu)){
  print(paste("Going through land use class", landuse))
  for (year in c(2000,2010,2020,2030,2040,2050)){
    print(paste("Going through year", year))
    colname = paste("LU",landuse,year, sep="_")
    print(colname)
    condition = (aggregated_IDCbrazil_LC3$landuselu==landuse) & (aggregated_IDCbrazil_LC3$year==year)
    col = aggregated_IDCbrazil_LC3[condition,c("ColRow30","value")]
    names(col)=c("ColRow30",colname)
    print(head(col))
    if (nrow(col)>0) result_IDCbrazil_LC3 = merge(result_IDCbrazil_LC3, col, by="ColRow30", all=TRUE)
    counter = counter+1
    #if (counter>5) break()
  }
  #if (counter>5) break()
}
result_IDCbrazil_LC3[is.na(result_IDCbrazil_LC3)] = 0

if(length(unique(IDCbrazil_LC3$ColRow30)) != nrow(result_IDCbrazil_LC3)) stop("Problem with transforming the GAMS output of 'IDCbrazil_LC3.csv' into table format. Too few ColRows!")

### Combine land uses/covers ###
# We use the ColRows of the Table_Land. If other tables have more ColRows,
# they are discarded. If other tables have less ColRows, they gaps are filled with zeros.
result_Land_IDCbrazil = data.frame(unique(result_IDCbrazil_LC3$ColRow30))
names(result_Land_IDCbrazil) = "ColRow30"
result_Land_IDCbrazil = merge(result_Land_IDCbrazil, result_PAS, by="ColRow30", all=TRUE)
result_Land_IDCbrazil = merge(result_Land_IDCbrazil, result_LANDCOVER_INIT, by="ColRow30", all=TRUE)
result_Land_IDCbrazil = merge(result_Land_IDCbrazil, result_IDCbrazil_LC3, by="ColRow30", all=TRUE)
#ok

names(result_Land_IDCbrazil)

# Sort the rows
result_Land_IDCbrazil=result_Land_IDCbrazil[order(result_Land_IDCbrazil$ColRow30),]

# Replace NA by zeros
#There is NAs because the different files have different numbers of ColRows
nrow(result_PAS) # [1] 2105
nrow(result_IDCbrazil_LC3) # [1] 2934
nrow(result_Land_IDCbrazil) # [1] 3001
result_Land_IDCbrazil[is.na(result_Land_IDCbrazil)] = 0

# Store
setwd(outputpath)
write.csv(result_Land_IDCbrazil, file="Land_IDCbrazil.csv", row.names=FALSE, quote=FALSE)
#table<-read.csv("Land_IDCbrazil.csv")
####### 2.c Land Use values from FC_LC3.CSV #####
setwd(inputpath)
FC_LC3 = read.csv(filename_FC_LC3, header=FALSE)
names(FC_LC3) = c("country","ColRow30","altitude","slope","soil","aezclass",
                     "landuselu","noidea1","noidea2","noidea3","year","value")
levels(FC_LC3$landuselu)
aggregated_FC_LC3 = aggregate(value~ColRow30+landuselu+year,
                                 data=FC_LC3, FUN=sum)
result_FC_LC3 = data.frame(unique(aggregated_FC_LC3$ColRow30))
names(result_FC_LC3) = "ColRow30"
counter = 0
for (landuse in unique(aggregated_FC_LC3$landuselu)){
  print(paste("Going through land use class", landuse))
  for (year in c(2000,2010,2020,2030,2040,2050)){
    print(paste("Going through year", year))
    colname = paste("LU",landuse,year, sep="_")
    print(colname)
    condition = (aggregated_FC_LC3$landuselu==landuse) & (aggregated_FC_LC3$year==year)
    col = aggregated_FC_LC3[condition,c("ColRow30","value")]
    names(col)=c("ColRow30",colname)
    print(head(col))
    if (nrow(col)>0) result_FC_LC3 = merge(result_FC_LC3, col, by="ColRow30", all=TRUE)
    counter = counter+1
    #if (counter>5) break()
  }
  #if (counter>5) break()
}
result_FC_LC3[is.na(result_FC_LC3)] = 0

if(length(unique(FC_LC3$ColRow30)) != nrow(result_FC_LC3)) stop("Problem with transforming the GAMS output of 'FC_LC3.csv' into table format. Too few ColRows!")

### Combine land uses/covers ###
#check number of rows in each file so make sure are using datefile with all colrows - should be 3001
nrow(result_FC_LC3)

# We use the ColRows of the Table_Land. If other tables have more ColRows,
# they are discarded. If other tables have less ColRows, they gaps are filled with zeros.
result_Land_FC = data.frame(unique(result_FC_LC3$ColRow30))
names(result_Land_FC) = "ColRow30"
result_Land_FC = merge(result_Land_FC, result_PAS, by="ColRow30", all=TRUE)
result_Land_FC = merge(result_Land_FC, result_LANDCOVER_INIT, by="ColRow30", all=TRUE)
result_Land_FC = merge(result_Land_FC, result_FC_LC3, by="ColRow30", all=TRUE)
#ok

names(result_Land_FC)

# Sort the rows
result_Land_FC=result_Land_FC[order(result_Land_FC$ColRow30),]

# Replace NA by zeros
#There is NAs because the different files have different numbers of ColRows
nrow(result_PAS) # [1] 2105
nrow(result_FC_LC3) # [1] 2934
nrow(result_Land_FC) # [1] 3001
result_Land_FC[is.na(result_Land_FC)] = 0

# Store
setwd(outputpath)
write.csv(result_Land_FC, file="Land_FC.csv", row.names=FALSE, quote=FALSE)
#table<-read.csv("Land_FC.csv")
####### 2.c Land Use values from FCnoSFA_LC3.CSV #####
setwd(inputpath)
FCnoSFA_LC3 = read.csv(filename_FCnoSFA_LC3, header=FALSE)
names(FCnoSFA_LC3) = c("country","ColRow30","altitude","slope","soil","aezclass",
                       "landuselu","noidea1","noidea2","noidea3","year","value")
aggregated_FCnoSFA_LC3 = aggregate(value~ColRow30+landuselu+year,
                                   data=FCnoSFA_LC3, FUN=sum)
result_FCnoSFA_LC3 = data.frame(unique(aggregated_FCnoSFA_LC3$ColRow30))
names(result_FCnoSFA_LC3) = "ColRow30"
counter = 0
for (landuse in unique(aggregated_FCnoSFA_LC3$landuselu)){
  print(paste("Going through land use class", landuse))
  for (year in c(2000,2010,2020,2030,2040,2050)){
    print(paste("Going through year", year))
    colname = paste("LU",landuse,year, sep="_")
    print(colname)
    condition = (aggregated_FCnoSFA_LC3$landuselu==landuse) & (aggregated_FCnoSFA_LC3$year==year)
    col = aggregated_FCnoSFA_LC3[condition,c("ColRow30","value")]
    names(col)=c("ColRow30",colname)
    print(head(col))
    if (nrow(col)>0) result_FCnoSFA_LC3 = merge(result_FCnoSFA_LC3, col, by="ColRow30", all=TRUE)
    counter = counter+1
    #if (counter>5) break()
  }
  #if (counter>5) break()
}
result_FCnoSFA_LC3[is.na(result_FCnoSFA_LC3)] = 0

if(length(unique(FCnoSFA_LC3$ColRow30)) != nrow(result_FCnoSFA_LC3)) stop("Problem with transforming the GAMS output of 'FCnoSFA_LC3.csv' into table format. Too few ColRows!")

### Combine land uses/covers ###
#check number of rows in each file so make sure are using datefile with all colrows - should be 3001
nrow(result_FCnoSFA_LC3)

# We use the ColRows of the Table_Land. If other tables have more ColRows,
# they are discarded. If other tables have less ColRows, they gaps are filled with zeros.
result_Land_FCnoSFA = data.frame(unique(result_FCnoSFA_LC3$ColRow30))
names(result_Land_FCnoSFA) = "ColRow30"
result_Land_FCnoSFA = merge(result_Land_FCnoSFA, result_PAS, by="ColRow30", all=TRUE)
result_Land_FCnoSFA = merge(result_Land_FCnoSFA, result_LANDCOVER_INIT, by="ColRow30", all=TRUE)
result_Land_FCnoSFA = merge(result_Land_FCnoSFA, result_FCnoSFA_LC3, by="ColRow30", all=TRUE)
#ok

names(result_Land_FCnoSFA)

# Sort the rows
result_Land_FCnoSFA=result_Land_FCnoSFA[order(result_Land_FCnoSFA$ColRow30),]

# Replace NA by zeros
#There is NAs because the different files have different numbers of ColRows
nrow(result_PAS) # [1] 2105
nrow(result_FCnoSFA_LC3) # [1] 2934
nrow(result_Land_FCnoSFA) # [1] 3001
result_Land_FCnoSFA[is.na(result_Land_FCnoSFA)] = 0

# Store
setwd(outputpath)
write.csv(result_Land_FCnoSFA, file="Land_FCnoSFA.csv", row.names=FALSE, quote=FALSE)
#table<-read.csv("Land_FCnoSFA.csv")
####### 2.c Land Use values from FCnoCRA_LC3.CSV #####
setwd(inputpath)
FCnoCRA_LC3 = read.csv(filename_FCnoCRA_LC3, header=FALSE)
names(FCnoCRA_LC3) = c("country","ColRow30","altitude","slope","soil","aezclass",
                      "landuselu","noidea1","noidea2","noidea3","year","value")
aggregated_FCnoCRA_LC3 = aggregate(value~ColRow30+landuselu+year,
                                  data=FCnoCRA_LC3, FUN=sum)
result_FCnoCRA_LC3 = data.frame(unique(aggregated_FCnoCRA_LC3$ColRow30))
names(result_FCnoCRA_LC3) = "ColRow30"
counter = 0
for (landuse in unique(aggregated_FCnoCRA_LC3$landuselu)){
  print(paste("Going through land use class", landuse))
  for (year in c(2000,2010,2020,2030,2040,2050)){
    print(paste("Going through year", year))
    colname = paste("LU",landuse,year, sep="_")
    print(colname)
    condition = (aggregated_FCnoCRA_LC3$landuselu==landuse) & (aggregated_FCnoCRA_LC3$year==year)
    col = aggregated_FCnoCRA_LC3[condition,c("ColRow30","value")]
    names(col)=c("ColRow30",colname)
    print(head(col))
    if (nrow(col)>0) result_FCnoCRA_LC3 = merge(result_FCnoCRA_LC3, col, by="ColRow30", all=TRUE)
    counter = counter+1
    #if (counter>5) break()
  }
  #if (counter>5) break()
}
result_FCnoCRA_LC3[is.na(result_FCnoCRA_LC3)] = 0

if(length(unique(FCnoCRA_LC3$ColRow30)) != nrow(result_FCnoCRA_LC3)) stop("Problem with transforming the GAMS output of 'FCnoCRA_LC3.csv' into table format. Too few ColRows!")

### Combine land uses/covers ###
#check number of rows in each file so make sure are using datefile with all colrows - should be 3001
nrow(result_FCnoCRA_LC3)

# We use the ColRows of the Table_Land. If other tables have more ColRows,
# they are discarded. If other tables have less ColRows, they gaps are filled with zeros.
result_Land_FCnoCRA = data.frame(unique(result_FCnoCRA_LC3$ColRow30))
names(result_Land_FCnoCRA) = "ColRow30"
result_Land_FCnoCRA = merge(result_Land_FCnoCRA, result_PAS, by="ColRow30", all=TRUE)
result_Land_FCnoCRA = merge(result_Land_FCnoCRA, result_LANDCOVER_INIT, by="ColRow30", all=TRUE)
result_Land_FCnoCRA = merge(result_Land_FCnoCRA, result_FCnoCRA_LC3, by="ColRow30", all=TRUE)
#ok

names(result_Land_FCnoCRA)

# Sort the rows
result_Land_FCnoCRA=result_Land_FCnoCRA[order(result_Land_FCnoCRA$ColRow30),]

# Replace NA by zeros
#There is NAs because the different files have different numbers of ColRows
nrow(result_PAS) # [1] 2105
nrow(result_FCnoCRA_LC3) # [1] 2934
nrow(result_Land_FCnoCRA) # [1] 3001
result_Land_FCnoCRA[is.na(result_Land_FCnoCRA)] = 0

# Store
setwd(outputpath)
write.csv(result_Land_FCnoCRA, file="Land_FCnoCRA.csv", row.names=FALSE, quote=FALSE)
#table<-read.csv("Land_FCnoCRA.csv")
####### 2.c Land Use values from FCnoCRAnoSFA_LC3.CSV (FC without LK)#####
setwd(inputpath)
FCnoCRAnoSFA_LC3 = read.csv(filename_FCnoCRAnoSFA_LC3, header=FALSE)
names(FCnoCRAnoSFA_LC3) = c("country","ColRow30","altitude","slope","soil","aezclass",
                       "landuselu","noidea1","noidea2","noidea3","year","value")
aggregated_FCnoCRAnoSFA_LC3 = aggregate(value~ColRow30+landuselu+year,
                                   data=FCnoCRAnoSFA_LC3, FUN=sum)
result_FCnoCRAnoSFA_LC3 = data.frame(unique(aggregated_FCnoCRAnoSFA_LC3$ColRow30))
names(result_FCnoCRAnoSFA_LC3) = "ColRow30"
counter = 0
for (landuse in unique(aggregated_FCnoCRAnoSFA_LC3$landuselu)){
  print(paste("Going through land use class", landuse))
  for (year in c(2000,2010,2020,2030,2040,2050)){
    print(paste("Going through year", year))
    colname = paste("LU",landuse,year, sep="_")
    print(colname)
    condition = (aggregated_FCnoCRAnoSFA_LC3$landuselu==landuse) & (aggregated_FCnoCRAnoSFA_LC3$year==year)
    col = aggregated_FCnoCRAnoSFA_LC3[condition,c("ColRow30","value")]
    names(col)=c("ColRow30",colname)
    print(head(col))
    if (nrow(col)>0) result_FCnoCRAnoSFA_LC3 = merge(result_FCnoCRAnoSFA_LC3, col, by="ColRow30", all=TRUE)
    counter = counter+1
    #if (counter>5) break()
  }
  #if (counter>5) break()
}
result_FCnoCRAnoSFA_LC3[is.na(result_FCnoCRAnoSFA_LC3)] = 0

if(length(unique(FCnoCRAnoSFA_LC3$ColRow30)) != nrow(result_FCnoCRAnoSFA_LC3)) stop("Problem with transforming the GAMS output of 'FCnoCRAnoSFA_LC3.csv' into table format. Too few ColRows!")

### Combine land uses/covers ###
#check number of rows in each file so make sure are using datefile with all colrows - should be 3001
nrow(result_FCnoCRAnoSFA_LC3)

# We use the ColRows of the Table_Land. If other tables have more ColRows,
# they are discarded. If other tables have less ColRows, they gaps are filled with zeros.
result_Land_FCnoCRAnoSFA = data.frame(unique(result_FCnoCRAnoSFA_LC3$ColRow30))
names(result_Land_FCnoCRAnoSFA) = "ColRow30"
result_Land_FCnoCRAnoSFA = merge(result_Land_FCnoCRAnoSFA, result_PAS, by="ColRow30", all=TRUE)
result_Land_FCnoCRAnoSFA = merge(result_Land_FCnoCRAnoSFA, result_LANDCOVER_INIT, by="ColRow30", all=TRUE)
result_Land_FCnoCRAnoSFA = merge(result_Land_FCnoCRAnoSFA, result_FCnoCRAnoSFA_LC3, by="ColRow30", all=TRUE)
#ok

names(result_Land_FCnoCRAnoSFA)

# Sort the rows
result_Land_FCnoCRAnoSFA=result_Land_FCnoCRAnoSFA[order(result_Land_FCnoCRAnoSFA$ColRow30),]

# Replace NA by zeros
#There is NAs because the different files have different numbers of ColRows
nrow(result_PAS) # [1] 2105
nrow(result_FCnoCRAnoSFA_LC3) # [1] 2934
nrow(result_Land_FCnoCRAnoSFA) # [1] 3001
result_Land_FCnoCRAnoSFA[is.na(result_Land_FCnoCRAnoSFA)] = 0

# Store
setwd(outputpath)
write.csv(result_Land_FCnoCRAnoSFA, file="Land_FCnoCRAnoSFA.csv", row.names=FALSE, quote=FALSE)

#table<-read.csv("Land_FCnoCRAnoSFA.csv")
########### 2. Creating reduced file sizes IDCno.csv, FC.csv ect################
setwd(outputpath)
Land_IDCno<-read.csv("Land_IDCno.csv")
names(Land_IDCno)
IDCno<-Land_IDCno[c("ColRow30","PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd",
                   "LU_CrpLnd_2010","LU_CrpLnd_2050","LU_GrsLnd_2010",
                   "LU_GrsLnd_2050","LU_MngFor_2010","LU_MngFor_2050", 
                   "LU_NatLnd_2010","LU_NatLnd_2050","LU_PltFor_2010",
                   "LU_PltFor_2050" ,"LU_PriFor_2010", "LU_PriFor_2050")]
write.csv(IDCno, file="IDCno.csv", row.names=FALSE, quote=FALSE)
remove(IDCno)
Land_IDCamazon<-read.csv("Land_IDCamazon.csv")
names(Land_IDCamazon)
IDCamazon<-Land_IDCamazon[c("ColRow30","PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd",
                    "LU_CrpLnd_2010","LU_CrpLnd_2050","LU_GrsLnd_2010",
                    "LU_GrsLnd_2050","LU_MngFor_2010","LU_MngFor_2050", 
                    "LU_NatLnd_2010","LU_NatLnd_2050","LU_PltFor_2010",
                    "LU_PltFor_2050" ,"LU_PriFor_2010", "LU_PriFor_2050")]
write.csv(IDCamazon, file="IDCamazon.csv", row.names=FALSE, quote=FALSE)
remove(IDCamazon)
Land_IDCbrazil<-read.csv("Land_IDCbrazil.csv")
names(Land_IDCbrazil)
IDCbrazil<-Land_IDCbrazil[c("ColRow30","PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd",
                    "LU_CrpLnd_2010","LU_CrpLnd_2050","LU_GrsLnd_2010",
                    "LU_GrsLnd_2050","LU_MngFor_2010","LU_MngFor_2050", 
                    "LU_NatLnd_2010","LU_NatLnd_2050","LU_PltFor_2010",
                    "LU_PltFor_2050" ,"LU_PriFor_2010", "LU_PriFor_2050")]
write.csv(IDCbrazil, file="IDCbrazil.csv", row.names=FALSE, quote=FALSE)
remove(IDCbrazil)
Land_FC<-read.csv("Land_FC.csv")
names(Land_FC)
FC<-Land_FC[c("ColRow30","PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd",
                   "LU_CrpLnd_2010","LU_CrpLnd_2050","LU_GrsLnd_2010",
                   "LU_GrsLnd_2050","LU_MngFor_2010","LU_MngFor_2050", 
                   "LU_NatLnd_2010","LU_NatLnd_2050","LU_PltFor_2010",
                   "LU_PltFor_2050" ,"LU_PriFor_2010", "LU_PriFor_2050",
                   "LU_ForReg_2050")]
write.csv(FC, file="FC.csv", row.names=FALSE, quote=FALSE)
remove(LandFC, FC)
Land_FCnoSFA<-read.csv("Land_FCnoSFA.csv")
FCnoSFA<-Land_FCnoSFA[c("ColRow30","PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd",
                    "LU_CrpLnd_2010","LU_CrpLnd_2050","LU_GrsLnd_2010",
                    "LU_GrsLnd_2050","LU_MngFor_2010","LU_MngFor_2050", 
                    "LU_NatLnd_2010","LU_NatLnd_2050","LU_PltFor_2010",
                    "LU_PltFor_2050" ,"LU_PriFor_2010", "LU_PriFor_2050",
                    "LU_ForReg_2050")]
write.csv(FCnoSFA, file="FCnoSFA.csv", row.names=FALSE, quote=FALSE)
Land_FCnoCRA<-read.csv("Land_FCnoCRA.csv")
FCnoCRA<-Land_FCnoCRA[c("ColRow30","PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd",
                        "LU_CrpLnd_2010","LU_CrpLnd_2050","LU_GrsLnd_2010",
                        "LU_GrsLnd_2050","LU_MngFor_2010","LU_MngFor_2050", 
                        "LU_NatLnd_2010","LU_NatLnd_2050","LU_PltFor_2010",
                        "LU_PltFor_2050" ,"LU_PriFor_2010", "LU_PriFor_2050",
                        "LU_ForReg_2050")]
Land_FCnoCRAnoSFA<-read.csv("Land_FCnoCRAnoSFA.csv")
write.csv(FCnoCRA, file="FCnoCRA.csv", row.names=FALSE, quote=FALSE)
FCnoCRAnoSFA<-Land_FCnoCRAnoSFA[c("ColRow30","PA_Forest","PA_NotRel","PA_OthNatLnd","PA_WetLnd",
                        "LU_CrpLnd_2010","LU_CrpLnd_2050","LU_GrsLnd_2010",
                        "LU_GrsLnd_2050","LU_MngFor_2010","LU_MngFor_2050", 
                        "LU_NatLnd_2010","LU_NatLnd_2050","LU_PltFor_2010",
                        "LU_PltFor_2050" ,"LU_PriFor_2010", "LU_PriFor_2050",
                        "LU_ForReg_2050")]
write.csv(FCnoCRAnoSFA, file="FCnoCRAnoSFA.csv", row.names=FALSE, quote=FALSE)
