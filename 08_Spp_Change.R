### Calculates the impacts on species #######
#Creates file of change per species colrow cobination: out_SppChange_[scenario]_[Habitat].csv
#Creates files of change per species (for all colrows):out_ChangeSpp_[scenario]_[Habitat].csv
#Files of change per colrow (for all species): out_ChangeCR_[scenario]_[Habitat].csv
#Scenarios - files for all scenarios are created
#two habitat affiliation assumptions are used Hab1 and Hab4 - 
#Hab1 - does not link forest speciesto the REFOR GLOBIOM category
#Hab4 - does link forest spceis to the REFOR GLOBIOM category
#in 2010 it is assumed that species range from IUCN and suitable habitat within a colrow have maximum overlap
#in 2050 this varies and gives rise to three change statistics: sppChange_IDCbrazil, sppChange_IDCbrazil2 and sppChange_IDCbrazil3
#sppChange_IDCbrazil - assumes that species range from IUCN and remaining suitable habitat within a colrow  still 
#have maximum overlap (effectively change occurs in areas outside range first so change has min impact)
#sppChange_IDCbrazil2 - assumes that change is evenly distributed irrespective of species range so loss of 
#suitable habitat within species range is in proportion to total loss within the cell. 
#sppChange_IDCbrazil3 - assumes maximim change - so that change occurs first within the species range

###Step 1: Set directories#################
#set working directory for outputs to be sent to
inputpath = "C:/R/Brazil1611/Inputs"
outputpath = "C:/R/Brazil1611/Outputs"

###STEP 2: import datasets####
#Note: make sure blanks at end of species names in csvs are removed 
#Make sure format is consistent for species and colrow columns for all tables as these are used 
#for joins
#import data for modelling units including cell id and area (columns colrow=cell id, CRarea=total colrow area)
setwd(outputpath)
colrow<-read.csv("colrow.csv", header=TRUE)

#import data from land use modelling showing amount of each lc/lu per cell 
Land_IDCno_2010<-read.csv("Land_IDCno_2010.csv", header=TRUE)
Land_IDCno_2050<-read.csv("Land_IDCno_2050.csv", header=TRUE)
Land_IDCamazon_2010<-read.csv("Land_IDCamazon_2010.csv", header=TRUE)
Land_IDCamazon_2050<-read.csv("Land_IDCamazon_2050.csv", header=TRUE)
Land_IDCbrazil_2010<-read.csv("Land_IDCbrazil_2010.csv", header=TRUE)
Land_IDCbrazil_2050<-read.csv("Land_IDCbrazil_2050.csv", header=TRUE)
Land_FC_2010<-read.csv("Land_FC_2010.csv", header=TRUE)
Land_FC_2050<-read.csv("Land_FC_2050.csv", header=TRUE) 
Land_FCnoSFA_2010<-read.csv("Land_FCnoSFA_2010.csv", header=TRUE)
Land_FCnoSFA_2050<-read.csv("Land_FCnoSFA_2050.csv", header=TRUE) 
Land_FCnoCRA_2010<-read.csv("Land_FCnoCRA_2010.csv", header=TRUE)
Land_FCnoCRA_2050<-read.csv("Land_FCnoCRA_2050.csv", header=TRUE) 
Land_FCnoCRAnoSFA_2010<-read.csv("Land_FCnoCRAnoSFA_2010.csv", header=TRUE)
Land_FCnoCRAnoSFA_2050<-read.csv("Land_FCnoCRAnoSFA_2050.csv", header=TRUE) 

#### Carry out crosswalk####
#import data for areas for species global ranges
#species_eoo<-read.csv("species_eoo.csv", header=TRUE)
#import data for species overlap with cells (from spatial analysis), ideally include info on spp 
setwd(inputpath)
spp_overlap<-read.csv("spp_overlap.csv", header=TRUE)
names(spp_overlap)
names(spp_overlap)<-c("sp_id","spp","area","colrow","CR_sp","spCRprop")

spp_list<-spp_overlap[c("spp")]
spp_list<-unique(spp_list)

#read in crosswalk between iucn habitats and land use (Globiom) classes
c_walk<-read.csv("HabTypes.csv", header=TRUE)

#read in raw habitat preferences
raw_HabPref<-read.csv("WCMC_GeneralHabitats_all_clean.csv", header=TRUE)
#choose only those with suitable habitat 
raw_HabPref<-subset(raw_HabPref,suitability== "Suitable")
str(raw_HabPref)
spp_HabPref<-merge(spp_list, raw_HabPref, by.x="spp", by.y="friendly_name", all=FALSE )
remove(raw_HabPref)

#merge/join crosswalk and IUCN suitable habitats using iucn habitats column 
spp_HabPref<-merge(c_walk, spp_HabPref, by.x="DESCRIPTION", by.y="description", all=FALSE )

#view dataframe's top rows
names(spp_HabPref)

#select only the relevant columns
spp_HabPref4<-spp_HabPref[,c("spp","taxonid","HabCat4")]
#remove rows with no data values - either proper nulls or just shown as "na"
spp_HabPref4<-subset(spp_HabPref4, !is.na("HabCat4"))
spp_HabPref4<-subset(spp_HabPref4, HabCat4!="na")
#remove duplicate rows (if any exist)
spp_HabPref4<-unique(spp_HabPref4)
#change names of columns to be a standard understandable format
names(spp_HabPref4)[names(spp_HabPref4)=="HabCat4"] <- "lulc"
names(spp_HabPref4)[names(spp_HabPref4)=="spp"] <- "spp"
names(spp_HabPref4)[names(spp_HabPref4)=="taxonid"] <- "sp_id"
#select only the relevant columns
spp_HabPref3<-spp_HabPref[,c("spp","taxonid","HabCat3")]
#remove rows with no data values - either proper nulls or just shown as "na"
spp_HabPref3<-subset(spp_HabPref3, !is.na("HabCat3"))
spp_HabPref3<-subset(spp_HabPref3, HabCat3!="na")
#remove duplicate rows (if any exist)
spp_HabPref3<-unique(spp_HabPref3)
#change names of columns to be a standard understandable format
names(spp_HabPref3)[names(spp_HabPref3)=="HabCat3"] <- "lulc"
names(spp_HabPref3)[names(spp_HabPref3)=="spp"] <- "spp"
names(spp_HabPref3)[names(spp_HabPref3)=="taxonid"] <- "sp_id"

#select only the relevant columns
spp_HabPref2<-spp_HabPref[,c("spp","taxonid","HabCat2")]
#remove rows with no data values - either proper nulls or just shown as "na"
spp_HabPref2<-subset(spp_HabPref2, !is.na("HabCat2"))
spp_HabPref2<-subset(spp_HabPref2, HabCat2!="na")
#remove duplicate rows (if any exist)
spp_HabPref2<-unique(spp_HabPref2)
#change names of columns to be a standard understandable format
names(spp_HabPref2)[names(spp_HabPref2)=="HabCat2"] <- "lulc"
names(spp_HabPref2)[names(spp_HabPref2)=="spp"] <- "spp"
names(spp_HabPref2)[names(spp_HabPref2)=="taxonid"] <- "sp_id"

#select only the relevant columns
spp_HabPref1<-spp_HabPref[,c("spp","taxonid","HabCat1")]
#remove rows with no data values - either proper nulls or just shown as "na"
spp_HabPref1<-subset(spp_HabPref1, !is.na("HabCat1"))
spp_HabPref1<-subset(spp_HabPref1, HabCat1!="na")
#remove duplicate rows (if any exist)
spp_HabPref1<-unique(spp_HabPref1)
#change names of columns to be a standard understandable format
names(spp_HabPref1)[names(spp_HabPref1)=="HabCat1"] <- "lulc"
names(spp_HabPref1)[names(spp_HabPref1)=="spp"] <- "spp"
names(spp_HabPref1)[names(spp_HabPref1)=="taxonid"] <- "sp_id"

#view data in habitat prefrences table 
str(spp_HabPref1)
str(spp_HabPref2)
str(spp_HabPref3)

summary(spp_HabPref1)
#should have fields:species (scientific name), lulc (land uses/covers in which the species occurs)

unique(spp_HabPref1$lulc)
unique(spp_HabPref2$lulc)
unique(spp_HabPref3$lulc)
unique(Land_IDCbrazil_2010$lulc)

### Hab 1  STEP 3a:Make a table of suitable habitat in IDCbrazil_2010####
#this creates table of all potentially suitable habitat available in each cell 
#(based on model outputs for each of the species - irrespective of whether species occurs there)
spplc_IDCbrazil_2010<-merge(spp_HabPref1,Land_IDCbrazil_2010, by.x="lulc", by.y="lulc", all=FALSE)

head(spplc_IDCbrazil_2010)
out_spplcCR_IDCbrazil_2010<-aggregate(lc_area~spp+sp_id+colrow,spplc_IDCbrazil_2010,FUN=sum)
names(out_spplcCR_IDCbrazil_2010)  

remove(spplc_IDCbrazil_2010)  
#concatonate values from species and colrow columns into a new column
out_spplcCR_IDCbrazil_2010$CR_sp <-paste (out_spplcCR_IDCbrazil_2010$colrow, 
                                      out_spplcCR_IDCbrazil_2010$sp_id, sep = "_", collapse = NULL)
names(spp_overlap)
names(out_spplcCR_IDCbrazil_2010)
spp_overlap_IDCbrazil_2010<-merge(spp_overlap, out_spplcCR_IDCbrazil_2010, by="CR_sp", by.y="CR_sp", all=FALSE)
names(spp_overlap_IDCbrazil_2010)
remove(out_spplcCR_IDCbrazil_2010)
spp_overlap_IDCbrazil_2010sub<-spp_overlap_IDCbrazil_2010[c( "CR_sp","sp_id.x","spp.x","spCRprop","colrow.x",
                                                     "lc_area" )]
remove(spp_overlap_IDCbrazil_2010)
out_SppOverlap_IDCbrazil_2010<-merge(colrow,spp_overlap_IDCbrazil_2010sub,by.x="colrow",by.y="colrow.x")
remove(spp_overlap_IDCbrazil_2010sub)

out_SppOverlap_IDCbrazil_2010$suitarea_max_IDCbrazil_2010<-ifelse(
  (out_SppOverlap_IDCbrazil_2010$spCRprop*out_SppOverlap_IDCbrazil_2010$CRArea) < out_SppOverlap_IDCbrazil_2010$lc_area, 
  (out_SppOverlap_IDCbrazil_2010$spCRprop*out_SppOverlap_IDCbrazil_2010$CRArea), out_SppOverlap_IDCbrazil_2010$lc_area)
setwd(outputpath)
write.csv(out_SppOverlap_IDCbrazil_2010,file="out_SppOverlap_IDCbrazil_2010.csv",row.names=FALSE, quote=FALSE)
out_SppOverlap_IDCbrazil_2010<-read.csv("out_SppOverlap_IDCbrazil_2010.csv")
###STEP 3b:Make a table of suitable habitat in IDCbrazil_2050####
#this creates table of all potentially suitable habitat available in each cell in 2050
#(based on model outputs for each of the species - irrespective of whether species occurs there)
spplc_IDCbrazil_2050<-merge(spp_HabPref1,Land_IDCbrazil_2050, by.x="lulc", by.y="lulc", all=FALSE)
#aggregate total area of suitable land cover in cell by species name
out_spplcCR_IDCbrazil_2050<-aggregate(lc_area~spp+sp_id+colrow,spplc_IDCbrazil_2050,FUN=sum)
names(out_spplcCR_IDCbrazil_2050)
remove(spplc_IDCbrazil_2050)
#concatonate values from species and colrow columns into a new column
out_spplcCR_IDCbrazil_2050$CR_sp <-paste (out_spplcCR_IDCbrazil_2050$colrow, 
                                      out_spplcCR_IDCbrazil_2050$sp_id, sep = "_", collapse = NULL)
out_spplcCR_IDCbrazil_2050<-out_spplcCR_IDCbrazil_2050[c("lc_area", "CR_sp" )]
#this creates a table of the suitable habitat for each species in each cell taking account species 
#overlap and assuming that the species preferentially occurs in suitable habitat
names(spp_overlap)
names(out_spplcCR_IDCbrazil_2050)
spp_overlap_IDCbrazil_2050<-merge(out_SppOverlap_IDCbrazil_2010, out_spplcCR_IDCbrazil_2050, by="CR_sp", by.y="CR_sp", all=FALSE, suffixes = c(".2010",".2050"))
names(spp_overlap_IDCbrazil_2050)
remove(out_spplcCR_IDCbrazil_2050)
out_SppOverlap_IDCbrazil_2050<-spp_overlap_IDCbrazil_2050
out_SppOverlap_IDCbrazil_2050$suitarea_max_IDCbrazil_2050<-ifelse(
  (out_SppOverlap_IDCbrazil_2050$spCRprop*out_SppOverlap_IDCbrazil_2050$CRArea) < out_SppOverlap_IDCbrazil_2050$lc_area.2050, 
  (out_SppOverlap_IDCbrazil_2050$spCRprop*out_SppOverlap_IDCbrazil_2050$CRArea), out_SppOverlap_IDCbrazil_2050$lc_area.2050)
out_SppOverlap_IDCbrazil_2050$suitarea_mean_IDCbrazil_2050<-ifelse(
  out_SppOverlap_IDCbrazil_2050$lc_area.2010==0, (out_SppOverlap_IDCbrazil_2050$spCRprop*out_SppOverlap_IDCbrazil_2050$lc_area.2050),
  ifelse((
    out_SppOverlap_IDCbrazil_2050$suitarea_max_IDCbrazil_2010*
    (out_SppOverlap_IDCbrazil_2050$lc_area.2050/out_SppOverlap_IDCbrazil_2050$lc_area.2010))>(out_SppOverlap_IDCbrazil_2050$spCRprop*out_SppOverlap_IDCbrazil_2050$CRArea),
    (out_SppOverlap_IDCbrazil_2050$spCRprop*out_SppOverlap_IDCbrazil_2050$CRArea),
    (out_SppOverlap_IDCbrazil_2050$suitarea_max_IDCbrazil_2010*
        (out_SppOverlap_IDCbrazil_2050$lc_area.2050/out_SppOverlap_IDCbrazil_2050$lc_area.2010))))
out_SppOverlap_IDCbrazil_2050$suitarea_min_IDCbrazil_2050<-ifelse(
  (out_SppOverlap_IDCbrazil_2050$lc_area.2050-out_SppOverlap_IDCbrazil_2050$lc_area.2010) >0,
  out_SppOverlap_IDCbrazil_2050$suitarea_max_IDCbrazil_2010, 
  ifelse((out_SppOverlap_IDCbrazil_2050$lc_area.2010-out_SppOverlap_IDCbrazil_2050$lc_area.2050)>
           (out_SppOverlap_IDCbrazil_2050$suitarea_max_IDCbrazil_2010),0,
         out_SppOverlap_IDCbrazil_2050$suitarea_max_IDCbrazil_2010-
           (out_SppOverlap_IDCbrazil_2050$lc_area.2010-out_SppOverlap_IDCbrazil_2050$lc_area.2050)))
###STEP 4a: Makes a table of total suitable habitat in the region IDCbrazil_2010 ####
#table is called:(out_spp_suitarearegion_IDCbrazil_2010)
out_spp_suitarearegion_IDCbrazil_2010<-aggregate(suitarea_max_IDCbrazil_2010~spp.x,
                                           out_SppOverlap_IDCbrazil_2010,FUN=sum)
#renaming columns/fields
names(out_spp_suitarearegion_IDCbrazil_2010)[
  names(out_spp_suitarearegion_IDCbrazil_2010) == 'suitarea_max_IDCbrazil_2010'] <- 'suitRegion_max_IDCbrazil_2010'
#save output
write.csv(out_spp_suitarearegion_IDCbrazil_2010,file="out_spp_suitarearegion_IDCbrazil_2010.csv",
          row.names=FALSE, quote=FALSE)
str(out_spp_suitarearegion_IDCbrazil_2010)
###STEP 5a: makes a table of change in suitable habitat for each spp IDCbrazil_2010-2050####
#view tables/dataframes to check column names from which to join tables
names(out_spp_suitarearegion_IDCbrazil_2010)
names(out_SppOverlap_IDCbrazil_2010)
names(out_SppOverlap_IDCbrazil_2050)
#join tables together to create one large table for calculations
out_SppChange_IDCbrazil<-merge(out_SppOverlap_IDCbrazil_2050, out_spp_suitarearegion_IDCbrazil_2010, 
                         by="spp.x",all=FALSE)
remove(out_SppOverlap_IDCbrazil_2010)
remove(out_SppOverlap_IDCbrazil_2050)
remove(out_spp_suitarearegion_IDCbrazil_2010)
names(out_SppChange_IDCbrazil)
out_SppChange_IDCbrazil$sppChange_IDCbrazil<-ifelse (
  out_SppChange_IDCbrazil$suitRegion_max_IDCbrazil_2010==0,0,
  ((out_SppChange_IDCbrazil$suitarea_max_IDCbrazil_2050-out_SppChange_IDCbrazil$suitarea_max_IDCbrazil_2010)/
     out_SppChange_IDCbrazil$suitRegion_max_IDCbrazil_2010))
out_SppChange_IDCbrazil$sppChange_IDCbrazil2<-ifelse (
  out_SppChange_IDCbrazil$suitRegion_max_IDCbrazil_2010==0,0,
  ((out_SppChange_IDCbrazil$suitarea_mean_IDCbrazil_2050-out_SppChange_IDCbrazil$suitarea_max_IDCbrazil_2010)/
     out_SppChange_IDCbrazil$suitRegion_max_IDCbrazil_2010))
out_SppChange_IDCbrazil$sppChange_IDCbrazil3<-ifelse (
  out_SppChange_IDCbrazil$suitRegion_max_IDCbrazil_2010==0,0,
  ((out_SppChange_IDCbrazil$suitarea_min_IDCbrazil_2050-out_SppChange_IDCbrazil$suitarea_max_IDCbrazil_2010)/
     out_SppChange_IDCbrazil$suitRegion_max_IDCbrazil_2010)) 
write.csv(out_SppChange_IDCbrazil,file="out_SppChange_IDCbrazil_Hab1.csv")
###STEP 6a: makes table of change combined across spp for each colrow IDCbrazil_2010-2050####
out_ChangeCR_IDCbrazil<-aggregate(
  cbind(sppChange_IDCbrazil,sppChange_IDCbrazil2,sppChange_IDCbrazil3)~colrow,out_SppChange_IDCbrazil,FUN=sum)
write.csv(out_ChangeCR_IDCbrazil,file="out_ChangeCR_IDCbrazil_Hab1.csv")
###STEP 7a: makes table of change combined across CRs for each spp IDCbrazil_2010-2050####
out_ChangeSpp_IDCbrazil<-aggregate(
  cbind(sppChange_IDCbrazil,sppChange_IDCbrazil2,sppChange_IDCbrazil3)~spp.x,out_SppChange_IDCbrazil,FUN=sum)
write.csv(out_ChangeSpp_IDCbrazil,file="out_ChangeSpp_IDCbrazil_Hab1.csv")

###STEP 3a:Make a table of suitable habitat in IDCamazon_2010####
#this creates table of all potentially suitable habitat available in each cell 
#(based on model outputs for each of the species - irrespective of whether species occurs there)
spplc_IDCamazon_2010<-merge(spp_HabPref1,Land_IDCamazon_2010, by.x="lulc", by.y="lulc", all=FALSE)

head(spplc_IDCamazon_2010)
out_spplcCR_IDCamazon_2010<-aggregate(lc_area~spp+sp_id+colrow,spplc_IDCamazon_2010,FUN=sum)
names(out_spplcCR_IDCamazon_2010)  

remove(spplc_IDCamazon_2010)  
#concatonate values from species and colrow columns into a new column
out_spplcCR_IDCamazon_2010$CR_sp <-paste (out_spplcCR_IDCamazon_2010$colrow, 
                                          out_spplcCR_IDCamazon_2010$sp_id, sep = "_", collapse = NULL)
names(spp_overlap)
names(out_spplcCR_IDCamazon_2010)
spp_overlap_IDCamazon_2010<-merge(spp_overlap, out_spplcCR_IDCamazon_2010, by="CR_sp", by.y="CR_sp", all=FALSE)
names(spp_overlap_IDCamazon_2010)
remove(out_spplcCR_IDCamazon_2010)
spp_overlap_IDCamazon_2010sub<-spp_overlap_IDCamazon_2010[c( "CR_sp","sp_id.x","spp.x","spCRprop","colrow.x",
                                                             "lc_area" )]
remove(spp_overlap_IDCamazon_2010)
out_SppOverlap_IDCamazon_2010<-merge(colrow,spp_overlap_IDCamazon_2010sub,by.x="colrow",by.y="colrow.x")
remove(spp_overlap_IDCamazon_2010sub)

out_SppOverlap_IDCamazon_2010$suitarea_max_IDCamazon_2010<-ifelse(
  (out_SppOverlap_IDCamazon_2010$spCRprop*out_SppOverlap_IDCamazon_2010$CRArea) < out_SppOverlap_IDCamazon_2010$lc_area, 
  (out_SppOverlap_IDCamazon_2010$spCRprop*out_SppOverlap_IDCamazon_2010$CRArea), out_SppOverlap_IDCamazon_2010$lc_area)
setwd(outputpath)
write.csv(out_SppOverlap_IDCamazon_2010,file="out_SppOverlap_IDCamazon_2010.csv",row.names=FALSE, quote=FALSE)
out_SppOverlap_IDCamazon_2010<-read.csv("out_SppOverlap_IDCamazon_2010.csv")
###STEP 3b:Make a table of suitable habitat in IDCamazon_2050####
#this creates table of all potentially suitable habitat available in each cell in 2050
#(based on model outputs for each of the species - irrespective of whether species occurs there)
spplc_IDCamazon_2050<-merge(spp_HabPref1,Land_IDCamazon_2050, by.x="lulc", by.y="lulc", all=FALSE)
#aggregate total area of suitable land cover in cell by species name
out_spplcCR_IDCamazon_2050<-aggregate(lc_area~spp+sp_id+colrow,spplc_IDCamazon_2050,FUN=sum)
names(out_spplcCR_IDCamazon_2050)
remove(spplc_IDCamazon_2050)
#concatonate values from species and colrow columns into a new column
out_spplcCR_IDCamazon_2050$CR_sp <-paste (out_spplcCR_IDCamazon_2050$colrow, 
                                          out_spplcCR_IDCamazon_2050$sp_id, sep = "_", collapse = NULL)
out_spplcCR_IDCamazon_2050<-out_spplcCR_IDCamazon_2050[c("lc_area", "CR_sp" )]
#this creates a table of the suitable habitat for each species in each cell taking account species 
#overlap and assuming that the species preferentially occurs in suitable habitat
names(spp_overlap)
names(out_spplcCR_IDCamazon_2050)
spp_overlap_IDCamazon_2050<-merge(out_SppOverlap_IDCamazon_2010, out_spplcCR_IDCamazon_2050, by="CR_sp", by.y="CR_sp", all=FALSE, suffixes = c(".2010",".2050"))
names(spp_overlap_IDCamazon_2050)
remove(out_spplcCR_IDCamazon_2050)
out_SppOverlap_IDCamazon_2050<-spp_overlap_IDCamazon_2050
out_SppOverlap_IDCamazon_2050$suitarea_max_IDCamazon_2050<-ifelse(
  (out_SppOverlap_IDCamazon_2050$spCRprop*out_SppOverlap_IDCamazon_2050$CRArea) < out_SppOverlap_IDCamazon_2050$lc_area.2050, 
  (out_SppOverlap_IDCamazon_2050$spCRprop*out_SppOverlap_IDCamazon_2050$CRArea), out_SppOverlap_IDCamazon_2050$lc_area.2050)
out_SppOverlap_IDCamazon_2050$suitarea_mean_IDCamazon_2050<-ifelse(
  out_SppOverlap_IDCamazon_2050$lc_area.2010==0, (out_SppOverlap_IDCamazon_2050$spCRprop*out_SppOverlap_IDCamazon_2050$lc_area.2050),
  ifelse((
    out_SppOverlap_IDCamazon_2050$suitarea_max_IDCamazon_2010*
      (out_SppOverlap_IDCamazon_2050$lc_area.2050/out_SppOverlap_IDCamazon_2050$lc_area.2010))>(out_SppOverlap_IDCamazon_2050$spCRprop*out_SppOverlap_IDCamazon_2050$CRArea),
    (out_SppOverlap_IDCamazon_2050$spCRprop*out_SppOverlap_IDCamazon_2050$CRArea),
    (out_SppOverlap_IDCamazon_2050$suitarea_max_IDCamazon_2010*
       (out_SppOverlap_IDCamazon_2050$lc_area.2050/out_SppOverlap_IDCamazon_2050$lc_area.2010))))
out_SppOverlap_IDCamazon_2050$suitarea_min_IDCamazon_2050<-ifelse(
  (out_SppOverlap_IDCamazon_2050$lc_area.2050-out_SppOverlap_IDCamazon_2050$lc_area.2010) >0,
  out_SppOverlap_IDCamazon_2050$suitarea_max_IDCamazon_2010, 
  ifelse((out_SppOverlap_IDCamazon_2050$lc_area.2010-out_SppOverlap_IDCamazon_2050$lc_area.2050)>
           (out_SppOverlap_IDCamazon_2050$suitarea_max_IDCamazon_2010),0,
         out_SppOverlap_IDCamazon_2050$suitarea_max_IDCamazon_2010-
           (out_SppOverlap_IDCamazon_2050$lc_area.2010-out_SppOverlap_IDCamazon_2050$lc_area.2050)))
###STEP 4a: Makes a table of total suitable habitat in the region IDCamazon_2010 ####
#table is called:(out_spp_suitarearegion_IDCamazon_2010)
out_spp_suitarearegion_IDCamazon_2010<-aggregate(suitarea_max_IDCamazon_2010~spp.x,
                                                 out_SppOverlap_IDCamazon_2010,FUN=sum)
#renaming columns/fields
names(out_spp_suitarearegion_IDCamazon_2010)[
  names(out_spp_suitarearegion_IDCamazon_2010) == 'suitarea_max_IDCamazon_2010'] <- 'suitRegion_max_IDCamazon_2010'
#save output
write.csv(out_spp_suitarearegion_IDCamazon_2010,file="out_spp_suitarearegion_IDCamazon_2010.csv",
          row.names=FALSE, quote=FALSE)
str(out_spp_suitarearegion_IDCamazon_2010)
###STEP 5a: makes a table of change in suitable habitat for each spp IDCamazon_2010-2050####
#view tables/dataframes to check column names from which to join tables
names(out_spp_suitarearegion_IDCamazon_2010)
names(out_SppOverlap_IDCamazon_2010)
names(out_SppOverlap_IDCamazon_2050)
#join tables together to create one large table for calculations
out_SppChange_IDCamazon<-merge(out_SppOverlap_IDCamazon_2050, out_spp_suitarearegion_IDCamazon_2010, 
                               by="spp.x",all=FALSE)
remove(out_SppOverlap_IDCamazon_2010)
remove(out_SppOverlap_IDCamazon_2050)
remove(out_spp_suitarearegion_IDCamazon_2010)
names(out_SppChange_IDCamazon)
out_SppChange_IDCamazon$sppChange_IDCamazon<-ifelse (
  out_SppChange_IDCamazon$suitRegion_max_IDCamazon_2010==0,0,
  ((out_SppChange_IDCamazon$suitarea_max_IDCamazon_2050-out_SppChange_IDCamazon$suitarea_max_IDCamazon_2010)/
     out_SppChange_IDCamazon$suitRegion_max_IDCamazon_2010))
out_SppChange_IDCamazon$sppChange_IDCamazon2<-ifelse (
  out_SppChange_IDCamazon$suitRegion_max_IDCamazon_2010==0,0,
  ((out_SppChange_IDCamazon$suitarea_mean_IDCamazon_2050-out_SppChange_IDCamazon$suitarea_max_IDCamazon_2010)/
     out_SppChange_IDCamazon$suitRegion_max_IDCamazon_2010))
out_SppChange_IDCamazon$sppChange_IDCamazon3<-ifelse (
  out_SppChange_IDCamazon$suitRegion_max_IDCamazon_2010==0,0,
  ((out_SppChange_IDCamazon$suitarea_min_IDCamazon_2050-out_SppChange_IDCamazon$suitarea_max_IDCamazon_2010)/
     out_SppChange_IDCamazon$suitRegion_max_IDCamazon_2010)) 
write.csv(out_SppChange_IDCamazon,file="out_SppChange_IDCamazon_Hab1.csv")
###STEP 6a: makes table of change combined across spp for each colrow IDCamazon_2010-2050####
out_ChangeCR_IDCamazon<-aggregate(
  cbind(sppChange_IDCamazon,sppChange_IDCamazon2,sppChange_IDCamazon3)~colrow,out_SppChange_IDCamazon,FUN=sum)
write.csv(out_ChangeCR_IDCamazon,file="out_ChangeCR_IDCamazon_Hab1.csv")
###STEP 7a: makes table of change combined across CRs for each spp IDCamazon_2010-2050####
out_ChangeSpp_IDCamazon<-aggregate(
  cbind(sppChange_IDCamazon,sppChange_IDCamazon2,sppChange_IDCamazon3)~spp.x,out_SppChange_IDCamazon,FUN=sum)
write.csv(out_ChangeSpp_IDCamazon,file="out_ChangeSpp_IDCamazon_Hab1.csv")

###STEP 3a:Make a table of suitable habitat in IDCno_2010####
#this creates table of all potentially suitable habitat available in each cell 
#(based on model outputs for each of the species - irrespective of whether species occurs there)
spplc_IDCno_2010<-merge(spp_HabPref1,Land_IDCno_2010, by.x="lulc", by.y="lulc", all=FALSE)

head(spplc_IDCno_2010)
out_spplcCR_IDCno_2010<-aggregate(lc_area~spp+sp_id+colrow,spplc_IDCno_2010,FUN=sum)
names(out_spplcCR_IDCno_2010)  

remove(spplc_IDCno_2010)  
#concatonate values from species and colrow columns into a new column
out_spplcCR_IDCno_2010$CR_sp <-paste (out_spplcCR_IDCno_2010$colrow, 
                                          out_spplcCR_IDCno_2010$sp_id, sep = "_", collapse = NULL)
names(spp_overlap)
names(out_spplcCR_IDCno_2010)
spp_overlap_IDCno_2010<-merge(spp_overlap, out_spplcCR_IDCno_2010, by="CR_sp", by.y="CR_sp", all=FALSE)
names(spp_overlap_IDCno_2010)
remove(out_spplcCR_IDCno_2010)
spp_overlap_IDCno_2010sub<-spp_overlap_IDCno_2010[c( "CR_sp","sp_id.x","spp.x","spCRprop","colrow.x",
                                                             "lc_area" )]
remove(spp_overlap_IDCno_2010)
out_SppOverlap_IDCno_2010<-merge(colrow,spp_overlap_IDCno_2010sub,by.x="colrow",by.y="colrow.x")
remove(spp_overlap_IDCno_2010sub)

out_SppOverlap_IDCno_2010$suitarea_max_IDCno_2010<-ifelse(
  (out_SppOverlap_IDCno_2010$spCRprop*out_SppOverlap_IDCno_2010$CRArea) < out_SppOverlap_IDCno_2010$lc_area, 
  (out_SppOverlap_IDCno_2010$spCRprop*out_SppOverlap_IDCno_2010$CRArea), out_SppOverlap_IDCno_2010$lc_area)
setwd(outputpath)
write.csv(out_SppOverlap_IDCno_2010,file="out_SppOverlap_IDCno_2010.csv",row.names=FALSE, quote=FALSE)
out_SppOverlap_IDCno_2010<-read.csv("out_SppOverlap_IDCno_2010.csv")
###STEP 3b:Make a table of suitable habitat in IDCno_2050####
#this creates table of all potentially suitable habitat available in each cell in 2050
#(based on model outputs for each of the species - irrespective of whether species occurs there)
spplc_IDCno_2050<-merge(spp_HabPref1,Land_IDCno_2050, by.x="lulc", by.y="lulc", all=FALSE)
#aggregate total area of suitable land cover in cell by species name
out_spplcCR_IDCno_2050<-aggregate(lc_area~spp+sp_id+colrow,spplc_IDCno_2050,FUN=sum)
names(out_spplcCR_IDCno_2050)
remove(spplc_IDCno_2050)
#concatonate values from species and colrow columns into a new column
out_spplcCR_IDCno_2050$CR_sp <-paste (out_spplcCR_IDCno_2050$colrow, 
                                          out_spplcCR_IDCno_2050$sp_id, sep = "_", collapse = NULL)
out_spplcCR_IDCno_2050<-out_spplcCR_IDCno_2050[c("lc_area", "CR_sp" )]
#this creates a table of the suitable habitat for each species in each cell taking account species 
#overlap and assuming that the species preferentially occurs in suitable habitat
names(spp_overlap)
names(out_spplcCR_IDCno_2050)
spp_overlap_IDCno_2050<-merge(out_SppOverlap_IDCno_2010, out_spplcCR_IDCno_2050, by="CR_sp", by.y="CR_sp", all=FALSE, suffixes = c(".2010",".2050"))
names(spp_overlap_IDCno_2050)
remove(out_spplcCR_IDCno_2050)
out_SppOverlap_IDCno_2050<-spp_overlap_IDCno_2050
out_SppOverlap_IDCno_2050$suitarea_max_IDCno_2050<-ifelse(
  (out_SppOverlap_IDCno_2050$spCRprop*out_SppOverlap_IDCno_2050$CRArea) < out_SppOverlap_IDCno_2050$lc_area.2050, 
  (out_SppOverlap_IDCno_2050$spCRprop*out_SppOverlap_IDCno_2050$CRArea), out_SppOverlap_IDCno_2050$lc_area.2050)
out_SppOverlap_IDCno_2050$suitarea_mean_IDCno_2050<-ifelse(
  out_SppOverlap_IDCno_2050$lc_area.2010==0, (out_SppOverlap_IDCno_2050$spCRprop*out_SppOverlap_IDCno_2050$lc_area.2050),
  ifelse((
    out_SppOverlap_IDCno_2050$suitarea_max_IDCno_2010*
      (out_SppOverlap_IDCno_2050$lc_area.2050/out_SppOverlap_IDCno_2050$lc_area.2010))>(out_SppOverlap_IDCno_2050$spCRprop*out_SppOverlap_IDCno_2050$CRArea),
    (out_SppOverlap_IDCno_2050$spCRprop*out_SppOverlap_IDCno_2050$CRArea),
    (out_SppOverlap_IDCno_2050$suitarea_max_IDCno_2010*
       (out_SppOverlap_IDCno_2050$lc_area.2050/out_SppOverlap_IDCno_2050$lc_area.2010))))
out_SppOverlap_IDCno_2050$suitarea_min_IDCno_2050<-ifelse(
  (out_SppOverlap_IDCno_2050$lc_area.2050-out_SppOverlap_IDCno_2050$lc_area.2010) >0,
  out_SppOverlap_IDCno_2050$suitarea_max_IDCno_2010, 
  ifelse((out_SppOverlap_IDCno_2050$lc_area.2010-out_SppOverlap_IDCno_2050$lc_area.2050)>
           (out_SppOverlap_IDCno_2050$suitarea_max_IDCno_2010),0,
         out_SppOverlap_IDCno_2050$suitarea_max_IDCno_2010-
           (out_SppOverlap_IDCno_2050$lc_area.2010-out_SppOverlap_IDCno_2050$lc_area.2050)))
###STEP 4a: Makes a table of total suitable habitat in the region IDCno_2010 ####
#table is called:(out_spp_suitarearegion_IDCno_2010)
out_spp_suitarearegion_IDCno_2010<-aggregate(suitarea_max_IDCno_2010~spp.x,
                                                 out_SppOverlap_IDCno_2010,FUN=sum)
#renaming columns/fields
names(out_spp_suitarearegion_IDCno_2010)[
  names(out_spp_suitarearegion_IDCno_2010) == 'suitarea_max_IDCno_2010'] <- 'suitRegion_max_IDCno_2010'
#save output
write.csv(out_spp_suitarearegion_IDCno_2010,file="out_spp_suitarearegion_IDCno_2010.csv",
          row.names=FALSE, quote=FALSE)
str(out_spp_suitarearegion_IDCno_2010)
###STEP 5a: makes a table of change in suitable habitat for each spp IDCno_2010-2050####
#view tables/dataframes to check column names from which to join tables
names(out_spp_suitarearegion_IDCno_2010)
names(out_SppOverlap_IDCno_2010)
names(out_SppOverlap_IDCno_2050)
#join tables together to create one large table for calculations
out_SppChange_IDCno<-merge(out_SppOverlap_IDCno_2050, out_spp_suitarearegion_IDCno_2010, 
                               by="spp.x",all=FALSE)
remove(out_SppOverlap_IDCno_2010)
remove(out_SppOverlap_IDCno_2050)
remove(out_spp_suitarearegion_IDCno_2010)
names(out_SppChange_IDCno)
out_SppChange_IDCno$sppChange_IDCno<-ifelse (
  out_SppChange_IDCno$suitRegion_max_IDCno_2010==0,0,
  ((out_SppChange_IDCno$suitarea_max_IDCno_2050-out_SppChange_IDCno$suitarea_max_IDCno_2010)/
     out_SppChange_IDCno$suitRegion_max_IDCno_2010))
out_SppChange_IDCno$sppChange_IDCno2<-ifelse (
  out_SppChange_IDCno$suitRegion_max_IDCno_2010==0,0,
  ((out_SppChange_IDCno$suitarea_mean_IDCno_2050-out_SppChange_IDCno$suitarea_max_IDCno_2010)/
     out_SppChange_IDCno$suitRegion_max_IDCno_2010))
out_SppChange_IDCno$sppChange_IDCno3<-ifelse (
  out_SppChange_IDCno$suitRegion_max_IDCno_2010==0,0,
  ((out_SppChange_IDCno$suitarea_min_IDCno_2050-out_SppChange_IDCno$suitarea_max_IDCno_2010)/
     out_SppChange_IDCno$suitRegion_max_IDCno_2010)) 
write.csv(out_SppChange_IDCno,file="out_SppChange_IDCno_Hab1.csv")
###STEP 6a: makes table of change combined across spp for each colrow IDCno_2010-2050####
out_ChangeCR_IDCno<-aggregate(
  cbind(sppChange_IDCno,sppChange_IDCno2,sppChange_IDCno3)~colrow,out_SppChange_IDCno,FUN=sum)
write.csv(out_ChangeCR_IDCno,file="out_ChangeCR_IDCno_Hab1.csv")
###STEP 7a: makes table of change combined across CRs for each spp IDCno_2010-2050####
out_ChangeSpp_IDCno<-aggregate(
  cbind(sppChange_IDCno,sppChange_IDCno2,sppChange_IDCno3)~spp.x,out_SppChange_IDCno,FUN=sum)
write.csv(out_ChangeSpp_IDCno,file="out_ChangeSpp_IDCno_Hab1.csv")

###STEP 3c:Make a table of suitable habitat in FC_2010####
#this creates table of all potentially suitable habitat available in each cell 
#(based on model outputs for each of the species - irrespective of whether species occurs there)
spplc_FC_2010<-merge(spp_HabPref1,Land_FC_2010, by.x="lulc", by.y="lulc", all=FALSE)
#aggregate total area of suitable land cover in cell by species name
out_spplcCR_FC_2010<-aggregate(lc_area~spp+sp_id+colrow,spplc_FC_2010,FUN=sum)
names(out_spplcCR_FC_2010)
remove(spplc_FC_2010)
#concatonate values from species and colrow columns into a new column
out_spplcCR_FC_2010$CR_sp <-paste (out_spplcCR_FC_2010$colrow, 
                                      out_spplcCR_FC_2010$sp_id, sep = "_", collapse = NULL)
#this creates a table of the suitable habitat for each species in each cell taking account species 
#overlap and assuming that the species preferentially occurs in suitable habitat
names(spp_overlap)
names(out_spplcCR_FC_2010)
spp_overlap_FC_2010<-merge(spp_overlap, out_spplcCR_FC_2010, by="CR_sp", by.y="CR_sp", all=FALSE)
names(spp_overlap_FC_2010)
remove(out_spplcCR_FC_2010)
spp_overlap_FC_2010sub<-spp_overlap_FC_2010[c( "CR_sp","sp_id.x","spp.x","spCRprop","colrow.x",
                                                     "lc_area" )]
remove(spp_overlap_FC_2010)
out_SppOverlap_FC_2010<-merge(colrow,spp_overlap_FC_2010sub,by.x="colrow",by.y="colrow.x")
remove(spp_overlap_FC_2010sub)
out_SppOverlap_FC_2010$suitarea_max_FC_2010<-ifelse(
  (out_SppOverlap_FC_2010$spCRprop*out_SppOverlap_FC_2010$CRArea) < out_SppOverlap_FC_2010$lc_area, 
  (out_SppOverlap_FC_2010$spCRprop*out_SppOverlap_FC_2010$CRArea), out_SppOverlap_FC_2010$lc_area)
write.csv(out_SppOverlap_FC_2010,file="out_SppOverlap_FC_2010.csv",row.names=FALSE, quote=FALSE)
###STEP 3b:Make a table of suitable habitat in FC_2050####
#this creates table of all potentially suitable habitat available in each cell in 2050
#(based on model outputs for each of the species - irrespective of whether species occurs there)
spplc_FC_2050<-merge(spp_HabPref1,Land_FC_2050, by.x="lulc", by.y="lulc", all=FALSE)
#aggregate total area of suitable land cover in cell by species name
out_spplcCR_FC_2050<-aggregate(lc_area~spp+sp_id+colrow,spplc_FC_2050,FUN=sum)
names(out_spplcCR_FC_2050)
remove(spplc_FC_2050)
#concatonate values from species and colrow columns into a new column
out_spplcCR_FC_2050$CR_sp <-paste (out_spplcCR_FC_2050$colrow, 
                                    out_spplcCR_FC_2050$sp_id, sep = "_", collapse = NULL)
out_spplcCR_FC_2050<-out_spplcCR_FC_2050[c("lc_area", "CR_sp" )]
#this creates a table of the suitable habitat for each species in each cell taking account species 
#overlap and assuming that the species preferentially occurs in suitable habitat
names(spp_overlap)
names(out_spplcCR_FC_2050)
spp_overlap_FC_2050<-merge(out_SppOverlap_FC_2010, out_spplcCR_FC_2050, by="CR_sp", by.y="CR_sp", all=FALSE, suffixes = c(".2010",".2050"))
names(spp_overlap_FC_2050)
remove(out_spplcCR_FC_2050)
out_SppOverlap_FC_2050<-spp_overlap_FC_2050
out_SppOverlap_FC_2050$suitarea_max_FC_2050<-ifelse(
  (out_SppOverlap_FC_2050$spCRprop*out_SppOverlap_FC_2050$CRArea) < out_SppOverlap_FC_2050$lc_area.2050, 
  (out_SppOverlap_FC_2050$spCRprop*out_SppOverlap_FC_2050$CRArea), out_SppOverlap_FC_2050$lc_area.2050)
out_SppOverlap_FC_2050$suitarea_mean_FC_2050<-ifelse(
  out_SppOverlap_FC_2050$lc_area.2010==0, (out_SppOverlap_FC_2050$spCRprop*out_SppOverlap_FC_2050$lc_area.2050),
  ifelse((
    out_SppOverlap_FC_2050$suitarea_max_FC_2010*
      (out_SppOverlap_FC_2050$lc_area.2050/out_SppOverlap_FC_2050$lc_area.2010))>
      (out_SppOverlap_FC_2050$spCRprop*out_SppOverlap_FC_2050$CRArea),
    (out_SppOverlap_FC_2050$spCRprop*out_SppOverlap_FC_2050$CRArea),
    (out_SppOverlap_FC_2050$suitarea_max_FC_2010*
       (out_SppOverlap_FC_2050$lc_area.2050/out_SppOverlap_FC_2050$lc_area.2010))))
out_SppOverlap_FC_2050$suitarea_min_FC_2050<-ifelse(
  (out_SppOverlap_FC_2050$lc_area.2050-out_SppOverlap_FC_2050$lc_area.2010) >0,
  out_SppOverlap_FC_2050$suitarea_max_FC_2010, 
  ifelse((out_SppOverlap_FC_2050$lc_area.2010-out_SppOverlap_FC_2050$lc_area.2050)>
           (out_SppOverlap_FC_2050$suitarea_max_FC_2010),
         0,
         out_SppOverlap_FC_2050$suitarea_max_FC_2010-
           (out_SppOverlap_FC_2050$lc_area.2010-out_SppOverlap_FC_2050$lc_area.2050)))
###STEP 4a: Makes a table of total suitable habitat in the region FC_2010 ####
#table is called:(out_spp_suitarearegion_FC_2010)
out_spp_suitarearegion_FC_2010<-aggregate(suitarea_max_FC_2010~spp.x,
                                           out_SppOverlap_FC_2010,FUN=sum)
#renaming columns/fields
names(out_spp_suitarearegion_FC_2010)[
  names(out_spp_suitarearegion_FC_2010) == 'suitarea_max_FC_2010'] <- 'suitRegion_max_FC_2010'
#save output
write.csv(out_spp_suitarearegion_FC_2010,file="out_spp_suitarearegion_FC_2010.csv",
          row.names=FALSE, quote=FALSE)
str(out_spp_suitarearegion_FC_2010)
###STEP 5a: makes a table of change in suitable habitat for each spp FC_2010-2050####
#view tables/dataframes to check column names from which to join tables
names(out_spp_suitarearegion_FC_2010)
names(out_SppOverlap_FC_2010)
names(out_SppOverlap_FC_2050)
#join tables together to create one large table for calculations
out_SppChange_FC<-merge(out_SppOverlap_FC_2050, out_spp_suitarearegion_FC_2010, 
                         by="spp.x",all=FALSE)
remove(out_SppOverlap_FC_2010)
remove(out_SppOverlap_FC_2050)
remove(out_spp_suitarearegion_FC_2010)
names(out_SppChange_FC)
out_SppChange_FC$sppChange_FC<-ifelse (
  out_SppChange_FC$suitRegion_max_FC_2010==0,0,
  ((out_SppChange_FC$suitarea_max_FC_2050-out_SppChange_FC$suitarea_max_FC_2010)/
     out_SppChange_FC$suitRegion_max_FC_2010))
out_SppChange_FC$sppChange_FC2<-ifelse (
  out_SppChange_FC$suitRegion_max_FC_2010==0,0,
  ((out_SppChange_FC$suitarea_mean_FC_2050-out_SppChange_FC$suitarea_max_FC_2010)/
     out_SppChange_FC$suitRegion_max_FC_2010))
out_SppChange_FC$sppChange_FC3<-ifelse (
  out_SppChange_FC$suitRegion_max_FC_2010==0,0,
  ((out_SppChange_FC$suitarea_min_FC_2050-out_SppChange_FC$suitarea_max_FC_2010)/
     out_SppChange_FC$suitRegion_max_FC_2010)) 
write.csv(out_SppChange_FC,file="out_SppChange_FC_Hab1.csv")
###STEP 6b: makes table of change combined across spp for each colrow FC_2010-2050####
out_ChangeCR_FC<-aggregate(
  cbind(sppChange_FC,sppChange_FC2,sppChange_FC3)~colrow,out_SppChange_FC,FUN=sum)
write.csv(out_ChangeCR_FC,file="out_ChangeCR_FC_Hab1.csv")
###STEP 7b: makes table of change combined across CRs for each spp FC_2010-2050####
out_ChangeSpp_FC<-aggregate(
  cbind(sppChange_FC,sppChange_FC2,sppChange_FC3)~spp.x,out_SppChange_FC,FUN=sum)
write.csv(out_ChangeSpp_FC,file="out_ChangeSpp_FC_Hab1.csv")

###STEP 3c:Make a table of suitable habitat in FCnoSFA_2010####
#this creates table of all potentially suitable habitat available in each cell 
#(based on model outputs for each of the species - irrespective of whether species occurs there)
spplc_FCnoSFA_2010<-merge(spp_HabPref1,Land_FCnoSFA_2010, by.x="lulc", by.y="lulc", all=FALSE)
#aggregate total area of suitable land cover in cell by species name
out_spplcCR_FCnoSFA_2010<-aggregate(lc_area~spp+sp_id+colrow,spplc_FCnoSFA_2010,FUN=sum)
names(out_spplcCR_FCnoSFA_2010)
remove(spplc_FCnoSFA_2010)
#concatonate values from species and colrow columns into a new column
out_spplcCR_FCnoSFA_2010$CR_sp <-paste (out_spplcCR_FCnoSFA_2010$colrow, 
                                       out_spplcCR_FCnoSFA_2010$sp_id, sep = "_", collapse = NULL)
#this creates a table of the suitable habitat for each species in each cell taking account species 
#overlap and assuming that the species preferentially occurs in suitable habitat
names(spp_overlap)
names(out_spplcCR_FCnoSFA_2010)
spp_overlap_FCnoSFA_2010<-merge(spp_overlap, out_spplcCR_FCnoSFA_2010, by="CR_sp", by.y="CR_sp", all=FALSE)
names(spp_overlap_FCnoSFA_2010)
remove(out_spplcCR_FCnoSFA_2010)
spp_overlap_FCnoSFA_2010sub<-spp_overlap_FCnoSFA_2010[c( "CR_sp","sp_id.x","spp.x","spCRprop","colrow.x",
                                                       "lc_area" )]
remove(spp_overlap_FCnoSFA_2010)
out_SppOverlap_FCnoSFA_2010<-merge(colrow,spp_overlap_FCnoSFA_2010sub,by.x="colrow",by.y="colrow.x")
remove(spp_overlap_FCnoSFA_2010sub)
out_SppOverlap_FCnoSFA_2010$suitarea_max_FCnoSFA_2010<-ifelse(
  (out_SppOverlap_FCnoSFA_2010$spCRprop*out_SppOverlap_FCnoSFA_2010$CRArea) < out_SppOverlap_FCnoSFA_2010$lc_area, 
  (out_SppOverlap_FCnoSFA_2010$spCRprop*out_SppOverlap_FCnoSFA_2010$CRArea), out_SppOverlap_FCnoSFA_2010$lc_area)
write.csv(out_SppOverlap_FCnoSFA_2010,file="out_SppOverlap_FCnoSFA_2010.csv",row.names=FALSE, quote=FALSE)
###STEP 3b:Make a table of suitable habitat in FCnoSFA_2050####
#this creates table of all potentially suitable habitat available in each cell in 2050
#(based on model outputs for each of the species - irrespective of whether species occurs there)
spplc_FCnoSFA_2050<-merge(spp_HabPref1,Land_FCnoSFA_2050, by.x="lulc", by.y="lulc", all=FALSE)
#aggregate total area of suitable land cover in cell by species name
out_spplcCR_FCnoSFA_2050<-aggregate(lc_area~spp+sp_id+colrow,spplc_FCnoSFA_2050,FUN=sum)
names(out_spplcCR_FCnoSFA_2050)
remove(spplc_FCnoSFA_2050)
#concatonate values from species and colrow columns into a new column
out_spplcCR_FCnoSFA_2050$CR_sp <-paste (out_spplcCR_FCnoSFA_2050$colrow, 
                                    out_spplcCR_FCnoSFA_2050$sp_id, sep = "_", collapse = NULL)
out_spplcCR_FCnoSFA_2050<-out_spplcCR_FCnoSFA_2050[c("lc_area", "CR_sp" )]
#this creates a table of the suitable habitat for each species in each cell taking account species 
#overlap and assuming that the species preferentially occurs in suitable habitat
names(spp_overlap)
names(out_spplcCR_FCnoSFA_2050)
spp_overlap_FCnoSFA_2050<-merge(out_SppOverlap_FCnoSFA_2010, out_spplcCR_FCnoSFA_2050, by="CR_sp", by.y="CR_sp", all=FALSE, suffixes = c(".2010",".2050"))
names(spp_overlap_FCnoSFA_2050)
remove(out_spplcCR_FCnoSFA_2050)
out_SppOverlap_FCnoSFA_2050<-spp_overlap_FCnoSFA_2050
out_SppOverlap_FCnoSFA_2050$suitarea_max_FCnoSFA_2050<-ifelse(
  (out_SppOverlap_FCnoSFA_2050$spCRprop*out_SppOverlap_FCnoSFA_2050$CRArea) < out_SppOverlap_FCnoSFA_2050$lc_area.2050, 
  (out_SppOverlap_FCnoSFA_2050$spCRprop*out_SppOverlap_FCnoSFA_2050$CRArea), out_SppOverlap_FCnoSFA_2050$lc_area.2050)
out_SppOverlap_FCnoSFA_2050$suitarea_mean_FCnoSFA_2050<-ifelse(
  out_SppOverlap_FCnoSFA_2050$lc_area.2010==0, (out_SppOverlap_FCnoSFA_2050$spCRprop*out_SppOverlap_FCnoSFA_2050$lc_area.2050),
  ifelse((
    out_SppOverlap_FCnoSFA_2050$suitarea_max_FCnoSFA_2010*
      (out_SppOverlap_FCnoSFA_2050$lc_area.2050/out_SppOverlap_FCnoSFA_2050$lc_area.2010))>(out_SppOverlap_FCnoSFA_2050$spCRprop*out_SppOverlap_FCnoSFA_2050$CRArea),
    (out_SppOverlap_FCnoSFA_2050$spCRprop*out_SppOverlap_FCnoSFA_2050$CRArea),
    (out_SppOverlap_FCnoSFA_2050$suitarea_max_FCnoSFA_2010*
       (out_SppOverlap_FCnoSFA_2050$lc_area.2050/out_SppOverlap_FCnoSFA_2050$lc_area.2010))))
out_SppOverlap_FCnoSFA_2050$suitarea_min_FCnoSFA_2050<-ifelse(
  (out_SppOverlap_FCnoSFA_2050$lc_area.2050-out_SppOverlap_FCnoSFA_2050$lc_area.2010) >0,
  out_SppOverlap_FCnoSFA_2050$suitarea_max_FCnoSFA_2010, 
  ifelse((out_SppOverlap_FCnoSFA_2050$lc_area.2010-out_SppOverlap_FCnoSFA_2050$lc_area.2050)>
           (out_SppOverlap_FCnoSFA_2050$suitarea_max_FCnoSFA_2010),
         0,
         out_SppOverlap_FCnoSFA_2050$suitarea_max_FCnoSFA_2010-
           (out_SppOverlap_FCnoSFA_2050$lc_area.2010-out_SppOverlap_FCnoSFA_2050$lc_area.2050)))
###STEP 4a: Makes a table of total suitable habitat in the region FCnoSFA_2010 ####
#table is called:(out_spp_suitarearegion_FCnoSFA_2010)
out_spp_suitarearegion_FCnoSFA_2010<-aggregate(suitarea_max_FCnoSFA_2010~spp.x,
                                           out_SppOverlap_FCnoSFA_2010,FUN=sum)
#renaming columns/fields
names(out_spp_suitarearegion_FCnoSFA_2010)[
  names(out_spp_suitarearegion_FCnoSFA_2010) == 'suitarea_max_FCnoSFA_2010'] <- 'suitRegion_max_FCnoSFA_2010'
#save output
write.csv(out_spp_suitarearegion_FCnoSFA_2010,file="out_spp_suitarearegion_FCnoSFA_2010.csv",
          row.names=FALSE, quote=FALSE)
str(out_spp_suitarearegion_FCnoSFA_2010)
###STEP 5a: makes a table of change in suitable habitat for each spp FCnoSFA_2010-2050####
#view tables/dataframes to check column names from which to join tables
names(out_spp_suitarearegion_FCnoSFA_2010)
names(out_SppOverlap_FCnoSFA_2010)
names(out_SppOverlap_FCnoSFA_2050)
#join tables together to create one large table for calculations
out_SppChange_FCnoSFA<-merge(out_SppOverlap_FCnoSFA_2050, out_spp_suitarearegion_FCnoSFA_2010, 
                         by="spp.x",all=FALSE)
remove(out_SppOverlap_FCnoSFA_2010)
remove(out_SppOverlap_FCnoSFA_2050)
remove(out_spp_suitarearegion_FCnoSFA_2010)
names(out_SppChange_FCnoSFA)
out_SppChange_FCnoSFA$sppChange_FCnoSFA<-ifelse (
  out_SppChange_FCnoSFA$suitRegion_max_FCnoSFA_2010==0,0,
  ((out_SppChange_FCnoSFA$suitarea_max_FCnoSFA_2050-out_SppChange_FCnoSFA$suitarea_max_FCnoSFA_2010)/
     out_SppChange_FCnoSFA$suitRegion_max_FCnoSFA_2010))
out_SppChange_FCnoSFA$sppChange_FCnoSFA2<-ifelse (
  out_SppChange_FCnoSFA$suitRegion_max_FCnoSFA_2010==0,0,
  ((out_SppChange_FCnoSFA$suitarea_mean_FCnoSFA_2050-out_SppChange_FCnoSFA$suitarea_max_FCnoSFA_2010)/
     out_SppChange_FCnoSFA$suitRegion_max_FCnoSFA_2010))
out_SppChange_FCnoSFA$sppChange_FCnoSFA3<-ifelse (
  out_SppChange_FCnoSFA$suitRegion_max_FCnoSFA_2010==0,0,
  ((out_SppChange_FCnoSFA$suitarea_min_FCnoSFA_2050-out_SppChange_FCnoSFA$suitarea_max_FCnoSFA_2010)/
     out_SppChange_FCnoSFA$suitRegion_max_FCnoSFA_2010)) 
write.csv(out_SppChange_FCnoSFA,file="out_SppChange_FCnoSFA_Hab1.csv")
###STEP 6b: makes table of change combined across spp for each colrow FCnoSFA_2010-2050####
out_ChangeCR_FCnoSFA<-aggregate(
  cbind(sppChange_FCnoSFA,sppChange_FCnoSFA2,sppChange_FCnoSFA3)~colrow,
  out_SppChange_FCnoSFA,FUN=sum)
write.csv(out_ChangeCR_FCnoSFA,file="out_ChangeCR_FCnoSFA_Hab1.csv")
###STEP 7b: makes table of change combined across CRs for each spp FCnoSFA_2010-2050####
out_ChangeSpp_FCnoSFA<-aggregate(
  cbind(sppChange_FCnoSFA,sppChange_FCnoSFA2,sppChange_FCnoSFA3)~spp.x,
  out_SppChange_FCnoSFA,FUN=sum)
write.csv(out_ChangeSpp_FCnoSFA,file="out_ChangeSpp_FCnoSFA_Hab1.csv")

###STEP 3c:Make a table of suitable habitat in FCnoCRA_2010####
#this creates table of all potentially suitable habitat available in each cell 
#(based on model outputs for each of the species - irrespective of whether species occurs there)
spplc_FCnoCRA_2010<-merge(spp_HabPref1,Land_FCnoCRA_2010, by.x="lulc", by.y="lulc", all=FALSE)
#aggregate total area of suitable land cover in cell by species name
out_spplcCR_FCnoCRA_2010<-aggregate(lc_area~spp+sp_id+colrow,spplc_FCnoCRA_2010,FUN=sum)
names(out_spplcCR_FCnoCRA_2010)
remove(spplc_FCnoCRA_2010)
#concatonate values from species and colrow columns into a new column
out_spplcCR_FCnoCRA_2010$CR_sp <-paste (out_spplcCR_FCnoCRA_2010$colrow, 
                                         out_spplcCR_FCnoCRA_2010$sp_id, sep = "_", collapse = NULL)
#this creates a table of the suitable habitat for each species in each cell taking account species 
#overlap and assuming that the species preferentially occurs in suitable habitat
names(spp_overlap)
names(out_spplcCR_FCnoCRA_2010)
spp_overlap_FCnoCRA_2010<-merge(spp_overlap, out_spplcCR_FCnoCRA_2010, by="CR_sp", by.y="CR_sp", all=FALSE)
names(spp_overlap_FCnoCRA_2010)
remove(out_spplcCR_FCnoCRA_2010)
spp_overlap_FCnoCRA_2010sub<-spp_overlap_FCnoCRA_2010[c( "CR_sp","sp_id.x","spp.x","spCRprop","colrow.x",
                                                           "lc_area" )]
remove(spp_overlap_FCnoCRA_2010)
out_SppOverlap_FCnoCRA_2010<-merge(colrow,spp_overlap_FCnoCRA_2010sub,by.x="colrow",by.y="colrow.x")
remove(spp_overlap_FCnoCRA_2010sub)
out_SppOverlap_FCnoCRA_2010$suitarea_max_FCnoCRA_2010<-ifelse(
  (out_SppOverlap_FCnoCRA_2010$spCRprop*out_SppOverlap_FCnoCRA_2010$CRArea) < out_SppOverlap_FCnoCRA_2010$lc_area, 
  (out_SppOverlap_FCnoCRA_2010$spCRprop*out_SppOverlap_FCnoCRA_2010$CRArea), out_SppOverlap_FCnoCRA_2010$lc_area)
write.csv(out_SppOverlap_FCnoCRA_2010,file="out_SppOverlap_FCnoCRA_2010.csv",row.names=FALSE, quote=FALSE)
###STEP 3b:Make a table of suitable habitat in FCnoCRA_2050####
#this creates table of all potentially suitable habitat available in each cell in 2050
#(based on model outputs for each of the species - irrespective of whether species occurs there)
spplc_FCnoCRA_2050<-merge(spp_HabPref1,Land_FCnoCRA_2050, by.x="lulc", by.y="lulc", all=FALSE)
#aggregate total area of suitable land cover in cell by species name
out_spplcCR_FCnoCRA_2050<-aggregate(lc_area~spp+sp_id+colrow,spplc_FCnoCRA_2050,FUN=sum)
names(out_spplcCR_FCnoCRA_2050)
remove(spplc_FCnoCRA_2050)
#concatonate values from species and colrow columns into a new column
out_spplcCR_FCnoCRA_2050$CR_sp <-paste (out_spplcCR_FCnoCRA_2050$colrow, 
                                    out_spplcCR_FCnoCRA_2050$sp_id, sep = "_", collapse = NULL)
out_spplcCR_FCnoCRA_2050<-out_spplcCR_FCnoCRA_2050[c("lc_area", "CR_sp" )]
#this creates a table of the suitable habitat for each species in each cell taking account species 
#overlap and assuming that the species preferentially occurs in suitable habitat
names(spp_overlap)
names(out_spplcCR_FCnoCRA_2050)
spp_overlap_FCnoCRA_2050<-merge(out_SppOverlap_FCnoCRA_2010, out_spplcCR_FCnoCRA_2050, by="CR_sp", by.y="CR_sp", all=FALSE, suffixes = c(".2010",".2050"))
names(spp_overlap_FCnoCRA_2050)
remove(out_spplcCR_FCnoCRA_2050)
out_SppOverlap_FCnoCRA_2050<-spp_overlap_FCnoCRA_2050
out_SppOverlap_FCnoCRA_2050$suitarea_max_FCnoCRA_2050<-ifelse(
  (out_SppOverlap_FCnoCRA_2050$spCRprop*out_SppOverlap_FCnoCRA_2050$CRArea) < out_SppOverlap_FCnoCRA_2050$lc_area.2050, 
  (out_SppOverlap_FCnoCRA_2050$spCRprop*out_SppOverlap_FCnoCRA_2050$CRArea), out_SppOverlap_FCnoCRA_2050$lc_area.2050)
out_SppOverlap_FCnoCRA_2050$suitarea_mean_FCnoCRA_2050<-ifelse(
  out_SppOverlap_FCnoCRA_2050$lc_area.2010==0, (out_SppOverlap_FCnoCRA_2050$spCRprop*out_SppOverlap_FCnoCRA_2050$lc_area.2050),
  ifelse((
    out_SppOverlap_FCnoCRA_2050$suitarea_max_FCnoCRA_2010*
      (out_SppOverlap_FCnoCRA_2050$lc_area.2050/out_SppOverlap_FCnoCRA_2050$lc_area.2010))>(out_SppOverlap_FCnoCRA_2050$spCRprop*out_SppOverlap_FCnoCRA_2050$CRArea),
    (out_SppOverlap_FCnoCRA_2050$spCRprop*out_SppOverlap_FCnoCRA_2050$CRArea),
    (out_SppOverlap_FCnoCRA_2050$suitarea_max_FCnoCRA_2010*
       (out_SppOverlap_FCnoCRA_2050$lc_area.2050/out_SppOverlap_FCnoCRA_2050$lc_area.2010))))
out_SppOverlap_FCnoCRA_2050$suitarea_min_FCnoCRA_2050<-ifelse(
  (out_SppOverlap_FCnoCRA_2050$lc_area.2050-out_SppOverlap_FCnoCRA_2050$lc_area.2010) >0,
  out_SppOverlap_FCnoCRA_2050$suitarea_max_FCnoCRA_2010, 
  ifelse((out_SppOverlap_FCnoCRA_2050$lc_area.2010-out_SppOverlap_FCnoCRA_2050$lc_area.2050)>
           (out_SppOverlap_FCnoCRA_2050$suitarea_max_FCnoCRA_2010),
         0,
         out_SppOverlap_FCnoCRA_2050$suitarea_max_FCnoCRA_2010-
           (out_SppOverlap_FCnoCRA_2050$lc_area.2010-out_SppOverlap_FCnoCRA_2050$lc_area.2050)))
###STEP 4a: Makes a table of total suitable habitat in the region FCnoCRA_2010 ####
#table is called:(out_spp_suitarearegion_FCnoCRA_2010)
out_spp_suitarearegion_FCnoCRA_2010<-aggregate(suitarea_max_FCnoCRA_2010~spp.x,
                                           out_SppOverlap_FCnoCRA_2010,FUN=sum)
#renaming columns/fields
names(out_spp_suitarearegion_FCnoCRA_2010)[
  names(out_spp_suitarearegion_FCnoCRA_2010) == 'suitarea_max_FCnoCRA_2010'] <- 'suitRegion_max_FCnoCRA_2010'
#save output
write.csv(out_spp_suitarearegion_FCnoCRA_2010,file="out_spp_suitarearegion_FCnoCRA_2010.csv",
          row.names=FALSE, quote=FALSE)
str(out_spp_suitarearegion_FCnoCRA_2010)
###STEP 5a: makes a table of change in suitable habitat for each spp FCnoCRA_2010-2050####
#view tables/dataframes to check column names from which to join tables
names(out_spp_suitarearegion_FCnoCRA_2010)
names(out_SppOverlap_FCnoCRA_2010)
names(out_SppOverlap_FCnoCRA_2050)
#join tables together to create one large table for calculations
out_SppChange_FCnoCRA<-merge(out_SppOverlap_FCnoCRA_2050, out_spp_suitarearegion_FCnoCRA_2010, 
                         by="spp.x",all=FALSE)
remove(out_SppOverlap_FCnoCRA_2010)
remove(out_SppOverlap_FCnoCRA_2050)
remove(out_spp_suitarearegion_FCnoCRA_2010)
names(out_SppChange_FCnoCRA)
out_SppChange_FCnoCRA$sppChange_FCnoCRA<-ifelse (
  out_SppChange_FCnoCRA$suitRegion_max_FCnoCRA_2010==0,0,
  ((out_SppChange_FCnoCRA$suitarea_max_FCnoCRA_2050-out_SppChange_FCnoCRA$suitarea_max_FCnoCRA_2010)/
     out_SppChange_FCnoCRA$suitRegion_max_FCnoCRA_2010))
out_SppChange_FCnoCRA$sppChange_FCnoCRA2<-ifelse (
  out_SppChange_FCnoCRA$suitRegion_max_FCnoCRA_2010==0,0,
  ((out_SppChange_FCnoCRA$suitarea_mean_FCnoCRA_2050-out_SppChange_FCnoCRA$suitarea_max_FCnoCRA_2010)/
     out_SppChange_FCnoCRA$suitRegion_max_FCnoCRA_2010))
out_SppChange_FCnoCRA$sppChange_FCnoCRA3<-ifelse (
  out_SppChange_FCnoCRA$suitRegion_max_FCnoCRA_2010==0,0,
  ((out_SppChange_FCnoCRA$suitarea_min_FCnoCRA_2050-out_SppChange_FCnoCRA$suitarea_max_FCnoCRA_2010)/
     out_SppChange_FCnoCRA$suitRegion_max_FCnoCRA_2010)) 
write.csv(out_SppChange_FCnoCRA,file="out_SppChange_FCnoCRA_Hab1.csv")
###STEP 6b: makes table of change combined across spp for each colrow FCnoCRA_2010-2050####
out_ChangeCR_FCnoCRA<-aggregate(
  cbind(sppChange_FCnoCRA,sppChange_FCnoCRA2,sppChange_FCnoCRA3)~colrow,
  out_SppChange_FCnoCRA,FUN=sum)
write.csv(out_ChangeCR_FCnoCRA,file="out_ChangeCR_FCnoCRA_Hab1.csv")
###STEP 7b: makes table of change combined across CRs for each spp FCnoCRA_2010-2050####
out_ChangeSpp_FCnoCRA<-aggregate(
  cbind(sppChange_FCnoCRA,sppChange_FCnoCRA2,sppChange_FCnoCRA3)~spp.x,
  out_SppChange_FCnoCRA,FUN=sum)
write.csv(out_ChangeSpp_FCnoCRA,file="out_ChangeSpp_FCnoCRA_Hab1.csv")

###STEP 3c:Make a table of suitable habitat in FCnoCRAnoSFA_2010####
#this creates table of all potentially suitable habitat available in each cell 
#(based on model outputs for each of the species - irrespective of whether species occurs there)
Land_FCnoCRAnoSFA_2010<-read.csv("Land_FCnoCRAnoSFA_2010.csv", header=TRUE)
Land_FCnoCRAnoSFA_2050<-read.csv("Land_FCnoCRAnoSFA_2050.csv", header=TRUE) 
spplc_FCnoCRAnoSFA_2010<-merge(spp_HabPref1,Land_FCnoCRAnoSFA_2010, by.x="lulc", by.y="lulc", all=FALSE)
#aggregate total area of suitable land cover in cell by species name
out_spplcCR_FCnoCRAnoSFA_2010<-aggregate(lc_area~spp+sp_id+colrow,spplc_FCnoCRAnoSFA_2010,FUN=sum)
names(out_spplcCR_FCnoCRAnoSFA_2010)
remove(spplc_FCnoCRAnoSFA_2010)
#concatonate values from species and colrow columns into a new column
out_spplcCR_FCnoCRAnoSFA_2010$CR_sp <-paste (out_spplcCR_FCnoCRAnoSFA_2010$colrow, 
                                       out_spplcCR_FCnoCRAnoSFA_2010$sp_id, sep = "_", collapse = NULL)
#this creates a table of the suitable habitat for each species in each cell taking account species 
#overlap and assuming that the species preferentially occurs in suitable habitat
names(spp_overlap)
names(out_spplcCR_FCnoCRAnoSFA_2010)
spp_overlap_FCnoCRAnoSFA_2010<-merge(spp_overlap, out_spplcCR_FCnoCRAnoSFA_2010, by="CR_sp", by.y="CR_sp", all=FALSE)
names(spp_overlap_FCnoCRAnoSFA_2010)
remove(out_spplcCR_FCnoCRAnoSFA_2010)
spp_overlap_FCnoCRAnoSFA_2010sub<-spp_overlap_FCnoCRAnoSFA_2010[c( "CR_sp","sp_id.x","spp.x","spCRprop","colrow.x",
                                                       "lc_area" )]
remove(spp_overlap_FCnoCRAnoSFA_2010)
out_SppOverlap_FCnoCRAnoSFA_2010<-merge(colrow,spp_overlap_FCnoCRAnoSFA_2010sub,by.x="colrow",by.y="colrow.x")
remove(spp_overlap_FCnoCRAnoSFA_2010sub)
out_SppOverlap_FCnoCRAnoSFA_2010$suitarea_max_FCnoCRAnoSFA_2010<-ifelse(
  (out_SppOverlap_FCnoCRAnoSFA_2010$spCRprop*out_SppOverlap_FCnoCRAnoSFA_2010$CRArea) < out_SppOverlap_FCnoCRAnoSFA_2010$lc_area, 
  (out_SppOverlap_FCnoCRAnoSFA_2010$spCRprop*out_SppOverlap_FCnoCRAnoSFA_2010$CRArea), out_SppOverlap_FCnoCRAnoSFA_2010$lc_area)
write.csv(out_SppOverlap_FCnoCRAnoSFA_2010,file="out_SppOverlap_FCnoCRAnoSFA_2010.csv",row.names=FALSE, quote=FALSE)
###STEP 3b:Make a table of suitable habitat in FCnoCRAnoSFA_2050####
#this creates table of all potentially suitable habitat available in each cell in 2050
#(based on model outputs for each of the species - irrespective of whether species occurs there)
spplc_FCnoCRAnoSFA_2050<-merge(spp_HabPref1,Land_FCnoCRAnoSFA_2050, by.x="lulc", by.y="lulc", all=FALSE)
#aggregate total area of suitable land cover in cell by species name
out_spplcCR_FCnoCRAnoSFA_2050<-aggregate(lc_area~spp+sp_id+colrow,spplc_FCnoCRAnoSFA_2050,FUN=sum)
names(out_spplcCR_FCnoCRAnoSFA_2050)
remove(spplc_FCnoCRAnoSFA_2050)
#concatonate values from species and colrow columns into a new column
out_spplcCR_FCnoCRAnoSFA_2050$CR_sp <-paste (out_spplcCR_FCnoCRAnoSFA_2050$colrow, 
                                    out_spplcCR_FCnoCRAnoSFA_2050$sp_id, sep = "_", collapse = NULL)
out_spplcCR_FCnoCRAnoSFA_2050<-out_spplcCR_FCnoCRAnoSFA_2050[c("lc_area", "CR_sp" )]
#this creates a table of the suitable habitat for each species in each cell taking account species 
#overlap and assuming that the species preferentially occurs in suitable habitat
names(spp_overlap)
names(out_spplcCR_FCnoCRAnoSFA_2050)
spp_overlap_FCnoCRAnoSFA_2050<-merge(out_SppOverlap_FCnoCRAnoSFA_2010, out_spplcCR_FCnoCRAnoSFA_2050, by="CR_sp", by.y="CR_sp", all=FALSE, suffixes = c(".2010",".2050"))
names(spp_overlap_FCnoCRAnoSFA_2050)
remove(out_spplcCR_FCnoCRAnoSFA_2050)
out_SppOverlap_FCnoCRAnoSFA_2050<-spp_overlap_FCnoCRAnoSFA_2050
out_SppOverlap_FCnoCRAnoSFA_2050$suitarea_max_FCnoCRAnoSFA_2050<-ifelse(
  (out_SppOverlap_FCnoCRAnoSFA_2050$spCRprop*out_SppOverlap_FCnoCRAnoSFA_2050$CRArea) < out_SppOverlap_FCnoCRAnoSFA_2050$lc_area.2050, 
  (out_SppOverlap_FCnoCRAnoSFA_2050$spCRprop*out_SppOverlap_FCnoCRAnoSFA_2050$CRArea), out_SppOverlap_FCnoCRAnoSFA_2050$lc_area.2050)
out_SppOverlap_FCnoCRAnoSFA_2050$suitarea_mean_FCnoCRAnoSFA_2050<-ifelse(
  out_SppOverlap_FCnoCRAnoSFA_2050$lc_area.2010==0, (out_SppOverlap_FCnoCRAnoSFA_2050$spCRprop*out_SppOverlap_FCnoCRAnoSFA_2050$lc_area.2050),
  ifelse((
    out_SppOverlap_FCnoCRAnoSFA_2050$suitarea_max_FCnoCRAnoSFA_2010*
      (out_SppOverlap_FCnoCRAnoSFA_2050$lc_area.2050/out_SppOverlap_FCnoCRAnoSFA_2050$lc_area.2010))>(out_SppOverlap_FCnoCRAnoSFA_2050$spCRprop*out_SppOverlap_FCnoCRAnoSFA_2050$CRArea),
    (out_SppOverlap_FCnoCRAnoSFA_2050$spCRprop*out_SppOverlap_FCnoCRAnoSFA_2050$CRArea),
    (out_SppOverlap_FCnoCRAnoSFA_2050$suitarea_max_FCnoCRAnoSFA_2010*
       (out_SppOverlap_FCnoCRAnoSFA_2050$lc_area.2050/out_SppOverlap_FCnoCRAnoSFA_2050$lc_area.2010))))
out_SppOverlap_FCnoCRAnoSFA_2050$suitarea_min_FCnoCRAnoSFA_2050<-ifelse(
  (out_SppOverlap_FCnoCRAnoSFA_2050$lc_area.2050-out_SppOverlap_FCnoCRAnoSFA_2050$lc_area.2010) >0,
  out_SppOverlap_FCnoCRAnoSFA_2050$suitarea_max_FCnoCRAnoSFA_2010, 
  ifelse((out_SppOverlap_FCnoCRAnoSFA_2050$lc_area.2010-out_SppOverlap_FCnoCRAnoSFA_2050$lc_area.2050)>
           (out_SppOverlap_FCnoCRAnoSFA_2050$suitarea_max_FCnoCRAnoSFA_2010),
         0,
         out_SppOverlap_FCnoCRAnoSFA_2050$suitarea_max_FCnoCRAnoSFA_2010-
           (out_SppOverlap_FCnoCRAnoSFA_2050$lc_area.2010-out_SppOverlap_FCnoCRAnoSFA_2050$lc_area.2050)))
###STEP 4a: Makes a table of total suitable habitat in the region FCnoCRAnoSFA_2010 ####
#table is called:(out_spp_suitarearegion_FCnoCRAnoSFA_2010)
out_spp_suitarearegion_FCnoCRAnoSFA_2010<-aggregate(suitarea_max_FCnoCRAnoSFA_2010~spp.x,
                                           out_SppOverlap_FCnoCRAnoSFA_2010,FUN=sum)
#renaming columns/fields
names(out_spp_suitarearegion_FCnoCRAnoSFA_2010)[
  names(out_spp_suitarearegion_FCnoCRAnoSFA_2010) == 'suitarea_max_FCnoCRAnoSFA_2010'] <- 'suitRegion_max_FCnoCRAnoSFA_2010'
#save output
write.csv(out_spp_suitarearegion_FCnoCRAnoSFA_2010,file="out_spp_suitarearegion_FCnoCRAnoSFA_2010.csv",
          row.names=FALSE, quote=FALSE)
str(out_spp_suitarearegion_FCnoCRAnoSFA_2010)
###STEP 5a: makes a table of change in suitable habitat for each spp FCnoCRAnoSFA_2010-2050####
#view tables/dataframes to check column names from which to join tables
names(out_spp_suitarearegion_FCnoCRAnoSFA_2010)
names(out_SppOverlap_FCnoCRAnoSFA_2010)
names(out_SppOverlap_FCnoCRAnoSFA_2050)
#join tables together to create one large table for calculations
out_SppChange_FCnoCRAnoSFA<-merge(out_SppOverlap_FCnoCRAnoSFA_2050, out_spp_suitarearegion_FCnoCRAnoSFA_2010, 
                         by="spp.x",all=FALSE)
remove(out_SppOverlap_FCnoCRAnoSFA_2010)
remove(out_SppOverlap_FCnoCRAnoSFA_2050)
remove(out_spp_suitarearegion_FCnoCRAnoSFA_2010)
names(out_SppChange_FCnoCRAnoSFA)
out_SppChange_FCnoCRAnoSFA$sppChange_FCnoCRAnoSFA<-ifelse (
  out_SppChange_FCnoCRAnoSFA$suitRegion_max_FCnoCRAnoSFA_2010==0,0,
  ((out_SppChange_FCnoCRAnoSFA$suitarea_max_FCnoCRAnoSFA_2050-out_SppChange_FCnoCRAnoSFA$suitarea_max_FCnoCRAnoSFA_2010)/
     out_SppChange_FCnoCRAnoSFA$suitRegion_max_FCnoCRAnoSFA_2010))
out_SppChange_FCnoCRAnoSFA$sppChange_FCnoCRAnoSFA2<-ifelse (
  out_SppChange_FCnoCRAnoSFA$suitRegion_max_FCnoCRAnoSFA_2010==0,0,
  ((out_SppChange_FCnoCRAnoSFA$suitarea_mean_FCnoCRAnoSFA_2050-out_SppChange_FCnoCRAnoSFA$suitarea_max_FCnoCRAnoSFA_2010)/
     out_SppChange_FCnoCRAnoSFA$suitRegion_max_FCnoCRAnoSFA_2010))
out_SppChange_FCnoCRAnoSFA$sppChange_FCnoCRAnoSFA3<-ifelse (
  out_SppChange_FCnoCRAnoSFA$suitRegion_max_FCnoCRAnoSFA_2010==0,0,
  ((out_SppChange_FCnoCRAnoSFA$suitarea_min_FCnoCRAnoSFA_2050-out_SppChange_FCnoCRAnoSFA$suitarea_max_FCnoCRAnoSFA_2010)/
     out_SppChange_FCnoCRAnoSFA$suitRegion_max_FCnoCRAnoSFA_2010)) 
write.csv(out_SppChange_FCnoCRAnoSFA,file="out_SppChange_FCnoCRAnoSFA_Hab1.csv")
###STEP 6b: makes table of change combined across spp for each colrow FCnoCRAnoSFA_2010-2050####
out_ChangeCR_FCnoCRAnoSFA<-aggregate(
  cbind(sppChange_FCnoCRAnoSFA,sppChange_FCnoCRAnoSFA2,sppChange_FCnoCRAnoSFA3)~colrow,
  out_SppChange_FCnoCRAnoSFA,FUN=sum)
write.csv(out_ChangeCR_FCnoCRAnoSFA,file="out_ChangeCR_FCnoCRAnoSFA_Hab1.csv")
###STEP 7b: makes table of change combined across CRs for each spp FCnoCRAnoSFA_2010-2050####
out_ChangeSpp_FCnoCRAnoSFA<-aggregate(
  cbind(sppChange_FCnoCRAnoSFA,sppChange_FCnoCRAnoSFA2,sppChange_FCnoCRAnoSFA3)~spp.x,
  out_SppChange_FCnoCRAnoSFA,FUN=sum)
write.csv(out_ChangeSpp_FCnoCRAnoSFA,file="out_ChangeSpp_FCnoCRAnoSFA_Hab1.csv")

###Hab 4  STEP 3a:Make a table of suitable habitat in IDCbrazil_2010####
#this creates table of all potentially suitable habitat available in each cell 
#(based on model outputs for each of the species - irrespective of whether species occurs there)
spplc_IDCbrazil_2010<-merge(spp_HabPref4,Land_IDCbrazil_2010, by.x="lulc", by.y="lulc", all=FALSE)

head(spplc_IDCbrazil_2010)
out_spplcCR_IDCbrazil_2010<-aggregate(lc_area~spp+sp_id+colrow,spplc_IDCbrazil_2010,FUN=sum)
names(out_spplcCR_IDCbrazil_2010)  

remove(spplc_IDCbrazil_2010)  
#concatonate values from species and colrow columns into a new column
out_spplcCR_IDCbrazil_2010$CR_sp <-paste (out_spplcCR_IDCbrazil_2010$colrow, 
                                    out_spplcCR_IDCbrazil_2010$sp_id, sep = "_", collapse = NULL)
names(spp_overlap)
names(out_spplcCR_IDCbrazil_2010)
spp_overlap_IDCbrazil_2010<-merge(spp_overlap, out_spplcCR_IDCbrazil_2010, by="CR_sp", by.y="CR_sp", all=FALSE)
names(spp_overlap_IDCbrazil_2010)
remove(out_spplcCR_IDCbrazil_2010)
spp_overlap_IDCbrazil_2010sub<-spp_overlap_IDCbrazil_2010[c( "CR_sp","sp_id.x","spp.x","spCRprop","colrow.x",
                                                 "lc_area" )]
remove(spp_overlap_IDCbrazil_2010)
out_SppOverlap_IDCbrazil_2010<-merge(colrow,spp_overlap_IDCbrazil_2010sub,by.x="colrow",by.y="colrow.x")
remove(spp_overlap_IDCbrazil_2010sub)

out_SppOverlap_IDCbrazil_2010$suitarea_max_IDCbrazil_2010<-ifelse(
  (out_SppOverlap_IDCbrazil_2010$spCRprop*out_SppOverlap_IDCbrazil_2010$CRArea) < out_SppOverlap_IDCbrazil_2010$lc_area, 
  (out_SppOverlap_IDCbrazil_2010$spCRprop*out_SppOverlap_IDCbrazil_2010$CRArea), out_SppOverlap_IDCbrazil_2010$lc_area)

setwd(outputpath)
write.csv(out_SppOverlap_IDCbrazil_2010,file="out_SppOverlap_IDCbrazil_2010.csv",row.names=FALSE, quote=FALSE)
out_SppOverlap_IDCbrazil_2010<-read.csv("out_SppOverlap_IDCbrazil_2010.csv")
###STEP 3b:Make a table of suitable habitat in IDCbrazil_2050####
#this creates table of all potentially suitable habitat available in each cell in 2050
#(based on model outputs for each of the species - irrespective of whether species occurs there)
spplc_IDCbrazil_2050<-merge(spp_HabPref1,Land_IDCbrazil_2050, by.x="lulc", by.y="lulc", all=FALSE)
#aggregate total area of suitable land cover in cell by species name
out_spplcCR_IDCbrazil_2050<-aggregate(lc_area~spp+sp_id+colrow,spplc_IDCbrazil_2050,FUN=sum)
names(out_spplcCR_IDCbrazil_2050)
remove(spplc_IDCbrazil_2050)
#concatonate values from species and colrow columns into a new column
out_spplcCR_IDCbrazil_2050$CR_sp <-paste (out_spplcCR_IDCbrazil_2050$colrow, 
                                    out_spplcCR_IDCbrazil_2050$sp_id, sep = "_", collapse = NULL)
out_spplcCR_IDCbrazil_2050<-out_spplcCR_IDCbrazil_2050[c("lc_area", "CR_sp" )]
#this creates a table of the suitable habitat for each species in each cell taking account species 
#overlap and assuming that the species preferentially occurs in suitable habitat
names(spp_overlap)
names(out_spplcCR_IDCbrazil_2050)
spp_overlap_IDCbrazil_2050<-merge(out_SppOverlap_IDCbrazil_2010, out_spplcCR_IDCbrazil_2050, by="CR_sp", by.y="CR_sp", all=FALSE, suffixes = c(".2010",".2050"))
names(spp_overlap_IDCbrazil_2050)
remove(out_spplcCR_IDCbrazil_2050)
out_SppOverlap_IDCbrazil_2050<-spp_overlap_IDCbrazil_2050
out_SppOverlap_IDCbrazil_2050$suitarea_max_IDCbrazil_2050<-ifelse(
  (out_SppOverlap_IDCbrazil_2050$spCRprop*out_SppOverlap_IDCbrazil_2050$CRArea) < out_SppOverlap_IDCbrazil_2050$lc_area.2050, 
  (out_SppOverlap_IDCbrazil_2050$spCRprop*out_SppOverlap_IDCbrazil_2050$CRArea), out_SppOverlap_IDCbrazil_2050$lc_area.2050)
out_SppOverlap_IDCbrazil_2050$suitarea_mean_IDCbrazil_2050<-ifelse(
  out_SppOverlap_IDCbrazil_2050$lc_area.2010==0, (out_SppOverlap_IDCbrazil_2050$spCRprop*out_SppOverlap_IDCbrazil_2050$lc_area.2050),
  ifelse((
    out_SppOverlap_IDCbrazil_2050$suitarea_max_IDCbrazil_2010*
      (out_SppOverlap_IDCbrazil_2050$lc_area.2050/out_SppOverlap_IDCbrazil_2050$lc_area.2010))>(out_SppOverlap_IDCbrazil_2050$spCRprop*out_SppOverlap_IDCbrazil_2050$CRArea),
    (out_SppOverlap_IDCbrazil_2050$spCRprop*out_SppOverlap_IDCbrazil_2050$CRArea),
    (out_SppOverlap_IDCbrazil_2050$suitarea_max_IDCbrazil_2010*
       (out_SppOverlap_IDCbrazil_2050$lc_area.2050/out_SppOverlap_IDCbrazil_2050$lc_area.2010))))
out_SppOverlap_IDCbrazil_2050$suitarea_min_IDCbrazil_2050<-ifelse(
  (out_SppOverlap_IDCbrazil_2050$lc_area.2050-out_SppOverlap_IDCbrazil_2050$lc_area.2010) >0,
  out_SppOverlap_IDCbrazil_2050$suitarea_max_IDCbrazil_2010, 
  ifelse((out_SppOverlap_IDCbrazil_2050$lc_area.2010-out_SppOverlap_IDCbrazil_2050$lc_area.2050)>
           (out_SppOverlap_IDCbrazil_2050$suitarea_max_IDCbrazil_2010),
         0,
         out_SppOverlap_IDCbrazil_2050$suitarea_max_IDCbrazil_2010-
           (out_SppOverlap_IDCbrazil_2050$lc_area.2010-out_SppOverlap_IDCbrazil_2050$lc_area.2050)))
###STEP 4a: Makes a table of total suitable habitat in the region IDCbrazil_2010 ####
#table is called:(out_spp_suitarearegion_IDCbrazil_2010)
out_spp_suitarearegion_IDCbrazil_2010<-aggregate(suitarea_max_IDCbrazil_2010~spp.x,
                                           out_SppOverlap_IDCbrazil_2010,FUN=sum)
#renaming columns/fields
names(out_spp_suitarearegion_IDCbrazil_2010)[
  names(out_spp_suitarearegion_IDCbrazil_2010) == 'suitarea_max_IDCbrazil_2010'] <- 'suitRegion_max_IDCbrazil_2010'
#save output
write.csv(out_spp_suitarearegion_IDCbrazil_2010,file="out_spp_suitarearegion_IDCbrazil_2010.csv",
          row.names=FALSE, quote=FALSE)
str(out_spp_suitarearegion_IDCbrazil_2010)
###STEP 5a: makes a table of change in suitable habitat for each spp IDCbrazil_2010-2050####
#view tables/dataframes to check column names from which to join tables
names(out_spp_suitarearegion_IDCbrazil_2010)
names(out_SppOverlap_IDCbrazil_2010)
names(out_SppOverlap_IDCbrazil_2050)
#join tables together to create one large table for calculations
out_SppChange_IDCbrazil<-merge(out_SppOverlap_IDCbrazil_2050, out_spp_suitarearegion_IDCbrazil_2010, 
                         by="spp.x",all=FALSE)
remove(out_SppOverlap_IDCbrazil_2010)
remove(out_SppOverlap_IDCbrazil_2050)
remove(out_spp_suitarearegion_IDCbrazil_2010)
names(out_SppChange_IDCbrazil)
out_SppChange_IDCbrazil$sppChange_IDCbrazil<-ifelse (
  out_SppChange_IDCbrazil$suitRegion_max_IDCbrazil_2010==0,0,
  ((out_SppChange_IDCbrazil$suitarea_max_IDCbrazil_2050-out_SppChange_IDCbrazil$suitarea_max_IDCbrazil_2010)/
     out_SppChange_IDCbrazil$suitRegion_max_IDCbrazil_2010))
out_SppChange_IDCbrazil$sppChange_IDCbrazil2<-ifelse (
  out_SppChange_IDCbrazil$suitRegion_max_IDCbrazil_2010==0,0,
  ((out_SppChange_IDCbrazil$suitarea_mean_IDCbrazil_2050-out_SppChange_IDCbrazil$suitarea_max_IDCbrazil_2010)/
     out_SppChange_IDCbrazil$suitRegion_max_IDCbrazil_2010))
out_SppChange_IDCbrazil$sppChange_IDCbrazil3<-ifelse (
  out_SppChange_IDCbrazil$suitRegion_max_IDCbrazil_2010==0,0,
  ((out_SppChange_IDCbrazil$suitarea_min_IDCbrazil_2050-out_SppChange_IDCbrazil$suitarea_max_IDCbrazil_2010)/
     out_SppChange_IDCbrazil$suitRegion_max_IDCbrazil_2010)) 
write.csv(out_SppChange_IDCbrazil,file="out_SppChange_IDCbrazil_Hab4.csv")
###STEP 6a: makes table of change combined across spp for each colrow IDCbrazil_2010-2050####
out_ChangeCR_IDCbrazil<-aggregate(
  cbind(sppChange_IDCbrazil,sppChange_IDCbrazil2,sppChange_IDCbrazil3)~colrow,out_SppChange_IDCbrazil,FUN=sum)
write.csv(out_ChangeCR_IDCbrazil,file="out_ChangeCR_IDCbrazil_Hab4.csv")
###STEP 7a: makes table of change combined across CRs for each spp IDCbrazil_2010-2050####
out_ChangeSpp_IDCbrazil<-aggregate(
  cbind(sppChange_IDCbrazil,sppChange_IDCbrazil2,sppChange_IDCbrazil3)~spp.x,out_SppChange_IDCbrazil,FUN=sum)
write.csv(out_ChangeSpp_IDCbrazil,file="out_ChangeSpp_IDCbrazil_Hab4.csv")

###STEP 3a:Make a table of suitable habitat in IDCno_2010####
#this creates table of all potentially suitable habitat available in each cell 
#(based on model outputs for each of the species - irrespective of whether species occurs there)
spplc_IDCno_2010<-merge(spp_HabPref4,Land_IDCno_2010, by.x="lulc", by.y="lulc", all=FALSE)

head(spplc_IDCno_2010)
out_spplcCR_IDCno_2010<-aggregate(lc_area~spp+sp_id+colrow,spplc_IDCno_2010,FUN=sum)
names(out_spplcCR_IDCno_2010)  

remove(spplc_IDCno_2010)  
#concatonate values from species and colrow columns into a new column
out_spplcCR_IDCno_2010$CR_sp <-paste (out_spplcCR_IDCno_2010$colrow, 
                                          out_spplcCR_IDCno_2010$sp_id, sep = "_", collapse = NULL)
names(spp_overlap)
names(out_spplcCR_IDCno_2010)
spp_overlap_IDCno_2010<-merge(spp_overlap, out_spplcCR_IDCno_2010, by="CR_sp", by.y="CR_sp", all=FALSE)
names(spp_overlap_IDCno_2010)
remove(out_spplcCR_IDCno_2010)
spp_overlap_IDCno_2010sub<-spp_overlap_IDCno_2010[c( "CR_sp","sp_id.x","spp.x","spCRprop","colrow.x",
                                                             "lc_area" )]
remove(spp_overlap_IDCno_2010)
out_SppOverlap_IDCno_2010<-merge(colrow,spp_overlap_IDCno_2010sub,by.x="colrow",by.y="colrow.x")
remove(spp_overlap_IDCno_2010sub)

out_SppOverlap_IDCno_2010$suitarea_max_IDCno_2010<-ifelse(
  (out_SppOverlap_IDCno_2010$spCRprop*out_SppOverlap_IDCno_2010$CRArea) < out_SppOverlap_IDCno_2010$lc_area, 
  (out_SppOverlap_IDCno_2010$spCRprop*out_SppOverlap_IDCno_2010$CRArea), out_SppOverlap_IDCno_2010$lc_area)

setwd(outputpath)
write.csv(out_SppOverlap_IDCno_2010,file="out_SppOverlap_IDCno_2010.csv",row.names=FALSE, quote=FALSE)
out_SppOverlap_IDCno_2010<-read.csv("out_SppOverlap_IDCno_2010.csv")
###STEP 3b:Make a table of suitable habitat in IDCno_2050####
#this creates table of all potentially suitable habitat available in each cell in 2050
#(based on model outputs for each of the species - irrespective of whether species occurs there)
spplc_IDCno_2050<-merge(spp_HabPref1,Land_IDCno_2050, by.x="lulc", by.y="lulc", all=FALSE)
#aggregate total area of suitable land cover in cell by species name
out_spplcCR_IDCno_2050<-aggregate(lc_area~spp+sp_id+colrow,spplc_IDCno_2050,FUN=sum)
names(out_spplcCR_IDCno_2050)
remove(spplc_IDCno_2050)
#concatonate values from species and colrow columns into a new column
out_spplcCR_IDCno_2050$CR_sp <-paste (out_spplcCR_IDCno_2050$colrow, 
                                          out_spplcCR_IDCno_2050$sp_id, sep = "_", collapse = NULL)
out_spplcCR_IDCno_2050<-out_spplcCR_IDCno_2050[c("lc_area", "CR_sp" )]
#this creates a table of the suitable habitat for each species in each cell taking account species 
#overlap and assuming that the species preferentially occurs in suitable habitat
names(spp_overlap)
names(out_spplcCR_IDCno_2050)
spp_overlap_IDCno_2050<-merge(out_SppOverlap_IDCno_2010, out_spplcCR_IDCno_2050, by="CR_sp", by.y="CR_sp", all=FALSE, suffixes = c(".2010",".2050"))
names(spp_overlap_IDCno_2050)
remove(out_spplcCR_IDCno_2050)
out_SppOverlap_IDCno_2050<-spp_overlap_IDCno_2050
out_SppOverlap_IDCno_2050$suitarea_max_IDCno_2050<-ifelse(
  (out_SppOverlap_IDCno_2050$spCRprop*out_SppOverlap_IDCno_2050$CRArea) < out_SppOverlap_IDCno_2050$lc_area.2050, 
  (out_SppOverlap_IDCno_2050$spCRprop*out_SppOverlap_IDCno_2050$CRArea), out_SppOverlap_IDCno_2050$lc_area.2050)
out_SppOverlap_IDCno_2050$suitarea_mean_IDCno_2050<-ifelse(
  out_SppOverlap_IDCno_2050$lc_area.2010==0, (out_SppOverlap_IDCno_2050$spCRprop*out_SppOverlap_IDCno_2050$lc_area.2050),
  ifelse((
    out_SppOverlap_IDCno_2050$suitarea_max_IDCno_2010*
      (out_SppOverlap_IDCno_2050$lc_area.2050/out_SppOverlap_IDCno_2050$lc_area.2010))>(out_SppOverlap_IDCno_2050$spCRprop*out_SppOverlap_IDCno_2050$CRArea),
    (out_SppOverlap_IDCno_2050$spCRprop*out_SppOverlap_IDCno_2050$CRArea),
    (out_SppOverlap_IDCno_2050$suitarea_max_IDCno_2010*
       (out_SppOverlap_IDCno_2050$lc_area.2050/out_SppOverlap_IDCno_2050$lc_area.2010))))
out_SppOverlap_IDCno_2050$suitarea_min_IDCno_2050<-ifelse(
  (out_SppOverlap_IDCno_2050$lc_area.2050-out_SppOverlap_IDCno_2050$lc_area.2010) >0,
  out_SppOverlap_IDCno_2050$suitarea_max_IDCno_2010, 
  ifelse((out_SppOverlap_IDCno_2050$lc_area.2010-out_SppOverlap_IDCno_2050$lc_area.2050)>
           (out_SppOverlap_IDCno_2050$suitarea_max_IDCno_2010),
         0,
         out_SppOverlap_IDCno_2050$suitarea_max_IDCno_2010-
           (out_SppOverlap_IDCno_2050$lc_area.2010-out_SppOverlap_IDCno_2050$lc_area.2050)))
###STEP 4a: Makes a table of total suitable habitat in the region IDCno_2010 ####
#table is called:(out_spp_suitarearegion_IDCno_2010)
out_spp_suitarearegion_IDCno_2010<-aggregate(suitarea_max_IDCno_2010~spp.x,
                                                 out_SppOverlap_IDCno_2010,FUN=sum)
#renaming columns/fields
names(out_spp_suitarearegion_IDCno_2010)[
  names(out_spp_suitarearegion_IDCno_2010) == 'suitarea_max_IDCno_2010'] <- 'suitRegion_max_IDCno_2010'
#save output
write.csv(out_spp_suitarearegion_IDCno_2010,file="out_spp_suitarearegion_IDCno_2010.csv",
          row.names=FALSE, quote=FALSE)
str(out_spp_suitarearegion_IDCno_2010)
###STEP 5a: makes a table of change in suitable habitat for each spp IDCno_2010-2050####
#view tables/dataframes to check column names from which to join tables
names(out_spp_suitarearegion_IDCno_2010)
names(out_SppOverlap_IDCno_2010)
names(out_SppOverlap_IDCno_2050)
#join tables together to create one large table for calculations
out_SppChange_IDCno<-merge(out_SppOverlap_IDCno_2050, out_spp_suitarearegion_IDCno_2010, 
                               by="spp.x",all=FALSE)
remove(out_SppOverlap_IDCno_2010)
remove(out_SppOverlap_IDCno_2050)
remove(out_spp_suitarearegion_IDCno_2010)
names(out_SppChange_IDCno)
out_SppChange_IDCno$sppChange_IDCno<-ifelse (
  out_SppChange_IDCno$suitRegion_max_IDCno_2010==0,0,
  ((out_SppChange_IDCno$suitarea_max_IDCno_2050-out_SppChange_IDCno$suitarea_max_IDCno_2010)/
     out_SppChange_IDCno$suitRegion_max_IDCno_2010))
out_SppChange_IDCno$sppChange_IDCno2<-ifelse (
  out_SppChange_IDCno$suitRegion_max_IDCno_2010==0,0,
  ((out_SppChange_IDCno$suitarea_mean_IDCno_2050-out_SppChange_IDCno$suitarea_max_IDCno_2010)/
     out_SppChange_IDCno$suitRegion_max_IDCno_2010))
out_SppChange_IDCno$sppChange_IDCno3<-ifelse (
  out_SppChange_IDCno$suitRegion_max_IDCno_2010==0,0,
  ((out_SppChange_IDCno$suitarea_min_IDCno_2050-out_SppChange_IDCno$suitarea_max_IDCno_2010)/
     out_SppChange_IDCno$suitRegion_max_IDCno_2010)) 
write.csv(out_SppChange_IDCno,file="out_SppChange_IDCno_Hab4.csv")
###STEP 6a: makes table of change combined across spp for each colrow IDCno_2010-2050####
out_ChangeCR_IDCno<-aggregate(
  cbind(sppChange_IDCno,sppChange_IDCno2,sppChange_IDCno3)~colrow,out_SppChange_IDCno,FUN=sum)
write.csv(out_ChangeCR_IDCno,file="out_ChangeCR_IDCno_Hab4.csv")
###STEP 7a: makes table of change combined across CRs for each spp IDCno_2010-2050####
out_ChangeSpp_IDCno<-aggregate(
  cbind(sppChange_IDCno,sppChange_IDCno2,sppChange_IDCno3)~spp.x,out_SppChange_IDCno,FUN=sum)
write.csv(out_ChangeSpp_IDCno,file="out_ChangeSpp_IDCno_Hab4.csv")

###STEP 3a:Make a table of suitable habitat in IDCamazon_2010####
#this creates table of all potentially suitable habitat available in each cell 
#(based on model outputs for each of the species - irrespective of whether species occurs there)
spplc_IDCamazon_2010<-merge(spp_HabPref4,Land_IDCamazon_2010, by.x="lulc", by.y="lulc", all=FALSE)

head(spplc_IDCamazon_2010)
out_spplcCR_IDCamazon_2010<-aggregate(lc_area~spp+sp_id+colrow,spplc_IDCamazon_2010,FUN=sum)
names(out_spplcCR_IDCamazon_2010)  

remove(spplc_IDCamazon_2010)  
#concatonate values from species and colrow columns into a new column
out_spplcCR_IDCamazon_2010$CR_sp <-paste (out_spplcCR_IDCamazon_2010$colrow, 
                                          out_spplcCR_IDCamazon_2010$sp_id, sep = "_", collapse = NULL)
names(spp_overlap)
names(out_spplcCR_IDCamazon_2010)
spp_overlap_IDCamazon_2010<-merge(spp_overlap, out_spplcCR_IDCamazon_2010, by="CR_sp", by.y="CR_sp", all=FALSE)
names(spp_overlap_IDCamazon_2010)
remove(out_spplcCR_IDCamazon_2010)
spp_overlap_IDCamazon_2010sub<-spp_overlap_IDCamazon_2010[c( "CR_sp","sp_id.x","spp.x","spCRprop","colrow.x",
                                                             "lc_area" )]
remove(spp_overlap_IDCamazon_2010)
out_SppOverlap_IDCamazon_2010<-merge(colrow,spp_overlap_IDCamazon_2010sub,by.x="colrow",by.y="colrow.x")
remove(spp_overlap_IDCamazon_2010sub)

out_SppOverlap_IDCamazon_2010$suitarea_max_IDCamazon_2010<-ifelse(
  (out_SppOverlap_IDCamazon_2010$spCRprop*out_SppOverlap_IDCamazon_2010$CRArea) < out_SppOverlap_IDCamazon_2010$lc_area, 
  (out_SppOverlap_IDCamazon_2010$spCRprop*out_SppOverlap_IDCamazon_2010$CRArea), out_SppOverlap_IDCamazon_2010$lc_area)

setwd(outputpath)
write.csv(out_SppOverlap_IDCamazon_2010,file="out_SppOverlap_IDCamazon_2010.csv",row.names=FALSE, quote=FALSE)
out_SppOverlap_IDCamazon_2010<-read.csv("out_SppOverlap_IDCamazon_2010.csv")
###STEP 3b:Make a table of suitable habitat in IDCamazon_2050####
#this creates table of all potentially suitable habitat available in each cell in 2050
#(based on model outputs for each of the species - irrespective of whether species occurs there)
spplc_IDCamazon_2050<-merge(spp_HabPref1,Land_IDCamazon_2050, by.x="lulc", by.y="lulc", all=FALSE)
#aggregate total area of suitable land cover in cell by species name
out_spplcCR_IDCamazon_2050<-aggregate(lc_area~spp+sp_id+colrow,spplc_IDCamazon_2050,FUN=sum)
names(out_spplcCR_IDCamazon_2050)
remove(spplc_IDCamazon_2050)
#concatonate values from species and colrow columns into a new column
out_spplcCR_IDCamazon_2050$CR_sp <-paste (out_spplcCR_IDCamazon_2050$colrow, 
                                          out_spplcCR_IDCamazon_2050$sp_id, sep = "_", collapse = NULL)
out_spplcCR_IDCamazon_2050<-out_spplcCR_IDCamazon_2050[c("lc_area", "CR_sp" )]
#this creates a table of the suitable habitat for each species in each cell taking account species 
#overlap and assuming that the species preferentially occurs in suitable habitat
names(spp_overlap)
names(out_spplcCR_IDCamazon_2050)
spp_overlap_IDCamazon_2050<-merge(out_SppOverlap_IDCamazon_2010, out_spplcCR_IDCamazon_2050, by="CR_sp", by.y="CR_sp", all=FALSE, suffixes = c(".2010",".2050"))
names(spp_overlap_IDCamazon_2050)
remove(out_spplcCR_IDCamazon_2050)
out_SppOverlap_IDCamazon_2050<-spp_overlap_IDCamazon_2050
out_SppOverlap_IDCamazon_2050$suitarea_max_IDCamazon_2050<-ifelse(
  (out_SppOverlap_IDCamazon_2050$spCRprop*out_SppOverlap_IDCamazon_2050$CRArea) < out_SppOverlap_IDCamazon_2050$lc_area.2050, 
  (out_SppOverlap_IDCamazon_2050$spCRprop*out_SppOverlap_IDCamazon_2050$CRArea), out_SppOverlap_IDCamazon_2050$lc_area.2050)
out_SppOverlap_IDCamazon_2050$suitarea_mean_IDCamazon_2050<-ifelse(
  out_SppOverlap_IDCamazon_2050$lc_area.2010==0, (out_SppOverlap_IDCamazon_2050$spCRprop*out_SppOverlap_IDCamazon_2050$lc_area.2050),
  ifelse((
    out_SppOverlap_IDCamazon_2050$suitarea_max_IDCamazon_2010*
      (out_SppOverlap_IDCamazon_2050$lc_area.2050/out_SppOverlap_IDCamazon_2050$lc_area.2010))>(out_SppOverlap_IDCamazon_2050$spCRprop*out_SppOverlap_IDCamazon_2050$CRArea),
    (out_SppOverlap_IDCamazon_2050$spCRprop*out_SppOverlap_IDCamazon_2050$CRArea),
    (out_SppOverlap_IDCamazon_2050$suitarea_max_IDCamazon_2010*
       (out_SppOverlap_IDCamazon_2050$lc_area.2050/out_SppOverlap_IDCamazon_2050$lc_area.2010))))
out_SppOverlap_IDCamazon_2050$suitarea_min_IDCamazon_2050<-ifelse(
  (out_SppOverlap_IDCamazon_2050$lc_area.2050-out_SppOverlap_IDCamazon_2050$lc_area.2010) >0,
  out_SppOverlap_IDCamazon_2050$suitarea_max_IDCamazon_2010, 
  ifelse((out_SppOverlap_IDCamazon_2050$lc_area.2010-out_SppOverlap_IDCamazon_2050$lc_area.2050)>
           (out_SppOverlap_IDCamazon_2050$suitarea_max_IDCamazon_2010),
         0,
         out_SppOverlap_IDCamazon_2050$suitarea_max_IDCamazon_2010-
           (out_SppOverlap_IDCamazon_2050$lc_area.2010-out_SppOverlap_IDCamazon_2050$lc_area.2050)))
###STEP 4a: Makes a table of total suitable habitat in the region IDCamazon_2010 ####
#table is called:(out_spp_suitarearegion_IDCamazon_2010)
out_spp_suitarearegion_IDCamazon_2010<-aggregate(suitarea_max_IDCamazon_2010~spp.x,
                                                 out_SppOverlap_IDCamazon_2010,FUN=sum)
#renaming columns/fields
names(out_spp_suitarearegion_IDCamazon_2010)[
  names(out_spp_suitarearegion_IDCamazon_2010) == 'suitarea_max_IDCamazon_2010'] <- 'suitRegion_max_IDCamazon_2010'
#save output
write.csv(out_spp_suitarearegion_IDCamazon_2010,file="out_spp_suitarearegion_IDCamazon_2010.csv",
          row.names=FALSE, quote=FALSE)
str(out_spp_suitarearegion_IDCamazon_2010)
###STEP 5a: makes a table of change in suitable habitat for each spp IDCamazon_2010-2050####
#view tables/dataframes to check column names from which to join tables
names(out_spp_suitarearegion_IDCamazon_2010)
names(out_SppOverlap_IDCamazon_2010)
names(out_SppOverlap_IDCamazon_2050)
#join tables together to create one large table for calculations
out_SppChange_IDCamazon<-merge(out_SppOverlap_IDCamazon_2050, out_spp_suitarearegion_IDCamazon_2010, 
                               by="spp.x",all=FALSE)
remove(out_SppOverlap_IDCamazon_2010)
remove(out_SppOverlap_IDCamazon_2050)
remove(out_spp_suitarearegion_IDCamazon_2010)
names(out_SppChange_IDCamazon)
out_SppChange_IDCamazon$sppChange_IDCamazon<-ifelse (
  out_SppChange_IDCamazon$suitRegion_max_IDCamazon_2010==0,0,
  ((out_SppChange_IDCamazon$suitarea_max_IDCamazon_2050-out_SppChange_IDCamazon$suitarea_max_IDCamazon_2010)/
     out_SppChange_IDCamazon$suitRegion_max_IDCamazon_2010))
out_SppChange_IDCamazon$sppChange_IDCamazon2<-ifelse (
  out_SppChange_IDCamazon$suitRegion_max_IDCamazon_2010==0,0,
  ((out_SppChange_IDCamazon$suitarea_mean_IDCamazon_2050-out_SppChange_IDCamazon$suitarea_max_IDCamazon_2010)/
     out_SppChange_IDCamazon$suitRegion_max_IDCamazon_2010))
out_SppChange_IDCamazon$sppChange_IDCamazon3<-ifelse (
  out_SppChange_IDCamazon$suitRegion_max_IDCamazon_2010==0,0,
  ((out_SppChange_IDCamazon$suitarea_min_IDCamazon_2050-out_SppChange_IDCamazon$suitarea_max_IDCamazon_2010)/
     out_SppChange_IDCamazon$suitRegion_max_IDCamazon_2010)) 
write.csv(out_SppChange_IDCamazon,file="out_SppChange_IDCamazon_Hab4.csv")
###STEP 6a: makes table of change combined across spp for each colrow IDCamazon_2010-2050####
out_ChangeCR_IDCamazon<-aggregate(
  cbind(sppChange_IDCamazon,sppChange_IDCamazon2,sppChange_IDCamazon3)~colrow,out_SppChange_IDCamazon,FUN=sum)
write.csv(out_ChangeCR_IDCamazon,file="out_ChangeCR_IDCamazon_Hab4.csv")
###STEP 7a: makes table of change combined across CRs for each spp IDCamazon_2010-2050####
out_ChangeSpp_IDCamazon<-aggregate(
  cbind(sppChange_IDCamazon,sppChange_IDCamazon2,sppChange_IDCamazon3)~spp.x,out_SppChange_IDCamazon,FUN=sum)
write.csv(out_ChangeSpp_IDCamazon,file="out_ChangeSpp_IDCamazon_Hab4.csv")

###STEP 3c:Make a table of suitable habitat in FC_2010####
#this creates table of all potentially suitable habitat available in each cell 
#(based on model outputs for each of the species - irrespective of whether species occurs there)
spplc_FC_2010<-merge(spp_HabPref4,Land_FC_2010, by.x="lulc", by.y="lulc", all=FALSE)
#aggregate total area of suitable land cover in cell by species name
out_spplcCR_FC_2010<-aggregate(lc_area~spp+sp_id+colrow,spplc_FC_2010,FUN=sum)
names(out_spplcCR_FC_2010)
remove(spplc_FC_2010)
#concatonate values from species and colrow columns into a new column
out_spplcCR_FC_2010$CR_sp <-paste (out_spplcCR_FC_2010$colrow, 
                                   out_spplcCR_FC_2010$sp_id, sep = "_", collapse = NULL)
#this creates a table of the suitable habitat for each species in each cell taking account species 
#overlap and assuming that the species preferentially occurs in suitable habitat
names(spp_overlap)
names(out_spplcCR_FC_2010)
spp_overlap_FC_2010<-merge(spp_overlap, out_spplcCR_FC_2010, by="CR_sp", by.y="CR_sp", all=FALSE)
names(spp_overlap_FC_2010)
remove(out_spplcCR_FC_2010)
spp_overlap_FC_2010sub<-spp_overlap_FC_2010[c( "CR_sp","sp_id.x","spp.x","spCRprop","colrow.x",
                                               "lc_area" )]
remove(spp_overlap_FC_2010)
out_SppOverlap_FC_2010<-merge(colrow,spp_overlap_FC_2010sub,by.x="colrow",by.y="colrow.x")
remove(spp_overlap_FC_2010sub)
out_SppOverlap_FC_2010$suitarea_max_FC_2010<-ifelse(
  (out_SppOverlap_FC_2010$spCRprop*out_SppOverlap_FC_2010$CRArea) < out_SppOverlap_FC_2010$lc_area, 
  (out_SppOverlap_FC_2010$spCRprop*out_SppOverlap_FC_2010$CRArea), out_SppOverlap_FC_2010$lc_area)
write.csv(out_SppOverlap_FC_2010,file="out_SppOverlap_FC_2010.csv",row.names=FALSE, quote=FALSE)
###STEP 3b:Make a table of suitable habitat in FC_2050####
#this creates table of all potentially suitable habitat available in each cell in 2050
#(based on model outputs for each of the species - irrespective of whether species occurs there)
spplc_FC_2050<-merge(spp_HabPref4,Land_FC_2050, by.x="lulc", by.y="lulc", all=FALSE)
#aggregate total area of suitable land cover in cell by species name
out_spplcCR_FC_2050<-aggregate(lc_area~spp+sp_id+colrow,spplc_FC_2050,FUN=sum)
names(out_spplcCR_FC_2050)
remove(spplc_FC_2050)
#concatonate values from species and colrow columns into a new column
out_spplcCR_FC_2050$CR_sp <-paste (out_spplcCR_FC_2050$colrow, 
                                    out_spplcCR_FC_2050$sp_id, sep = "_", collapse = NULL)
out_spplcCR_FC_2050<-out_spplcCR_FC_2050[c("lc_area", "CR_sp" )]
#this creates a table of the suitable habitat for each species in each cell taking account species 
#overlap and assuming that the species preferentially occurs in suitable habitat
names(spp_overlap)
names(out_spplcCR_FC_2050)
spp_overlap_FC_2050<-merge(out_SppOverlap_FC_2010, out_spplcCR_FC_2050, by="CR_sp", by.y="CR_sp", all=FALSE, suffixes = c(".2010",".2050"))
names(spp_overlap_FC_2050)
remove(out_spplcCR_FC_2050)
out_SppOverlap_FC_2050<-spp_overlap_FC_2050
out_SppOverlap_FC_2050$suitarea_max_FC_2050<-ifelse(
  (out_SppOverlap_FC_2050$spCRprop*out_SppOverlap_FC_2050$CRArea) < out_SppOverlap_FC_2050$lc_area.2050, 
  (out_SppOverlap_FC_2050$spCRprop*out_SppOverlap_FC_2050$CRArea), out_SppOverlap_FC_2050$lc_area.2050)
out_SppOverlap_FC_2050$suitarea_mean_FC_2050<-ifelse(
  out_SppOverlap_FC_2050$lc_area.2010==0, (out_SppOverlap_FC_2050$spCRprop*out_SppOverlap_FC_2050$lc_area.2050),
  ifelse((
    out_SppOverlap_FC_2050$suitarea_max_FC_2010*
      (out_SppOverlap_FC_2050$lc_area.2050/out_SppOverlap_FC_2050$lc_area.2010))>(out_SppOverlap_FC_2050$spCRprop*out_SppOverlap_FC_2050$CRArea),
    (out_SppOverlap_FC_2050$spCRprop*out_SppOverlap_FC_2050$CRArea),
    (out_SppOverlap_FC_2050$suitarea_max_FC_2010*
       (out_SppOverlap_FC_2050$lc_area.2050/out_SppOverlap_FC_2050$lc_area.2010))))
out_SppOverlap_FC_2050$suitarea_min_FC_2050<-ifelse(
  (out_SppOverlap_FC_2050$lc_area.2050-out_SppOverlap_FC_2050$lc_area.2010) >0,
  out_SppOverlap_FC_2050$suitarea_max_FC_2010, 
  ifelse((out_SppOverlap_FC_2050$lc_area.2010-out_SppOverlap_FC_2050$lc_area.2050)>
           (out_SppOverlap_FC_2050$suitarea_max_FC_2010),
         0,
         out_SppOverlap_FC_2050$suitarea_max_FC_2010-
           (out_SppOverlap_FC_2050$lc_area.2010-out_SppOverlap_FC_2050$lc_area.2050)))
###STEP 4a: Makes a table of total suitable habitat in the region FC_2010 ####
#table is called:(out_spp_suitarearegion_FC_2010)
out_spp_suitarearegion_FC_2010<-aggregate(suitarea_max_FC_2010~spp.x,
                                           out_SppOverlap_FC_2010,FUN=sum)
#renaming columns/fields
names(out_spp_suitarearegion_FC_2010)[
  names(out_spp_suitarearegion_FC_2010) == 'suitarea_max_FC_2010'] <- 'suitRegion_max_FC_2010'
#save output
write.csv(out_spp_suitarearegion_FC_2010,file="out_spp_suitarearegion_FC_2010.csv",
          row.names=FALSE, quote=FALSE)
str(out_spp_suitarearegion_FC_2010)
###STEP 5a: makes a table of change in suitable habitat for each spp FC_2010-2050####
#view tables/dataframes to check column names from which to join tables
names(out_spp_suitarearegion_FC_2010)
names(out_SppOverlap_FC_2010)
names(out_SppOverlap_FC_2050)
#join tables together to create one large table for calculations
out_SppChange_FC<-merge(out_SppOverlap_FC_2050, out_spp_suitarearegion_FC_2010, 
                         by="spp.x",all=FALSE)
remove(out_SppOverlap_FC_2010)
remove(out_SppOverlap_FC_2050)
remove(out_spp_suitarearegion_FC_2010)
names(out_SppChange_FC)
out_SppChange_FC$sppChange_FC<-ifelse (
  out_SppChange_FC$suitRegion_max_FC_2010==0,0,
  ((out_SppChange_FC$suitarea_max_FC_2050-out_SppChange_FC$suitarea_max_FC_2010)/
     out_SppChange_FC$suitRegion_max_FC_2010))
out_SppChange_FC$sppChange_FC2<-ifelse (
  out_SppChange_FC$suitRegion_max_FC_2010==0,0,
  ((out_SppChange_FC$suitarea_mean_FC_2050-out_SppChange_FC$suitarea_max_FC_2010)/
     out_SppChange_FC$suitRegion_max_FC_2010))
out_SppChange_FC$sppChange_FC3<-ifelse (
  out_SppChange_FC$suitRegion_max_FC_2010==0,0,
  ((out_SppChange_FC$suitarea_min_FC_2050-out_SppChange_FC$suitarea_max_FC_2010)/
     out_SppChange_FC$suitRegion_max_FC_2010)) 
write.csv(out_SppChange_FC,file="out_SppChange_FC_Hab4.csv")
###STEP 6b: makes table of change combined across spp for each colrow FC_2010-2050####
out_ChangeCR_FC<-aggregate(
  cbind(sppChange_FC,sppChange_FC2,sppChange_FC3)~colrow,out_SppChange_FC,FUN=sum)
write.csv(out_ChangeCR_FC,file="out_ChangeCR_FC_Hab4.csv")
###STEP 7b: makes table of change combined across CRs for each spp FC_2010-2050####
out_ChangeSpp_FC<-aggregate(
  cbind(sppChange_FC,sppChange_FC2,sppChange_FC3)~spp.x,out_SppChange_FC,FUN=sum)
write.csv(out_ChangeSpp_FC,file="out_ChangeSpp_FC_Hab4.csv")

###STEP 3c:Make a table of suitable habitat in FCnoSFA_2010####
#this creates table of all potentially suitable habitat available in each cell 
#(based on model outputs for each of the species - irrespective of whether species occurs there)
spplc_FCnoSFA_2010<-merge(spp_HabPref4,Land_FCnoSFA_2010, by.x="lulc", by.y="lulc", all=FALSE)
#aggregate total area of suitable land cover in cell by species name
out_spplcCR_FCnoSFA_2010<-aggregate(lc_area~spp+sp_id+colrow,spplc_FCnoSFA_2010,FUN=sum)
names(out_spplcCR_FCnoSFA_2010)
remove(spplc_FCnoSFA_2010)
#concatonate values from species and colrow columns into a new column
out_spplcCR_FCnoSFA_2010$CR_sp <-paste (out_spplcCR_FCnoSFA_2010$colrow, 
                                             out_spplcCR_FCnoSFA_2010$sp_id, sep = "_", collapse = NULL)
#this creates a table of the suitable habitat for each species in each cell taking account species 
#overlap and assuming that the species preferentially occurs in suitable habitat
names(spp_overlap)
names(out_spplcCR_FCnoSFA_2010)
spp_overlap_FCnoSFA_2010<-merge(spp_overlap, out_spplcCR_FCnoSFA_2010, by="CR_sp", by.y="CR_sp", all=FALSE)
names(spp_overlap_FCnoSFA_2010)
remove(out_spplcCR_FCnoSFA_2010)
spp_overlap_FCnoSFA_2010sub<-spp_overlap_FCnoSFA_2010[c( "CR_sp","sp_id.x","spp.x","spCRprop","colrow.x",
                                                                   "lc_area" )]
remove(spp_overlap_FCnoSFA_2010)
out_SppOverlap_FCnoSFA_2010<-merge(colrow,spp_overlap_FCnoSFA_2010sub,by.x="colrow",by.y="colrow.x")
remove(spp_overlap_FCnoSFA_2010sub)
out_SppOverlap_FCnoSFA_2010$suitarea_max_FCnoSFA_2010<-ifelse(
  (out_SppOverlap_FCnoSFA_2010$spCRprop*out_SppOverlap_FCnoSFA_2010$CRArea) < out_SppOverlap_FCnoSFA_2010$lc_area, 
  (out_SppOverlap_FCnoSFA_2010$spCRprop*out_SppOverlap_FCnoSFA_2010$CRArea), out_SppOverlap_FCnoSFA_2010$lc_area)
write.csv(out_SppOverlap_FCnoSFA_2010,file="out_SppOverlap_FCnoSFA_2010.csv",row.names=FALSE, quote=FALSE)
###STEP 3b:Make a table of suitable habitat in FCnoSFA_2050####
#this creates table of all potentially suitable habitat available in each cell in 2050
#(based on model outputs for each of the species - irrespective of whether species occurs there)
spplc_FCnoSFA_2050<-merge(spp_HabPref4,Land_FCnoSFA_2050, by.x="lulc", by.y="lulc", all=FALSE)
#aggregate total area of suitable land cover in cell by species name
out_spplcCR_FCnoSFA_2050<-aggregate(lc_area~spp+sp_id+colrow,spplc_FCnoSFA_2050,FUN=sum)
names(out_spplcCR_FCnoSFA_2050)
remove(spplc_FCnoSFA_2050)
#concatonate values from species and colrow columns into a new column
out_spplcCR_FCnoSFA_2050$CR_sp <-paste (out_spplcCR_FCnoSFA_2050$colrow, 
                                   out_spplcCR_FCnoSFA_2050$sp_id, sep = "_", collapse = NULL)
out_spplcCR_FCnoSFA_2050<-out_spplcCR_FCnoSFA_2050[c("lc_area", "CR_sp" )]
#this creates a table of the suitable habitat for each species in each cell taking account species 
#overlap and assuming that the species preferentially occurs in suitable habitat
names(spp_overlap)
names(out_spplcCR_FCnoSFA_2050)
spp_overlap_FCnoSFA_2050<-merge(out_SppOverlap_FCnoSFA_2010, out_spplcCR_FCnoSFA_2050, by="CR_sp", by.y="CR_sp", all=FALSE, suffixes = c(".2010",".2050"))
names(spp_overlap_FCnoSFA_2050)
remove(out_spplcCR_FCnoSFA_2050)
out_SppOverlap_FCnoSFA_2050<-spp_overlap_FCnoSFA_2050
out_SppOverlap_FCnoSFA_2050$suitarea_max_FCnoSFA_2050<-ifelse(
  (out_SppOverlap_FCnoSFA_2050$spCRprop*out_SppOverlap_FCnoSFA_2050$CRArea) < out_SppOverlap_FCnoSFA_2050$lc_area.2050, 
  (out_SppOverlap_FCnoSFA_2050$spCRprop*out_SppOverlap_FCnoSFA_2050$CRArea), out_SppOverlap_FCnoSFA_2050$lc_area.2050)
out_SppOverlap_FCnoSFA_2050$suitarea_mean_FCnoSFA_2050<-ifelse(
  out_SppOverlap_FCnoSFA_2050$lc_area.2010==0, (out_SppOverlap_FCnoSFA_2050$spCRprop*out_SppOverlap_FCnoSFA_2050$lc_area.2050),
  ifelse((
    out_SppOverlap_FCnoSFA_2050$suitarea_max_FCnoSFA_2010*
      (out_SppOverlap_FCnoSFA_2050$lc_area.2050/out_SppOverlap_FCnoSFA_2050$lc_area.2010))>(out_SppOverlap_FCnoSFA_2050$spCRprop*out_SppOverlap_FCnoSFA_2050$CRArea),
    (out_SppOverlap_FCnoSFA_2050$spCRprop*out_SppOverlap_FCnoSFA_2050$CRArea),
    (out_SppOverlap_FCnoSFA_2050$suitarea_max_FCnoSFA_2010*
       (out_SppOverlap_FCnoSFA_2050$lc_area.2050/out_SppOverlap_FCnoSFA_2050$lc_area.2010))))
out_SppOverlap_FCnoSFA_2050$suitarea_min_FCnoSFA_2050<-ifelse(
  (out_SppOverlap_FCnoSFA_2050$lc_area.2050-out_SppOverlap_FCnoSFA_2050$lc_area.2010) >0,
  out_SppOverlap_FCnoSFA_2050$suitarea_max_FCnoSFA_2010, 
  ifelse((out_SppOverlap_FCnoSFA_2050$lc_area.2010-out_SppOverlap_FCnoSFA_2050$lc_area.2050)>
           (out_SppOverlap_FCnoSFA_2050$suitarea_max_FCnoSFA_2010),
         0,
         out_SppOverlap_FCnoSFA_2050$suitarea_max_FCnoSFA_2010-
           (out_SppOverlap_FCnoSFA_2050$lc_area.2010-out_SppOverlap_FCnoSFA_2050$lc_area.2050)))
###STEP 4a: Makes a table of total suitable habitat in the region FCnoSFA_2010 ####
#table is called:(out_spp_suitarearegion_FCnoSFA_2010)
out_spp_suitarearegion_FCnoSFA_2010<-aggregate(suitarea_max_FCnoSFA_2010~spp.x,
                                          out_SppOverlap_FCnoSFA_2010,FUN=sum)
#renaming columns/fields
names(out_spp_suitarearegion_FCnoSFA_2010)[
  names(out_spp_suitarearegion_FCnoSFA_2010) == 'suitarea_max_FCnoSFA_2010'] <- 'suitRegion_max_FCnoSFA_2010'
#save output
write.csv(out_spp_suitarearegion_FCnoSFA_2010,file="out_spp_suitarearegion_FCnoSFA_2010.csv",
          row.names=FALSE, quote=FALSE)
str(out_spp_suitarearegion_FCnoSFA_2010)
###STEP 5a: makes a table of change in suitable habitat for each spp FCnoSFA_2010-2050####
#view tables/dataframes to check column names from which to join tables
names(out_spp_suitarearegion_FCnoSFA_2010)
names(out_SppOverlap_FCnoSFA_2010)
names(out_SppOverlap_FCnoSFA_2050)
#join tables together to create one large table for calculations
out_SppChange_FCnoSFA<-merge(out_SppOverlap_FCnoSFA_2050, out_spp_suitarearegion_FCnoSFA_2010, 
                        by="spp.x",all=FALSE)
remove(out_SppOverlap_FCnoSFA_2010)
remove(out_SppOverlap_FCnoSFA_2050)
remove(out_spp_suitarearegion_FCnoSFA_2010)
names(out_SppChange_FCnoSFA)
out_SppChange_FCnoSFA$sppChange_FCnoSFA<-ifelse (
  out_SppChange_FCnoSFA$suitRegion_max_FCnoSFA_2010==0,0,
  ((out_SppChange_FCnoSFA$suitarea_max_FCnoSFA_2050-out_SppChange_FCnoSFA$suitarea_max_FCnoSFA_2010)/
     out_SppChange_FCnoSFA$suitRegion_max_FCnoSFA_2010))
out_SppChange_FCnoSFA$sppChange_FCnoSFA2<-ifelse (
  out_SppChange_FCnoSFA$suitRegion_max_FCnoSFA_2010==0,0,
  ((out_SppChange_FCnoSFA$suitarea_mean_FCnoSFA_2050-out_SppChange_FCnoSFA$suitarea_max_FCnoSFA_2010)/
     out_SppChange_FCnoSFA$suitRegion_max_FCnoSFA_2010))
out_SppChange_FCnoSFA$sppChange_FCnoSFA3<-ifelse (
  out_SppChange_FCnoSFA$suitRegion_max_FCnoSFA_2010==0,0,
  ((out_SppChange_FCnoSFA$suitarea_min_FCnoSFA_2050-out_SppChange_FCnoSFA$suitarea_max_FCnoSFA_2010)/
     out_SppChange_FCnoSFA$suitRegion_max_FCnoSFA_2010)) 
write.csv(out_SppChange_FCnoSFA,file="out_SppChange_FCnoSFA_Hab4.csv")
###STEP 6b: makes table of change combined across spp for each colrow FCnoSFA_2010-2050####
out_ChangeCR_FCnoSFA<-aggregate(
  cbind(sppChange_FCnoSFA,sppChange_FCnoSFA2,sppChange_FCnoSFA3)~colrow,
  out_SppChange_FCnoSFA,FUN=sum)
write.csv(out_ChangeCR_FCnoSFA,file="out_ChangeCR_FCnoSFA_Hab4.csv")
###STEP 7b: makes table of change combined across CRs for each spp FCnoSFA_2010-2050####
out_ChangeSpp_FCnoSFA<-aggregate(
  cbind(sppChange_FCnoSFA,sppChange_FCnoSFA2,sppChange_FCnoSFA3)~spp.x,
  out_SppChange_FCnoSFA,FUN=sum)
write.csv(out_ChangeSpp_FCnoSFA,file="out_ChangeSpp_FCnoSFA_Hab4.csv")

###STEP 3c:Make a table of suitable habitat in FCnoCRA_2010####
#this creates table of all potentially suitable habitat available in each cell 
#(based on model outputs for each of the species - irrespective of whether species occurs there)
spplc_FCnoCRA_2010<-merge(spp_HabPref4,Land_FCnoCRA_2010, by.x="lulc", by.y="lulc", all=FALSE)
#aggregate total area of suitable land cover in cell by species name
out_spplcCR_FCnoCRA_2010<-aggregate(lc_area~spp+sp_id+colrow,spplc_FCnoCRA_2010,FUN=sum)
names(out_spplcCR_FCnoCRA_2010)
remove(spplc_FCnoCRA_2010)
#concatonate values from species and colrow columns into a new column
out_spplcCR_FCnoCRA_2010$CR_sp <-paste (out_spplcCR_FCnoCRA_2010$colrow, 
                                             out_spplcCR_FCnoCRA_2010$sp_id, sep = "_", collapse = NULL)
#this creates a table of the suitable habitat for each species in each cell taking account species 
#overlap and assuming that the species preferentially occurs in suitable habitat
names(spp_overlap)
names(out_spplcCR_FCnoCRA_2010)
spp_overlap_FCnoCRA_2010<-merge(spp_overlap, out_spplcCR_FCnoCRA_2010, by="CR_sp", by.y="CR_sp", all=FALSE)
names(spp_overlap_FCnoCRA_2010)
remove(out_spplcCR_FCnoCRA_2010)
spp_overlap_FCnoCRA_2010sub<-spp_overlap_FCnoCRA_2010[c( "CR_sp","sp_id.x","spp.x","spCRprop","colrow.x",
                                                                   "lc_area" )]
remove(spp_overlap_FCnoCRA_2010)
out_SppOverlap_FCnoCRA_2010<-merge(colrow,spp_overlap_FCnoCRA_2010sub,by.x="colrow",by.y="colrow.x")
remove(spp_overlap_FCnoCRA_2010sub)
out_SppOverlap_FCnoCRA_2010$suitarea_max_FCnoCRA_2010<-ifelse(
  (out_SppOverlap_FCnoCRA_2010$spCRprop*out_SppOverlap_FCnoCRA_2010$CRArea) < out_SppOverlap_FCnoCRA_2010$lc_area, 
  (out_SppOverlap_FCnoCRA_2010$spCRprop*out_SppOverlap_FCnoCRA_2010$CRArea), out_SppOverlap_FCnoCRA_2010$lc_area)
write.csv(out_SppOverlap_FCnoCRA_2010,file="out_SppOverlap_FCnoCRA_2010.csv",row.names=FALSE, quote=FALSE)
###STEP 3b:Make a table of suitable habitat in FCnoCRA_2050####
#this creates table of all potentially suitable habitat available in each cell in 2050
#(based on model outputs for each of the species - irrespective of whether species occurs there)
spplc_FCnoCRA_2050<-merge(spp_HabPref4,Land_FCnoCRA_2050, by.x="lulc", by.y="lulc", all=FALSE)
#aggregate total area of suitable land cover in cell by species name
out_spplcCR_FCnoCRA_2050<-aggregate(lc_area~spp+sp_id+colrow,spplc_FCnoCRA_2050,FUN=sum)
names(out_spplcCR_FCnoCRA_2050)
remove(spplc_FCnoCRA_2050)
#concatonate values from species and colrow columns into a new column
out_spplcCR_FCnoCRA_2050$CR_sp <-paste (out_spplcCR_FCnoCRA_2050$colrow, 
                                             out_spplcCR_FCnoCRA_2050$sp_id, sep = "_", collapse = NULL)
out_spplcCR_FCnoCRA_2050<-out_spplcCR_FCnoCRA_2050[c("lc_area", "CR_sp" )]
#this creates a table of the suitable habitat for each species in each cell taking account species 
#overlap and assuming that the species preferentially occurs in suitable habitat
names(spp_overlap)
names(out_spplcCR_FCnoCRA_2050)
spp_overlap_FCnoCRA_2050<-merge(out_SppOverlap_FCnoCRA_2010, out_spplcCR_FCnoCRA_2050, by="CR_sp", by.y="CR_sp", all=FALSE, suffixes = c(".2010",".2050"))
names(spp_overlap_FCnoCRA_2050)
remove(out_spplcCR_FCnoCRA_2050)
out_SppOverlap_FCnoCRA_2050<-spp_overlap_FCnoCRA_2050
out_SppOverlap_FCnoCRA_2050$suitarea_max_FCnoCRA_2050<-ifelse(
  (out_SppOverlap_FCnoCRA_2050$spCRprop*out_SppOverlap_FCnoCRA_2050$CRArea) < out_SppOverlap_FCnoCRA_2050$lc_area.2050, 
  (out_SppOverlap_FCnoCRA_2050$spCRprop*out_SppOverlap_FCnoCRA_2050$CRArea), out_SppOverlap_FCnoCRA_2050$lc_area.2050)
out_SppOverlap_FCnoCRA_2050$suitarea_mean_FCnoCRA_2050<-ifelse(
  out_SppOverlap_FCnoCRA_2050$lc_area.2010==0, (out_SppOverlap_FCnoCRA_2050$spCRprop*out_SppOverlap_FCnoCRA_2050$lc_area.2050),
  ifelse((
    out_SppOverlap_FCnoCRA_2050$suitarea_max_FCnoCRA_2010*
      (out_SppOverlap_FCnoCRA_2050$lc_area.2050/out_SppOverlap_FCnoCRA_2050$lc_area.2010))>(out_SppOverlap_FCnoCRA_2050$spCRprop*out_SppOverlap_FCnoCRA_2050$CRArea),
    (out_SppOverlap_FCnoCRA_2050$spCRprop*out_SppOverlap_FCnoCRA_2050$CRArea),
    (out_SppOverlap_FCnoCRA_2050$suitarea_max_FCnoCRA_2010*
       (out_SppOverlap_FCnoCRA_2050$lc_area.2050/out_SppOverlap_FCnoCRA_2050$lc_area.2010))))
out_SppOverlap_FCnoCRA_2050$suitarea_min_FCnoCRA_2050<-ifelse(
  (out_SppOverlap_FCnoCRA_2050$lc_area.2050-out_SppOverlap_FCnoCRA_2050$lc_area.2010) >0,
  out_SppOverlap_FCnoCRA_2050$suitarea_max_FCnoCRA_2010, 
  ifelse((out_SppOverlap_FCnoCRA_2050$lc_area.2010-out_SppOverlap_FCnoCRA_2050$lc_area.2050)>
           (out_SppOverlap_FCnoCRA_2050$suitarea_max_FCnoCRA_2010),
         0,
         out_SppOverlap_FCnoCRA_2050$suitarea_max_FCnoCRA_2010-
           (out_SppOverlap_FCnoCRA_2050$lc_area.2010-out_SppOverlap_FCnoCRA_2050$lc_area.2050)))
###STEP 4a: Makes a table of total suitable habitat in the region FCnoCRA_2010 ####
#table is called:(out_spp_suitarearegion_FCnoCRA_2010)
out_spp_suitarearegion_FCnoCRA_2010<-aggregate(suitarea_max_FCnoCRA_2010~spp.x,
                                                    out_SppOverlap_FCnoCRA_2010,FUN=sum)
#renaming columns/fields
names(out_spp_suitarearegion_FCnoCRA_2010)[
  names(out_spp_suitarearegion_FCnoCRA_2010) == 'suitarea_max_FCnoCRA_2010'] <- 'suitRegion_max_FCnoCRA_2010'
#save output
write.csv(out_spp_suitarearegion_FCnoCRA_2010,file="out_spp_suitarearegion_FCnoCRA_2010.csv",
          row.names=FALSE, quote=FALSE)
str(out_spp_suitarearegion_FCnoCRA_2010)
###STEP 5a: makes a table of change in suitable habitat for each spp FCnoCRA_2010-2050####
#view tables/dataframes to check column names from which to join tables
names(out_spp_suitarearegion_FCnoCRA_2010)
names(out_SppOverlap_FCnoCRA_2010)
names(out_SppOverlap_FCnoCRA_2050)
#join tables together to create one large table for calculations
out_SppChange_FCnoCRA<-merge(out_SppOverlap_FCnoCRA_2050, out_spp_suitarearegion_FCnoCRA_2010, 
                                  by="spp.x",all=FALSE)
remove(out_SppOverlap_FCnoCRA_2010)
remove(out_SppOverlap_FCnoCRA_2050)
remove(out_spp_suitarearegion_FCnoCRA_2010)
names(out_SppChange_FCnoCRA)
out_SppChange_FCnoCRA$sppChange_FCnoCRA<-ifelse (
  out_SppChange_FCnoCRA$suitRegion_max_FCnoCRA_2010==0,0,
  ((out_SppChange_FCnoCRA$suitarea_max_FCnoCRA_2050-out_SppChange_FCnoCRA$suitarea_max_FCnoCRA_2010)/
     out_SppChange_FCnoCRA$suitRegion_max_FCnoCRA_2010))
out_SppChange_FCnoCRA$sppChange_FCnoCRA2<-ifelse (
  out_SppChange_FCnoCRA$suitRegion_max_FCnoCRA_2010==0,0,
  ((out_SppChange_FCnoCRA$suitarea_mean_FCnoCRA_2050-out_SppChange_FCnoCRA$suitarea_max_FCnoCRA_2010)/
     out_SppChange_FCnoCRA$suitRegion_max_FCnoCRA_2010))
out_SppChange_FCnoCRA$sppChange_FCnoCRA3<-ifelse (
  out_SppChange_FCnoCRA$suitRegion_max_FCnoCRA_2010==0,0,
  ((out_SppChange_FCnoCRA$suitarea_min_FCnoCRA_2050-out_SppChange_FCnoCRA$suitarea_max_FCnoCRA_2010)/
     out_SppChange_FCnoCRA$suitRegion_max_FCnoCRA_2010)) 
write.csv(out_SppChange_FCnoCRA,file="out_SppChange_FCnoCRA_Hab4.csv")
###STEP 6b: makes table of change combined across spp for each colrow FCnoCRA_2010-2050####
out_ChangeCR_FCnoCRA<-aggregate(
  cbind(sppChange_FCnoCRA,sppChange_FCnoCRA2,sppChange_FCnoCRA3)~colrow,
  out_SppChange_FCnoCRA,FUN=sum)
write.csv(out_ChangeCR_FCnoCRA,file="out_ChangeCR_FCnoCRA_Hab4.csv")
###STEP 7b: makes table of change combined across CRs for each spp FCnoCRA_2010-2050####
out_ChangeSpp_FCnoCRA<-aggregate(
  cbind(sppChange_FCnoCRA,sppChange_FCnoCRA2,sppChange_FCnoCRA3)~spp.x,
  out_SppChange_FCnoCRA,FUN=sum)
write.csv(out_ChangeSpp_FCnoCRA,file="out_ChangeSpp_FCnoCRA_Hab4.csv")

###STEP 3c:Make a table of suitable habitat in FCnoCRAnoSFA_2010####
#this creates table of all potentially suitable habitat available in each cell 
#(based on model outputs for each of the species - irrespective of whether species occurs there)
Land_FCnoCRAnoSFA_2010<-read.csv("Land_FCnoCRAnoSFA_2010.csv", header=TRUE)
Land_FCnoCRAnoSFA_2050<-read.csv("Land_FCnoCRAnoSFA_2050.csv", header=TRUE) 
spplc_FCnoCRAnoSFA_2010<-merge(spp_HabPref4,Land_FCnoCRAnoSFA_2010, by.x="lulc", by.y="lulc", all=FALSE)
#aggregate total area of suitable land cover in cell by species name
out_spplcCR_FCnoCRAnoSFA_2010<-aggregate(lc_area~spp+sp_id+colrow,spplc_FCnoCRAnoSFA_2010,FUN=sum)
names(out_spplcCR_FCnoCRAnoSFA_2010)
remove(spplc_FCnoCRAnoSFA_2010)
#concatonate values from species and colrow columns into a new column
out_spplcCR_FCnoCRAnoSFA_2010$CR_sp <-paste (out_spplcCR_FCnoCRAnoSFA_2010$colrow, 
                                          out_spplcCR_FCnoCRAnoSFA_2010$sp_id, sep = "_", collapse = NULL)
#this creates a table of the suitable habitat for each species in each cell taking account species 
#overlap and assuming that the species preferentially occurs in suitable habitat
names(spp_overlap)
names(out_spplcCR_FCnoCRAnoSFA_2010)
spp_overlap_FCnoCRAnoSFA_2010<-merge(spp_overlap, out_spplcCR_FCnoCRAnoSFA_2010, by="CR_sp", by.y="CR_sp", all=FALSE)
names(spp_overlap_FCnoCRAnoSFA_2010)
remove(out_spplcCR_FCnoCRAnoSFA_2010)
spp_overlap_FCnoCRAnoSFA_2010sub<-spp_overlap_FCnoCRAnoSFA_2010[c( "CR_sp","sp_id.x","spp.x","spCRprop","colrow.x",
                                                             "lc_area" )]
remove(spp_overlap_FCnoCRAnoSFA_2010)
out_SppOverlap_FCnoCRAnoSFA_2010<-merge(colrow,spp_overlap_FCnoCRAnoSFA_2010sub,by.x="colrow",by.y="colrow.x")
remove(spp_overlap_FCnoCRAnoSFA_2010sub)
out_SppOverlap_FCnoCRAnoSFA_2010$suitarea_max_FCnoCRAnoSFA_2010<-ifelse(
  (out_SppOverlap_FCnoCRAnoSFA_2010$spCRprop*out_SppOverlap_FCnoCRAnoSFA_2010$CRArea) < out_SppOverlap_FCnoCRAnoSFA_2010$lc_area, 
  (out_SppOverlap_FCnoCRAnoSFA_2010$spCRprop*out_SppOverlap_FCnoCRAnoSFA_2010$CRArea), out_SppOverlap_FCnoCRAnoSFA_2010$lc_area)
write.csv(out_SppOverlap_FCnoCRAnoSFA_2010,file="out_SppOverlap_FCnoCRAnoSFA_2010.csv",row.names=FALSE, quote=FALSE)
###STEP 3b:Make a table of suitable habitat in FCnoCRAnoSFA_2050####
#this creates table of all potentially suitable habitat available in each cell in 2050
#(based on model outputs for each of the species - irrespective of whether species occurs there)
spplc_FCnoCRAnoSFA_2050<-merge(spp_HabPref4,Land_FCnoCRAnoSFA_2050, by.x="lulc", by.y="lulc", all=FALSE)
#aggregate total area of suitable land cover in cell by species name
out_spplcCR_FCnoCRAnoSFA_2050<-aggregate(lc_area~spp+sp_id+colrow,spplc_FCnoCRAnoSFA_2050,FUN=sum)
names(out_spplcCR_FCnoCRAnoSFA_2050)
remove(spplc_FCnoCRAnoSFA_2050)
#concatonate values from species and colrow columns into a new column
out_spplcCR_FCnoCRAnoSFA_2050$CR_sp <-paste (out_spplcCR_FCnoCRAnoSFA_2050$colrow, 
                                             out_spplcCR_FCnoCRAnoSFA_2050$sp_id, sep = "_", collapse = NULL)
out_spplcCR_FCnoCRAnoSFA_2050<-out_spplcCR_FCnoCRAnoSFA_2050[c("lc_area", "CR_sp" )]
#this creates a table of the suitable habitat for each species in each cell taking account species 
#overlap and assuming that the species preferentially occurs in suitable habitat
names(spp_overlap)
names(out_spplcCR_FCnoCRAnoSFA_2050)
spp_overlap_FCnoCRAnoSFA_2050<-merge(out_SppOverlap_FCnoCRAnoSFA_2010, out_spplcCR_FCnoCRAnoSFA_2050, by="CR_sp", by.y="CR_sp", all=FALSE, suffixes = c(".2010",".2050"))
names(spp_overlap_FCnoCRAnoSFA_2050)
remove(out_spplcCR_FCnoCRAnoSFA_2050)
out_SppOverlap_FCnoCRAnoSFA_2050<-spp_overlap_FCnoCRAnoSFA_2050
out_SppOverlap_FCnoCRAnoSFA_2050$suitarea_max_FCnoCRAnoSFA_2050<-ifelse(
  (out_SppOverlap_FCnoCRAnoSFA_2050$spCRprop*out_SppOverlap_FCnoCRAnoSFA_2050$CRArea) < out_SppOverlap_FCnoCRAnoSFA_2050$lc_area.2050, 
  (out_SppOverlap_FCnoCRAnoSFA_2050$spCRprop*out_SppOverlap_FCnoCRAnoSFA_2050$CRArea), out_SppOverlap_FCnoCRAnoSFA_2050$lc_area.2050)
out_SppOverlap_FCnoCRAnoSFA_2050$suitarea_mean_FCnoCRAnoSFA_2050<-ifelse(
  out_SppOverlap_FCnoCRAnoSFA_2050$lc_area.2010==0, (out_SppOverlap_FCnoCRAnoSFA_2050$spCRprop*out_SppOverlap_FCnoCRAnoSFA_2050$lc_area.2050),
  ifelse((
    out_SppOverlap_FCnoCRAnoSFA_2050$suitarea_max_FCnoCRAnoSFA_2010*
      (out_SppOverlap_FCnoCRAnoSFA_2050$lc_area.2050/out_SppOverlap_FCnoCRAnoSFA_2050$lc_area.2010))>(out_SppOverlap_FCnoCRAnoSFA_2050$spCRprop*out_SppOverlap_FCnoCRAnoSFA_2050$CRArea),
    (out_SppOverlap_FCnoCRAnoSFA_2050$spCRprop*out_SppOverlap_FCnoCRAnoSFA_2050$CRArea),
    (out_SppOverlap_FCnoCRAnoSFA_2050$suitarea_max_FCnoCRAnoSFA_2010*
       (out_SppOverlap_FCnoCRAnoSFA_2050$lc_area.2050/out_SppOverlap_FCnoCRAnoSFA_2050$lc_area.2010))))
out_SppOverlap_FCnoCRAnoSFA_2050$suitarea_min_FCnoCRAnoSFA_2050<-ifelse(
  (out_SppOverlap_FCnoCRAnoSFA_2050$lc_area.2050-out_SppOverlap_FCnoCRAnoSFA_2050$lc_area.2010) >0,
  out_SppOverlap_FCnoCRAnoSFA_2050$suitarea_max_FCnoCRAnoSFA_2010, 
  ifelse((out_SppOverlap_FCnoCRAnoSFA_2050$lc_area.2010-out_SppOverlap_FCnoCRAnoSFA_2050$lc_area.2050)>
           (out_SppOverlap_FCnoCRAnoSFA_2050$suitarea_max_FCnoCRAnoSFA_2010),
         0,
         out_SppOverlap_FCnoCRAnoSFA_2050$suitarea_max_FCnoCRAnoSFA_2010-
           (out_SppOverlap_FCnoCRAnoSFA_2050$lc_area.2010-out_SppOverlap_FCnoCRAnoSFA_2050$lc_area.2050)))
###STEP 4a: Makes a table of total suitable habitat in the region FCnoCRAnoSFA_2010 ####
#table is called:(out_spp_suitarearegion_FCnoCRAnoSFA_2010)
out_spp_suitarearegion_FCnoCRAnoSFA_2010<-aggregate(suitarea_max_FCnoCRAnoSFA_2010~spp.x,
                                                    out_SppOverlap_FCnoCRAnoSFA_2010,FUN=sum)
#renaming columns/fields
names(out_spp_suitarearegion_FCnoCRAnoSFA_2010)[
  names(out_spp_suitarearegion_FCnoCRAnoSFA_2010) == 'suitarea_max_FCnoCRAnoSFA_2010'] <- 'suitRegion_max_FCnoCRAnoSFA_2010'
#save output
write.csv(out_spp_suitarearegion_FCnoCRAnoSFA_2010,file="out_spp_suitarearegion_FCnoCRAnoSFA_2010.csv",
          row.names=FALSE, quote=FALSE)
str(out_spp_suitarearegion_FCnoCRAnoSFA_2010)
###STEP 5a: makes a table of change in suitable habitat for each spp FCnoCRAnoSFA_2010-2050####
#view tables/dataframes to check column names from which to join tables
names(out_spp_suitarearegion_FCnoCRAnoSFA_2010)
names(out_SppOverlap_FCnoCRAnoSFA_2010)
names(out_SppOverlap_FCnoCRAnoSFA_2050)
#join tables together to create one large table for calculations
out_SppChange_FCnoCRAnoSFA<-merge(out_SppOverlap_FCnoCRAnoSFA_2050, out_spp_suitarearegion_FCnoCRAnoSFA_2010, 
                                  by="spp.x",all=FALSE)
remove(out_SppOverlap_FCnoCRAnoSFA_2010)
remove(out_SppOverlap_FCnoCRAnoSFA_2050)
remove(out_spp_suitarearegion_FCnoCRAnoSFA_2010)
names(out_SppChange_FCnoCRAnoSFA)
out_SppChange_FCnoCRAnoSFA$sppChange_FCnoCRAnoSFA<-ifelse (
  out_SppChange_FCnoCRAnoSFA$suitRegion_max_FCnoCRAnoSFA_2010==0,0,
  ((out_SppChange_FCnoCRAnoSFA$suitarea_max_FCnoCRAnoSFA_2050-out_SppChange_FCnoCRAnoSFA$suitarea_max_FCnoCRAnoSFA_2010)/
     out_SppChange_FCnoCRAnoSFA$suitRegion_max_FCnoCRAnoSFA_2010))
out_SppChange_FCnoCRAnoSFA$sppChange_FCnoCRAnoSFA2<-ifelse (
  out_SppChange_FCnoCRAnoSFA$suitRegion_max_FCnoCRAnoSFA_2010==0,0,
  ((out_SppChange_FCnoCRAnoSFA$suitarea_mean_FCnoCRAnoSFA_2050-out_SppChange_FCnoCRAnoSFA$suitarea_max_FCnoCRAnoSFA_2010)/
     out_SppChange_FCnoCRAnoSFA$suitRegion_max_FCnoCRAnoSFA_2010))
out_SppChange_FCnoCRAnoSFA$sppChange_FCnoCRAnoSFA3<-ifelse (
  out_SppChange_FCnoCRAnoSFA$suitRegion_max_FCnoCRAnoSFA_2010==0,0,
  ((out_SppChange_FCnoCRAnoSFA$suitarea_min_FCnoCRAnoSFA_2050-out_SppChange_FCnoCRAnoSFA$suitarea_max_FCnoCRAnoSFA_2010)/
     out_SppChange_FCnoCRAnoSFA$suitRegion_max_FCnoCRAnoSFA_2010)) 
write.csv(out_SppChange_FCnoCRAnoSFA,file="out_SppChange_FCnoCRAnoSFA_Hab4.csv")
###STEP 6b: makes table of change combined across spp for each colrow FCnoCRAnoSFA_2010-2050####
out_ChangeCR_FCnoCRAnoSFA<-aggregate(
  cbind(sppChange_FCnoCRAnoSFA,sppChange_FCnoCRAnoSFA2,sppChange_FCnoCRAnoSFA3)~colrow,
  out_SppChange_FCnoCRAnoSFA,FUN=sum)
write.csv(out_ChangeCR_FCnoCRAnoSFA,file="out_ChangeCR_FCnoCRAnoSFA_Hab4.csv")
###STEP 7b: makes table of change combined across CRs for each spp FCnoCRAnoSFA_2010-2050####
out_ChangeSpp_FCnoCRAnoSFA<-aggregate(
  cbind(sppChange_FCnoCRAnoSFA,sppChange_FCnoCRAnoSFA2,sppChange_FCnoCRAnoSFA3)~spp.x,
  out_SppChange_FCnoCRAnoSFA,FUN=sum)
write.csv(out_ChangeSpp_FCnoCRAnoSFA,file="out_ChangeSpp_FCnoCRAnoSFA_Hab4.csv")

###STEP 8: make table comparing different scnearios####
setwd(outputpath)
out_ChangeSpp_IDCbrazil_Hab1<-read.csv("out_ChangeSpp_IDCbrazil_Hab1.csv", row.names = 1)
out_ChangeSpp_FC_Hab1<-read.csv("out_ChangeSpp_FC_Hab1.csv", row.names = 1)
out_ChangeSpp_FCnoSFA_Hab1<-read.csv("out_ChangeSpp_FCnoSFA_Hab1.csv", row.names = 1)
out_ChangeSpp_FCnoCRA_Hab1<-read.csv("out_ChangeSpp_FCnoCRA_Hab1.csv", row.names = 1)
out_ChangeSpp_FCnoCRAnoSFA_Hab1<-read.csv("out_ChangeSpp_FCnoCRAnoSFA_Hab1.csv", row.names = 1)
out_ChangeSpp_IDCbrazil_Hab4<-read.csv("out_ChangeSpp_IDCbrazil_Hab4.csv", row.names = 1)
out_ChangeSpp_FC_Hab4<-read.csv("out_ChangeSpp_FC_Hab4.csv", row.names = 1)
out_ChangeSpp_FCnoSFA_Hab4<-read.csv("out_ChangeSpp_FCnoSFA_Hab4.csv", row.names = 1)
out_ChangeSpp_FCnoCRA_Hab4<-read.csv("out_ChangeSpp_FCnoCRA_Hab4.csv", row.names = 1)
out_ChangeSpp_FCnoCRAnoSFA_Hab4<-read.csv("out_ChangeSpp_FCnoCRAnoSFA_Hab4.csv", row.names = 1)
out_ChangeSpp_IDCno_Hab1<-read.csv("out_ChangeSpp_IDCno_Hab1.csv", row.names = 1)
out_ChangeSpp_IDCamazon_Hab1<-read.csv("out_ChangeSpp_IDCamazon_Hab1.csv", row.names = 1)
out_ChangeSpp_IDCno_Hab4<-read.csv("out_ChangeSpp_IDCno_Hab4.csv", row.names = 1)
out_ChangeSpp_IDCamazon_Hab4<-read.csv("out_ChangeSpp_IDCamazon_Hab4.csv", row.names = 1)

out_ChangeCR_IDCbrazil_Hab1<-read.csv("out_ChangeCR_IDCbrazil_Hab1.csv", row.names = 1)
out_ChangeCR_FC_Hab1<-read.csv("out_ChangeCR_FC_Hab1.csv", row.names = 1)
out_ChangeCR_FCnoSFA_Hab1<-read.csv("out_ChangeCR_FCnoSFA_Hab1.csv", row.names = 1)
out_ChangeCR_FCnoCRA_Hab1<-read.csv("out_ChangeCR_FCnoCRA_Hab1.csv", row.names = 1)
out_ChangeCR_FCnoCRAnoSFA_Hab1<-read.csv("out_ChangeCR_FCnoCRAnoSFA_Hab1.csv", row.names = 1)
out_ChangeCR_IDCbrazil_Hab4<-read.csv("out_ChangeCR_IDCbrazil_Hab4.csv", row.names = 1)
out_ChangeCR_FC_Hab4<-read.csv("out_ChangeCR_FC_Hab4.csv", row.names = 1)
out_ChangeCR_FCnoSFA_Hab4<-read.csv("out_ChangeCR_FCnoSFA_Hab4.csv", row.names = 1)
out_ChangeCR_FCnoCRA_Hab4<-read.csv("out_ChangeCR_FCnoCRA_Hab4.csv", row.names = 1)
out_ChangeCR_FCnoCRAnoSFA_Hab4<-read.csv("out_ChangeCR_FCnoCRAnoSFA_Hab4.csv", row.names = 1)
out_ChangeCR_IDCno_Hab1<-read.csv("out_ChangeCR_IDCno_Hab1.csv", row.names = 1)
out_ChangeCR_IDCamazon_Hab1<-read.csv("out_ChangeCR_IDCamazon_Hab1.csv", row.names = 1)
out_ChangeCR_IDCno_Hab4<-read.csv("out_ChangeCR_IDCno_Hab4.csv", row.names = 1)
out_ChangeCR_IDCamazon_Hab4<-read.csv("out_ChangeCR_IDCamazon_Hab4.csv", row.names = 1)


out_ChangeSpp_all_Hab1<-merge(out_ChangeSpp_FC_Hab1,out_ChangeSpp_IDCbrazil_Hab1,by="spp.x")
out_ChangeSpp_all_Hab1<-merge(out_ChangeSpp_all_Hab1,out_ChangeSpp_IDCno_Hab1,by="spp.x")
out_ChangeSpp_all_Hab1<-merge(out_ChangeSpp_all_Hab1,out_ChangeSpp_IDCamazon_Hab1,by="spp.x")
out_ChangeSpp_all_Hab1<-merge(out_ChangeSpp_all_Hab1,out_ChangeSpp_FCnoSFA_Hab1,by="spp.x")
out_ChangeSpp_all_Hab1<-merge(out_ChangeSpp_all_Hab1,out_ChangeSpp_FCnoCRA_Hab1,by="spp.x")
out_ChangeSpp_all_Hab1<-merge(out_ChangeSpp_all_Hab1,out_ChangeSpp_FCnoCRAnoSFA_Hab1,by="spp.x")
names(out_ChangeSpp_all_Hab1)
out_ChangeSpp_all_Hab1<-out_ChangeSpp_all_Hab1[c("spp.x","sppChange_IDCno","sppChange_IDCamazon",
                                                 "sppChange_IDCbrazil","sppChange_FC",
                                                 "sppChange_FCnoSFA","sppChange_FCnoCRA",
                                                 "sppChange_FCnoCRAnoSFA","sppChange_IDCno2",
                                                 "sppChange_IDCamazon2","sppChange_IDCbrazil2",
                                                 "sppChange_FC2","sppChange_FCnoSFA2",
                                                 "sppChange_FCnoCRA2","sppChange_FCnoCRAnoSFA2",
                                                 "sppChange_IDCno3","sppChange_IDCamazon3",
                                                 "sppChange_IDCbrazil3","sppChange_FC3",
                                                 "sppChange_FCnoSFA3","sppChange_FCnoCRA3",
                                                 "sppChange_FCnoCRAnoSFA3")]

out_ChangeCR_all_Hab1<-merge(out_ChangeCR_FC_Hab1,out_ChangeCR_IDCbrazil_Hab1,by="colrow")
out_ChangeCR_all_Hab1<-merge(out_ChangeCR_all_Hab1,out_ChangeCR_IDCno_Hab1,by="colrow")
out_ChangeCR_all_Hab1<-merge(out_ChangeCR_all_Hab1,out_ChangeCR_IDCamazon_Hab1,by="colrow")
out_ChangeCR_all_Hab1<-merge(out_ChangeCR_all_Hab1,out_ChangeCR_FCnoSFA_Hab1,by="colrow")
out_ChangeCR_all_Hab1<-merge(out_ChangeCR_all_Hab1,out_ChangeCR_FCnoCRA_Hab1,by="colrow")
out_ChangeCR_all_Hab1<-merge(out_ChangeCR_all_Hab1,out_ChangeCR_FCnoCRAnoSFA_Hab1,by="colrow")
out_ChangeCR_all_Hab1<-out_ChangeCR_all_Hab1[c("colrow","sppChange_IDCno","sppChange_IDCamazon",
                                               "sppChange_IDCbrazil","sppChange_FC",
                                                 "sppChange_FCnoSFA","sppChange_FCnoCRA",
                                               "sppChange_FCnoCRAnoSFA","sppChange_IDCno2",
                                               "sppChange_IDCamazon2","sppChange_IDCbrazil2",
                                               "sppChange_FC2","sppChange_FCnoSFA2",
                                               "sppChange_FCnoCRA2","sppChange_FCnoCRAnoSFA2",
                                               "sppChange_IDCno3","sppChange_IDCamazon3",
                                               "sppChange_IDCbrazil3","sppChange_FC3",
                                               "sppChange_FCnoSFA3","sppChange_FCnoCRA3",
                                               "sppChange_FCnoCRAnoSFA3" )]

write.csv(out_ChangeSpp_all_Hab1,file="out_ChangeSpp_all_Hab1.csv")
write.csv(out_ChangeCR_all_Hab1,file="out_ChangeCR_all_Hab1.csv")

out_ChangeSpp_all_Hab4<-merge(out_ChangeSpp_FC_Hab4,out_ChangeSpp_IDCbrazil_Hab4,by="spp.x")
out_ChangeSpp_all_Hab4<-merge(out_ChangeSpp_all_Hab4,out_ChangeSpp_IDCno_Hab4,by="spp.x")
out_ChangeSpp_all_Hab4<-merge(out_ChangeSpp_all_Hab4,out_ChangeSpp_IDCamazon_Hab4,by="spp.x")
out_ChangeSpp_all_Hab4<-merge(out_ChangeSpp_all_Hab4,out_ChangeSpp_FCnoSFA_Hab4,by="spp.x")
out_ChangeSpp_all_Hab4<-merge(out_ChangeSpp_all_Hab4,out_ChangeSpp_FCnoCRA_Hab4,by="spp.x")
out_ChangeSpp_all_Hab4<-merge(out_ChangeSpp_all_Hab4,out_ChangeSpp_FCnoCRAnoSFA_Hab4,by="spp.x")
out_ChangeSpp_all_Hab4<-out_ChangeSpp_all_Hab4[c("spp.x","sppChange_IDCno","sppChange_IDCamazon",
                                                 "sppChange_IDCbrazil","sppChange_FC",
                                                 "sppChange_FCnoSFA","sppChange_FCnoCRA",
                                                 "sppChange_FCnoCRAnoSFA","sppChange_IDCno2",
                                                 "sppChange_IDCamazon2","sppChange_IDCbrazil2",
                                                 "sppChange_FC2","sppChange_FCnoSFA2",
                                                 "sppChange_FCnoCRA2","sppChange_FCnoCRAnoSFA2",
                                                 "sppChange_IDCno3","sppChange_IDCamazon3",
                                                 "sppChange_IDCbrazil3","sppChange_FC3",
                                                 "sppChange_FCnoSFA3","sppChange_FCnoCRA3",
                                                 "sppChange_FCnoCRAnoSFA3")]
out_ChangeCR_all_Hab4<-merge(out_ChangeCR_FC_Hab4,out_ChangeCR_IDCbrazil_Hab4,by="colrow")
out_ChangeCR_all_Hab4<-merge(out_ChangeCR_all_Hab4,out_ChangeCR_IDCno_Hab4,by="colrow")
out_ChangeCR_all_Hab4<-merge(out_ChangeCR_all_Hab4,out_ChangeCR_IDCamazon_Hab4,by="colrow")
out_ChangeCR_all_Hab4<-merge(out_ChangeCR_all_Hab4,out_ChangeCR_FCnoSFA_Hab4,by="colrow")
out_ChangeCR_all_Hab4<-merge(out_ChangeCR_all_Hab4,out_ChangeCR_FCnoCRA_Hab4,by="colrow")
out_ChangeCR_all_Hab4<-merge(out_ChangeCR_all_Hab4,out_ChangeCR_FCnoCRAnoSFA_Hab4,by="colrow")
out_ChangeCR_all_Hab4<-out_ChangeCR_all_Hab4[c("colrow","sppChange_IDCno","sppChange_IDCamazon",
                                                "sppChange_IDCbrazil","sppChange_FC",
                                                "sppChange_FCnoSFA","sppChange_FCnoCRA",
                                                "sppChange_FCnoCRAnoSFA","sppChange_IDCno2",
                                                "sppChange_IDCamazon2","sppChange_IDCbrazil2",
                                                "sppChange_FC2","sppChange_FCnoSFA2",
                                                "sppChange_FCnoCRA2","sppChange_FCnoCRAnoSFA2",
                                                "sppChange_IDCno3","sppChange_IDCamazon3",
                                                "sppChange_IDCbrazil3","sppChange_FC3",
                                                "sppChange_FCnoSFA3","sppChange_FCnoCRA3",
                                                "sppChange_FCnoCRAnoSFA3" )]
write.csv(out_ChangeSpp_all_Hab4,file="out_ChangeSpp_all_Hab4.csv")
write.csv(out_ChangeCR_all_Hab4,file="out_ChangeCR_all_Hab4.csv")

out_ChangeCR_all_Hab4<-read.csv("out_ChangeCR_all_Hab4.csv")
out_ChangeCR_all_Hab1<-read.csv("out_ChangeCR_all_Hab1.csv")
out_ChangeCR_all_Habs<-merge(out_ChangeCR_all_Hab1,out_ChangeCR_all_Hab4, 
                              suffixes = c(".hab1",".hab4"), by="colrow")
write.csv(out_ChangeCR_all_Habs,file="out_ChangeCR_all_Habs.csv")

out_ChangeSpp_all_Hab4<-read.csv("out_ChangeSpp_all_Hab4.csv")
out_ChangeSpp_all_Hab1<-read.csv("out_ChangeSpp_all_Hab1.csv")
out_ChangeSpp_all_Habs<-merge(out_ChangeSpp_all_Hab1,out_ChangeSpp_all_Hab4, 
                              suffixes = c(".hab1",".hab4"), by="spp.x")
write.csv(out_ChangeSpp_all_Habs,file="out_ChangeSpp_all_Habs.csv")

out_ChangeSpp_all_Habs<-read.csv("out_ChangeSpp_all_Habs.csv")
out_ChangeSpp_all_Habs$habDif<-ifelse(
  (out_ChangeSpp_all_Habs$sppChange_FC.hab1<out_ChangeSpp_all_Habs$sppChange_FC.hab4),1,0)
sum(out_ChangeSpp_all_Habs$habDif)


hist(out_ChangeSpp_all_Habs$sppChange_IDCno.hab1, 
     breaks=c(-1,-0.5,-0.3,-0.05,0.05,0.25,100), plot=FALSE)
hist(out_ChangeSpp_all_Habs$sppChange_IDCamazon.hab1, 
     breaks=c(-1,-0.5,-0.3,-0.05,0.05,0.25,100), plot=FALSE)
hist(out_ChangeSpp_all_Habs$sppChange_IDCbrazil.hab1, 
     breaks=c(-1,-0.5,-0.3,-0.05,0.05,0.25,100), plot=FALSE)
hist(out_ChangeSpp_all_Habs$sppChange_FC.hab1, 
     breaks=c(-1,-0.5,-0.3,-0.05,0.05,0.25,100), plot=FALSE)
hist(out_ChangeSpp_all_Habs$sppChange_FCnoSFA.hab1, 
     breaks=c(-1,-0.5,-0.3,-0.05,0.05,0.25,100), plot=FALSE)
hist(out_ChangeSpp_all_Habs$sppChange_FCnoCRA.hab1, 
     breaks=c(-1,-0.5,-0.3,-0.05,0.05,0.25,100), plot=FALSE)
hist(out_ChangeSpp_all_Habs$sppChange_FCnoCRAnoSFA.hab1, 
     breaks=c(-1,-0.5,-0.3,-0.05,0.05,0.25,100), plot=FALSE)
hist(out_ChangeSpp_all_Habs$sppChange_IDCno.hab4, 
     breaks=c(-1,-0.5,-0.3,-0.05,0.05,0.25,100), plot=FALSE)
hist(out_ChangeSpp_all_Habs$sppChange_IDCamazon.hab4, 
     breaks=c(-1,-0.5,-0.3,-0.05,0.05,0.25,100), plot=FALSE)
hist(out_ChangeSpp_all_Habs$sppChange_IDCbrazil.hab4, 
     breaks=c(-1,-0.5,-0.3,-0.05,0.05,0.25,100), plot=FALSE)
hist(out_ChangeSpp_all_Habs$sppChange_FC.hab4, 
     breaks=c(-1,-0.5,-0.3,-0.05,0.05,0.25,100), plot=FALSE)
hist(out_ChangeSpp_all_Habs$sppChange_FCnoSFA.hab4, 
     breaks=c(-1,-0.5,-0.3,-0.05,0.05,0.25,100), plot=FALSE)
hist(out_ChangeSpp_all_Habs$sppChange_FCnoCRA.hab4, 
     breaks=c(-1,-0.5,-0.3,-0.05,0.05,0.25,100), plot=FALSE)
hist(out_ChangeSpp_all_Habs$sppChange_FCnoCRAnoSFA.hab4, 
     breaks=c(-1,-0.5,-0.3,-0.05,0.05,0.25,100), plot=FALSE)

hist(out_ChangeSpp_all_Habs$sppChange_IDCno2.hab1, 
     breaks=c(-1,-0.5,-0.3,-0.05,0.05,0.25,100), plot=FALSE)
hist(out_ChangeSpp_all_Habs$sppChange_IDCamazon2.hab1, 
     breaks=c(-1,-0.5,-0.3,-0.05,0.05,0.25,100), plot=FALSE)
hist(out_ChangeSpp_all_Habs$sppChange_IDCbrazil2.hab1, 
     breaks=c(-1,-0.5,-0.3,-0.05,0.05,0.25,100), plot=FALSE)
hist(out_ChangeSpp_all_Habs$sppChange_FC2.hab1, 
     breaks=c(-1,-0.5,-0.3,-0.05,0.05,0.25,100), plot=FALSE)
hist(out_ChangeSpp_all_Habs$sppChange_FCnoSFA2.hab1, 
     breaks=c(-1,-0.5,-0.3,-0.05,0.05,0.25,100), plot=FALSE)
hist(out_ChangeSpp_all_Habs$sppChange_FCnoCRA2.hab1, 
     breaks=c(-1,-0.5,-0.3,-0.05,0.05,0.25,100), plot=FALSE)
hist(out_ChangeSpp_all_Habs$sppChange_FCnoCRAnoSFA2.hab1, 
     breaks=c(-1,-0.5,-0.3,-0.05,0.05,0.25,100), plot=FALSE)
hist(out_ChangeSpp_all_Habs$sppChange_IDCno2.hab4, 
     breaks=c(-1,-0.5,-0.3,-0.05,0.05,0.25,100), plot=FALSE)
hist(out_ChangeSpp_all_Habs$sppChange_IDCamazon2.hab4, 
     breaks=c(-1,-0.5,-0.3,-0.05,0.05,0.25,100), plot=FALSE)
hist(out_ChangeSpp_all_Habs$sppChange_IDCbrazil2.hab4, 
     breaks=c(-1,-0.5,-0.3,-0.05,0.05,0.25,100), plot=FALSE)
hist(out_ChangeSpp_all_Habs$sppChange_FC2.hab4, 
     breaks=c(-1,-0.5,-0.3,-0.05,0.05,0.25,100), plot=FALSE)
hist(out_ChangeSpp_all_Habs$sppChange_FCnoSFA2.hab4, 
     breaks=c(-1,-0.5,-0.3,-0.05,0.05,0.25,100), plot=FALSE)
hist(out_ChangeSpp_all_Habs$sppChange_FCnoCRA2.hab4, 
     breaks=c(-1,-0.5,-0.3,-0.05,0.05,0.25,100), plot=FALSE)
hist(out_ChangeSpp_all_Habs$sppChange_FCnoCRAnoSFA2.hab4, 
     breaks=c(-1,-0.5,-0.3,-0.05,0.05,0.25,100), plot=FALSE)

hist(out_ChangeSpp_all_Habs$sppChange_IDCno3.hab1, 
     breaks=c(-1,-0.5,-0.3,-0.05,0.05,0.25,100), plot=FALSE)
hist(out_ChangeSpp_all_Habs$sppChange_IDCamazon3.hab1, 
     breaks=c(-1,-0.5,-0.3,-0.05,0.05,0.25,100), plot=FALSE)
hist(out_ChangeSpp_all_Habs$sppChange_IDCbrazil3.hab1, 
     breaks=c(-1,-0.5,-0.3,-0.05,0.05,0.25,100), plot=FALSE)
hist(out_ChangeSpp_all_Habs$sppChange_FC3.hab1, 
     breaks=c(-1,-0.5,-0.3,-0.05,0.05,0.25,100), plot=FALSE)
hist(out_ChangeSpp_all_Habs$sppChange_FCnoSFA3.hab1, 
     breaks=c(-1,-0.5,-0.3,-0.05,0.05,0.25,100), plot=FALSE)
hist(out_ChangeSpp_all_Habs$sppChange_FCnoCRA3.hab1, 
     breaks=c(-1,-0.5,-0.3,-0.05,0.05,0.25,100), plot=FALSE)
hist(out_ChangeSpp_all_Habs$sppChange_FCnoCRAnoSFA3.hab1, 
     breaks=c(-1,-0.5,-0.3,-0.05,0.05,0.25,100), plot=FALSE)
hist(out_ChangeSpp_all_Habs$sppChange_IDCno3.hab4, 
     breaks=c(-1,-0.5,-0.3,-0.05,0.05,0.25,100), plot=FALSE)
hist(out_ChangeSpp_all_Habs$sppChange_IDCamazon3.hab4, 
     breaks=c(-1,-0.5,-0.3,-0.05,0.05,0.25,100), plot=FALSE)
hist(out_ChangeSpp_all_Habs$sppChange_IDCbrazil3.hab4, 
     breaks=c(-1,-0.5,-0.3,-0.05,0.05,0.25,100), plot=FALSE)
hist(out_ChangeSpp_all_Habs$sppChange_FC3.hab4, 
     breaks=c(-1,-0.5,-0.3,-0.05,0.05,0.25,100), plot=FALSE)
hist(out_ChangeSpp_all_Habs$sppChange_FCnoSFA3.hab4, 
     breaks=c(-1,-0.5,-0.3,-0.05,0.05,0.25,100), plot=FALSE)
hist(out_ChangeSpp_all_Habs$sppChange_FCnoCRA3.hab4, 
     breaks=c(-1,-0.5,-0.3,-0.05,0.05,0.25,100), plot=FALSE)
hist(out_ChangeSpp_all_Habs$sppChange_FCnoCRAnoSFA3.hab4, 
     breaks=c(-1,-0.5,-0.3,-0.05,0.05,0.25,100), plot=FALSE)

spp2010<-read.csv("out_spp_suitarearegion_IDCbrazil_2010.csv")
names(spp2010)
out_ChangeSpp_all_Habs<-merge(out_ChangeSpp_all_Habs,spp2010, by="spp.x")
View(out_ChangeSpp_all_Habs[out_ChangeSpp_all_Habs$spp.x=="Panthera onca"|
                              out_ChangeSpp_all_Habs$spp.x=="Aratinga solstitialis"|
                              out_ChangeSpp_all_Habs$spp.x=="Tolypeutes tricinctus"|
                              out_ChangeSpp_all_Habs$spp.x=="Chrysocyon brachyurus"|
                              out_ChangeSpp_all_Habs$spp.x=="Cebus xanthosternos"|
                              out_ChangeSpp_all_Habs$spp.x=="Penelope ochrogaster",])


View(out_ChangeSpp_all_Habs[out_ChangeSpp_all_Habs$spp.x=="Aratinga solstitialis",])

Gains<-out_ChangeSpp_all_Habs[out_ChangeSpp_all_Habs$sppChange_FC.hab4>(0),]
###STEP 9: Preparing final output data######
#IDCbrazil####
setwd(outputpath)
out_SppChange_IDCbrazil<-read.csv("out_SppChange_IDCbrazil_Hab1.csv", row.names = 1)
out_SppChange_IDCbrazil_prep<-out_SppChange_IDCbrazil[c("spp.x","colrow", "suitarea_max_IDCbrazil_2010","sppChange_IDCbrazil",
                                            "sppChange_IDCbrazil2","sppChange_IDCbrazil3")]
names(out_SppChange_IDCbrazil_prep)<-c("spp.x","colrow","suitarea_max_2010","Change2010_2050a",
                                 "Change2010_2050b","Change2010_2050c")
out_SppChange_IDCbrazil2<-reshape(out_SppChange_IDCbrazil_prep,timevar="spp.x",idvar="colrow", 
                            direction="wide")
out_ChangeCR_IDCbrazil_Hab1<-read.csv("out_ChangeCR_IDCbrazil_Hab1.csv", row.names = 1)
names(out_ChangeCR_IDCbrazil_Hab1)<-c("colrow","TotalChange10_50a",
                                 "TotalChange10_50b","TotalChange10_50c")
out_SppChange_IDCbrazil3<-merge(out_ChangeCR_IDCbrazil_Hab1,out_SppChange_IDCbrazil2,all=TRUE)
write.csv(out_SppChange_IDCbrazil3,file="Spp_results_Hab1_IDCbrazil.csv")

spp_list<-out_SppChange_IDCbrazil[c("spp.x")]
spp_list<-unique(spp_list)
View(spp_list)
remove(spp_list)
remove(out_SppChange_IDCbrazil3, out_SppChange_IDCbrazil2,out_SppChange_IDCbrazil_prep,out_SppChange_IDCbrazil, 
       out_ChangeCR_IDCbrazil_Hab1)

out_SppChange_IDCbrazil<-read.csv("out_SppChange_IDCbrazil_Hab4.csv", row.names = 1)
out_SppChange_IDCbrazil_prep<-out_SppChange_IDCbrazil[c("spp.x","colrow", "suitarea_max_IDCbrazil_2010","sppChange_IDCbrazil",
                                            "sppChange_IDCbrazil2","sppChange_IDCbrazil3")]
names(out_SppChange_IDCbrazil_prep)<-c("spp.x","colrow","suitarea_max_2010","Change2010_2050a",
                                 "Change2010_2050b","Change2010_2050c")
out_SppChange_IDCbrazil2<-reshape(out_SppChange_IDCbrazil_prep,timevar="spp.x",idvar="colrow", 
                            direction="wide")
out_ChangeCR_IDCbrazil_Hab4<-read.csv("out_ChangeCR_IDCbrazil_Hab4.csv", row.names = 1)
names(out_ChangeCR_IDCbrazil_Hab4)<-c("colrow","TotalChange10_50a",
                                "TotalChange10_50b","TotalChange10_50c")
out_SppChange_IDCbrazil3<-merge(out_ChangeCR_IDCbrazil_Hab4,out_SppChange_IDCbrazil2,all=TRUE)
write.csv(out_SppChange_IDCbrazil3,file="Spp_results_Hab4_IDCbrazil.csv")
remove(out_SppChange_IDCbrazil3, out_SppChange_IDCbrazil2,out_SppChange_IDCbrazil_prep,out_SppChange_IDCbrazil, 
       out_ChangeCR_IDCbrazil_Hab4)
#FC####
out_SppChange_FC<-read.csv("out_SppChange_FC_Hab1.csv", row.names = 1)
out_SppChange_FC_prep<-out_SppChange_FC[c("spp.x","colrow", "suitarea_max_FC_2010","sppChange_FC",
                                            "sppChange_FC2","sppChange_FC3")]
names(out_SppChange_FC_prep)<-c("spp.x","colrow","suitarea_max_2010","Change2010_2050a",
                                 "Change2010_2050b","Change2010_2050c")
out_SppChange_FC2<-reshape(out_SppChange_FC_prep,timevar="spp.x",idvar="colrow", 
                            direction="wide")
out_ChangeCR_FC_Hab1<-read.csv("out_ChangeCR_FC_Hab1.csv", row.names = 1)
names(out_ChangeCR_FC_Hab1)<-c("colrow","TotalChange10_50a",
                                "TotalChange10_50b","TotalChange10_50c")
out_SppChange_FC3<-merge(out_ChangeCR_FC_Hab1,out_SppChange_FC2,all=TRUE)
write.csv(out_SppChange_FC3,file="Spp_results_Hab1_FC.csv")
remove(out_SppChange_FC3, out_SppChange_FC2,out_SppChange_FC_prep,out_SppChange_FC, 
       out_ChangeCR_FC_Hab1)

out_SppChange_FC<-read.csv("out_SppChange_FC_Hab4.csv", row.names = 1)
out_SppChange_FC_prep<-out_SppChange_FC[c("spp.x","colrow", "suitarea_max_FC_2010","sppChange_FC",
                                            "sppChange_FC2","sppChange_FC3")]
names(out_SppChange_FC_prep)<-c("spp.x","colrow","suitarea_max_2010","Change2010_2050a",
                                 "Change2010_2050b","Change2010_2050c")
out_SppChange_FC2<-reshape(out_SppChange_FC_prep,timevar="spp.x",idvar="colrow", 
                            direction="wide")
out_ChangeCR_FC_Hab4<-read.csv("out_ChangeCR_FC_Hab4.csv", row.names = 1)
names(out_ChangeCR_FC_Hab4)<-c("colrow","TotalChange10_50a",
                                "TotalChange10_50b","TotalChange10_50c")
out_SppChange_FC3<-merge(out_ChangeCR_FC_Hab4,out_SppChange_FC2,all=TRUE)
write.csv(out_SppChange_FC3,file="Spp_results_Hab4_FC.csv")
remove(out_SppChange_FC3, out_SppChange_FC2,out_SppChange_FC_prep,out_SppChange_FC, 
       out_ChangeCR_FC_Hab4)
#FCnoSFA####
out_SppChange_FCnoSFA<-read.csv("out_SppChange_FCnoSFA_Hab1.csv", row.names = 1)
out_SppChange_FCnoSFA_prep<-out_SppChange_FCnoSFA[c("spp.x","colrow", "suitarea_max_FCnoSFA_2010",
                                                    "sppChange_FCnoSFA",
                                            "sppChange_FCnoSFA2","sppChange_FCnoSFA3")]
names(out_SppChange_FCnoSFA_prep)<-c("spp.x","colrow","suitarea_max_2010","Change2010_2050a",
                                 "Change2010_2050b","Change2010_2050c")
out_SppChange_FCnoSFA2<-reshape(out_SppChange_FCnoSFA_prep,timevar="spp.x",idvar="colrow", 
                            direction="wide")
out_ChangeCR_FCnoSFA_Hab1<-read.csv("out_ChangeCR_FCnoSFA_Hab1.csv", row.names = 1)
names(out_ChangeCR_FCnoSFA_Hab1)<-c("colrow","TotalChange10_50a",
                                "TotalChange10_50b","TotalChange10_50c")
out_SppChange_FCnoSFA3<-merge(out_ChangeCR_FCnoSFA_Hab1,out_SppChange_FCnoSFA2,all=TRUE)
write.csv(out_SppChange_FCnoSFA3,file="Spp_results_Hab1_FCnoSFA.csv")
remove(out_SppChange_FCnoSFA3, out_SppChange_FCnoSFA2,out_SppChange_FCnoSFA_prep,out_SppChange_FCnoSFA, 
       out_ChangeCR_FCnoSFA_Hab1)

out_SppChange_FCnoSFA<-read.csv("out_SppChange_FCnoSFA_Hab4.csv", row.names = 1)
out_SppChange_FCnoSFA_prep<-out_SppChange_FCnoSFA[c("spp.x","colrow", "suitarea_max_FCnoSFA_2010",
                                                    "sppChange_FCnoSFA",
                                                    "sppChange_FCnoSFA2","sppChange_FCnoSFA3")]
names(out_SppChange_FCnoSFA_prep)<-c("spp.x","colrow","suitarea_max_2010","Change2010_2050a",
                                 "Change2010_2050b","Change2010_2050c")
out_SppChange_FCnoSFA2<-reshape(out_SppChange_FCnoSFA_prep,timevar="spp.x",idvar="colrow", 
                            direction="wide")
out_ChangeCR_FCnoSFA_Hab4<-read.csv("out_ChangeCR_FCnoSFA_Hab4.csv", row.names = 1)
names(out_ChangeCR_FCnoSFA_Hab4)<-c("colrow","TotalChange10_50a",
                                "TotalChange10_50b","TotalChange10_50c")
out_SppChange_FCnoSFA3<-merge(out_ChangeCR_FCnoSFA_Hab4,out_SppChange_FCnoSFA2,all=TRUE)
write.csv(out_SppChange_FCnoSFA3,file="Spp_results_Hab4_FCnoSFA.csv")
remove(out_SppChange_FCnoSFA3, out_SppChange_FCnoSFA2,out_SppChange_FCnoSFA_prep,out_SppChange_FCnoSFA, 
       out_ChangeCR_FCnoSFA_Hab4)
#FCnoCRA####
out_SppChange_FCnoCRA<-read.csv("out_SppChange_FCnoCRA_Hab1.csv", row.names = 1)
out_SppChange_FCnoCRA_prep<-out_SppChange_FCnoCRA[c("spp.x","colrow", "suitarea_max_FCnoCRA_2010",
                                                    "sppChange_FCnoCRA",
                                                    "sppChange_FCnoCRA2","sppChange_FCnoCRA3")]
names(out_SppChange_FCnoCRA_prep)<-c("spp.x","colrow","suitarea_max_2010","Change2010_2050a",
                                     "Change2010_2050b","Change2010_2050c")
out_SppChange_FCnoCRA2<-reshape(out_SppChange_FCnoCRA_prep,timevar="spp.x",idvar="colrow", 
                                direction="wide")
out_ChangeCR_FCnoCRA_Hab1<-read.csv("out_ChangeCR_FCnoCRA_Hab1.csv", row.names = 1)
names(out_ChangeCR_FCnoCRA_Hab1)<-c("colrow","TotalChange10_50a",
                                    "TotalChange10_50b","TotalChange10_50c")
out_SppChange_FCnoCRA3<-merge(out_ChangeCR_FCnoCRA_Hab1,out_SppChange_FCnoCRA2,all=TRUE)
write.csv(out_SppChange_FCnoCRA3,file="Spp_results_Hab1_FCnoCRA.csv")
remove(out_SppChange_FCnoCRA3, out_SppChange_FCnoCRA2,out_SppChange_FCnoCRA_prep,out_SppChange_FCnoCRA, 
       out_ChangeCR_FCnoCRA_Hab1)

out_SppChange_FCnoCRA<-read.csv("out_SppChange_FCnoCRA_Hab4.csv", row.names = 1)
out_SppChange_FCnoCRA_prep<-out_SppChange_FCnoCRA[c("spp.x","colrow", "suitarea_max_FCnoCRA_2010",
                                                    "sppChange_FCnoCRA",
                                                    "sppChange_FCnoCRA2","sppChange_FCnoCRA3")]
names(out_SppChange_FCnoCRA_prep)<-c("spp.x","colrow","suitarea_max_2010","Change2010_2050a",
                                     "Change2010_2050b","Change2010_2050c")
out_SppChange_FCnoCRA2<-reshape(out_SppChange_FCnoCRA_prep,timevar="spp.x",idvar="colrow", 
                                direction="wide")
out_ChangeCR_FCnoCRA_Hab4<-read.csv("out_ChangeCR_FCnoCRA_Hab4.csv", row.names = 1)
names(out_ChangeCR_FCnoCRA_Hab4)<-c("colrow","TotalChange10_50a",
                                    "TotalChange10_50b","TotalChange10_50c")
out_SppChange_FCnoCRA3<-merge(out_ChangeCR_FCnoCRA_Hab4,out_SppChange_FCnoCRA2,all=TRUE)
write.csv(out_SppChange_FCnoCRA3,file="Spp_results_Hab4_FCnoCRA.csv")
remove(out_SppChange_FCnoCRA3, out_SppChange_FCnoCRA2,out_SppChange_FCnoCRA_prep,out_SppChange_FCnoCRA, 
       out_ChangeCR_FCnoCRA_Hab4)
#FCnoCRAnoSFA####
out_SppChange_FCnoCRAnoSFA<-read.csv("out_SppChange_FCnoCRAnoSFA_Hab1.csv", row.names = 1)
out_SppChange_FCnoCRAnoSFA_prep<-out_SppChange_FCnoCRAnoSFA[c("spp.x","colrow", "suitarea_max_FCnoCRAnoSFA_2010","sppChange_FCnoCRAnoSFA",
                                            "sppChange_FCnoCRAnoSFA2","sppChange_FCnoCRAnoSFA3")]
names(out_SppChange_FCnoCRAnoSFA_prep)<-c("spp.x","colrow","suitarea_max_2010","Change2010_2050a",
                                 "Change2010_2050b","Change2010_2050c")
out_SppChange_FCnoCRAnoSFA2<-reshape(out_SppChange_FCnoCRAnoSFA_prep,timevar="spp.x",idvar="colrow", 
                            direction="wide")
out_ChangeCR_FCnoCRAnoSFA_Hab1<-read.csv("out_ChangeCR_FCnoCRAnoSFA_Hab1.csv", row.names = 1)
names(out_ChangeCR_FCnoCRAnoSFA_Hab1)<-c("colrow","TotalChange10_50a",
                                "TotalChange10_50b","TotalChange10_50c")
out_SppChange_FCnoCRAnoSFA3<-merge(out_ChangeCR_FCnoCRAnoSFA_Hab1,out_SppChange_FCnoCRAnoSFA2,all=TRUE)
write.csv(out_SppChange_FCnoCRAnoSFA3,file="Spp_results_Hab1_FCnoCRAnoSFA.csv")
remove(out_SppChange_FCnoCRAnoSFA3, out_SppChange_FCnoCRAnoSFA2,out_SppChange_FCnoCRAnoSFA_prep,out_SppChange_FCnoCRAnoSFA, 
       out_ChangeCR_FCnoCRAnoSFA_Hab1)

out_SppChange_FCnoCRAnoSFA<-read.csv("out_SppChange_FCnoCRAnoSFA_Hab4.csv", row.names = 1)
out_SppChange_FCnoCRAnoSFA_prep<-out_SppChange_FCnoCRAnoSFA[c("spp.x","colrow", "suitarea_max_FCnoCRAnoSFA_2010","sppChange_FCnoCRAnoSFA",
                                            "sppChange_FCnoCRAnoSFA2","sppChange_FCnoCRAnoSFA3")]
names(out_SppChange_FCnoCRAnoSFA_prep)<-c("spp.x","colrow","suitarea_max_2010","Change2010_2050a",
                                 "Change2010_2050b","Change2010_2050c")
out_SppChange_FCnoCRAnoSFA2<-reshape(out_SppChange_FCnoCRAnoSFA_prep,timevar="spp.x",idvar="colrow", 
                            direction="wide")
out_ChangeCR_FCnoCRAnoSFA_Hab4<-read.csv("out_ChangeCR_FCnoCRAnoSFA_Hab4.csv", row.names = 1)
names(out_ChangeCR_FCnoCRAnoSFA_Hab4)<-c("colrow","TotalChange10_50a",
                                "TotalChange10_50b","TotalChange10_50c")
out_SppChange_FCnoCRAnoSFA3<-merge(out_ChangeCR_FCnoCRAnoSFA_Hab4,out_SppChange_FCnoCRAnoSFA2,all=TRUE)
write.csv(out_SppChange_FCnoCRAnoSFA3,file="Spp_results_Hab4_FCnoCRAnoSFA.csv")
remove(out_SppChange_FCnoCRAnoSFA3, out_SppChange_FCnoCRAnoSFA2,out_SppChange_FCnoCRAnoSFA_prep,out_SppChange_FCnoCRAnoSFA, 
       out_ChangeCR_FCnoCRAnoSFA_Hab4)

