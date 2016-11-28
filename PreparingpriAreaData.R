inputpath = "C:/R/Brazil/ICMBio"
setwd(inputpath)
ExportOutput<-read.csv("Export_Output_2.txt")
View(ExportOutput)
ExportOutput$PA<-ifelse(ExportOutput$FID_PAinGL==0,"Y","N")
levels(ExportOutput$EA)
ExportOutput$PriAr<-ExportOutput$EA
PriAr_Overlap<-ExportOutput[c("Colrow","PriAr","PA","Area")]
PriAr_Overlap<-PriAr_Overlap[!PriAr_Overlap$Colrow==" ",]

CR_area<-aggregate(Area~Colrow,data=PriAr_Overlap,FUN=sum)
PriAr_Overlap2<-merge(CR_area,PriAr_Overlap,by="Colrow",suffixe=c(".CR",""))
names(PriAr_Overlap2)
PriAr_Overlap2$prop<-PriAr_Overlap2$Area/PriAr_Overlap2$Area.CR
names(PriAr_Overlap2)
PriAr_Overlap2<-PriAr_Overlap2[c("Colrow","PriAr","PA","prop")]

outputpath = "C:/R/ICMBio/Inputs150423"
setwd(outputpath)
write.csv(PriAr_Overlap2, file="PriAr_Overlap2.csv")
write.csv(PriAr_Overlap, file="PriAr_Overlap.csv")