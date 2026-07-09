#==============================================================#
#--------------------------------------------------------------#
#SCRIPT FOR ABUNDANCE ANALYSIS AND FIG 6 IN KANYAMA ET AL. 2026#
#                 WRITTEN BY G. DARGIE, NOV. 2025              #
#--------------------------------------------------------------#
#==============================================================#

#======#
#SET UP#
#======#


#SET WORKING DIRECTORY
#NB: USERS WILL NEED TO SET THEIR OWN WORKING DIRECTORIES

#NB: THIS CODE WRITES FILES TO A FOLDER IN A LOOP. USERS WILL NEED TO SET THE FOLDER WHERE THE OUTPUT FILES WILL BE SAVED
output_dir <- "PATH/TO/YOUR/OUTPUT/FOLDER"

#READ IN DATA
BD<-read.csv("FieldData-6Sites-Cleaned_12FEB.csv", header=TRUE)

names(BD)

#============================================#
#SUBSET THE DATA INTO PLOT AND 250M DATABASES#
#============================================#

#MAKE THE METHOD USED INTO A FACTOR
BD$Method<-as.factor(BD$Method)
levels(BD$Method)


#CREATE 250M DATABASE BY SUBESTTING BY METHOD
DB_250<-BD[which(BD$Method=="Rapid"),]

#CREATE PLOT DATABASE BY SUBSETTING BY METHOD

DB_PLOT<-BD[which(BD$Method=="Full plot"),]


#CHECK IT WORKED

head(DB_PLOT)
head(DB_250)


#===============================#
#PLOT DATABASE SPECIES ABUNDANCE#
#===============================#

#ADD A COLUMN CALLED "Abundance" WHICH WILL HAVE A VALUE OF 1 FOR EACH OBSERVATION. THIS WILL ALLOW US TO SUM THE SPECIES.
Abundance<-rep(1, length(DB_PLOT$Species))

#ADD THIS TO THE DATAFRAME
DB_PLOT<-cbind(DB_PLOT,Abundance)


#CREATE A NEW DATAFRAME WHICH HAS ABUNDANCE OF EACH SPECIES PER PLOT
ABUNDANCE_DB<-aggregate(DB_PLOT$Abundance~ DB_PLOT$River+DB_PLOT$Mapping.Classification..Bart.+DB_PLOT$Site+DB_PLOT$Dist+DB_PLOT$Species+DB_PLOT$Final.peat.depth.Crezee.et.al.2022..LOI.or.corrected..m., data = DB_PLOT, sum)


#RENAME THE COLUMNS
names(ABUNDANCE_DB) [1]<-"River"
names(ABUNDANCE_DB) [2]<-"Class"
names(ABUNDANCE_DB) [3]<-"Site"
names(ABUNDANCE_DB) [4]<-"Dist"
names(ABUNDANCE_DB) [5]<-"Species"
names(ABUNDANCE_DB) [6]<-"Peat_Depth"
names(ABUNDANCE_DB) [7]<-"Abundance"

#CREATE A COMBINED LABEL SO WE CAN LATER MATCH THE VALUES FROM THE FOLLOWING DATABASE WE WILL CREATE WITH VALUES FROM THIS ONE 
COMBINED_LABEL<-with(ABUNDANCE_DB, paste0(Site,":",Dist))

#ADD LABELS TO THE DATAFRAME
ABUNDANCE_DB<-cbind(ABUNDANCE_DB,COMBINED_LABEL)



#NOW NEED TO WORK OUT TOTAL NUMBER OF INDIVIDUALS PER PLOT
TOTAL_INDIVIDUALS_PER_PLOT<-aggregate(DB_PLOT$Abundance~ DB_PLOT$Site+DB_PLOT$Dist, data = DB_PLOT, sum)

#ALSO RENAME THE COLUMNS FOR THIS DATAFRAME TOO
names(TOTAL_INDIVIDUALS_PER_PLOT) [1]<-"Site"
names(TOTAL_INDIVIDUALS_PER_PLOT) [2]<-"Dist"
names(TOTAL_INDIVIDUALS_PER_PLOT) [3]<-"Abundance"


#CREATE A COMBINED LABEL FOR THIS DATAFRAME
COMBINED_LABEL<-with(TOTAL_INDIVIDUALS_PER_PLOT, paste0(Site,":",Dist))

#ADD THIS TO DATAFRAME
TOTAL_INDIVIDUALS_PER_PLOT<-cbind(TOTAL_INDIVIDUALS_PER_PLOT,COMBINED_LABEL)

#NOW USE LABELS TO MATCH THE TWO DATAFRAMES AND CREATE A NEW VECTOR WHICH IS THE LENGTH OF THE FIRST DATAFRAME, BUT CONTAINS TOTAL NUMBER OF SPECIES PER PLOT.
SUMMED_ABUNDANCE<- TOTAL_INDIVIDUALS_PER_PLOT$Abundance[match(ABUNDANCE_DB$COMBINED_LABEL,TOTAL_INDIVIDUALS_PER_PLOT$COMBINED_LABEL)]

#ADD THIS VECTOR TO THE ABUNDANCE DATAFRAME
ABUNDANCE_DB<-cbind(ABUNDANCE_DB,SUMMED_ABUNDANCE)


head(ABUNDANCE_DB)




#===============#
#TOTAL ABUNDANCE#
#===============#

ABUNDANCE_AGGREGATED<-as.data.frame(aggregate(ABUNDANCE_DB$Abundance~ ABUNDANCE_DB$Species, data = ABUNDANCE_DB, sum))

names(ABUNDANCE_AGGREGATED) [1]<-"Species"
names(ABUNDANCE_AGGREGATED) [2]<-"Abundance"

RANKED_ABUNDANCE<-ABUNDANCE_AGGREGATED[order(ABUNDANCE_AGGREGATED$Abundance, decreasing=TRUE),]


write.csv(RANKED_ABUNDANCE, file = file.path(output_dir, "RANKED_ABUNDANCE.csv"))

#====================================#
#ABUNDANCE RANKED FOR EACH WATER TYPE#
#====================================#


#FIRST SUBSET BY RIVER TYPE

CONGO_ABUNDANCE<-ABUNDANCE_DB[which(ABUNDANCE_DB$River=="Congo"),]

RUKI_ABUNDANCE<-ABUNDANCE_DB[which(ABUNDANCE_DB$River=="Ruki"),]



#NOW AGGREGATE BY SPECIES FOR EACH RIVER TYPE, SO YOU HAVE TOTAL NUMBER OF INDIVIDUALS FOR EACH SEPCIES

CONGO_ABUNDANCE_AGGREGATED<-as.data.frame(aggregate(CONGO_ABUNDANCE$Abundance~ CONGO_ABUNDANCE$Species, data = CONGO_ABUNDANCE, sum))

names(CONGO_ABUNDANCE_AGGREGATED) [1]<-"Species"
names(CONGO_ABUNDANCE_AGGREGATED) [2]<-"Abundance"

CONGO_RANKED_ABUNDANCE<-CONGO_ABUNDANCE_AGGREGATED[order(CONGO_ABUNDANCE_AGGREGATED$Abundance, decreasing=TRUE),]





RUKI_ABUNDANCE_AGGREGATED<-as.data.frame(aggregate(RUKI_ABUNDANCE$Abundance~ RUKI_ABUNDANCE$Species, data = RUKI_ABUNDANCE, sum))

names(RUKI_ABUNDANCE_AGGREGATED) [1]<-"Species"
names(RUKI_ABUNDANCE_AGGREGATED) [2]<-"Abundance"

RUKI_RANKED_ABUNDANCE<-RUKI_ABUNDANCE_AGGREGATED[order(RUKI_ABUNDANCE_AGGREGATED$Abundance, decreasing=TRUE),]




#WRITE OUT TO A CSV FILE

write.csv(CONGO_RANKED_ABUNDANCE, file = file.path(output_dir, "CONGO_RANKED_ABUNDANCE.csv"))

write.csv(RUKI_RANKED_ABUNDANCE, file = file.path(output_dir, "RUKI_RANKED_ABUNDANCE.csv"))

#=====================================#
#ABUNDANCE RANKED FOR EACH FOREST TYPE#
#=====================================#

head(ABUNDANCE_DB)


#FIRST SUBSET BY FOREST TYPE

SF_ABUNDANCE<-ABUNDANCE_DB[which(ABUNDANCE_DB$Class=="Terra firme"),]
HW_ABUNDANCE<-ABUNDANCE_DB[which(ABUNDANCE_DB$Class=="Hardwood swamp"),]
PALM_ABUNDANCE<-ABUNDANCE_DB[which(ABUNDANCE_DB$Class=="Palm-dominated swamp"),]



#NOW AGGREGATE BY SPECIES FOR EACH FOREST TYPE, SO YOU HAVE TOTAL NUMBER OF INDIVIDUALS FOR EACH SPECIES

SF_ABUNDANCE_AGGREGATED<-as.data.frame(aggregate(SF_ABUNDANCE$Abundance~ SF_ABUNDANCE$Species, data = SF_ABUNDANCE, sum))

names(SF_ABUNDANCE_AGGREGATED) [1]<-"Species"
names(SF_ABUNDANCE_AGGREGATED) [2]<-"Abundance"


HW_ABUNDANCE_AGGREGATED<-as.data.frame(aggregate(HW_ABUNDANCE$Abundance~ HW_ABUNDANCE$Species, data = HW_ABUNDANCE, sum))

names(HW_ABUNDANCE_AGGREGATED) [1]<-"Species"
names(HW_ABUNDANCE_AGGREGATED) [2]<-"Abundance"


PALM_ABUNDANCE_AGGREGATED<-as.data.frame(aggregate(PALM_ABUNDANCE$Abundance~ PALM_ABUNDANCE$Species, data = PALM_ABUNDANCE, sum))

names(PALM_ABUNDANCE_AGGREGATED) [1]<-"Species"
names(PALM_ABUNDANCE_AGGREGATED) [2]<-"Abundance"


#NOW RANK THEM IN DECREASING ORDER

SF_RANKED_ABUNDANCE<-SF_ABUNDANCE_AGGREGATED[order(SF_ABUNDANCE_AGGREGATED$Abundance, decreasing=TRUE),]

HW_RANKED_ABUNDANCE<-HW_ABUNDANCE_AGGREGATED[order(HW_ABUNDANCE_AGGREGATED$Abundance, decreasing=TRUE),]

PALM_RANKED_ABUNDANCE<-PALM_ABUNDANCE_AGGREGATED[order(PALM_ABUNDANCE_AGGREGATED$Abundance, decreasing=TRUE),]



#WRITE OUT TO A CSV FILE

write.csv(SF_RANKED_ABUNDANCE, file = file.path(output_dir, "SF_RANKED_ABUNDANCE.csv"))

write.csv(HW_RANKED_ABUNDANCE, file = file.path(output_dir, "HW_RANKED_ABUNDANCE.csv"))

write.csv(PALM_RANKED_ABUNDANCE, file = file.path(output_dir, "PALM_RANKED_ABUNDANCE.csv"))


#===============================================#
#ABUNDANCE RANKED FOR EACH WATER AND FOREST TYPE#
#===============================================#

#FIRST SUBSET BY WATER AND FOREST TYPE

WW_SF_ABUNDANCE<-ABUNDANCE_DB[which(ABUNDANCE_DB$Class=="Terra firme" & ABUNDANCE_DB$River=="Congo"),]

WW_HW_ABUNDANCE<-ABUNDANCE_DB[which(ABUNDANCE_DB$Class=="Hardwood swamp" & ABUNDANCE_DB$River=="Congo"),]

WW_PALM_ABUNDANCE<-ABUNDANCE_DB[which(ABUNDANCE_DB$Class=="Palm-dominated swamp" & ABUNDANCE_DB$River=="Congo"),]


BW_SF_ABUNDANCE<-ABUNDANCE_DB[which(ABUNDANCE_DB$Class=="Terra firme" & ABUNDANCE_DB$River=="Ruki"),]

BW_HW_ABUNDANCE<-ABUNDANCE_DB[which(ABUNDANCE_DB$Class=="Hardwood swamp" & ABUNDANCE_DB$River=="Ruki"),]

BW_PALM_ABUNDANCE<-ABUNDANCE_DB[which(ABUNDANCE_DB$Class=="Palm-dominated swamp" & ABUNDANCE_DB$River=="Ruki"),]



#NOW AGGREGATE BY SPECIES FOR EACH FOREST TYPE, SO YOU HAVE TOTAL NUMBER OF INDIVIDUALS FOR EACH SPECIES

WW_SF_ABUNDANCE_AGGREGATED<-as.data.frame(aggregate(WW_SF_ABUNDANCE$Abundance~ WW_SF_ABUNDANCE$Species, data = WW_SF_ABUNDANCE, sum))

names(WW_SF_ABUNDANCE_AGGREGATED) [1]<-"Species"
names(WW_SF_ABUNDANCE_AGGREGATED) [2]<-"Abundance"



WW_HW_ABUNDANCE_AGGREGATED<-as.data.frame(aggregate(WW_HW_ABUNDANCE$Abundance~ WW_HW_ABUNDANCE$Species, data = WW_HW_ABUNDANCE, sum))

names(WW_HW_ABUNDANCE_AGGREGATED) [1]<-"Species"
names(WW_HW_ABUNDANCE_AGGREGATED) [2]<-"Abundance"



WW_PALM_ABUNDANCE_AGGREGATED<-as.data.frame(aggregate(WW_PALM_ABUNDANCE$Abundance~ WW_PALM_ABUNDANCE$Species, data = WW_PALM_ABUNDANCE, sum))

names(WW_PALM_ABUNDANCE_AGGREGATED) [1]<-"Species"
names(WW_PALM_ABUNDANCE_AGGREGATED) [2]<-"Abundance"




BW_SF_ABUNDANCE_AGGREGATED<-as.data.frame(aggregate(BW_SF_ABUNDANCE$Abundance~ BW_SF_ABUNDANCE$Species, data = BW_SF_ABUNDANCE, sum))

names(BW_SF_ABUNDANCE_AGGREGATED) [1]<-"Species"
names(BW_SF_ABUNDANCE_AGGREGATED) [2]<-"Abundance"



BW_HW_ABUNDANCE_AGGREGATED<-as.data.frame(aggregate(BW_HW_ABUNDANCE$Abundance~ BW_HW_ABUNDANCE$Species, data = BW_HW_ABUNDANCE, sum))

names(BW_HW_ABUNDANCE_AGGREGATED) [1]<-"Species"
names(BW_HW_ABUNDANCE_AGGREGATED) [2]<-"Abundance"



BW_PALM_ABUNDANCE_AGGREGATED<-as.data.frame(aggregate(BW_PALM_ABUNDANCE$Abundance~ BW_PALM_ABUNDANCE$Species, data = BW_PALM_ABUNDANCE, sum))

names(BW_PALM_ABUNDANCE_AGGREGATED) [1]<-"Species"
names(BW_PALM_ABUNDANCE_AGGREGATED) [2]<-"Abundance"


#NOW RANK THEM IN DECREASING ORDER

WW_SF_RANKED_ABUNDANCE<-WW_SF_ABUNDANCE_AGGREGATED[order(WW_SF_ABUNDANCE_AGGREGATED$Abundance, decreasing=TRUE),]

WW_HW_RANKED_ABUNDANCE<-WW_HW_ABUNDANCE_AGGREGATED[order(WW_HW_ABUNDANCE_AGGREGATED$Abundance, decreasing=TRUE),]

WW_PALM_RANKED_ABUNDANCE<-WW_PALM_ABUNDANCE_AGGREGATED[order(WW_PALM_ABUNDANCE_AGGREGATED$Abundance, decreasing=TRUE),]



BW_SF_RANKED_ABUNDANCE<-BW_SF_ABUNDANCE_AGGREGATED[order(BW_SF_ABUNDANCE_AGGREGATED$Abundance, decreasing=TRUE),]

BW_HW_RANKED_ABUNDANCE<-BW_HW_ABUNDANCE_AGGREGATED[order(BW_HW_ABUNDANCE_AGGREGATED$Abundance, decreasing=TRUE),]

BW_PALM_RANKED_ABUNDANCE<-BW_PALM_ABUNDANCE_AGGREGATED[order(BW_PALM_ABUNDANCE_AGGREGATED$Abundance, decreasing=TRUE),]





#WRITE OUT TO A CSV FILE

write.csv(WW_SF_RANKED_ABUNDANCE, file = file.path(output_dir, "WW_SF_RANKED_ABUNDANCE.csv"))

write.csv(WW_HW_RANKED_ABUNDANCE, file = file.path(output_dir, "WW_HW_RANKED_ABUNDANCE.csv"))

write.csv(WW_PALM_RANKED_ABUNDANCE, file = file.path(output_dir, "WW_PALM_RANKED_ABUNDANCE.csv"))


write.csv(BW_SF_RANKED_ABUNDANCE, file = file.path(output_dir, "BW_SF_RANKED_ABUNDANCE.csv"))

write.csv(BW_HW_RANKED_ABUNDANCE, file = file.path(output_dir, "BW_HW_RANKED_ABUNDANCE.csv"))

write.csv(BW_PALM_RANKED_ABUNDANCE, file = file.path(output_dir, "BW_PALM_RANKED_ABUNDANCE.csv"))




#=======================#
#ABUNDANCE VS PEAT DEPTH#
#=======================#



names(ABUNDANCE_DB)

#FIRST GET THE TOP 20 SPECIES

ABUNDANCE_AGGREGATED<-as.data.frame(aggregate(ABUNDANCE_DB$Abundance~ ABUNDANCE_DB$Species, data = ABUNDANCE_DB, sum))

names(ABUNDANCE_AGGREGATED) [1]<-"Species"
names(ABUNDANCE_AGGREGATED) [2]<-"Abundance"

RANKED_ABUNDANCE<-ABUNDANCE_AGGREGATED[order(ABUNDANCE_AGGREGATED$Abundance, decreasing=TRUE),]

RANKED_ABUNDANCE_TOP_20<-RANKED_ABUNDANCE$Species[1:20]


#NOW CREATE A LOOP WHERE YOU LOOP THROUGH THESE 20 SPECIES AND PLOT ABUNDANCE AGAINST PEAT DEPTH FOR EACH SPECIES ADDING A LINEAR MODEL




for (i in RANKED_ABUNDANCE_TOP_20){

	
	SUBSET<-ABUNDANCE_DB[which(ABUNDANCE_DB$Species==i),]


	filename<-paste0("output_dir", i, "_abundance_vs_peat_depth.png")

	png(filename, width=800, height=600)

	par(mar=c(6,6,1,1))

	plot(SUBSET$Peat_Depth,(SUBSET$Abundance*12.5), ylab="Abundance (stems per hectare)", xlab="", cex.lab=3,cex.axis=2)
	mtext("Peat Depth (m)", line=4, side=1,  cex=3) 


	abline(lm((SUBSET$Abundance*12.5)~SUBSET$Peat_Depth))

	SUMMARY<-summary(lm(SUBSET$Abundance~SUBSET$Peat_Depth))


	PVAL<-round((SUMMARY$coefficients[2, 4]),3)
	
	text(max(SUBSET$Peat_Depth)*0.66, max((SUBSET$Abundance*12.5))*0.66, paste0("p=", PVAL), cex=3)

  	dev.off()

	filename2<-paste0("output_dir", i, "_diagnostic_plots.png")

	png(filename2, width=800, height=600)

	par(mfrow = c(2, 2))

	plot(lm(SUBSET$Abundance~SUBSET$Peat_Depth))

	dev.off()
	
	}
	


#================================#
#ABUNDANCE VS DISTANCE FROM RIVER#
#================================#

#BRING IN THE DISTANCE FROM RIVER DATA CALCULATED IN QGIS 


DIST_RIVER<-read.csv("Dist_to_river.csv", header=TRUE)

names(DIST_RIVER)

COMBINED_LABEL<-with(DIST_RIVER, paste0(Site,":",Dist))

DIST_RIVER<-cbind(DIST_RIVER,COMBINED_LABEL)

Distance_To_River<- DIST_RIVER$HubDist[match(ABUNDANCE_DB$COMBINED_LABEL,DIST_RIVER$COMBINED_LABEL)]

ABUNDANCE_DB<-cbind(ABUNDANCE_DB,Distance_To_River)



#NOW RUN A LOOP PLOTTING SPECIES ABUNDANCE AGAINST DISTNACE FROM RIVER


for (i in RANKED_ABUNDANCE_TOP_20){

	
	SUBSET<-ABUNDANCE_DB[which(ABUNDANCE_DB$Species==i),]

	filename <- file.path(output_dir, paste0(i, "_abundance_vs_river_dist.png"))

	png(filename, width=800, height=600)

	par(mar=c(6,6,1,1))

	plot(SUBSET$Distance_To_River,(SUBSET$Abundance*12.5), ylab="Abundance (stems per hectare)", xlab="", cex.lab=3,cex.axis=2)

	mtext("Distance to River (km)", line=4, side=1,  cex=3) 


	abline(lm((SUBSET$Abundance*12.5)~SUBSET$Distance_To_River))

	SUMMARY<-summary(lm(SUBSET$Abundance~SUBSET$Distance_To_River))


	PVAL<-round((SUMMARY$coefficients[2, 4]),4)
	
	text(max(SUBSET$Distance_To_River)*0.66, max((SUBSET$Abundance*12.5))*0.66, paste0("p=", PVAL), cex=3)

  	dev.off()

	filename2 <- file.path(output_dir, paste0(i, "_diagnostic_plots.png"))

	png(filename2, width=800, height=600)

	par(mfrow = c(2, 2))

	plot(lm(SUBSET$Abundance~SUBSET$Distance_To_River))

	dev.off()
	
	}
	


#==============================================#
#ABUNDANCE VS DISTANCE FROM RIVER BY WATER TYPE#
#==============================================#



CONGO_ABUNDANCE<-ABUNDANCE_DB[which(ABUNDANCE_DB$River=="Congo"),]

RUKI_ABUNDANCE<-ABUNDANCE_DB[which(ABUNDANCE_DB$River=="Ruki"),]





#FIRST GET THE TOP 20 SPECIES FOR EACH WATER TYPE

CONGO_ABUNDANCE_AGGREGATED<-as.data.frame(aggregate(CONGO_ABUNDANCE$Abundance~ CONGO_ABUNDANCE$Species, data = CONGO_ABUNDANCE, sum))

RUKI_ABUNDANCE_AGGREGATED<-as.data.frame(aggregate(RUKI_ABUNDANCE$Abundance~ RUKI_ABUNDANCE$Species, data = RUKI_ABUNDANCE, sum))


names(CONGO_ABUNDANCE_AGGREGATED) [1]<-"Species"
names(CONGO_ABUNDANCE_AGGREGATED) [2]<-"Abundance"

names(RUKI_ABUNDANCE_AGGREGATED) [1]<-"Species"
names(RUKI_ABUNDANCE_AGGREGATED) [2]<-"Abundance"




CONGO_RANKED_ABUNDANCE<-CONGO_ABUNDANCE_AGGREGATED[order(CONGO_ABUNDANCE_AGGREGATED$Abundance, decreasing=TRUE),]

CONGO_RANKED_ABUNDANCE_TOP_20<-CONGO_RANKED_ABUNDANCE$Species[1:20]




RUKI_RANKED_ABUNDANCE<-RUKI_ABUNDANCE_AGGREGATED[order(RUKI_ABUNDANCE_AGGREGATED$Abundance, decreasing=TRUE),]

RUKI_RANKED_ABUNDANCE_TOP_20<-RUKI_RANKED_ABUNDANCE$Species[1:20]





Distance_To_River<- DIST_RIVER$HubDist[match(CONGO_ABUNDANCE$COMBINED_LABEL,DIST_RIVER$COMBINED_LABEL)]

CONGO_ABUNDANCE<-cbind(CONGO_ABUNDANCE,Distance_To_River)



Distance_To_River<- DIST_RIVER$HubDist[match(RUKI_ABUNDANCE$COMBINED_LABEL,DIST_RIVER$COMBINED_LABEL)]

RUKI_ABUNDANCE<-cbind(RUKI_ABUNDANCE,Distance_To_River)




#CONGO LOOP


for (i in CONGO_RANKED_ABUNDANCE_TOP_20){

	
	SUBSET<-CONGO_ABUNDANCE[which(CONGO_ABUNDANCE$Species==i),]

	filename<-paste0("output_dir", i, "_abundance_vs_river_dist_Congo.png")	

	png(filename, width=800, height=600)

	plot(SUBSET$Distance_To_River,SUBSET$Abundance, ylab="Abundance", xlab="Distance to River (km)")


	abline(lm(SUBSET$Abundance~SUBSET$Distance_To_River))

	SUMMARY<-summary(lm(SUBSET$Abundance~SUBSET$Distance_To_River))


	PVAL<-round((SUMMARY$coefficients[2, 4]),4)
	
	text(max(SUBSET$Distance_To_River)*0.66, max(SUBSET$Abundance)*0.66, paste0("p=", PVAL))

  	dev.off()

	filename2<-paste0("output_dir", i, "_diagnostic_plots.png")	

	png(filename2, width=800, height=600)

	par(mfrow = c(2, 2))

	plot(lm(SUBSET$Abundance~SUBSET$Distance_To_River))

	dev.off()
	
	}





#RUKI LOOP


for (i in RUKI_RANKED_ABUNDANCE_TOP_20){
	
	SUBSET<-RUKI_ABUNDANCE[which(RUKI_ABUNDANCE$Species==i),]

	filename<-paste0("output_dir", i, "_abundance_vs_river_dist_Ruki.png")		

	png(filename, width=800, height=600)

	plot(SUBSET$Distance_To_River,SUBSET$Abundance, ylab="Abundance", xlab="Distance to River (km)")


	abline(lm(SUBSET$Abundance~SUBSET$Distance_To_River))

	SUMMARY<-summary(lm(SUBSET$Abundance~SUBSET$Distance_To_River))


	PVAL<-round((SUMMARY$coefficients[2, 4]),4)
	
	text(max(SUBSET$Distance_To_River)*0.66, max(SUBSET$Abundance)*0.66, paste0("p=", PVAL))

  	dev.off()

	filename2<-paste0("output_dir", i, "_diagnostic_plots.png")	

	png(filename2, width=800, height=600)

	par(mfrow = c(2, 2))

	plot(lm(SUBSET$Abundance~SUBSET$Distance_To_River))

	dev.off()
	
	}
