#==================================================================#
#------------------------------------------------------------------#
#SCRIPT FOR FIG 2 AND SUPP. MATERIAL FIG 1-6 IN KANYAMA ET AL. 2026#
#                   WRITTEN BY G. DARGIE, NOV. 2025			 #
#------------------------------------------------------------------#
#==================================================================#

#======#
#SET UP#
#======#


#SET WORKING DIRECTORY
#NB: USER'S WILL NEED TO SET THEIR OWN WORKING DIRECTORIES


#READ IN DATA (BD=BASE DE DONNÉES)
BD<-read.csv("FieldData-6Sites-Cleaned_12JUNE25.csv", header=TRUE)

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

#======================================#
#PLOT DATABASE SPECIES RICHNESS BY AREA#
#======================================#

#TELL R THAT FAMILY, GENUS AND SEPCIES ARE FACTORS, AS IS SITE AND DISTANCE
#BUT AFTER SUBSETTING REMEMBER TO USE "DROPLEVELS" TO STOP UNUSED FACTORS LINGERING AND BEING COUNTED.

DB_PLOT$Family<-as.factor(DB_PLOT$Family)
DB_PLOT$Genus<-as.factor(DB_PLOT$Genus)
DB_PLOT$Species<-as.factor(DB_PLOT$Species)
DB_PLOT$Site<-as.factor(DB_PLOT$Site)
DB_PLOT$Dist..m.<-as.factor(DB_PLOT$Dist..m.)
DB_PLOT$River<-as.factor(DB_PLOT$River)


levels(DB_PLOT$Family)
levels(DB_PLOT$Genus)
levels(DB_PLOT$Species)
levels(DB_PLOT$Site)
levels(DB_PLOT$Dist..m.)
levels(DB_PLOT$River)


#GET SPECIES NUMBER PER SITE
#THE FUNCTION USED GIVES THE LENGTH (I.E. THE NUMBER) OF LEVELS OF SPECIES, DROPPING ANY UNUSED LEVELS.

RICHNESS<-t(tapply(DB_PLOT$Species, list(DB_PLOT$Site,DB_PLOT$Dist..m.), function(x){length(levels(droplevels(x)))}))


write.csv(RICHNESS,file = "Richness_Data.csv")


#===============#
#---------------#
#DISPLAYING DATA#
#---------------#
#===============#


#NEED A PLOT FOR EACH TRANSECT THAT SHOWS PEAT DEPTH, CANOPY HEIGHT, NUMBER OF SPECIES


#NUMBER OF SPECIES IS THE "RICHNESS" TABLE


#NEED TO COLLAPSE DOWN THE DATABASE SO YOU HAVE JUST ONE LINE PER PLOT WITH A MEAN CANOPY HEIGHT
CANOPY_HEIGHT<-aggregate(DB_PLOT$Moyen.Hauteur~ DB_PLOT$River+DB_PLOT$Mapping.Classification..Bart.+DB_PLOT$Site+DB_PLOT$Dist, data = DB_PLOT, mean)

#RENAME THE COLUMNS
names(CANOPY_HEIGHT) [1]<-"River"
names(CANOPY_HEIGHT) [2]<-"Class"
names(CANOPY_HEIGHT) [3]<-"Site"
names(CANOPY_HEIGHT) [4]<-"Dist"
names(CANOPY_HEIGHT) [5]<-"Canopy_Height"

#ADD A ROWS FOR ANY SAVANNAH POINTS- THESE ARE CURRENTLY NOT WITHIN THE DB_PLOT DATABASE BECAUSE IF IT WAS SAVANNAH THEY DO NOT HAVE FOREST PLOT DATA.

new_rows <- data.frame(
  River = c("Congo"),
  Class = c("Savannah"),
  Site = c("Boboka"),
  Dist = c(1000),
  Canopy_Height=c(0)
)

CANOPY_HEIGHT<-rbind(CANOPY_HEIGHT,new_rows)


#REPLACE THE TERM "TERRE FIRME" WITH "SFFM", AND HARDWOOD WITH "HPSF" AND PALM WITH "PPSF". NB: SEASONALLY FLOODED FORESTS CALLED TERRE FIRME IN ORIGINAL DATABASE BECAUSE IN CREZEE ET AL. 2022 SEASONALLY FLOODED FOREST AND TERRE FIRME FOREST WAS AMALGAMATED INTO ONE "TERRE FIRME" CLASS.
CANOPY_HEIGHT$Class[CANOPY_HEIGHT$Class== "Terra firme"] <- "SFMS"
CANOPY_HEIGHT$Class[CANOPY_HEIGHT$Class== "Hardwood swamp"] <- "HPSF"
CANOPY_HEIGHT$Class[CANOPY_HEIGHT$Class== "Palm-dominated swamp"] <- "PPSF"




#NEED TO COLLAPSE DOWN THE DATABASE SO YOU HAVE JUST ONE LINE PER PLOT WITH A PEAT DEPTH FOR THIS PLOT
PEAT_DEPTH<-aggregate(DB_PLOT$Final.peat.depth.Crezee.et.al.2022..LOI.or.corrected..m.~ DB_PLOT$River+DB_PLOT$Mapping.Classification..Bart.+DB_PLOT$Site+DB_PLOT$Dist, data = DB_PLOT, mean)

#RENAME THE COLUMNS
names(PEAT_DEPTH) [1]<-"River"
names(PEAT_DEPTH) [2]<-"Class"
names(PEAT_DEPTH) [3]<-"Site"
names(PEAT_DEPTH) [4]<-"Dist"
names(PEAT_DEPTH) [5]<-"Peat_Depth"

#ADD A ROWS FOR ANY SAVANNAH POINTS

new_rows <- data.frame(
  River = c("Congo"),
  Class = c("Savannah"),
  Site = c("Boboka"),
  Dist = c(1000),
  Peat_Depth=c(0)
)

PEAT_DEPTH<-rbind(PEAT_DEPTH,new_rows)

#REPLACE THE TERM "TERRE FIRME" WITH "SEASONALLY FLOODED SWAMP"

PEAT_DEPTH$Class[PEAT_DEPTH$Class== "Terra firme"] <- "Seasonally flooded forest"



########################################################################################################################
########################################################################################################################
#**********************************************************************************************************************#
#==================================================  1.BOBOKA =========================================================#
#**********************************************************************************************************************#
########################################################################################################################
########################################################################################################################

#SET UP THE PLOTTING WINDOW


#CREATE THE MATRIX FIRST
layout.matrix <- matrix(c(1,2,3), nrow = 3, ncol = 1)

#DEFINE HEIGHTS
layout(layout.matrix, heights = c(1,1,1))



#-------------------------#
#FIRST PANEL-CANOPY HEIGHT#
#-------------------------#
par(mar=c(1.5,9,1.5,2))


#SO THAT THE BARPLOTS ARE PLOTTED IN THE CORRECT ORDER RE-ORDER YOUR NAMES ARGUMENT (WHICH IS TRANSECT DISTANCE)- THIS IS ONLY AN ISSUE FOR BOBOKA

ordered_indices <- order(CANOPY_HEIGHT$Dist[which(CANOPY_HEIGHT$Site=="Boboka")])
ordered_values <- CANOPY_HEIGHT$Canopy_Height[which(CANOPY_HEIGHT$Site=="Boboka")][ordered_indices]  # Reorder values
ordered_names <- CANOPY_HEIGHT$Dist[which(CANOPY_HEIGHT$Site=="Boboka")][ordered_indices]  # Reorder names
ordered_class<- CANOPY_HEIGHT$Class[which(CANOPY_HEIGHT$Site=="Boboka")][ordered_indices]

group_colours <- c("SFMS" = "darkgreen", "HPSF" = "darkolivegreen4", "PPSF"="darkolivegreen3", "Savannah"="yellow" )

bp<-barplot(ordered_values, names.arg=ordered_names, col = group_colours[ordered_class], ylim=c(0,37), ylab="", yaxt="n",xaxt="n", cex.lab=2.3, cex.axis=2)
mtext(text="Canopy Height (m)", side=2, line=4, cex=2)
axis(2, at = seq(0, 30, by = 10), labels = c("0", "10", "20", "30"),cex.axis=2)

text(bp[c(1,3,5,7,9,11)], 33, labels=ordered_class[seq(1, length(ordered_class), 2)], cex=2)
text(bp[c(2,4,6,8,10,12)], 33, labels=ordered_class[seq(2, length(ordered_class), 2)], cex=2)



#-----------------------#
#SECOND PANEL-PEAT DEPTH#
#-----------------------#

ordered_indices_peat <- order(PEAT_DEPTH$Dist[which(PEAT_DEPTH$Site=="Boboka")])
ordered_values_peat <- PEAT_DEPTH$Peat_Depth[which(PEAT_DEPTH$Site=="Boboka")][ordered_indices]  # Reorder values
ordered_names_peat <- PEAT_DEPTH$Dist[which(PEAT_DEPTH$Site=="Boboka")][ordered_indices]  # Reorder names



par(mar=c(1.5,9,1.5,2))
barplot(ordered_values_peat*-1, names.arg=ordered_names, col="brown", ylab="", xaxt="n", yaxt="n",ylim=c(-6,0), cex.lab=2.3, cex.axis=2)
axis(2, at = seq(-6, 0, by = 1), labels = c("6", "5", "4", "3", "2", "1", "0"),cex.axis=2)
mtext(text="Peat Depth (m)", side=2, line=4, cex=2)


#----------------------------------------------------#
#THIRD PANEL-NUMBER OF SPECIES PER PLOT I.E. RICHNESS#
#----------------------------------------------------#

par(mar=c(6,9,1.5,2))


#ordered_values_richness <- RICHNESS[,"Boboka"][ordered_indices]  # Reorder values

barplot(RICHNESS[,"Boboka"] , names.arg=c("0","1","2","3","4","5","6","7","8","9","10","11"), ylab="", xlab="", ylim=c(0,25), cex.lab=2.5, cex.axis=2, cex.names=2)
mtext(text="No. of Species", side=2, line=4, cex=2)
mtext(text="Distance Along Transect (km)", side=1, line=4, cex=2)


#########################################################################################################################
#########################################################################################################################
#***********************************************************************************************************************#
#=================================================== 2. BOLENGO ========================================================#
#***********************************************************************************************************************#
#########################################################################################################################
#########################################################################################################################

#SET UP THE PLOTTING WINDOW


#CREATE THE MATRIX FIRST
layout.matrix <- matrix(c(1,2,3), nrow = 3, ncol = 1)

#DEFINE HEIGHTS
layout(layout.matrix, heights = c(1,1,1))


#-------------------------#
#FIRST PANEL-CANOPY HEIGHT#
#-------------------------#
par(mar=c(1.5,9,1.5,2))


#SO THAT THE BARPLOTS ARE PLOTTED IN THE CORRECT ORDER RE-ORDER YOUR NAMES ARGUMENT (WHICH IS TRANSECT DISTANCE)- THIS IS ONLY AN ISSUE FOR BOBOKA

ordered_indices <- order(CANOPY_HEIGHT$Dist[which(CANOPY_HEIGHT$Site=="Bolengo")])
ordered_values <- CANOPY_HEIGHT$Canopy_Height[which(CANOPY_HEIGHT$Site=="Bolengo")][ordered_indices]  # Reorder values
ordered_names <- CANOPY_HEIGHT$Dist[which(CANOPY_HEIGHT$Site=="Bolengo")][ordered_indices]  # Reorder names
ordered_class<- CANOPY_HEIGHT$Class[which(CANOPY_HEIGHT$Site=="Bolengo")][ordered_indices]

group_colours <- c("SFMS" = "darkgreen", "HPSF" = "darkolivegreen4", "PPSF"="darkolivegreen3", "Savannah"="yellow" )

bp<-barplot(ordered_values, names.arg=ordered_names, col = group_colours[ordered_class], ylim=c(0,37), ylab="", yaxt="n",xaxt="n", cex.lab=2.3, cex.axis=2)
mtext(text="Canopy Height (m)", side=2, line=4, cex=2)
axis(2, at = seq(0, 30, by = 10), labels = c("0", "10", "20", "30"),cex.axis=2)

text(bp[c(1,3,5,7)], 33, labels=ordered_class[seq(1, length(ordered_class), 2)],cex=2)
text(bp[c(2,4,6)], 33, labels=ordered_class[seq(2, length(ordered_class), 2)],cex=2)


#-----------------------#
#SECOND PANEL-PEAT DEPTH#
#-----------------------#

ordered_indices_peat <- order(PEAT_DEPTH$Dist[which(PEAT_DEPTH$Site=="Bolengo")])
ordered_values_peat <- PEAT_DEPTH$Peat_Depth[which(PEAT_DEPTH$Site=="Bolengo")][ordered_indices]  # Reorder values
ordered_names_peat <- PEAT_DEPTH$Dist[which(PEAT_DEPTH$Site=="Bolengo")][ordered_indices]  # Reorder names



par(mar=c(1.5,9,1.5,2))
barplot(ordered_values_peat*-1, names.arg=ordered_names, col="brown", ylab="", xaxt="n", yaxt="n",ylim=c(-6,0), cex.lab=2.3, cex.axis=2)
axis(2, at = seq(-6, 0, by = 1), labels = c("6", "5", "4", "3", "2", "1", "0"),cex.axis=2)
mtext(text="Peat Depth (m)", side=2, line=4, cex=2)


#----------------------------------------------------#
#THIRD PANEL-NUMBER OF SPECIES PER PLOT I.E. RICHNESS#
#----------------------------------------------------#

par(mar=c(6,9,1.5,2))

Bolengo_no_NA <- RICHNESS[,"Bolengo"][!is.na(RICHNESS[,"Bolengo"])]

barplot(Bolengo_no_NA, names.arg=c("0","1","2","3","4","5","6"), ylab="", xlab="Distance Along Transect (km)", ylim=c(0,25), cex.lab=2.3, cex.axis=2, cex.names=2)
mtext(text="No. of Species", side=2, line=4, cex=2)



##########################################################################################################################
##########################################################################################################################
#************************************************************************************************************************#
#=================================================== 3. BONDAMBA ========================================================#
#************************************************************************************************************************#
##########################################################################################################################
##########################################################################################################################

#SET UP THE PLOTTING WINDOW

#CREATE THE MATRIX FIRST
layout.matrix <- matrix(c(1,2,3), nrow = 3, ncol = 1)

#DEFINE HEIGHTS
layout(layout.matrix, heights = c(1,1,1))



#-------------------------#
#FIRST PANEL-CANOPY HEIGHT#
#-------------------------#
par(mar=c(1.5,9,1.5,2))


#SO THAT THE BARPLOTS ARE PLOTTED IN THE CORRECT ORDER RE-ORDER YOUR NAMES ARGUMENT (WHICH IS TRANSECT DISTANCE)- THIS IS ONLY AN ISSUE FOR BOBOKA

ordered_indices <- order(CANOPY_HEIGHT$Dist[which(CANOPY_HEIGHT$Site=="Bondamba")])
ordered_values <- CANOPY_HEIGHT$Canopy_Height[which(CANOPY_HEIGHT$Site=="Bondamba")][ordered_indices]  # Reorder values
ordered_names <- CANOPY_HEIGHT$Dist[which(CANOPY_HEIGHT$Site=="Bondamba")][ordered_indices]  # Reorder names
ordered_class<- CANOPY_HEIGHT$Class[which(CANOPY_HEIGHT$Site=="Bondamba")][ordered_indices]

group_colours <- c("SFMS" = "darkgreen", "HPSF" = "darkolivegreen4", "PPSF"="darkolivegreen3", "Savannah"="yellow" )

bp<-barplot(ordered_values, names.arg=ordered_names, col = group_colours[ordered_class], ylim=c(0,37), ylab="", yaxt="n",xaxt="n", cex.lab=2.3, cex.axis=2)
mtext(text="Canopy Height (m)", side=2, line=4, cex=2)
axis(2, at = seq(0, 30, by = 10), labels = c("0", "10", "20", "30"),cex.axis=2)

text(bp[c(1,3,5,7)], 33, labels=ordered_class[seq(1, length(ordered_class), 2)],cex=2)
text(bp[c(2,4,6,8)], 33, labels=ordered_class[seq(2, length(ordered_class), 2)],cex=2)

#-----------------------#
#SECOND PANEL-PEAT DEPTH#
#-----------------------#

ordered_indices_peat <- order(PEAT_DEPTH$Dist[which(PEAT_DEPTH$Site=="Bondamba")])
ordered_values_peat <- PEAT_DEPTH$Peat_Depth[which(PEAT_DEPTH$Site=="Bondamba")][ordered_indices]  # Reorder values
ordered_names_peat <- PEAT_DEPTH$Dist[which(PEAT_DEPTH$Site=="Bondamba")][ordered_indices]  # Reorder names



par(mar=c(1.5,9,1.5,2))
barplot(ordered_values_peat*-1, names.arg=ordered_names, col="brown", ylab="", xaxt="n", yaxt="n",ylim=c(-6,0), cex.lab=2.3, cex.axis=2)
axis(2, at = seq(-6, 0, by = 1), labels = c("6", "5", "4", "3", "2", "1", "0"),cex.axis=2)
mtext(text="Peat Depth (m)", side=2, line=4, cex=2)


#----------------------------------------------------#
#THIRD PANEL-NUMBER OF SPECIES PER PLOT I.E. RICHNESS#
#----------------------------------------------------#


par(mar=c(6,9,1.5,2))

Bondamba_no_NA <- RICHNESS[,"Bondamba"][!is.na(RICHNESS[,"Bondamba"])]

barplot(Bondamba_no_NA, names.arg=c("0","1","2","3","4","5","6","7"), ylab="", xlab="Distance Along Transect (km)", ylim=c(0,25), cex.lab=2.3, cex.axis=2, cex.names=2)
mtext(text="No. of Species", side=2, line=4, cex=2)






########################################################################################################################
########################################################################################################################
#**********************************************************************************************************************#
#=================================================== 4. IPOMBO ========================================================#
#**********************************************************************************************************************#
########################################################################################################################
########################################################################################################################

#SET UP THE PLOTTING WINDOW


#CREATE THE MATRIX FIRST
layout.matrix <- matrix(c(1,2,3), nrow = 3, ncol = 1)

#DEFINE HEIGHTS
layout(layout.matrix, heights = c(1,1,1))



#-------------------------#
#FIRST PANEL-CANOPY HEIGHT#
#-------------------------#
par(mar=c(1.5,9,1.5,2))


#SO THAT THE BARPLOTS ARE PLOTTED IN THE CORRECT ORDER RE-ORDER YOUR NAMES ARGUMENT (WHICH IS TRANSECT DISTANCE)- THIS IS ONLY AN ISSUE FOR BOBOKA

ordered_indices <- order(CANOPY_HEIGHT$Dist[which(CANOPY_HEIGHT$Site=="Ipombo")])
ordered_values <- CANOPY_HEIGHT$Canopy_Height[which(CANOPY_HEIGHT$Site=="Ipombo")][ordered_indices]  # Reorder values
ordered_names <- CANOPY_HEIGHT$Dist[which(CANOPY_HEIGHT$Site=="Ipombo")][ordered_indices]  # Reorder names
ordered_class<- CANOPY_HEIGHT$Class[which(CANOPY_HEIGHT$Site=="Ipombo")][ordered_indices]

group_colours <- c("SFMS" = "darkgreen", "HPSF" = "darkolivegreen4", "PPSF"="darkolivegreen3", "Savannah"="yellow" )

bp<-barplot(ordered_values, names.arg=ordered_names, col = group_colours[ordered_class], ylim=c(0,37), ylab="", yaxt="n",xaxt="n", cex.lab=2.3, cex.axis=2)
mtext(text="Canopy Height (m)", side=2, line=4, cex=2)
axis(2, at = seq(0, 30, by = 10), labels = c("0", "10", "20", "30"),cex.axis=2)

text(bp[c(1,3,5,7)], 33, labels=ordered_class[seq(1, length(ordered_class), 2)],cex=2)
text(bp[c(2,4,6)], 33, labels=ordered_class[seq(2, length(ordered_class), 2)],cex=2)

#-----------------------#
#SECOND PANEL-PEAT DEPTH#
#-----------------------#

ordered_indices_peat <- order(PEAT_DEPTH$Dist[which(PEAT_DEPTH$Site=="Ipombo")])
ordered_values_peat <- PEAT_DEPTH$Peat_Depth[which(PEAT_DEPTH$Site=="Ipombo")][ordered_indices]  # Reorder values
ordered_names_peat <- PEAT_DEPTH$Dist[which(PEAT_DEPTH$Site=="Ipombo")][ordered_indices]  # Reorder names



par(mar=c(1.5,9,1.5,2))
barplot(ordered_values_peat*-1, names.arg=ordered_names, col="brown", ylab="", xaxt="n", yaxt="n",ylim=c(-6,0), cex.lab=2.3, cex.axis=2)
axis(2, at = seq(-6, 0, by = 1), labels = c("6", "5", "4", "3", "2", "1", "0"),cex.axis=2)
mtext(text="Peat Depth (m)", side=2, line=4, cex=2)


#----------------------------------------------------#
#THIRD PANEL-NUMBER OF SPECIES PER PLOT I.E. RICHNESS#
#----------------------------------------------------#


par(mar=c(6,9,1.5,2))

Ipombo_no_NA <- RICHNESS[,"Ipombo"][!is.na(RICHNESS[,"Ipombo"])]

barplot(Ipombo_no_NA, names.arg=c("0","1","2","3","4","5","6"), ylab="", xlab="Distance Along Transect (km)", ylim=c(0,25), cex.lab=2.3, cex.axis=2, cex.names=2)
mtext(text="No. of Species", side=2, line=4, cex=2)





########################################################################################################################
########################################################################################################################
#**********************************************************************************************************************#
#=================================================== 5. LOBAKA ========================================================#
#**********************************************************************************************************************#
########################################################################################################################
########################################################################################################################

#SET UP THE PLOTTING WINDOW


#CREATE THE MATRIX FIRST
layout.matrix <- matrix(c(1,2,3), nrow = 3, ncol = 1)

#DEFINE HEIGHTS
layout(layout.matrix, heights = c(1,1,1))


#-------------------------#
#FIRST PANEL-CANOPY HEIGHT#
#-------------------------#
par(mar=c(1.5,9,1.5,2))


#SO THAT THE BARPLOTS ARE PLOTTED IN THE CORRECT ORDER RE-ORDER YOUR NAMES ARGUMENT (WHICH IS TRANSECT DISTANCE)- THIS IS ONLY AN ISSUE FOR BOBOKA

ordered_indices <- order(CANOPY_HEIGHT$Dist[which(CANOPY_HEIGHT$Site=="Lobaka")])
ordered_values <- CANOPY_HEIGHT$Canopy_Height[which(CANOPY_HEIGHT$Site=="Lobaka")][ordered_indices]  # Reorder values
ordered_names <- CANOPY_HEIGHT$Dist[which(CANOPY_HEIGHT$Site=="Lobaka")][ordered_indices]  # Reorder names
ordered_class<- CANOPY_HEIGHT$Class[which(CANOPY_HEIGHT$Site=="Lobaka")][ordered_indices]

group_colours <- c("SFMS" = "darkgreen", "HPSF" = "darkolivegreen4", "PPSF"="darkolivegreen3", "Savannah"="yellow" )

bp<-barplot(ordered_values, names.arg=ordered_names, col = group_colours[ordered_class], ylim=c(0,37), ylab="", yaxt="n",xaxt="n", cex.lab=2.3, cex.axis=2)
mtext(text="Canopy Height (m)", side=2, line=4, cex=2)
axis(2, at = seq(0, 30, by = 10), labels = c("0", "10", "20", "30"),cex.axis=2)

text(bp[c(1,3,5,7)], 33, labels=ordered_class[seq(1, length(ordered_class), 2)],cex=2)
text(bp[c(2,4,6)], 33, labels=ordered_class[seq(2, length(ordered_class), 2)],cex=2)

#-----------------------#
#SECOND PANEL-PEAT DEPTH#
#-----------------------#

ordered_indices_peat <- order(PEAT_DEPTH$Dist[which(PEAT_DEPTH$Site=="Lobaka")])
ordered_values_peat <- PEAT_DEPTH$Peat_Depth[which(PEAT_DEPTH$Site=="Lobaka")][ordered_indices]  # Reorder values
ordered_names_peat <- PEAT_DEPTH$Dist[which(PEAT_DEPTH$Site=="Lobaka")][ordered_indices]  # Reorder names



par(mar=c(1.5,9,1.5,2))
barplot(ordered_values_peat*-1, names.arg=ordered_names, col="brown", ylab="", xaxt="n", yaxt="n",ylim=c(-6,0), cex.lab=2.3, cex.axis=2)
axis(2, at = seq(-6, 0, by = 1), labels = c("6", "5", "4", "3", "2", "1", "0"),cex.axis=2)
mtext(text="Peat Depth (m)", side=2, line=4, cex=2)


#----------------------------------------------------#
#THIRD PANEL-NUMBER OF SPECIES PER PLOT I.E. RICHNESS#
#----------------------------------------------------#

par(mar=c(6,9,1.5,2))

Lobaka_no_NA <- RICHNESS[,"Lobaka"][!is.na(RICHNESS[,"Lobaka"])]

barplot(Lobaka_no_NA, names.arg=c("0","1","2","3","4","5","6"), ylab="", xlab="Distance Along Transect (km)", ylim=c(0,25), cex.lab=2.3, cex.axis=2, cex.names=2)
mtext(text="No. of Species", side=2, line=4, cex=2)





#######################################################################################################################
#######################################################################################################################
#*********************************************************************************************************************#
#=================================================== 6. MPEKA ========================================================#
#*********************************************************************************************************************#
#######################################################################################################################
#######################################################################################################################

#SET UP THE PLOTTING WINDOW


#CREATE THE MATRIX FIRST
layout.matrix <- matrix(c(1,2,3), nrow = 3, ncol = 1)

#DEFINE HEIGHTS
layout(layout.matrix, heights = c(1,1,1))



#-------------------------#
#FIRST PANEL-CANOPY HEIGHT#
#-------------------------#
par(mar=c(1.5,9,1.5,2))


#SO THAT THE BARPLOTS ARE PLOTTED IN THE CORRECT ORDER RE-ORDER YOUR NAMES ARGUMENT (WHICH IS TRANSECT DISTANCE)- THIS IS ONLY AN ISSUE FOR BOBOKA

ordered_indices <- order(CANOPY_HEIGHT$Dist[which(CANOPY_HEIGHT$Site=="Mpeka")])
ordered_values <- CANOPY_HEIGHT$Canopy_Height[which(CANOPY_HEIGHT$Site=="Mpeka")][ordered_indices]  # Reorder values
ordered_names <- CANOPY_HEIGHT$Dist[which(CANOPY_HEIGHT$Site=="Mpeka")][ordered_indices]  # Reorder names
ordered_class<- CANOPY_HEIGHT$Class[which(CANOPY_HEIGHT$Site=="Mpeka")][ordered_indices]

group_colours <- c("SFMS" = "darkgreen", "HPSF" = "darkolivegreen4", "PPSF"="darkolivegreen3", "Savannah"="yellow" )

bp<-barplot(ordered_values, names.arg=ordered_names, col = group_colours[ordered_class], ylim=c(0,37), ylab="", yaxt="n",xaxt="n", cex.lab=2.3, cex.axis=2)
mtext(text="Canopy Height (m)", side=2, line=4, cex=2)
axis(2, at = seq(0, 30, by = 10), labels = c("0", "10", "20", "30"),cex.axis=2)

text(bp[c(1,3,5,7,9,11)], 33, labels=ordered_class[seq(1, length(ordered_class), 2)],cex=2)
text(bp[c(2,4,6,8,10)], 33, labels=ordered_class[seq(2, length(ordered_class), 2)],cex=2)


#-----------------------#
#SECOND PANEL-PEAT DEPTH#
#-----------------------#

ordered_indices_peat <- order(PEAT_DEPTH$Dist[which(PEAT_DEPTH$Site=="Mpeka")])
ordered_values_peat <- PEAT_DEPTH$Peat_Depth[which(PEAT_DEPTH$Site=="Mpeka")][ordered_indices]  # Reorder values
ordered_names_peat <- PEAT_DEPTH$Dist[which(PEAT_DEPTH$Site=="Mpeka")][ordered_indices]  # Reorder names



par(mar=c(1.5,9,1.5,2))
barplot(ordered_values_peat*-1, names.arg=ordered_names, col="brown", ylab="", xaxt="n", yaxt="n",ylim=c(-6,0), cex.lab=2.3, cex.axis=2)
axis(2, at = seq(-6, 0, by = 1), labels = c("6", "5", "4", "3", "2", "1", "0"),cex.axis=2)
mtext(text="Peat Depth (m)", side=2, line=4, cex=2)


#----------------------------------------------------#
#THIRD PANEL-NUMBER OF SPECIES PER PLOT I.E. RICHNESS#
#----------------------------------------------------#


par(mar=c(6,9,1.5,2))

Mpeka_no_NA <- RICHNESS[,"Mpeka"][!is.na(RICHNESS[,"Mpeka"])]

barplot(Mpeka_no_NA, names.arg=c("0","1","2","3","4","5","6","7","8","9","10"), ylab="", xlab="Distance Along Transect (km)", ylim=c(0,25), cex.lab=2.3, cex.axis=2, cex.names=2)
mtext(text="No. of Species", side=2, line=4, cex=2)
