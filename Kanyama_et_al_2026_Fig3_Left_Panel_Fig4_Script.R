#============================================================#
#------------------------------------------------------------#
#SCRIPT FOR FIG 3 LEFT PANEL AND FIG 4 IN KANYAMA ET AL. 2026#
#               WRITTEN BY G. DARGIE, NOV. 2025              #
#------------------------------------------------------------#
#============================================================#

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

#==============================#
#REMOVE RAPHIA FROM THE DATASET#
#==============================#

levels(as.factor(DB_PLOT$Species))

DB_PLOT<-DB_PLOT[which(DB_PLOT$Species!="Raphia laurentii De Wild." & DB_PLOT$Species!="Raphia sese De Wild."),]

#CHECK THIS WORKED
levels(as.factor(DB_PLOT$Species))


#===============================================================#
#GET DATA INTO GROUPS AND GET MINIMUM STEM NUMBER FOR EACH GROUP#
#===============================================================#

#YOU WANT TO COMPARE THE NUMBER OF STEMS FROM THE BLACK WATER AND WHITE WATER PLOTS


BLACKWATER_PLOTS<-DB_PLOT[which(DB_PLOT$Rivers..Sites.=="Ruki"),]

WHITEWATER_PLOTS<-DB_PLOT[which(DB_PLOT$Rivers..Sites.=="Congo"),]

length(BLACKWATER_PLOTS$Species) #655
length(WHITEWATER_PLOTS$Species) #1073




#THEN COMPARE THE NUMBER OF STEMS FOR HARDWOOD, PALM AND SF FOREST PLOTS


HARDWOOD_PLOTS<-DB_PLOT[which(DB_PLOT$Mapping.Classification..Bart.=="Hardwood swamp"),]

PALM_PLOTS<-DB_PLOT[which(DB_PLOT$Mapping.Classification..Bart.=="Palm-dominated swamp"),]

SF_PLOTS<-DB_PLOT[which(DB_PLOT$Mapping.Classification..Bart.=="Terra firme"),]

length(HARDWOOD_PLOTS$Species) #866
length(PALM_PLOTS$Species) #462
length(SF_PLOTS$Species) #400





#THEN COMPARE THE TOTAL NUMBER OF STEMS IN HARDWOOD, PALM AND SEASONALLY FLOODED PLOTS ON THE WHITE WATER AND BLACK WATER RIVERS

BLACKWATER_HARDWOOD_PLOTS<-DB_PLOT[which(DB_PLOT$Rivers..Sites.=="Ruki" & DB_PLOT$Mapping.Classification..Bart.=="Hardwood swamp"),]

BLACKWATER_PALM_PLOTS<-DB_PLOT[which(DB_PLOT$Rivers..Sites.=="Ruki" & DB_PLOT$Mapping.Classification..Bart.=="Palm-dominated swamp"),]

BLACKWATER_SF_PLOTS<-DB_PLOT[which(DB_PLOT$Rivers..Sites.=="Ruki" & DB_PLOT$Mapping.Classification..Bart.=="Terra firme"),]


WHITEWATER_HARDWOOD_PLOTS<-DB_PLOT[which(DB_PLOT$Rivers..Sites.=="Congo" & DB_PLOT$Mapping.Classification..Bart.=="Hardwood swamp"),]

WHITEWATER_PALM_PLOTS<-DB_PLOT[which(DB_PLOT$Rivers..Sites.=="Congo" & DB_PLOT$Mapping.Classification..Bart.=="Palm-dominated swamp"),]

WHITEWATER_SF_PLOTS<-DB_PLOT[which(DB_PLOT$Rivers..Sites.=="Congo" & DB_PLOT$Mapping.Classification..Bart.=="Terra firme"),]


length(BLACKWATER_HARDWOOD_PLOTS$Species) #308
length(BLACKWATER_PALM_PLOTS$Species) #194
length(BLACKWATER_SF_PLOTS$Species) #153
length(WHITEWATER_HARDWOOD_PLOTS$Species) #558
length(WHITEWATER_PALM_PLOTS$Species) #268
length(WHITEWATER_SF_PLOTS$Species) #247


#=========================#
#SAMPLING WITH REPLACEMENT#
#=========================#

#--------------------#
#--------------------#
#BLACK VS WHITE WATER#
#--------------------#
#--------------------#


#----------------------#
#START WITH BLACK WATER#
#----------------------#

DF_BLACKWATER_RESAMPLED<-c()									#CREATE EMPTY VECTOR, WHICH AS YOU LOOP THROUGH 1000 TIMES BECOMES A DATAFRAME, AS YOU KEEP APPENDING EACH OF THE 1000 RUNS


for (j in 1:1000){										#START THE 1000 LOOPS
  
  
  RESAMPLED_BLACKWATER_PLOTS <- lapply(1:655, function(size) {			#USE LAPPLY TO RANDOMLY RESAMPLE WITH REPLACEMENT THE SPECIES DATA GOING FROM 1 STEM UP TO 655 STEMS
    sample(BLACKWATER_PLOTS$Species, size = size, replace = TRUE)			
  })
  
  
  y <- c()												#CREATE AN EMPTY VECTOR y, WHICH WILL CONTAIN THE NUMBER OF SPECIES IN THE 1 TO 655 RANDOMLY RESAMPLED STEMS. SO y HAS A LENGTH OF 655
  
  for (i in 1:655) {										#WE WILL USE A LOOP INSIDE THE LOOP TO RETURN THE NUMBER OF SPECIES IN THE 1 TO 655 RANDOMLY RESAMPLED STEMS
    x<-(length(levels(as.factor(RESAMPLED_BLACKWATER_PLOTS[[i]]))))
    y<-c(y,x)
  }
  
  DF_BLACKWATER_RESAMPLED<-cbind(DF_BLACKWATER_RESAMPLED, y)				#THEN ADD y TO THE "DF_BLACKWATER_RESAMPLED" DATAFRAME WHICH WILL END UP WITH 1000 COLUMNS OF LENGTH 655, AND CONTAINS THE NUMBER OF SPECIES 
  #FROM THE RANDOMLY SAMPLED STEMS. ROW 1 REPRESENT THE RANDOM SAMPLING WHEN THERE IS 1 STEM, 655 REPRESENTS THE RANDOM SAMPLING WITH 655 STEMS
}													#END LOOP




BW_LOWER_95CI <- apply(DF_BLACKWATER_RESAMPLED, 1, function(x) sort(x)[25])
BW_UPPER_95CI <- apply(DF_BLACKWATER_RESAMPLED, 1, function(x) sort(x)[975])



#-------------------------#
#NOW REPEAT FOR WHITEWATER#
#-------------------------#


DF_WHITEWATER_RESAMPLED<-c()									#CREATE EMPTY VECTOR, WHICH AS YOU LOOP THROUGH 1000 TIMES BECOMES A DATAFRAME, AS YOU KEEP APPENDING EACH OF THE 1000 RUNS


for (j in 1:1000){										#START THE 1000 LOOPS
  
  
  RESAMPLED_WHITEWATER_PLOTS <- lapply(1:655, function(size) {			#USE LAPPLY TO RANDOMLY RESAMPLE THE SPECIES DATA GOING FROM 1 STEM UP TO 655 STEMS
    sample(WHITEWATER_PLOTS$Species, size = size, replace = TRUE)			
  })
  
  
  y <- c()												#CREATE AN EMPTY VECTOR y, WHICH WILL CONTAIN THE NUMBER OF SPECIES IN THE 1 TO 655 RANDOMLY RESAMPLED STEMS. SO y HAS A LENGTH OF 655
  
  for (i in 1:655) {										#WE WILL USE A LOOP INSIDE THE LOOP TO RETURN THE NUMBER OF SPECIES IN THE 1 TO 655 RANDOMLY RESAMPLED STEMS
    x<-(length(levels(as.factor(RESAMPLED_WHITEWATER_PLOTS[[i]]))))
    y<-c(y,x)
  }
  
  DF_WHITEWATER_RESAMPLED<-cbind(DF_WHITEWATER_RESAMPLED, y)				#THEN ADD y TO THE "DF_WHITEWATER_RESAMPLED" DATAFRAME WHICH WILL END UP WITH 1000 COLUMNS OF LENGTH 655, AND CONTAINS THE NUMBER OF SPECIES 
  #FROM THE RANDOMLY SAMPLED STEMS. ROW 1 REPRESENT THE RANDOM SAMPLING WHEN THERE IS 1 STEM, 655 REPRESENTS THE RANDOM SAMPLING WITH 655 STEMS
}




WW_LOWER_95CI <- apply(DF_WHITEWATER_RESAMPLED, 1, function(x) sort(x)[25])
WW_UPPER_95CI <- apply(DF_WHITEWATER_RESAMPLED, 1, function(x) sort(x)[975])


#---------------#
#CHECK IT WORKED#
#---------------#

#NOW YOU NEED TO GET THE MEAN NUMBER OF SPECIES PER STEM NUMBER FOR EACH DATAFRAME

BLACKWATER_RESAMPLED_MEAN_NO_SPECIES<-rowMeans(DF_BLACKWATER_RESAMPLED)

WHITEWATER_RESAMPLED_MEAN_NO_SPECIES<-rowMeans(DF_WHITEWATER_RESAMPLED)

head(BLACKWATER_RESAMPLED_MEAN_NO_SPECIES)
head(WHITEWATER_RESAMPLED_MEAN_NO_SPECIES)


head(DF_WHITEWATER_RESAMPLED)
head(DF_BLACKWATER_RESAMPLED)

#------------------------#
#FIGURE 3 LEFT PANEL PLOT#
#------------------------#

par(mar=c(6,6,1,3))

plot(x = 1,                 
     xlab = "", 
     ylab = "",
     xlim=c(0,655), 
     ylim=c(0,100),
     yaxt="n",
     xaxt="n",
     frame.plot=F,
     type = "n")

STEM<-c(1:655)
polygon(c(STEM, rev(STEM)), c(BW_UPPER_95CI, rev(BW_LOWER_95CI)), col = rgb(0, 0, 0, 0.2), border = NA)  
polygon(c(STEM, rev(STEM)), c(WW_UPPER_95CI, rev(WW_LOWER_95CI)), col = rgb(9/255,158/255,115/255, 0.2), border = NA)  

par(new=TRUE)
par(mar=c(6,6,1,3))
plot(BLACKWATER_RESAMPLED_MEAN_NO_SPECIES, typ="l", col="black", xlab="",ylab="", xlim=c(0,655), ylim=c(0,100), lwd=2,cex.lab=2.5, cex.axis=1.5)
mtext("Number of Stems", side=1, line=4, cex=2.5)
par(new=TRUE)
par(mar=c(6,6,1,3))
plot(WHITEWATER_RESAMPLED_MEAN_NO_SPECIES, typ="l", col=rgb(9/255,158/255,115/255), xlab="",ylab="", xlim=c(0,655), ylim=c(0,100),lwd=2, cex.lab=2.5, cex.axis=1.5)
mtext("Number of Species", side=2, line=4, cex=2.5)

legend(220,40, legend = c("Blackwater mean", "Blackwater 95th CI","Whitewater mean", "Whitewater 95th CI"), 
       col = c("black", rgb(0, 0, 0, 0.2),rgb(9/255,158/255,115/255), rgb(9/255,158/255,115/255,0.2)), lty = c(1, 1,1,1), lwd= c(2, 4,2,4), cex=1.5, bty="n")



#----------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------#
#BLACKWATER HARDWOOD, PALM, SEASONALLY FLOODED VS WHITEWATER HARDWOOD, PALM, SEASONALLY FLOODED#
#----------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------#


#-------------------------------#
#START WITH WHITE WATER HARDWOOD#
#-------------------------------#



DF_WHITEWATER_HARDWOOD_RESAMPLED<-c()							#CREATE EMPTY VECTOR, WHICH AS YOU LOOP THROUGH 1000 TIMES BECOMES A DATAFRAME, AS YOU KEEP APPENDING EACH OF THE 1000 RUNS


for (j in 1:1000){										#START THE 1000 LOOPS
  
  
  RESAMPLED_WHITEWATER_HARDWOOD_PLOTS <- lapply(1:153, function(size) {		#USE LAPPLY TO RANDOMLY RESAMPLE WITH REPLACEMENT THE SPECIES DATA GOING FROM 1 STEM UP TO 153 STEMS
    sample(WHITEWATER_HARDWOOD_PLOTS$Species, size = size, replace = TRUE)			
  })
  
  
  y <- c()												#CREATE AN EMPTY VECTOR y, WHICH WILL CONTAIN THE NUMBER OF SPECIES IN THE 1 TO 153 RANDOMLY RESAMPLED STEMS. SO y HAS A LENGTH OF 153 
  
  for (i in 1:153) {										#WE WILL USE A LOOP INSIDE THE LOOP TO RETURN THE NUMBER OF SPECIES IN THE 1 TO 153 RANDOMLY RESAMPLED STEMS
    x<-(length(levels(as.factor(RESAMPLED_WHITEWATER_HARDWOOD_PLOTS[[i]]))))
    y<-c(y,x)
  }
  
  DF_WHITEWATER_HARDWOOD_RESAMPLED<-cbind(DF_WHITEWATER_HARDWOOD_RESAMPLED, y)	#THEN ADD y TO THE "DF_BLACKWATER_RESAMPLED" DATAFRAME WHICH WILL END UP WITH 1000 COLUMNS OF LENGTH 153 , AND CONTAINS THE NUMBER OF SPECIES 
  #FROM THE RANDOMLY SAMPLED STEMS. ROW 1 REPRESENT THE RANDOM SAMPLING WHEN THERE IS 1 STEM, 655 REPRESENTS THE RANDOM SAMPLING WITH 153 STEMS
}													#END LOOP


WW_HW_LOWER_95CI <- apply(DF_WHITEWATER_HARDWOOD_RESAMPLED, 1, function(x) sort(x)[25])
WW_HW_UPPER_95CI <- apply(DF_WHITEWATER_HARDWOOD_RESAMPLED, 1, function(x) sort(x)[975])


#---------------------#
#THEN WHITE WATER PALM#
#---------------------#



DF_WHITEWATER_PALM_RESAMPLED<-c()								#CREATE EMPTY VECTOR, WHICH AS YOU LOOP THROUGH 1000 TIMES BECOMES A DATAFRAME, AS YOU KEEP APPENDING EACH OF THE 1000 RUNS


for (j in 1:1000){										#START THE 1000 LOOPS
  
  
  RESAMPLED_WHITEWATER_PALM_PLOTS <- lapply(1:153, function(size) {			#USE LAPPLY TO RANDOMLY RESAMPLE WITH REPLACEMENT THE SPECIES DATA GOING FROM 1 STEM UP TO 153 STEMS
    sample(WHITEWATER_PALM_PLOTS$Species, size = size, replace = TRUE)			
  })
  
  
  y <- c()												#CREATE AN EMPTY VECTOR y, WHICH WILL CONTAIN THE NUMBER OF SPECIES IN THE 1 TO 153 RANDOMLY RESAMPLED STEMS. SO y HAS A LENGTH OF 153 
  
  for (i in 1:153) {										#WE WILL USE A LOOP INSIDE THE LOOP TO RETURN THE NUMBER OF SPECIES IN THE 1 TO 153 RANDOMLY RESAMPLED STEMS
    x<-(length(levels(as.factor(RESAMPLED_WHITEWATER_PALM_PLOTS[[i]]))))
    y<-c(y,x)
  }
  
  DF_WHITEWATER_PALM_RESAMPLED<-cbind(DF_WHITEWATER_PALM_RESAMPLED, y)		#THEN ADD y TO THE "DF_BLACKWATER_RESAMPLED" DATAFRAME WHICH WILL END UP WITH 1000 COLUMNS OF LENGTH 153 , AND CONTAINS THE NUMBER OF SPECIES 
  #FROM THE RANDOMLY SAMPLED STEMS. ROW 1 REPRESENT THE RANDOM SAMPLING WHEN THERE IS 1 STEM, 153 REPRESENTS THE RANDOM SAMPLING WITH 153 STEMS
}													#END LOOP


WW_PALM_LOWER_95CI <- apply(DF_WHITEWATER_PALM_RESAMPLED, 1, function(x) sort(x)[25])
WW_PALM_UPPER_95CI <- apply(DF_WHITEWATER_PALM_RESAMPLED, 1, function(x) sort(x)[975])


#-------------------#
#THEN WHITE WATER SF#
#-------------------#



DF_WHITEWATER_SF_RESAMPLED<-c()								#CREATE EMPTY VECTOR, WHICH AS YOU LOOP THROUGH 1000 TIMES BECOMES A DATAFRAME, AS YOU KEEP APPENDING EACH OF THE 1000 RUNS


for (j in 1:1000){										#START THE 1000 LOOPS
  
  
  RESAMPLED_WHITEWATER_SF_PLOTS <- lapply(1:153, function(size) {			#USE LAPPLY TO RANDOMLY RESAMPLE WITH REPLACEMENT THE SPECIES DATA GOING FROM 1 STEM UP TO 153 STEMS
    sample(WHITEWATER_SF_PLOTS$Species, size = size, replace = TRUE)			
  })
  
  
  y <- c()												#CREATE AN EMPTY VECTOR y, WHICH WILL CONTAIN THE NUMBER OF SPECIES IN THE 1 TO 153 RANDOMLY RESAMPLED STEMS. SO y HAS A LENGTH OF 153 
  
  for (i in 1:153) {										#WE WILL USE A LOOP INSIDE THE LOOP TO RETURN THE NUMBER OF SPECIES IN THE 1 TO 153 RANDOMLY RESAMPLED STEMS
    x<-(length(levels(as.factor(RESAMPLED_WHITEWATER_SF_PLOTS[[i]]))))
    y<-c(y,x)
  }
  
  DF_WHITEWATER_SF_RESAMPLED<-cbind(DF_WHITEWATER_SF_RESAMPLED, y)			#THEN ADD y TO THE "DF_BLACKWATER_RESAMPLED" DATAFRAME WHICH WILL END UP WITH 1000 COLUMNS OF LENGTH 153 , AND CONTAINS THE NUMBER OF SPECIES 
  #FROM THE RANDOMLY SAMPLED STEMS. ROW 1 REPRESENT THE RANDOM SAMPLING WHEN THERE IS 1 STEM, 153 REPRESENTS THE RANDOM SAMPLING WITH 153 STEMS
}													#END LOOP



WW_SF_LOWER_95CI <- apply(DF_WHITEWATER_SF_RESAMPLED, 1, function(x) sort(x)[25])
WW_SF_UPPER_95CI <- apply(DF_WHITEWATER_SF_RESAMPLED, 1, function(x) sort(x)[975])

#-------------------------------#
#MOVE ON TO BLACK WATER HARDWOOD#
#-------------------------------#



DF_BLACKWATER_HARDWOOD_RESAMPLED<-c()							#CREATE EMPTY VECTOR, WHICH AS YOU LOOP THROUGH 1000 TIMES BECOMES A DATAFRAME, AS YOU KEEP APPENDING EACH OF THE 1000 RUNS


for (j in 1:1000){										#START THE 1000 LOOPS
  
  
  RESAMPLED_BLACKWATER_HARDWOOD_PLOTS <- lapply(1:153, function(size) {		#USE LAPPLY TO RANDOMLY RESAMPLE WITH REPLACEMENT THE SPECIES DATA GOING FROM 1 STEM UP TO 153 STEMS
    sample(BLACKWATER_HARDWOOD_PLOTS$Species, size = size, replace = TRUE)			
  })
  
  
  y <- c()												#CREATE AN EMPTY VECTOR y, WHICH WILL CONTAIN THE NUMBER OF SPECIES IN THE 1 TO 153 RANDOMLY RESAMPLED STEMS. SO y HAS A LENGTH OF 153 
  
  for (i in 1:153) {										#WE WILL USE A LOOP INSIDE THE LOOP TO RETURN THE NUMBER OF SPECIES IN THE 1 TO 153 RANDOMLY RESAMPLED STEMS
    x<-(length(levels(as.factor(RESAMPLED_BLACKWATER_HARDWOOD_PLOTS[[i]]))))
    y<-c(y,x)
  }
  
  DF_BLACKWATER_HARDWOOD_RESAMPLED<-cbind(DF_BLACKWATER_HARDWOOD_RESAMPLED, y)	#THEN ADD y TO THE "DF_BLACKWATER_RESAMPLED" DATAFRAME WHICH WILL END UP WITH 1000 COLUMNS OF LENGTH 655, AND CONTAINS THE NUMBER OF SPECIES 
  #FROM THE RANDOMLY SAMPLED STEMS. ROW 1 REPRESENT THE RANDOM SAMPLING WHEN THERE IS 1 STEM, 153 REPRESENTS THE RANDOM SAMPLING WITH 153 STEMS
}													#END LOOP




BW_HW_LOWER_95CI <- apply(DF_BLACKWATER_HARDWOOD_RESAMPLED, 1, function(x) sort(x)[25])
BW_HW_UPPER_95CI <- apply(DF_BLACKWATER_HARDWOOD_RESAMPLED, 1, function(x) sort(x)[975])

#---------------------#
#THEN BLACK WATER PALM#
#---------------------#



DF_BLACKWATER_PALM_RESAMPLED<-c()								#CREATE EMPTY VECTOR, WHICH AS YOU LOOP THROUGH 1000 TIMES BECOMES A DATAFRAME, AS YOU KEEP APPENDING EACH OF THE 1000 RUNS


for (j in 1:1000){										#START THE 1000 LOOPS
  
  
  RESAMPLED_BLACKWATER_PALM_PLOTS <- lapply(1:153, function(size) {			#USE LAPPLY TO RANDOMLY RESAMPLE WITH REPLACEMENT THE SPECIES DATA GOING FROM 1 STEM UP TO 153 STEMS
    sample(BLACKWATER_PALM_PLOTS$Species, size = size, replace = TRUE)			
  })
  
  
  y <- c()												#CREATE AN EMPTY VECTOR y, WHICH WILL CONTAIN THE NUMBER OF SPECIES IN THE 1 TO 153 RANDOMLY RESAMPLED STEMS. SO y HAS A LENGTH OF 153 
  
  for (i in 1:153) {										#WE WILL USE A LOOP INSIDE THE LOOP TO RETURN THE NUMBER OF SPECIES IN THE 1 TO 153 RANDOMLY RESAMPLED STEMS
    x<-(length(levels(as.factor(RESAMPLED_BLACKWATER_PALM_PLOTS[[i]]))))
    y<-c(y,x)
  }
  
  DF_BLACKWATER_PALM_RESAMPLED<-cbind(DF_BLACKWATER_PALM_RESAMPLED, y)		#THEN ADD y TO THE "DF_BLACKWATER_RESAMPLED" DATAFRAME WHICH WILL END UP WITH 1000 COLUMNS OF LENGTH 153 , AND CONTAINS THE NUMBER OF SPECIES 
  #FROM THE RANDOMLY SAMPLED STEMS. ROW 1 REPRESENT THE RANDOM SAMPLING WHEN THERE IS 1 STEM, 153 REPRESENTS THE RANDOM SAMPLING WITH 153 STEMS
}													#END LOOP



BW_PALM_LOWER_95CI <- apply(DF_BLACKWATER_PALM_RESAMPLED, 1, function(x) sort(x)[25])
BW_PALM_UPPER_95CI <- apply(DF_BLACKWATER_PALM_RESAMPLED, 1, function(x) sort(x)[975])


#----------------------#
#FINALLY BLACK WATER SF#
#----------------------#



DF_BLACKWATER_SF_RESAMPLED<-c()								#CREATE EMPTY VECTOR, WHICH AS YOU LOOP THROUGH 1000 TIMES BECOMES A DATAFRAME, AS YOU KEEP APPENDING EACH OF THE 1000 RUNS


for (j in 1:1000){										#START THE 1000 LOOPS
  
  
  RESAMPLED_BLACKWATER_SF_PLOTS <- lapply(1:153, function(size) {			#USE LAPPLY TO RANDOMLY RESAMPLE WITH REPLACEMENT THE SPECIES DATA GOING FROM 1 STEM UP TO 153 STEMS
    sample(BLACKWATER_SF_PLOTS$Species, size = size, replace = TRUE)			
  })
  
  
  y <- c()												#CREATE AN EMPTY VECTOR y, WHICH WILL CONTAIN THE NUMBER OF SPECIES IN THE 1 TO 153 RANDOMLY RESAMPLED STEMS. SO y HAS A LENGTH OF 153 
  
  for (i in 1:153) {										#WE WILL USE A LOOP INSIDE THE LOOP TO RETURN THE NUMBER OF SPECIES IN THE 1 TO 153 RANDOMLY RESAMPLED STEMS
    x<-(length(levels(as.factor(RESAMPLED_BLACKWATER_SF_PLOTS[[i]]))))
    y<-c(y,x)
  }
  
  DF_BLACKWATER_SF_RESAMPLED<-cbind(DF_BLACKWATER_SF_RESAMPLED, y)			#THEN ADD y TO THE "DF_BLACKWATER_RESAMPLED" DATAFRAME WHICH WILL END UP WITH 1000 COLUMNS OF LENGTH 153 , AND CONTAINS THE NUMBER OF SPECIES 
  #FROM THE RANDOMLY SAMPLED STEMS. ROW 1 REPRESENT THE RANDOM SAMPLING WHEN THERE IS 1 STEM, 153 REPRESENTS THE RANDOM SAMPLING WITH 153 STEMS
}													#END LOOP



BW_SF_LOWER_95CI <- apply(DF_BLACKWATER_SF_RESAMPLED, 1, function(x) sort(x)[25])
BW_SF_UPPER_95CI <- apply(DF_BLACKWATER_SF_RESAMPLED, 1, function(x) sort(x)[975])

#---------------#
#CHECK IT WORKED#
#---------------#

BLACKWATER_HARDWOOD_RESAMPLED_MEAN_NO_SPECIES<-rowMeans(DF_BLACKWATER_HARDWOOD_RESAMPLED)

BLACKWATER_PALM_RESAMPLED_MEAN_NO_SPECIES<-rowMeans(DF_BLACKWATER_PALM_RESAMPLED)

BLACKWATER_SF_RESAMPLED_MEAN_NO_SPECIES<-rowMeans(DF_BLACKWATER_SF_RESAMPLED)



WHITEWATER_HARDWOOD_RESAMPLED_MEAN_NO_SPECIES<-rowMeans(DF_WHITEWATER_HARDWOOD_RESAMPLED)

WHITEWATER_PALM_RESAMPLED_MEAN_NO_SPECIES<-rowMeans(DF_WHITEWATER_PALM_RESAMPLED)

WHITEWATER_SF_RESAMPLED_MEAN_NO_SPECIES<-rowMeans(DF_WHITEWATER_SF_RESAMPLED)


head(BLACKWATER_HARDWOOD_RESAMPLED_MEAN_NO_SPECIES)
head(BLACKWATER_PALM_RESAMPLED_MEAN_NO_SPECIES)
head(BLACKWATER_SF_RESAMPLED_MEAN_NO_SPECIES)

head(WHITEWATER_HARDWOOD_RESAMPLED_MEAN_NO_SPECIES)
head(WHITEWATER_PALM_RESAMPLED_MEAN_NO_SPECIES)
head(WHITEWATER_SF_RESAMPLED_MEAN_NO_SPECIES)



head(DF_BLACKWATER_HARDWOOD_RESAMPLED)
head(DF_BLACKWATER_PALM_RESAMPLED)
head(DF_BLACKWATER_SF_RESAMPLED)

head(DF_WHITEWATER_HARDWOOD_RESAMPLED)
head(DF_WHITEWATER_PALM_RESAMPLED)
head(DF_WHITEWATER_SF_RESAMPLED)



#-------------#
#FIGURE 4 PLOT#
#-------------#

#SET UP THE PLOTTING WINDOW


#CREATE THE MATRIX FIRST
layout.matrix<-matrix(c(1,2,3,4), nrow = 1, ncol = 4)
layout(mat = layout.matrix,
       heights = c(5,5,5,5), # Heights of the rows
       widths = c(5,5,5,5)) # Widths of the columns


#FIRST PLOT- HARDWOOD

par(mar=c(7,7,7,1))

plot(x = 1,                 
     xlab = "", 
     ylab = "",
     xlim=c(0,153), 
     ylim=c(0,60),
     yaxt="n",
     xaxt="n",
     frame.plot=F,
     type = "n")



STEM<-c(1:153)
polygon(c(STEM, rev(STEM)), c(BW_HW_UPPER_95CI, rev(BW_HW_LOWER_95CI)), col = rgb(0, 0, 0, 0.2), border = NA)  
polygon(c(STEM, rev(STEM)), c(WW_HW_UPPER_95CI, rev(WW_HW_LOWER_95CI)), col = rgb(9/255,158/255,115/255, 0.2), border = NA)



par(new=TRUE)
plot(BLACKWATER_HARDWOOD_RESAMPLED_MEAN_NO_SPECIES, typ="l", col="black", lwd=2, xlab="",ylab="", main="Hardwood-dominated
peat swamp forest",cex.main=3,cex.lab=3,cex.axis=2, xlim=c(0,153), ylim=c(0,60))
par(new=TRUE)
plot(WHITEWATER_HARDWOOD_RESAMPLED_MEAN_NO_SPECIES, typ="l", col=rgb(9/255,158/255,115/255), lwd=2,xlab="",ylab="", xlim=c(0,153), ylim=c(0,60),cex.lab=3,cex.axis=2)


mtext("Number of Stems", cex=2, side=1, line=4)
mtext("Number of Species", cex=2, side=2, line=4)


#SECOND PLOT- PALM

par(mar=c(7,7,7,1))

plot(x = 1,                 
     xlab = "", 
     ylab = "",
     xlim=c(0,153), 
     ylim=c(0,60),
     yaxt="n",
     xaxt="n",
     frame.plot=F,
     type = "n")



STEM<-c(1:153)
polygon(c(STEM, rev(STEM)), c(BW_PALM_UPPER_95CI, rev(BW_PALM_LOWER_95CI)), col = rgb(0, 0, 0, 0.2), border = NA)  
polygon(c(STEM, rev(STEM)), c(WW_PALM_UPPER_95CI, rev(WW_PALM_LOWER_95CI)), col = rgb(9/255,158/255,115/255, 0.2), border = NA)


par(new=TRUE)
plot(BLACKWATER_PALM_RESAMPLED_MEAN_NO_SPECIES, typ="l", col="black", lwd=2,xlab="",ylab="", main="Palm-dominated
peat swamp forest",cex.main=3,cex.lab=3,cex.axis=2,xlim=c(0,153), ylim=c(0,60))
par(new=TRUE)
plot(WHITEWATER_PALM_RESAMPLED_MEAN_NO_SPECIES, typ="l", col=rgb(9/255,158/255,115/255), lwd=2,xlab="",ylab="", xlim=c(0,153), ylim=c(0,60),cex.lab=3,cex.axis=2,)


mtext("Number of Stems", cex=2, side=1, line=4)
mtext("Number of Species", cex=2, side=2, line=4)


#THIRD PLOT- SF

par(mar=c(7,7,7,1))

plot(x = 1,                 
     xlab = "", 
     ylab = "",
     xlim=c(0,153), 
     ylim=c(0,60),
     yaxt="n",
     xaxt="n",
     frame.plot=F,
     type = "n")


STEM<-c(1:153)
polygon(c(STEM, rev(STEM)), c(BW_SF_UPPER_95CI, rev(BW_SF_LOWER_95CI)), col = rgb(0, 0, 0, 0.2), border = NA)  
polygon(c(STEM, rev(STEM)), c(WW_SF_UPPER_95CI, rev(WW_SF_LOWER_95CI)), col = rgb(9/255,158/255,115/255, 0.2), border = NA)


par(new=TRUE)
plot(BLACKWATER_SF_RESAMPLED_MEAN_NO_SPECIES, typ="l", col="black", lwd=2,xlab="",ylab="", cex.lab=1.5, main="Swamp forest
on mineral soil",cex.main=3,cex.lab=3,cex.axis=2,xlim=c(0,153), ylim=c(0,60))
par(new=TRUE)
plot(WHITEWATER_SF_RESAMPLED_MEAN_NO_SPECIES, typ="l", col=rgb(9/255,158/255,115/255), lwd=2,xlab="",ylab="", xlim=c(0,153), ylim=c(0,60),cex.lab=3,cex.axis=2)


mtext("Number of Stems", cex=2, side=1, line=4)
mtext("Number of Species", cex=2, side=2, line=4)


par(mar=c(0,0,0,0))

plot(x = 1,                 
     xlab = "", 
     ylab = "",
     xlim = c(0, 10), 
     ylim = c(0, 10),
     yaxt="n",
     xaxt="n",
     frame.plot=F,
     type = "n")



legend(0,10, legend = c("Blackwater mean", "Blackwater 95th CI", "Whitewater mean", "Whitewater 95th CI"),
       col = c("black", rgb(0,0,0,0.2), rgb(9/255,119/255,115/255), rgb(9/255,119/255,115/255, 0.2)), 
       lty = c(1,1,1, 1), lwd=c(2,4,2,4), cex=3, bty="n")


