# Plant_Diversity

Data and code required for reproducing the figures featured in the following publication:

Kanyama et al. (2026). Plant diversity and composition of peatlands adjacent to the white- and blackwater rivers in the central Congo Basin. *Philosophical Transactions od the Royal Society B: Biological Sciences*, 381, DOI:10.1098/rstb.2024.0479

Please cite this paper if you're using any of the data or code presented here.

# Code


## Kanyama_et_al_2026_Fig2_and_Supp_Material_Figs_Script

This file contains the R code used to produce Figure 2 and Supplementary Material Figures 2 to 6.

## Kanyama_et_al_2026_Fig3_Right_Panel_Script

This file contains the R code used to produce the right panel in Figure 3.

## Kanyama_et_al_2026_Fig3_Left_Panel_Fig4_Script

This file contains the R code used to produce the left panel in Fugure 3 and Figure 4.

## Kanyama_et_al_2026_Fig5_Script

This file contains the R code used to produce Figure 5.

## Kanyama_et_al_2026_Fig6_and_Abundance_Analysis_Script

This file contains the R code used to produce Figure 6 and the abundance analysis.


# Data

## Kanayama_et_al_2026_main_data

This file contains the principal data used in Kanayama et al. 2026.

The columns are as follows:

**Date** This is the date of sampling.\
**Site** This is the name of the transect.\
**Transect** This is the transect code.\
**Dist.(m)**	This is the distance of the sampling point along the transect in meters.\
**Latitude**	This is the latitude, in decimal degrees, of the sampling point.\
**Longitude** This is the longitude, in decimal degrees, of the sampling point.\
**Alt.(m)**	This is the altitude of the sampling point in meters above sea level, according to the GPS unit.\
**Rivers (Sites)** This is the name of the river adjacent to the transect.\
**Method**	This refers to whether a full forest plot was carried out at the sampling location, or whether a more rapid assessment was made. In Kanyama et al. 2026, only the full forest plot data was used.\
**Microtopography**	This is a discription of the microtopography of the sampling location.\
**Dominant Species**	This is the dominant vegetation species at the sampling location.\
**Moyen Hauteur**	This is the mean height (in meters) of 5 trees measured at the sampling location.\
**Mapping Classification (Bart)**	This is the vegetation classification assigned to this sampling location for the peatland mapping work in Crezee et al. (2022). NB: Seasonally flooded forests were grouped with terre firme forests for the mapping work.\
**Species** This is the species of the individual tree or palm.\
**Genus**	This is the genus of the individual tree or palm.\		
**Family**	This is the family of the individual tree or palm.\	
**POM** This is the point of measurement of the stem diameter in meters from the ground surface.\
**DBH (cm)**	This is the diameter (in centimeters) at breast height of the individual tree or palm.\
**Height (m)**	This is height of the of the individual tree or palm in meters.\
**Number of Living Fronds**	This is the number of living fronds for Raphia laurentii individuals.\
**Stem Height (m)**	This is the height of the stem for Raphia sese individuals in meters.\
**Frond Height (m)**	This is the height of the tallest frond for Raphia laurentii and Raphia sese individuals in meters.\
**Nappe phreat. (cm)** This is the height of the water table at the point of sampling in centimeters. A positive number represents a water table above the ground surface, and a negative number represents a water table below the surface.\
**Peat depth(cm)**	This is the original peat depth measured in the field in centimeters measured by pushing poles into the ground until the underlying mineral layer was reached.\
**Final peat depth Crezee et al 2022 (LOI or corrected. m)** This is the corrected peat depth measurement in meters as used in the Crezee et al. (2022) paper. Loss on ignition was used to determine organic matter content of the peat cores, to give a true peat depth measurement, based on a peat definition of a minimum depth of 30 cm and a minimum organic matter content of 65%. The relationship between true peat depth determined by LOI and epat depth measured using poles was used to correct all pole peat depth measurements.


## Dist_to_river

This file contains the output from a QGIS analysis used to obtain the distance of each sampling point to the nearest river. This distance was used in the analysis shown in Figure 6 and this file is required in order for the Kanyama_et_al_2026_Fig6_and_Abundance_Analysis_Script to run correctly.

The columns are as follows:

**field_1**	This is a QGIS unique identifier for each sample point.\
**Site**	This is the transect name.\
**Dist**	This is the distance of the sampling point along the transect in meters.\
**Latitude**	This is the latitude, in decimal degrees, of the sampling point.\
**Longitude** This is the longitude, in decimal degrees, of the sampling point.\
**HubName**	 This is the unique identifier for a digitised section of river.\
**HubDist**  This is the distance in kilometers fof each sampling point to the nearest digitised river segment, as calculated using the "Distance to nearest hub" tool in QGIS.

