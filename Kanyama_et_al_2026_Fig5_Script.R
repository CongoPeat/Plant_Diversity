#=======================================#
#---------------------------------------#
#SCRIPT FOR FIG 5 IN KANYAMA ET AL. 2026#
#  WRITTEN BY D. HAWTHORNE, NOV. 2025   #
#---------------------------------------#
#=======================================#


library(labdsv)
library(vegan)
library(ellipse)
library(dplyr)
library(ggplot2)
library(ggrepel)

data<-read.csv(file="NMDS_Data_Transposed.csv")
veg<-data[,c(-1,-2,-3)]

nm <- metaMDS(veg)


nmds.scores<-as.data.frame(scores(nm)$sites)

veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100)
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}

nmds.scores <- nmds.scores %>%
  mutate(Site = as.factor(data$Site),
	Plot = as.factor(data$Plot),
        Type = as.factor(data$Type))


ellipse_df <- data.frame()


for(g in levels(nmds.scores$Type)){
  ellipse_df <- rbind(ellipse_df, cbind(as.data.frame(with(nmds.scores[nmds.scores$Type==g,],
                                                     veganCovEllipse(cov.wt(cbind(NMDS1,NMDS2),
                                                                            wt=rep(1/length(NMDS1),length(NMDS1)))$cov,
                                                                     center=c(mean(NMDS1),mean(NMDS2)))))
                                  ,Type=g))
}


nmds_species <- as.data.frame(scores(nm, display = "species"))
nmds_species$species <- rownames(nmds_species)


=====Indicator Values=======
library(labdsv)
my_p <- 0.05
Groups<-data[,1]
indicators <- indval(veg, clustering=Groups) # Note more restrictive p-value
summary(indicators, p=my_p)
veg_indic <- veg[,which(colnames(veg) %in% names(indicators$pval[indicators$pval<my_p]))]

sig_species <- names(indicators$pval[indicators$pval < my_p])
nmds_sig_species <- nmds_species[rownames(nmds_species) %in% sig_species, ]


====Plotting=======
NMDS_plot <- ggplot(data = nmds.scores, aes(NMDS1, NMDS2)) +
    geom_point(aes(color = Site, shape = Site)) + # adding different colours and shapes for points at different distances
    geom_text(label=data$Plot, position="dodge")+
geom_text_repel(
    data = nmds_sig_species,
    aes(label = make.cepnames(rownames(nmds_sig_species))),
    color = "red",
    size = 2
  )  +
    geom_path(data=ellipse_df, aes(x=NMDS1, y=NMDS2, colour=Type), linewidth=1) +     guides(color = guide_legend(override.aes = list(linetype=c(NA,NA,NA,NA,NA,NA,NA,NA,NA)))) + # removes lines from legend
    theme_bw() + # adding theme
    theme(panel.grid = element_blank()) + # remove background grid
    scale_color_manual(name = "Site", # legend title
                       labels = c("Mpeka","Bolengo","Bondamba","Boboka","Ipombo","Lobaka"), # adjusting legend labels
                      values = c("gold", "chartreuse3",
                                  "deepskyblue2", "salmon", "green2", "red", "purple", "pink", "blue")) + # customising colours
    scale_shape_manual("Site", # legend title
                       labels = c("Mpeka","Bolengo","Bondamba","Boboka","Ipombo","Lobaka"), # adjusting legend labels
                       values = c(21, 22, 23, 24, 25, 4)) # customising shapes


