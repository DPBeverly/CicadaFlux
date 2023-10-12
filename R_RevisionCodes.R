##Quick richness test for the sites


d_ourTrees <- read.csv("D:/Dropbox/Projects/Indiana/Data/CicadaFlux/SupplementalTable.csv")
d_AvgTrees <- read.csv("D:/Dropbox/Projects/Indiana/Data/CicadaFlux/CicadaFluxPlotDataMMSF_Revisions.csv")
d_AvgTreesGW <- read.csv("D:/Dropbox/Projects/Indiana/Data/CicadaFlux/CicadaFluxPlotDataGW_Revisions.csv")

d_AvgTreesMMSF <- read.csv("D:/Dropbox/Projects/Indiana/Data/CicadaFlux/MMSF_PhenoTransectCoords.csv")
d_AvgTreesGW <- read.csv("D:/Dropbox/Projects/Indiana/Data/CicadaFlux/Griffy_PhenoTransectsCoords.csv")




length(unique(d_ourTrees$Species[d_ourTrees$Site=="MorganMonroe"]))
length(unique(d_AvgTreesMMSF$Species))
length(unique(d_ourTrees$Species[d_ourTrees$Site=="GW"]))
length(unique(d_AvgTreesGW$Species))


length(unique(d_AvgTreesMMSF$Species))/ sqrt(length(d_AvgTreesMMSF$Species))
length(unique(d_ourTrees$Species[d_ourTrees$Site=="MorganMonroe"]))/ sqrt(length(d_ourTrees$Species[d_ourTrees$Site=="MorganMonroe"]))


length(unique(d_AvgTreesGW$Species))/ sqrt(length(d_AvgTreesGW$Species))
length(unique(d_ourTrees$Species[d_ourTrees$Site=="GW"]))/ sqrt(length(d_ourTrees$Species[d_ourTrees$Site=="GW"]))


length(unique(d_AvgTreesGW$Species))


d_GWPheno = d_AvgTreesGW %>%
  #  dplyr::mutate(TIMESTAMP_agg = floor_date(TIMESTAMP_ct, unit = "day"), ) %>%
  dplyr::group_by(Species) %>%
  dplyr::summarise(Counts = n()) 

d_MMSFPheno = d_AvgTreesMMSF %>%
  #  dplyr::mutate(TIMESTAMP_agg = floor_date(TIMESTAMP_ct, unit = "day"), ) %>%
  dplyr::group_by(Species.ID) %>%
  dplyr::summarise(Counts = n()) 

d_MMSFTrees = d_ourTrees[d_ourTrees$Site=="MorganMonroe",] %>%
  #  dplyr::mutate(TIMESTAMP_agg = floor_date(TIMESTAMP_ct, unit = "day"), ) %>%
  dplyr::group_by(Species) %>%
  dplyr::summarise(Counts = n()) 

d_GWTrees = d_ourTrees[d_ourTrees$Site=="GW",] %>%
  #  dplyr::mutate(TIMESTAMP_agg = floor_date(TIMESTAMP_ct, unit = "day"), ) %>%
  dplyr::group_by(Species) %>%
  dplyr::summarise(Counts = n()) 



d_MMSFPheno$Overlap <- c("Yes", "Yes", "No", "No","No", "Yes", "Yes", 
                         "No", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
d_MMSFPheno$Proportion <- d_MMSFPheno$Counts/ sum(d_MMSFPheno$Counts)
ggplot() +
  geom_col(data = d_MMSFPheno, aes(x = reorder(Species.ID, -Counts), y = as.numeric(Counts),
                                   color = Overlap, fill = Overlap), alpha = 0.8) +

  scale_color_manual(values = c("grey", "black"))+
  scale_fill_manual(values = c("grey", "black"))+
  xlab(expression(paste("Species"))) +
  ylab(expression(paste("Stem Count"))) +
  # ylim(-05,12)+
  # xlim(0,200)+
  theme(legend.position = "NA",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species", size=30),
        legend.text=element_text(size=30),
        axis.text.x=element_text(angle=-90)) 

ggplot() +
  # geom_col(data = d_MMSFPheno, aes(x = reorder(Species.ID, -Counts), y = as.numeric(Counts)),
  #          color = "grey70", alpha = 0.8) +
  
  geom_col(data = d_MMSFTrees, aes(x = reorder(Species, -Counts), y = as.numeric(Counts)),
           color = "blue", alpha = 0.8) +
  # 
  xlab(expression(paste("Species"))) +
  ylab(expression(paste("Stem Count"))) +
  # ylim(-05,12)+
  # xlim(0,200)+
  theme(legend.position = "NA",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species", size=30),
        legend.text=element_text(size=30),
        axis.text.x=element_text(angle=-90)) 



######################

d_GWPheno$Overlap <- c("Yes", "Yes", "Yes", "No","No", "No", "Yes", 
                         "Yes", "No", "Yes", "No", "No", "Yes", "No", "Yes")

d_GWPheno$Proportion <- d_GWPheno$Counts/ sum(d_GWPheno$Counts)

ggplot() +
  geom_col(data = d_GWPheno, aes(x = reorder(Species, -Counts), y = as.numeric(Counts),
                                   color = Overlap, fill = Overlap), alpha = 0.8) +
  
  scale_color_manual(values = c("grey", "black"))+
  scale_fill_manual(values = c("grey", "black"))+
  xlab(expression(paste("Species"))) +
  ylab(expression(paste("Stem Count"))) +
  # ylim(-05,12)+
  # xlim(0,200)+
  theme(legend.position = "NA",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species", size=30),
        legend.text=element_text(size=30),
        axis.text.x=element_text(angle=-90)) 


P

ggplot() +
  # geom_col(data = d_MMSFPheno, aes(x = reorder(Species.ID, -Counts), y = as.numeric(Counts)),
  #          color = "grey70", alpha = 0.8) +
  
  geom_col(data = d_GWTrees, aes(x = reorder(Species, -Counts), y = as.numeric(Counts)),
           color = "blue", alpha = 0.8) +
  # 
  xlab(expression(paste("Species"))) +
  ylab(expression(paste("Stem Count"))) +
  # ylim(-05,12)+
  # xlim(0,200)+
  theme(legend.position = "NA",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species", size=30),
        legend.text=element_text(size=30),
        axis.text.x=element_text(angle=-90)) 



