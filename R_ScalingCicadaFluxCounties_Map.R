##//This script serves to scale the effect of cicada holes across the counties contain periodical cicadas
##//Note this script is separate as the data and maps require significant computing power 

#//Load Libraries
#//Get Packages
require(sp) #//Data management tools in arc
require(raster) #//Spatial analysis toos in arc
require(rgdal) #//Required to get drivers to read data
require(rgeos) #//This is like Analysis tools in arc
require(maptools) #//
require(gstat) #//Kriging Tools (ArcGIS Geospatial Statistics toolbox)
require(glmulti) #// General linear model tools
require(leaps) #//Linear Model Tools
require(geoR) #//
# devtools::install_version("sf", version = "1.0.8")
library(sf) # Simple Features for R
library(rnaturalearth) # World Map Data from Natural Earth
library(here) # A Simpler Way to Find Your Files
library(stars) # Spatiotemporal Arrays, Raster and Vector Data Cubes
library(dplyr) # A Grammar of Data Manipulation
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(ggnewscale) # Multiple Fill and Color Scales in 'ggplot2'
library(scico) # Colour Palettes Based on the Scientific Colour-Maps
library(geobgu) # install from GitHub ("michaeldorman/geobgu")
library(ggrepel) # Automatically Position Non-Overlapping Text Labels with 'ggplot2'
library(nngeo)

theme_set(theme_classic())
GoAvsGo <- c("#6F263D", "#236192", "#A2AAAD", "#000000")
cicadaColors <- c( "#191919", "#943b37" , "#fc0d03")

##//Soil respiration rasters are obstained from Global Gridded 1-km Annual Soil Respiration and Uncertainty Derived from SRDB V3
"https://daac.ornl.gov/CMS/guides/CMS_Global_Soil_Respiration.html"
##//Download data move .img files to a common working directory
##//  This analysis will use the mean, lower quantile (25) and upper quantile (75) estimates 

##//After downloading .img files in working directories and update data pathways
SoilFlux <- read_stars("D:/Dropbox/Projects/Indiana/Data/CicadaFlux/GIS/CMS_Global_Soil_Respiration_1736/CMS_Global_Soil_Respiration_1736/data/Clipped_soil_respiration_mean_eastern_US.img")
SoilFlux_25 <- read_stars("D:/Dropbox/Projects/Indiana/Data/CicadaFlux/GIS/CMS_Global_Soil_Respiration_1736/CMS_Global_Soil_Respiration_1736/data/Clipped_soil_respiration_Q25_eastern_US.img")
SoilFlux_75 <- read_stars("D:/Dropbox/Projects/Indiana/Data/CicadaFlux/GIS/CMS_Global_Soil_Respiration_1736/CMS_Global_Soil_Respiration_1736/data/Clipped_soil_respiration_Q75_eastern_US.img")

##//Updating CRS
SoilFlux2 <- SoilFlux %>%
  st_transform("+proj=moll")

SoilFlux_252 <- SoilFlux_25 %>%
  st_transform("+proj=moll")

SoilFlux_752 <- SoilFlux_75 %>%
  st_transform("+proj=moll")

#//World map for reference
worldmap <- ne_countries(scale = "small", returnclass = "sf")

##//Build maps
mesoam <- worldmap %>%
  filter(region_wb == "North America") %>%
  dplyr::select(admin) %>%
  st_transform("+init=epsg:4326")

##// Active cicada broods shapefiles "https://data.fs.usda.gov/geodata/edw/datasets.php?xmlKeyword=cicada"
##  download shapefiles, move files to working directory, and update pathways
cicadapolys <- st_read("D:/Dropbox/Projects/Indiana/Data/CicadaFlux/GIS/CicadaCounties.shp")
county_polys <- cicadapolys %>% st_transform("+proj=moll")

##//Data frame with estimates of forest
cicadaForestpolys <- st_read("D:/Dropbox/Projects/Indiana/Data/CicadaFlux/GIS/Counties_cicadas_forest_prop_shapefile/Counties_cicadas_forest_prop_shapefile/Counties_cicadas_forest_prop.shp")
cicadaForest_polys <- cicadaForestpolys %>% st_transform("+proj=moll")

##//State outlines
##//Generic state shapefile (https://lehd.ces.census.gov/data/schema/latest/lehd_shapefiles.html)
##  download shapefiles, move files to working directory, and update pathways
statepolys <- st_read("D:/Dropbox/Projects/Indiana/Data/GIS/tl_2021_us_state/tl_2021_us_state.shp")
state_polys <- statepolys %>% st_transform("+proj=moll")

##//Clipping fluxes to forested county area
Cicada_county <- st_crop(SoilFlux2, cicadaForest_polys)
Cicada_county25 <- st_crop(SoilFlux_252, cicadaForest_polys)
Cicada_county75 <- st_crop(SoilFlux_752, cicadaForest_polys)

##//Scaling acreas to m2
cicadaForest_polys$S_USACou_m2 <- cicadaForest_polys$S_USACou_4 * 4046.86

##//Reconstructing data 
##//RS == (((1/2)*annual_median_flux_rate * total_forested_areas_m2) * Enrichment_Factor(2.5%)) / converting grams to gG
##//This is used for the average(mean), 25, and 75 quantiles to generate the effect plus uncertainty

FigureTable <- rbind(
  data.frame(Treatment = "Cicadas",
             RS = ((((median(Cicada_county$Clipped_soil_respiration_mean_eastern_US.img, na.rm = T)*0.5) * sum(cicadaForest_polys$S_USACou_m2 * cicadaForest_polys$prop_fore0, na.rm = T)))*1.025)/1e9,
             
             RS_25 = ((((median(Cicada_county25$Clipped_soil_respiration_Q25_eastern_US.img, na.rm = T)*0.5) * sum(cicadaForest_polys$S_USACou_m2 * cicadaForest_polys$prop_fore0, na.rm = T)))*1.025)/1e9,
             
             RS_75 = ((((median(Cicada_county75$Clipped_soil_respiration_Q75_eastern_US.img, na.rm = T)*0.5) * sum(cicadaForest_polys$S_USACou_m2 * cicadaForest_polys$prop_fore0, na.rm = T)))*1.025)/1e9
             
             ),
  
  data.frame(Treatment = "No-Cicadas",
            RS = (((median(Cicada_county$Clipped_soil_respiration_mean_eastern_US.img, na.rm = T)*0.5) * sum(cicadaForest_polys$S_USACou_m2 * cicadaForest_polys$prop_fore0, na.rm = T)))/1e9,
                  
            RS_25 = (((median(Cicada_county25$Clipped_soil_respiration_Q25_eastern_US.img, na.rm = T)*0.5) * sum(cicadaForest_polys$S_USACou_m2 * cicadaForest_polys$prop_fore0, na.rm = T)))/1e9,
            
            RS_75 = (((median(Cicada_county75$Clipped_soil_respiration_Q75_eastern_US.img, na.rm = T)*0.5) * sum(cicadaForest_polys$S_USACou_m2 * cicadaForest_polys$prop_fore0, na.rm = T)))/1e9
            
            )
  )   
       
##//Flux barplot
ggplot()+
  geom_col(data = FigureTable, aes(x = Treatment, 
                                   y = RS, color = Treatment,
                                   fill = Treatment), 
           alpha = 0.4, width = .75) +  
  geom_errorbar(data = FigureTable, aes(ymin = RS_25, ymax = RS_75, x= Treatment, color = Treatment),
                width=0.2, size = 3) +
  scale_color_manual(values = rev(cicadaColors[1:2])) +
  scale_fill_manual(values = rev(cicadaColors[1:2])) +
  ylab(expression(paste("Annual R" [" S"] , " [Gg" , "  CO" ["2"], " Forest"^"-1 ", "0.5y"^"-1", "]"))) +
  theme(legend.position= "NA",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30))

##//What is the difference in carbon flux?
(carbonDiff <- FigureTable$RS[1] - FigureTable$RS[2])
