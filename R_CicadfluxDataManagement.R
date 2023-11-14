##//This script helps outline data used in the analysis of cicada hole effects on soil respiration
# Load the zen4R library and other packages
library(zen4R)
library(rstudioapi)
library(readr)
library(emmeans)
library(sf)
library(raster)
library(geoR)
library(gstat)
library(fields)
library(rgdal)
library(tidyverse)
library(maptools) 
library(rgeos)
library(tmap)
library(sp) 
library(readxl)
library(ggplot2)
library(lubridate)
library(hutils)
library(brms)
library(modelr)
library(sjPlot)
library(sjstats)
library(RColorBrewer)
library(tidybayes)
library(dplyr)
library(tidyr)
library(tidyselect)
library(stringr)
library(gridExtra)

##//Themes and color ramps
theme_set(theme_classic())
GoAvsGo <- c("#6F263D", "#236192", "#A2AAAD", "#000000")

#################################################################
##//Update where you have code, data, and working directory defined
MyDir <- dirname(rstudioapi::getActiveDocumentContext()$path)

##//Set the directory 
setwd(MyDir)
getwd()

##// Or chose the directory 
# choose.dir()
# dirname(rstudioapi::getActiveDocumentContext()$path)

##//Define the DOI for the Zenodo record you want to download data from
doi <- "10.5281/zenodo.10125404" 

dir(dirname(rstudioapi::getActiveDocumentContext()$path))

##//Download data from Zenodo
CicadaData <- download_zenodo(
  doi,
  path = dirname(rstudioapi::getActiveDocumentContext()$path),
  files = list(),
  logger = NULL,
  quiet = FALSE)

##//Check to make sure the data files have been downloaded to the correct directory
dir(dirname(rstudioapi::getActiveDocumentContext()$path))

##//Read in the chamber data
Rs_plots <- read_csv(paste(dirname(rstudioapi::getActiveDocumentContext()$path), 
                           "CicadaChamberFlux_2021.csv", sep = "/"))

##//Removing the one outlier in the data
Rs_plots <- Rs_plots[Rs_plots$Corr_LinFlux<=6,]

##//Read in the cicada hole data
meholes <- read_csv(paste(dirname(rstudioapi::getActiveDocumentContext()$path), 
                           "MMSF_GW_KF_CICADAHOLES_2021.csv", sep = "/"))

##//Updating string names
meholes$Fungi_Type <- ifelse(meholes$Fungi_Type=="ECM", "EcM", meholes$Fungi_Type)


##//Shapefiles used in the study 
##//Collar shapefiles
MMSF_pheno <- readOGR(dsn = dirname(rstudioapi::getActiveDocumentContext()$path), 
                      layer="PhenoHoles_MMSF_v2")
###//Plot boundary
MMSF_pheno_bound <- readOGR(dsn = dirname(rstudioapi::getActiveDocumentContext()$path), 
                            layer="PhenoHoles_Dissolve_20_v2")

##//other than the data listed in the Zenodo repository you 
#    will need to independently go download data from:
    # AmeriFlux for the US-MMS site
    # Global Gridded 1-km Annual Soil Respiration and Uncertainty Derived from SRDB V3
    # Active brood maps provided by the USDA


##//Grab micromet data from AmeriFlux website
"https://ameriflux.lbl.gov/sites/siteinfo/US-MMS"
#//Download the US-MMS Base version (CC-BY-4.0) containing data hourly fluxes and meteorological variables
##//Once downloaded unzip the folder in the working directory
##//Note the data you download will contain more data than used in the manuscript as data is updated quarterly

##//AmeriFlux data
##//This is only an example the data file name may change based on when data is downloaded
##//Update the file path to match local pathways 

##//This is only an example the data file name may change based on when data is downloaded!!!!!!!!!!
AmeriFlux_File <- paste(dirname(rstudioapi::getActiveDocumentContext()$path), 
                        "AMF_US-MMS_BASE-BADM_21-5/AMF_US-MMS_BASE_HR_21-5.csv" , sep = "/")
dat_46m <- read.csv(AmeriFlux_File, 
                    skip = 2, na.strings = -9999)

####//Global respiration estimates 
##//Soil respiration rasters are obstained from Global Gridded 1-km Annual Soil Respiration and Uncertainty Derived from SRDB V3
"https://daac.ornl.gov/CMS/guides/CMS_Global_Soil_Respiration.html"
##//Download data move .img files to a common working directory
##//  This analysis will use the mean, lower quantile (25) and upper quantile (75) estimates 

##//After downloading .img files in working directories and update data pathways
SoilFlux <- read_stars( paste(dirname(rstudioapi::getActiveDocumentContext()$path), 
  "CMS_Global_Soil_Respiration_1736/CMS_Global_Soil_Respiration_1736/data/Clipped_soil_respiration_mean_eastern_US.img",
  sep = "/")) 
SoilFlux_25 <- read_stars(paste(dirname(rstudioapi::getActiveDocumentContext()$path),
  "CMS_Global_Soil_Respiration_1736/CMS_Global_Soil_Respiration_1736/data/Clipped_soil_respiration_Q25_eastern_US.img",
  sep = "/"))
SoilFlux_75 <- read_stars(paste(dirname(rstudioapi::getActiveDocumentContext()$path),
  "/CMS_Global_Soil_Respiration_1736/CMS_Global_Soil_Respiration_1736/data/Clipped_soil_respiration_Q75_eastern_US.img", 
  sep = "/"))


##// Active cicada broods shapefiles "https://data.fs.usda.gov/geodata/edw/datasets.php?xmlKeyword=cicada"
##  download shapefiles, move files to working directory, and update pathways
cicadapolys <- st_read(paste(dirname(rstudioapi::getActiveDocumentContext()$path), 
                              "CicadaCounties.shp", sep = "/"))

##//Optional data product for making the maps but not used in the R code
# ##//State outlines
# ##//Generic state shapefile (https://lehd.ces.census.gov/data/schema/latest/lehd_shapefiles.html)
# ##  download shapefiles, move files to working directory, and update pathways
# statepolys <- st_read("D:/Dropbox/Projects/Indiana/Data/GIS/tl_2021_us_state/tl_2021_us_state.shp")
# state_polys <- statepolys %>% st_transform("+proj=moll")



