README.md
The Cicada flux repository houses the scripts, codes, and workflows for obtaining results associated with the The forest, the cicadas, and the holey fluxes manuscript.
# Title: The forest, the cicadas, and the holey fluxes: periodical cicada impacts on soil respiration depends on tree mycorrhizal type 
## Authors: Daniel P. Beverly1,2, *, Elizabeth Huenupi2 (ehuenupi@iu.edu), Adrien Gandolfo1 (ad.gandolfo@gmail.com), Clara J. Lietzke3 (clietzke@iu.edu), Darren L. Ficklin4 (dficklin@indiana.edu) Mallory L. Barnes1 (malbarn@iu.edu), Jonathan D. Raff1,3 (jdraff@indiana.edu), Kimberly A. Novick1 (knovick@indiana.edu), and Richard P. Phillips2 (rpp6@indiana.edu)

## Abstract:
### The emergence of billions of periodical cicadas affects plant and animal communities profoundly, yet little is known about cicada impacts on soil carbon fluxes. We investigated the effects of Brood X cicadas (Magicicada septendecim, M. cassinii, and M. septendeculain) on soil CO2 fluxes (RS) in two Indiana forests. We hypothesized RS would be sensitive to emergence hole density, with the greatest effects occurring in soils with the lowest ambient fluxes. In support of our hypothesis, RS increased with increasing hole density, and greater effects were observed near AM-associating trees (which expressed lower ambient fluxes) than near EcM-associating trees. Additionally, RS from emergence holes increased temperature sensitivity (Q10) of RS by 13%, elevating the Q10 of ecosystem respiration. Brood X cicadas increased annual RS by ~2.5%, translating to an additional 717 Gg of CO2 across forested areas. As such, periodical cicadas can have substantial effects on soil processes and biogeochemistry. 



### Two Questions are developed on how cicada emergene influences soil respiration and carbon fluxes:

#### *__1)__* To what extent does cicada emergence hole density influence R[S]?

#### *__2)__* How do soils beneath different tree types (i.e., AM vs EcM mycorrhizal fungi) modulate these effects?
  
#### *__3)__* To what extent do emergence holes influence ecosystem respiration (RECO)?
  
  
#####        Mechanistic hypothesis: 
  
![Conceptual processes influencing RS following cicada emergence and predictions for tree mycorrhizal responses to cicada hole densities. Cicada emergence holes increase water infiltration **(a))**  reducing CO2 and oxygen diffusion creating an anaerobic environment which should decrease C fluxes **(b))**. Conversely, emergence holes can improve aerobic conditions thus increasing autotrophic respiration (Ra) or prime microbial respiration (Rh) via the modulation of dissolved organic C for both mycorrhizal types. The magnitude and direction of the RS response to cicada emergence holes is likely influenced by precipitation patterns and soil saturation levels directly following rain events. Detailed microbial metabolic processes within soils beneath AM- and EcM-associating trees are expected to vary based on biogeochemical responses to changes in baseline soil C stocks and transported dissolved organic C enhancing microbial decomposition (i.e., priming).](D:/Dropbox/Projects/Indiana/Data/CicadaFlux/CicadaConceptualFigure.png)


## Build Notes
### This code was built using R version 4.3.1 and RStudio 2023.06.1+524 "Mountain Hydrangea" Release for windows
### This code requires the following R packages: zen4R; rstudioapi; readr; emmeans; sf; raster; geoR; gstat
###     fields; rgdal; tidyverse; maptools; tmap; sp; readxl; ggplot2; lubridate; hutils; brms; modelr; sjPlot;
###     sjstats; RColorBrewer; tidybayes; dplyr; tidyr; tidyselect; stringr; gridExtra; stars; rstan
### NOTE: Running entire script with models and model outputs will take several hours and require upwards of 33 GB of RAM


This repository contains the markdown file (.RMD) that reproduces figures and analysis within the manuscript.

Files: "CicadaFlux.Rproj"
The RStudio Project File

"CicadaFlux_ResultsOutput_Final.html" HTML output of compiled results for the data analysis

"CicadaFlux_ResultsOutput_Final.Rmd" RMD file to regenerate the output files and figures

"Figures"
Folder containing figures found in the manuscript

"R_CicadfluxDataManagement.R" Optional data management script, but the same routine is found in the RMD file

"R_ScalingCicadaFluxCounties_Map.R"
Code use to generate scaling analysis and supplemental bar graph in in figure S4

Analysis Workflow Downloading the repository download the repository via GitHub ("https://github.com/DPBeverly/CicadaFlux")

The data collected for this study are located on Zenodo: ("https://zenodo.org/records/10125404") "10.5281/zenodo.10125404" NOTE: The RMD file will automatically retrieve the data using the DOI so there is no data to download from Zenodo

After you download the repository, download the AmeriFlux data associated with US-MMS AmeriFlux Site. Grab micromet data from AmeriFlux website "https://ameriflux.lbl.gov/sites/siteinfo/US-MMS"

#//Download the US-MMS Base version (CC-BY-4.0) containing data hourly fluxes and meteorological variables ##//Once downloaded unzip the folder in the working directory housing rmd files ##//Note the data you download will likely contain more data than used in the manuscript as data is updated quarterly

####//Global respiration estimates ##//Soil respiration rasters are obtained from Global Gridded 1-km Annual Soil Respiration and Uncertainty Derived from SRDB V3 "https://daac.ornl.gov/CMS/guides/CMS_Global_Soil_Respiration.html" ##//Download data move .img files to the common working directory housing rmd files ##// This analysis will use the mean, lower quantile (25) and upper quantile (75) estimates

##// Active cicada broods shapefiles "https://data.fs.usda.gov/geodata/edw/datasets.php?xmlKeyword=cicada" ##// download shapefiles, move files to working directory, and update pathways

Data Analysis and codes for cicada flux analysis that corresponds to the Ecology Letters manuscript: The forest, the cicadas, and the holey fluxes: periodical cicada impacts on soil respiration 1 depends on tree mycorrhizal type (DOI).

Datasets used include:
Chamber respiration data: CicadaChamberFlux_2021.csv
"day" == Date
"UnqMeas"  == Unique measurement ID
"Type"  == Type of measurements (1 == Spot measurement)
"Etime"  == measurement time
"Tcham"  == chamber temperature (C)
"Pressure"  == Air pressure (kPa)
"H2O" == H20 concentration 
"CO2" == CO2 concentration
"Cdry" == Dry corrected C 
"Tbench" == Bench temperature (C)
"RH" == Chamber relative humidity (%)
"Tboard" == Board Temperature (C)
"Vin" == Input voltage (volts)
"CO2ABS" = Absolute CO2 Concentration
"H2OABS" == Absolute H2O concentration
"Hour" == Hour of the day
"DOY" == Day of year
"RAWCO2" == Raw CO2 signal
"RAWCO2REF" ==  CO2 signal reference
"RAWH2O" == Raw H2O signal
"RAWH2OREF" == H2O signal reference
"OBS" == Observation number
"VCham" == Chamber volume
"Offset" == Collar offset
"Area" == Chamber area 
"VTotal" == Total measurement volume
"ExpFlux" == Exponential Efflux
"ExpFluxCV"  == Coefficient of variance on the Exponential Efflux
"Exp_dCdt" == Exponential Efflux change in co2 per unit time
"ExpR2" == Regression coefficient on the Exponential Efflux       
"LinFlux" == Linear Efflux
"LinFluxCV" == Coefficient of variance on the Linear Efflux
"Lin_dCdt" == Linear Efflux change in co2 per unit time
"LinR2" == Regression coefficient on the Linear Efflux  
"LinFluxSE" == Standard Error on the Linear Efflux  
"File" == File containing observation            
"Collar" == Collar ID
"LinReg_R2" == Linear regression coefficient 
"CO2_t0" == CO2 concentration at point zero
"CO2_t0_LCI" == Lower CI on CO2 concentration at point zero
"CO2_t0_HCI" == Upper CI CO2 concentration at point zero
"dCdt" == Slope of change in CO2 per unit time         
"dCdt_LCI"  == Lower CI for the slope of change in CO2 per unit time
"dCdt_HCI" == Upper CI for the slope of change in CO2 per unit time
"Site" == Site of measurement 
"Date" == Date of Measurement
"SoilT_C" == Soil Temperature (C)
"SoilVWC_pct" == Soil Moisture (% VWC)
"n_holes" == Number of cicada holes
"Corr_offset_cm" == Actual collar offset
"Pair" == Collar pair ID
"Species"== Tree species ID
"Symbiont" == Fungal symbiont 
"Hole_Collar" == holes per collar
"Hole_Plot_m2" == holes per sq meter 
"Latitude"  == Measurement latitude
"Longitude" == Measurement longitude 
"Altitude"  == Measurement altitude 
"VCollar" == Corrected chamber volume
"Corr_VTotal" ==  Corrected chamber volume based    
"Corr_LinFlux" == Corrected linear efflux
"Corr_LinFlux_LCI" == Lower CI for corrected linear efflux
"Corr_LinFlux_HCI" == Upper CI for corrected linear efflux


Cicada hole counts throughout the phenology transects:
"Site"== Measurement site
"Tree_ID" == TREE ID
"Species" == Tree Species
"Fungi_Type" == Fungal Symbiont
"Lat" == measurement latitude 
"Long" == measurement longitude
"Holes_mx" == Maximum number of the cicada holes
"Holes_mx_sd" == standard deviation number of the cicada holes
"Holes_md_mx" == median number of the cicada holes
 
Shape files: 

MMSF_Collars_v2==shapefile containing collar location and metadata for collars
PhenoHoles_Dissolve_20_v2 == shapefile containing boundary (~20m) around phenology transect and associated metadata
