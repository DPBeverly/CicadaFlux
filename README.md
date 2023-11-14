
The Cicada flux repository houses the scripts, codes, and workflows for obtaining results associated with the The forest, the cicadas, and the holey fluxes manuscript.

This repository contains the markdown file (.RMD) that reproduces figures and analysis within the manuscript.

Files: 
"CicadaFlux.Rproj"  
 The RStudio Project File

"CicadaFlux_ResultsOutput_Final.html" 
 HTML output of compiled results for the data analysis

"CicadaFlux_ResultsOutput_Final.Rmd" 
RMD file to regenerate the output files and figures

"Figures"  
Folder containing figures found in the manuscript

"R_CicadfluxDataManagement.R" 
Optional data management script, but the same routine is found in the RMD file
        
"R_ScalingCicadaFluxCounties_Map.R"   
Code use to generate scaling analysis and supplemental bar graph in in figure S4

Analysis Workflow
Downloading the repository download the repository via GitHub ("https://github.com/DPBeverly/CicadaFlux")

The data collected for this study are located on Zenodo: ("https://zenodo.org/records/10125404")
"10.5281/zenodo.10125404" 
NOTE: The RMD file will automatically retrieve the data using the DOI so there is no data to download from Zenodo

After you download the repository, download the AmeriFlux data associated with US-MMS AmeriFlux Site.
Grab micromet data from AmeriFlux website "https://ameriflux.lbl.gov/sites/siteinfo/US-MMS"

#//Download the US-MMS Base version (CC-BY-4.0) containing data hourly fluxes and meteorological variables
##//Once downloaded unzip the folder in the working directory housing rmd files
##//Note the data you download will likely contain more data than used in the manuscript as data is updated quarterly

####//Global respiration estimates 
##//Soil respiration rasters are obtained from Global Gridded 1-km Annual Soil Respiration and Uncertainty Derived from SRDB V3
"https://daac.ornl.gov/CMS/guides/CMS_Global_Soil_Respiration.html"
##//Download data move .img files to the common working directory housing rmd files
##//  This analysis will use the mean, lower quantile (25) and upper quantile (75) estimates 

##// Active cicada broods shapefiles "https://data.fs.usda.gov/geodata/edw/datasets.php?xmlKeyword=cicada"
##  download shapefiles, move files to working directory, and update pathways
