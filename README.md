# README

## Cicada flux repository: data and code for Beverly et al. 2023 

This repository houses the scripts, codes, and workflows for obtaining results associated with the manuscript: 

### The forest, the cicadas, and the holey fluxes: periodical cicada impacts on soil respiration depends on tree mycorrhizal type 

#### Authors:

- Daniel P. Beverly *corresponding author* (Email: [dbeverl@iu.edu](mailto:dbeverl@iu.edu))
- Elizabeth Huenupi
- Adrien Gandolfo
- Clara J. Lietzke 
- Darren L. Ficklin
- Mallory L. Barnes
- Jonathan D. Raff
- Kimberly A. Novick
- Richard P. Phillips

---

### Abstract:

The emergence of billions of periodical cicadas affects plant and animal communities profoundly, yet little is known about cicada impacts on soil carbon fluxes. We investigated the effects of Brood X cicadas (Magicicada septendecim, M. cassinii, and M. septendeculain) on soil CO2 fluxes (RS) in two Indiana forests. We hypothesized RS would be sensitive to emergence hole density, with the greatest effects occurring in soils with the lowest ambient fluxes. In support of our hypothesis, RS increased with increasing hole density, and greater effects were observed near AM-associating trees (which expressed lower ambient fluxes) than near EcM-associating trees. Additionally, RS from emergence holes increased temperature sensitivity (Q10) of RS by 13%, elevating the Q10 of ecosystem respiration. Brood X cicadas increased annual RS by ~2.5%, translating to an additional 717 Gg of CO2 across forested areas. As such, periodical cicadas can have substantial effects on soil processes and biogeochemistry. 

---

### Research Questions: 

Our key questions on how cicada emergence influences soil respiration and carbon fluxes are:

1. To what extent does cicada emergence hole density influence R[S]?
2. How do soils beneath different tree types (i.e., AM vs EcM mycorrhizal fungi) modulate these effects?
3. To what extent do emergence holes influence ecosystem respiration (RECO)?


## Build Notes

This code was built using R version 4.3.1 and RStudio 2023.06.1+524 "Mountain Hydrangea" Release for windows

### Required R Packages

The following R packages are required to run the code:

- `zen4R`
- `rstudioapi`
- `readr`
- `emmeans`
- `sf`
- `raster`
- `geoR`
- `gstat`
- `fields`
- `rgdal`
- `tidyverse`
- `maptools`
- `tmap`
- `sp`
- `readxl`
- `ggplot2`
- `lubridate`
- `hutils`
- `brms`
- `modelr`
- `sjPlot`
- `sjstats`
- `RColorBrewer`
- `tidybayes`
- `dplyr`
- `tidyr`
- `tidyselect`
- `stringr`
- `gridExtra`
- `stars`
- `rstan`

**NOTE:** Running entire script, including models and model outputs will take several hours and require upwards of 33 GB of RAM. 

### Repository Contents


- **CicadaFlux.Rproj**: The RStudio Project File.
- **CicadaFlux_ResultsOutput_Final.html**: HTML output of knitted .RMD (compiled results for the data analysis) 
- **CicadaFlux_ResultsOutput_Final.Rmd**: RMD file to reproduce all analyses and generate all figures presented in the analysis.
- **Figures/**: Folder containing figures found in the manuscript.
- **R_CicadfluxDataManagement.R**:  Optional data management script, but the same routine is found in the RMD file
- **R_ScalingCicadaFluxCounties_Map.R**: Code use to generate scaling analysis and supplemental bar graph in in figure S4

---

## Data Access and Analysis Workflow

To reproduce the analysis presented in the manuscript, follow these steps:

1. **Repository Download**
   - Clone the repository or fork your own copy from GitHub:
     https://github.com/DPBeverly/CicadaFlux

2. **Data Access**
   - The primary data for this study is hosted on Zenodo: https://zenodo.org/record/10125404
     - DOI: `10.5281/zenodo.8431547`
   - *Note*: The R Markdown file automatically retrieves the dataset using the DOI, so manual download from Zenodo is not required.

3. **AmeriFlux Data Download**
   - Obtain the AmeriFlux micrometeorological data associated with the US-MMS site from the AmeriFlux website: https://ameriflux.lbl.gov/sites/siteinfo/US-MMS
   - Download the US-MMS Base version (CC-BY-4.0) which contains hourly flux and meteorological variables.

4. **Data Preparation**
   - Once downloaded, unzip the folder in the working directory where the R Markdown files are located.
   - *Note*: The downloaded dataset may contain more recent data than was used in the manuscript, as the data on AmeriFlux is updated quarterly.

5. **Global Respiration Estimates**
   - Access the global soil respiration rasters derived from SRDB V3: https://daac.ornl.gov/CMS/guides/CMS_Global_Soil_Respiration.html
   - Download the data and move the `.img` files to the common working directory for use in the analysis. Utilize the mean, 25th percentile (lower quantile), and 75th percentile (upper quantile) estimates for the analysis.

## Dataset Descriptions

### Chamber Respiration Data: `CicadaChamberFlux_2021.csv`

This file contains the following variables:

- `day`: Date of measurement.
- `UnqMeas`: Unique measurement ID.
- `Type`: Type of measurement (1 = Spot measurement).
- `Etime`: Measurement time.
- `Tcham`: Chamber temperature (°C).
- `Pressure`: Air pressure (kPa).
- `H2O`: H2O concentration.
- `CO2`: CO2 concentration.
- `Cdry`: Dry corrected C.
- `Tbench`: Bench temperature (°C).
- `RH`: Chamber relative humidity (%).
- `Tboard`: Board temperature (°C).
- `Vin`: Input voltage (volts).
- `CO2ABS`: Absolute CO2 concentration.
- `H2OABS`: Absolute H2O concentration.
- `Hour`: Hour of the day.
- `DOY`: Day of the year.
- `RAWCO2`: Raw CO2 signal.
- `RAWCO2REF`: CO2 signal reference.
- `RAWH2O`: Raw H2O signal.
- `RAWH2OREF`: H2O signal reference.
- `OBS`: Observation number.
- `VCham`: Chamber volume.
- `Offset`: Collar offset.
- `Area`: Chamber area.
- `VTotal`: Total measurement volume.
- `ExpFlux`: Exponential efflux.
- `ExpFluxCV`: Coefficient of variance on the exponential efflux.
- `Exp_dCdt`: Exponential efflux change in CO2 per unit time.
- `ExpR2`: Regression coefficient on the exponential efflux.
- `LinFlux`: Linear efflux.
- `LinFluxCV`: Coefficient of variance on the linear efflux.
- `Lin_dCdt`: Linear efflux change in CO2 per unit time.
- `LinR2`: Regression coefficient on the linear efflux.
- `LinFluxSE`: Standard error on the linear efflux.
- `File`: File containing observation.
- `Collar`: Collar ID.
- `LinReg_R2`: Linear regression coefficient.
- `CO2_t0`: CO2 concentration at point zero.
- `CO2_t0_LCI`: Lower confidence interval on CO2 concentration at point zero.
- `CO2_t0_HCI`: Upper confidence interval on CO2 concentration at point zero.
- `dCdt`: Slope of change in CO2 per unit time.
- `dCdt_LCI`: Lower confidence interval for the slope of change in CO2 per unit time.
- `dCdt_HCI`: Upper confidence interval for the slope of change in CO2 per unit time.
- `Site`: Site of measurement.
- `Date`: Date of measurement.
- `SoilT_C`: Soil temperature (°C).
- `SoilVWC_pct`: Soil moisture (% VWC).
- `n_holes`: Number of cicada holes.
- `Corr_offset_cm`: Actual collar offset.
- `Pair`: Collar pair ID.
- `Species`: Tree species ID.
- `Symbiont`: Fungal symbiont.
- `Hole_Collar`: Holes per collar.
- `Hole_Plot_m2`: Holes per square meter.
- `Latitude`: Measurement latitude.
- `Longitude`: Measurement longitude.
- `Altitude`: Measurement altitude.
- `VCollar`: Corrected chamber volume.
- `Corr_VTotal`: Corrected total measurement volume.
- `Corr_LinFlux`: Corrected linear efflux.
- `Corr_LinFlux_LCI`: Lower confidence interval for corrected linear efflux.
- `Corr_LinFlux_HCI`: Upper confidence interval for corrected linear efflux.

### Cicada Hole Counts Data

This includes counts of cicada holes throughout the phenology transects with variables:

- `Site`: Measurement site.
- `Tree_ID`: TREE ID.
- `Species`: Tree species.
- `Fungi_Type`: Fungal symbiont.
- `Lat`: Measurement latitude.
- `Long`: Measurement longitude.
- `Holes_mx`: Maximum number of the cicada holes.
- `Holes_mx_sd`: Standard deviation number of the cicada holes.
- `Holes_md_mx`: Median number of the cicada holes.

### Shape Files

The following shape files are used:

- `MMSF_Collars_v2`: Shapefile containing collar location and metadata for collars.
- `PhenoHoles_Dissolve_20_v2`: Shapefile containing boundary (~20m) around phenology transect and associated metadata.

#### Active Cicada Broods Shapefiles

- [Active cicada broods shapefiles](https://data.fs.usda.gov/geodata/edw/datasets.php?xmlKeyword=cicada): These files include the modified estimates of forest coverage by county.

### Download and Preparation Instructions

1. Download the shapefiles and move the files to the working directory.
2. Ensure pathways in the R scripts are updated to reflect the new locations of the shapefiles.
