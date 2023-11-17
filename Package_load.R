# Load Packages
packages <- c("zen4R", "rstudioapi", "readr", "emmeans", "sf", "raster", "gstat", 
              "fields", "tidyverse", "maptools", "tmap", "sp", "readxl", "ggplot2", 
              "lubridate", "hutils", "brms", "modelr", "sjPlot", "sjstats", 
              "RColorBrewer", "tidybayes", "dplyr", "tidyr", "tidyselect", "stringr", 
              "gridExtra", "stars", "rstan")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    warning(paste("Package", pkg, "is not installed. Please install it from your preferred CRAN Mirror"))
    install.packages(pkg, repos = "http://cran.us.r-project.org") #Can be changed
    library(pkg, character.only = TRUE)
  }
}

# Install rgdal if not available
if (!require("rgdal", character.only = TRUE)) {
  message("rgdal is not available on CRAN. A legacy version can be installed from the CRAN archive. Please refer to the supplementary instructions document for installation details.")
  # Supplementary instructions can detail the process to download and install from archive
}

# Install geoR with XQuartz dependency note for MacOS users
if (!require("geoR", character.only = TRUE)) {
  message("geoR may require XQuartz to be installed on macOS. Please refer to the supplementary instructions document for installation details.")
}
