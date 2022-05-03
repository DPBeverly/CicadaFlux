###///Spatial interpolation of cicada holes and fluxes

#remove(list=ls())

require(sf)
require(raster)
require(geoR)
require(INLA)
require(gstat)
require(fields)
require(rgdal)
library(tidyverse)
library(spatstat)  # Used for the dirichlet tessellation function
library(maptools)  # Used for conversion from SPDF to ppp
library(raster)    # Used to clip out thiessen polygons
library(rgeos)
library(tmap)


theme_set(theme_minimal())
GoAvsGo <- c("#6F263D", "#236192", "#A2AAAD", "#000000")

mm_pheno <- read.csv("D:/Dropbox/Projects/Indiana/Data/CicadaFlux/GIS/MMSF_Cicada_Tree_Coords_2021.csv")

mm_pheno_sum <- mm_pheno %>%
  dplyr::group_by(Tree_ID, Species, Fungi_Type) %>% # group by minutes
  dplyr::summarise(Lat = mean(Lat, na.rm = T),
                   Long = mean(Long, na.rm = T),
                   Holes_mx = max(Holes_cnt_mx, na.rm = T),
                   Holes_mx_sd = sd(Holes_cnt_mx, na.rm = T),
                   
                   Holes_md_mx = max(Holes_cnt_md, na.rm = T),
                   Holes_md_sd = sd(Holes_cnt_md, na.rm = T)) %>%  
  na.omit()


write.csv(mm_pheno_sum, "D:/Dropbox/Projects/Indiana/Data/CicadaFlux/GIS/MMSF_Cicada_Pheno/PhenoHoles.csv")

plot(mm_pheno_sum$Lat, mm_pheno_sum$Long)
plot(mm_pheno_sum$Holes_mx, mm_pheno_sum$Holes_md_mx)
plot(mm_pheno_sum$Holes_mx, mm_pheno_sum$Holes_mx_sd)


#"D:/Dropbox/Projects/Indiana/Data/CicadaFlux/GIS/MMSF_Cicada_Pheno"

##//Data folder
setwd("D:/Dropbox/Projects/Indiana/Data/CicadaFlux/GIS")

dir()

##//Collar shapefiles
MMSF_collar <- readOGR(dsn ="D:/Dropbox/Projects/Indiana/Data/CicadaFlux/GIS", layer="CicadaFluxCollars_MMSF")
GW_collar <- readOGR(dsn ="D:/Dropbox/Projects/Indiana/Data/CicadaFlux/GIS", layer="CicadaFluxCollars_GW")
KF_collar <- readOGR(dsn ="D:/Dropbox/Projects/Indiana/Data/CicadaFlux/GIS", layer="CicadaFluxCollars_KF_v2")

###//Plot boundary
MMSF_bound <- readOGR(dsn ="D:/Dropbox/Projects/Indiana/Data/CicadaFlux/GIS", layer="CicadaFluxCollarsBoundary_MMSF")
GW_bound <- readOGR(dsn ="D:/Dropbox/Projects/Indiana/Data/CicadaFlux/GIS", layer="CicadaFluxCollarsBoundary_GW")
KF_bound <- readOGR(dsn ="D:/Dropbox/Projects/Indiana/Data/CicadaFlux/GIS", layer="CicadaFluxCollarsBoundary_KF")

##//Converting holes to numeric
MMSF_collar@data$Hole_Plot_ <- as.numeric(as.character(MMSF_collar@data$Hole_Plot_))
GW_collar@data$Hole_Plot_ <- as.numeric(as.character(GW_collar@data$Hole_Plot_))
KF_collar@data$Hole_Plot_ <- as.numeric(as.character(KF_collar@data$Hole_Plot_))

#//Have a look
plot(MMSF_bound, lwd = 2)
plot(MMSF_collar, pch = 16, col='blue', cex =(MMSF_collar[[7]])*0.1, add=TRUE)

plot(GW_bound, lwd = 2)
plot(GW_collar, pch = 16, col='blue', cex =(GW_collar[[7]])*0.1, add=TRUE)

plot(KF_bound, lwd = 2)
plot(KF_collar, pch = 16, col='blue', cex =(KF_collar[[7]])*0.1, add=TRUE)




z <- gzcon(url("http://colby.edu/~mgimond/Spatial/Data/precip.rds"))
P <- readRDS(z)

P@data$Precip_in
GW_collar@data$Hole_Plot_
# Create a tessellated surface
th  <-  as(dirichlet(as.ppp(GW_collar)), "SpatialPolygons")

# The dirichlet function does not carry over projection information
# requiring that this information be added manually
proj4string(th) <- proj4string(GW_collar)

# The tessellated surface does not store attribute information
# from the point data layer. We'll use the over() function (from the sp
# package) to join the point attributes to the tesselated surface via
# a spatial join. The over() function creates a dataframe that will need to
# be added to the `th` object thus creating a SpatialPolygonsDataFrame object
th.z     <- over(th, GW_collar, fn=mean)
th.spdf  <-  SpatialPolygonsDataFrame(th, th.z)

# Finally, we'll clip the tessellated  surface to the Texas boundaries
th.clp   <- raster::intersect(GW_bound,th.spdf)

# Map the data
tm_shape(th.clp) + 
  tm_polygons(col="Hole_Plot_", palette="RdBu", auto.palette.mapping=FALSE,
              title="Predicted Cicada Holes m2") +
  tm_legend(legend.outside=TRUE)




P <- GW_collar
W <- GW_bound

P@bbox <- W@bbox

library(gstat) # Use gstat's idw routine
library(sp)    # Used for the spsample function

# Create an empty grid where n is the total number of cells
grd              <- as.data.frame(spsample(P, "regular", n=500000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

# Add P's projection information to the empty grid
proj4string(P) <- proj4string(P) # Temp fix until new proj env is adopted
proj4string(grd) <- proj4string(P)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
P.idw <- gstat::idw(Hole_Plot_ ~ 1, P, newdata=grd, idp=6.0)
#P.idw <- gstat::idw(Symbiont ~ 1, P, newdata=grd, idp=4.0)

# Convert to raster object then clip to Texas
r       <- raster(P.idw)
plot(r)
plot(GW_bound, lwd = 2, add = TRUE)
r.m     <- mask(r, W)

# Plot
tm_shape(r.m) + 
  tm_raster(n=4,palette = GoAvsGo, auto.palette.mapping = FALSE,
            title="Predicted Cicada Hole Density m2") + 
  tm_shape(P) + tm_dots(size=1.2) +
  tm_legend(legend.outside=TRUE)



P <- KF_collar
W <- KF_bound

P@bbox <- W@bbox

library(gstat) # Use gstat's idw routine
library(sp)    # Used for the spsample function

# Create an empty grid where n is the total number of cells
grd              <- as.data.frame(spsample(P, "regular", n=500000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

# Add P's projection information to the empty grid
proj4string(P) <- proj4string(P) # Temp fix until new proj env is adopted
proj4string(grd) <- proj4string(P)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
P.idw <- gstat::idw(Hole_Plot_ ~ 1, P, newdata=grd, idp=6.0)
#P.idw <- gstat::idw(Symbiont ~ 1, P, newdata=grd, idp=4.0)

# Convert to raster object then clip to Texas
r       <- raster(P.idw)
plot(r)
plot(KF_bound, lwd = 2, add = TRUE)
r.m     <- mask(r, W)

# Plot
tm_shape(r.m) + 
  tm_raster(n=4,palette = GoAvsGo, auto.palette.mapping = FALSE,
            title="Predicted Cicada Hole Density m2") + 
  tm_shape(P) + tm_dots(size=1.2) +
  tm_legend(legend.outside=TRUE)

################################################################################################
P <- MMSF_collar
W <- MMSF_bound

P@bbox <- W@bbox

library(gstat) # Use gstat's idw routine
library(sp)    # Used for the spsample function

# Create an empty grid where n is the total number of cells
grd              <- as.data.frame(spsample(P, "regular", n=500000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

# Add P's projection information to the empty grid
proj4string(P) <- proj4string(P) # Temp fix until new proj env is adopted
proj4string(grd) <- proj4string(P)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
P.idw <- gstat::idw(Hole_Plot_ ~ 1, P, newdata=grd, idp=6.0)
#P.idw <- gstat::idw(Symbiont ~ 1, P, newdata=grd, idp=4.0)

# Convert to raster object then clip to Texas
r       <- raster(P.idw)
plot(r)
plot(MMSF_bound, lwd = 2, add = TRUE)
r.m     <- mask(r, W)

# Plot
tm_shape(r.m) + 
  tm_raster(n=4,palette = GoAvsGo, auto.palette.mapping = FALSE,
            title="Predicted Cicada Hole Density m2") + 
  tm_shape(P) + tm_dots(size=1.2) +
  tm_legend(legend.outside=TRUE)


####################################################################################################################
################################################################################################
P <- MMSF_collar
W <- MMSF_bound

P@bbox <- W@bbox

library(gstat) # Use gstat's idw routine
library(sp)    # Used for the spsample function

# Create an empty grid where n is the total number of cells
grd              <- as.data.frame(spsample(P, "regular", n=500000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

# Add P's projection information to the empty grid
proj4string(P) <- proj4string(P) # Temp fix until new proj env is adopted
proj4string(grd) <- proj4string(P)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
P.idw <- gstat::idw(Symbiont ~ 1, P, newdata=grd, idp=6.0)
#P.idw <- gstat::idw(Symbiont ~ 1, P, newdata=grd, idp=4.0)

# Convert to raster object then clip to Texas
r       <- raster(P.idw)
plot(r)
plot(MMSF_bound, lwd = 2, add = TRUE)
r.m     <- mask(r, W)
plot(r.m)
# Plot
tm_shape(r.m) + 
  tm_raster(n=2,palette = GoAvsGo, auto.palette.mapping = FALSE,
            title="AM (1) and EcM (2) Density") + 
  tm_shape(P) + tm_dots(size=1.2, col = "white") +
  tm_legend(legend.outside=TRUE)


writeRaster(r.m, "MMSF_AM_EcM.img")


####################################################################################################################
################################################################################################
P <- GW_collar
W <- GW_bound

P@bbox <- W@bbox

library(gstat) # Use gstat's idw routine
library(sp)    # Used for the spsample function

# Create an empty grid where n is the total number of cells
grd              <- as.data.frame(spsample(P, "regular", n=500000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

# Add P's projection information to the empty grid
proj4string(P) <- proj4string(P) # Temp fix until new proj env is adopted
proj4string(grd) <- proj4string(P)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
P.idw <- gstat::idw(Symbiont ~ 1, P, newdata=grd, idp=6.0)
#P.idw <- gstat::idw(Symbiont ~ 1, P, newdata=grd, idp=4.0)

# Convert to raster object then clip to Texas
r       <- raster(P.idw)
plot(r)
plot(MMSF_bound, lwd = 2, add = TRUE)
r.m     <- mask(r, W)
plot(r.m)
# Plot
tm_shape(r.m) + 
  tm_raster(n=2,palette = GoAvsGo, auto.palette.mapping = FALSE,
            title="AM (1) and EcM (2) Density") + 
  tm_shape(P) + tm_dots(size=1.2, col = "white") +
  tm_legend(legend.outside=TRUE)


####################################################################################################################
################################################################################################
P <- KF_collar
W <- KF_bound

P@bbox <- W@bbox

library(gstat) # Use gstat's idw routine
library(sp)    # Used for the spsample function

# Create an empty grid where n is the total number of cells
grd              <- as.data.frame(spsample(P, "regular", n=500000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

# Add P's projection information to the empty grid
proj4string(P) <- proj4string(P) # Temp fix until new proj env is adopted
proj4string(grd) <- proj4string(P)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
P.idw <- gstat::idw(Symbiont ~ 1, P, newdata=grd, idp=6.0)
#P.idw <- gstat::idw(Symbiont ~ 1, P, newdata=grd, idp=4.0)

# Convert to raster object then clip to Texas
r       <- raster(P.idw)
plot(r)
plot(MMSF_bound, lwd = 2, add = TRUE)
r.m     <- mask(r, W)
plot(r.m)
# Plot
tm_shape(r.m) + 
  tm_raster(n=2,palette = GoAvsGo, auto.palette.mapping = FALSE,
            title="AM (1) and EcM (2) Density") + 
  tm_shape(P) + tm_dots(size=1.2, col = "white") +
  tm_legend(legend.outside=TRUE)







############################################################################################
##//Data folder
setwd("D:/Dropbox/Projects/Indiana/Data/CicadaFlux/GIS/MMSF_Cicada_Pheno")

dir()

##//Collar shapefiles
MMSF_pheno <- readOGR(dsn ="D:/Dropbox/Projects/Indiana/Data/CicadaFlux/GIS/MMSF_Cicada_Pheno", 
                       layer="PhenoHoles")

###//Plot boundary
MMSF_pheno_bound <- readOGR(dsn ="D:/Dropbox/Projects/Indiana/Data/CicadaFlux/GIS/MMSF_Cicada_Pheno", 
                            layer="PhenoHoles_Dissolve_15")

##//Converting holes to numeric
MMSF_pheno@data$Holes_mx <- as.numeric(as.character(MMSF_pheno@data$Holes_mx))
MMSF_pheno@data$Holes_mx_m2 <- MMSF_pheno@data$Holes_mx * 4 

MMSF_pheno@data$Species <- as.factor(as.character(MMSF_pheno@data$Species))
MMSF_pheno@data$Fungi_Type <- as.factor(as.character(MMSF_pheno@data$Fungi_Type))
MMSF_pheno@data$Fungi_Type_Scale[MMSF_pheno@data$Fungi_Type=="AM"] <- 0.2
MMSF_pheno@data$Fungi_Type_Scale[MMSF_pheno@data$Fungi_Type=="EcM"] <- 0.05


MMSF_pheno@data$Holes_mx_m2_nzinf <- ifelse(MMSF_pheno@data$Holes_mx_m2==0, NA, MMSF_pheno@data$Holes_mx_m2) 
hist(MMSF_pheno@data$Holes_mx_m2_nzinf)
#//Have a look
plot(MMSF_pheno_bound, lwd = 2)
plot(MMSF_pheno, pch = 16, col='blue', cex =(MMSF_pheno[[7]])*0.1, add=TRUE)

hist(MMSF_pheno@data$Holes_mx_m2_nzinf, breaks = 10)

str(MMSF_pheno@data)
plot(MMSF_pheno@data$Species, MMSF_pheno@data$Holes_mx)
plot(MMSF_pheno@data$Fungi_Type, MMSF_pheno@data$Holes_mx)
plot(MMSF_pheno@data$Fungi_Type, MMSF_pheno@data$Holes_mx_m2)
plot(MMSF_pheno@data$Fungi_Type, MMSF_pheno@data$Holes_mx_s)
plot(MMSF_pheno@data$Fungi_Type, MMSF_pheno@data$Holes_mx_m2_nzinf)



P <- MMSF_pheno
W <- MMSF_pheno_bound

P@bbox <- W@bbox

# Create an empty grid where n is the total number of cells
grd              <- as.data.frame(spsample(P, "regular", n=500000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

# Add P's projection information to the empty grid
proj4string(P) <- proj4string(P) # Temp fix until new proj env is adopted
proj4string(grd) <- proj4string(P)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
P.idw <- gstat::idw(Fungi_Type ~ 1, P, newdata=grd, idp = 3.0)
#P.idw <- gstat::idw(Symbiont ~ 1, P, newdata=grd, idp=4.0)

# Convert to raster object then clip to Texas
r       <- raster(P.idw)
plot(r)
r_FT <- r

plot(MMSF_pheno_bound, lwd = 2, add = TRUE)
r.m     <- mask(r, W)


plot(r.m)

# Plot
tm_shape(r.m) + 
  tm_raster(palette = topo.colors(2), auto.palette.mapping = FALSE,
            title="AM (1) and EcM (2) Density") + 
  tm_shape(P) + tm_dots(size=1.2, col = "grey") +
  #legend.outside.size(5)+
  tm_legend(legend.outside=TRUE)

###########################################################################################
# Interpolate the grid cells using a power value of 2 (idp=2.0)
P.idw <- gstat::idw(Fungi_Type_Scale ~ 1, P, newdata=grd, idp = 4)
#P.idw <- gstat::idw(Symbiont ~ 1, P, newdata=grd, idp=4.0)

# Convert to raster object then clip to Texas
r       <- raster(P.idw)
r_FTS <- r

plot(r)
plot(MMSF_pheno_bound, lwd = 2, add = TRUE)
r.m     <- mask(r, W)
plot(r.m)
# Plot
tm_shape(r.m) + 
  tm_raster(n = 30, palette = topo.colors(2), auto.palette.mapping = FALSE,
            title="AM (~0.2) <-> EcM (~0.05) ") + 
  tm_shape(P) + tm_dots(size=1.2, col = "white") +
  tm_legend(legend.outside=TRUE)




###################
##//Species Model

# Interpolate the grid cells using a power value of 2 (idp=2.0)
P.idw <- gstat::idw(Species ~ 1, P, newdata=grd, idp = 3.0)
#P.idw <- gstat::idw(Symbiont ~ 1, P, newdata=grd, idp=4.0)

# Convert to raster object then clip to Texas
r <- raster(P.idw)
plot(r)
plot(MMSF_pheno_bound, lwd = 2, add = TRUE)
r.m     <- mask(r, W)
plot(r.m)
# Plot
tm_shape(r.m) + 
  tm_raster(n = 50, palette = topo.colors(2), auto.palette.mapping = FALSE,
            title="Local Species Variation") + 
  tm_shape(P) + tm_dots(size=1.2, col = "white") +
  tm_legend(legend.outside=TRUE)


###################################
##//Hole Model

# Interpolate the grid cells using a power value of 2 (idp=2.0)
P.idw <- gstat::idw(Holes_mx_m2 ~ 1, P, newdata=grd, idp = 10.0)
#P.idw <- gstat::idw(Symbiont ~ 1, P, newdata=grd, idp=4.0)

# Convert to raster object then clip to Texas
r <- raster(P.idw)
r_HD <- r

plot(r)
plot(MMSF_pheno_bound, lwd = 2, add = TRUE)
r.m     <- mask(r, W)
plot(r.m)
# Plot
tm_shape(r.m) + 
  tm_raster(n=50,palette = topo.colors(8), auto.palette.mapping = FALSE,
            title="Cicada Hole per m2") + 
  tm_shape(P) + tm_dots(size=1.2, col = "white") +
  tm_legend(legend.outside=TRUE)

##############################################################################################
# Interpolate the grid cells using a power value of 2 (idp=2.0)
P.idw <- gstat::idw(Holes_mx_m2 ~ 1, P, newdata=grd, idp = 3)
#P.idw <- gstat::idw(Symbiont ~ 1, P, newdata=grd, idp=4.0)

# Convert to raster object then clip to Texas
r <- raster(P.idw)
r_HD2 <- r

plot(r)
plot(MMSF_pheno_bound, lwd = 2, add = TRUE)
r.m     <- mask(r, W)
plot(r.m)
# Plot
tm_shape(r.m) + 
  tm_raster(n=50,palette = topo.colors(10), auto.palette.mapping = FALSE,
            title="Cicada Hole per m2") + 
  tm_shape(P) + tm_dots(size=1.2, col = "white") +
  tm_legend(legend.outside=TRUE)





###################################
##//Hole Variation Model

# Interpolate the grid cells using a power value of 2 (idp=2.0)
P.idw <- gstat::idw(Holes_mx_s^2 ~ 1, P, newdata=grd, idp = 10.0)
#P.idw <- gstat::idw(Symbiont ~ 1, P, newdata=grd, idp=4.0)

# Convert to raster object then clip to Texas
r <- raster(P.idw)
r_HD_var <- r

plot(r)
plot(MMSF_pheno_bound, lwd = 2, add = TRUE)
r.m     <- mask(r, W)
plot(r.m)
# Plot
tm_shape(r.m) + 
  tm_raster(n = 40,palette = topo.colors(8), auto.palette.mapping = FALSE,
            title="Cicada Hole Variation per Count") + 
  tm_shape(P) + tm_dots(size = 1.2, col = "white") +
  tm_legend(legend.outside=TRUE)


####################################################################################

r_FT_HD <- r_FTS * r_HD2


plot(r_FT_HD)
plot(MMSF_pheno_bound, lwd = 2, add = TRUE)
r.m     <- mask(r_FT_HD, W)
plot(r.m)
# Plot
tm_shape(r.m) + 
  tm_raster(n = 40,palette = topo.colors(2), auto.palette.mapping = FALSE,
            title="Flux Increase (%)") + 
  tm_shape(P) + tm_dots(size = 1.2, col = "white") +
  tm_legend(legend.outside=TRUE)



########################################################################################

hist(MMSF_pheno@data$Holes_mx_m2[MMSF_pheno@data$Holes_mx_s<3], breaks = 10)
hist(MMSF_pheno@data$Holes_mx_s, breaks = 10)


str(MMSF_pheno@data)
plot(MMSF_pheno@data$Species, MMSF_pheno@data$Holes_mx)
plot(MMSF_pheno@data$Fungi_Type, MMSF_pheno@data$Holes_mx)
plot(MMSF_pheno@data$Fungi_Type, MMSF_pheno@data$Holes_mx_m2_nzinf)
plot(MMSF_pheno@data$Fungi_Type, MMSF_pheno@data$Holes_mx_m2_nzinf)

hist(MMSF_pheno@data$Holes_mx_m2_nzinf)
#//Have a look
plot(MMSF_pheno_bound, lwd = 2)
plot(MMSF_pheno, pch = 16, col='blue', cex =(MMSF_pheno[[12]])*0.1, add=TRUE)

hist(MMSF_pheno@data$Holes_mx_m2_nzinf, breaks = 20)

str(MMSF_pheno@data)
plot(MMSF_pheno@data$Species, MMSF_pheno@data$Holes_mx_m2_nzinf)
plot(MMSF_pheno@data$Fungi_Type, MMSF_pheno@data$Holes_mx)
plot(MMSF_pheno@data$Fungi_Type, MMSF_pheno@data$Holes_mx_m2_nzinf)
plot(MMSF_pheno@data$Fungi_Type, MMSF_pheno@data$Holes_mx_s)


#################################################################################################
#################################################################################################
###//gettign the rmarkdown to work


require(sf)
require(raster)
require(geoR)
require(INLA)
require(gstat)
require(fields)
require(rgdal)
library(tidyverse)
library(spatstat)  # Used for the dirichlet tessellation function
library(maptools)  # Used for conversion from SPDF to ppp
library(raster)    # Used to clip out thiessen polygons
library(rgeos)
library(tmap)
library(gstat) # Use gstat's idw routine
library(sp)    # Used for the spsample function


###// Scalers based on fungal types
AM_scaler
EcM_scaler
##//Collar shapefiles
MMSF_pheno <- readOGR(dsn ="D:/Dropbox/Projects/Indiana/Data/CicadaFlux/GIS/MMSF_Cicada_Pheno/Take_Two", 
                      layer="PhenoHoles_MMSF_v2")
###//Plot boundary
MMSF_pheno_bound <- readOGR(dsn ="D:/Dropbox/Projects/Indiana/Data/CicadaFlux/GIS/MMSF_Cicada_Pheno/Take_Two", 
                            layer="PhenoHoles_Dissolve_20_v2")

##//Converting holes to numeric
MMSF_pheno@data$Holes_mx <- as.numeric(as.character(MMSF_pheno@data$Holes_mx))
MMSF_pheno@data$Holes_mx_m2 <- MMSF_pheno@data$Holes_mx * 4 

MMSF_pheno@data$Species <- as.factor(as.character(MMSF_pheno@data$Species))
MMSF_pheno@data$Fungi_Type <- as.factor(as.character(MMSF_pheno@data$Fungi_Type))
MMSF_pheno@data$Fungi_Type_Scale[MMSF_pheno@data$Fungi_Type=="AM"] <- AM_scaler
MMSF_pheno@data$Fungi_Type_Scale[MMSF_pheno@data$Fungi_Type=="EcM"] <- EcM_scaler

##//Removing zero cicada hole measurements
MMSF_pheno@data$Holes_mx_m2_nzinf <- ifelse(MMSF_pheno@data$Holes_mx_m2==0, NA, MMSF_pheno@data$Holes_mx_m2) 

#//Have a look
plot(MMSF_pheno@data$Species, MMSF_pheno@data$Holes_mx_m2_nzinf)

plot(MMSF_pheno@data$Fungi_Type, MMSF_pheno@data$Holes_mx_m2_nzinf)
plot(MMSF_pheno@data$Holes_mx_m2, MMSF_pheno@data$Holes_mx_m2_nzinf, xlim = c(0,60), ylim = c(0,60))
abline(0,1)

##//Inverse distance weighted plots
P <- MMSF_pheno
W <- MMSF_pheno_bound

##//Update bounding box
P@bbox <- W@bbox

##//Create an empty grid where n is the total number of cells
grd              <- as.data.frame(spsample(P, "regular", n=500000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

##//Add P's projection information to the empty grid
proj4string(P) <- proj4string(P) # Temp fix until new proj env is adopted
proj4string(grd) <- proj4string(P)
# 
# grdx <- makegrid(P@coords, n = 3, nsig = 2, cellsize = 1, offset = rep(0.5, 2),
# 	pretty = TRUE)
# grdx
# 
# plot(grdx)
# image(r)
# points(spsample(P,n=1000,type="regular"), pch=3, cex=.5)
# 
# plot(spsample(P,n=1000,type="regular"))
# 
# points(pts@coords, col = "blue", pch = 16)


###//Interpolate the grid cells
P.idw <- gstat::idw(Fungi_Type_Scale ~ 1, P, newdata=grd, idp = 5)

###//Convert to raster object then clip to boundary
r <- raster(P.idw)
r_fungi <- r
r.m  <- mask(r, W)
r_fungi <- r.m



##//Plot
tm_shape(r.m) + 
  tm_raster(palette = topo.colors(2), auto.palette.mapping = FALSE,
            title="AM (1) and EcM (2) Density") + 
  tm_shape(P) + tm_dots(size=1.2, col = "grey") +
  #legend.outside.size(5)+
  tm_legend(legend.outside=TRUE)

###//Cicada hole density
##############################################################################################
##//Interpolate the grid cells 
P.idw <- gstat::idw(Holes_mx_m2 ~ 1, P, newdata=grd, na.action = na.omit, idp = 8)
#P.idw <- gstat::idw(Symbiont ~ 1, P, newdata=grd, idp=4.0)

###//Convert to raster object then clip to boundary
r <- raster(P.idw)
r_holes <- r
r.m <- mask(r, W)
r_holes <- r.m
##//Plot
tm_shape(r.m) + 
  tm_raster(n = 5, palette = heat.colors(5), auto.palette.mapping = FALSE,
            title="Cicada Hole per m2") + 
  tm_shape(P) + tm_dots(size=1.2, col = "grey") +
  tm_legend(legend.outside=TRUE)


##//Hole Variation Model
##############################################################################################
##//Interpolate the grid cells
P.idw <- gstat::idw(Holes_mx_s^2 ~ 1, P, newdata=grd, idp = 10.0)

##//Convert to raster object then clip to boundary
r <- raster(P.idw)
r_holes_var <- r
r.m     <- mask(r, W)

##//Plot
tm_shape(r.m) + 
  tm_raster(n = 40,palette = topo.colors(8), auto.palette.mapping = FALSE,
            title="Cicada Hole Variation per Count") + 
  tm_shape(P) + tm_dots(size = 1.2, col = "white") +
  tm_legend(legend.outside=TRUE)


driver_stack <- raster::stack(r_fungi, r_holes, r_holes_var)


MMSF_collars <- readOGR(dsn ="D:/Dropbox/Projects/Indiana/Data/CicadaFlux/GIS/MMSF_Cicada_Pheno/Take_Two",
                       layer="MMSF_Collars_v2")


MMSF_collars

plot(r_fungi)
plot(MMSF_collars, add = TRUE)
unique(Rs_plots$day[Rs_plots$Site=="MorganMonroe"])
Rs_plots_MMSF_1 <- Rs_plots[Rs_plots$Site=="MorganMonroe" & Rs_plots$day=="2021-06-11",]

names(Rs_plots_MMSF_1)
Rs_plots_MMSF_1h <- Rs_plots_MMSF_1[Rs_plots_MMSF_1$Hole_Collar==1 ,c(37, 51, 53, 61)]
Rs_plots_MMSF_1nh <- Rs_plots_MMSF_1[Rs_plots_MMSF_1$Hole_Collar==0 ,c(37, 51, 53, 61)]

Rs_plots_MMSF_1_11 <- merge(Rs_plots_MMSF_1h, Rs_plots_MMSF_1nh, by = c("Pair"))
names(Rs_plots_MMSF_1_11)[4] <- "Holes"; names(Rs_plots_MMSF_1_11)[7] <- "NoHoles" 

plot(Rs_plots_MMSF_1_11$Holes, Rs_plots_MMSF_1_11$NoHoles, 
     xlim = c(0,4), ylim = c(0,4), cex = 2, pch = 16, col = as.factor(Rs_plots_MMSF_1_11$Symbiont.x))
abline(0,1)



Rs_plots_MMSF_2 <- Rs_plots[Rs_plots$Site=="MorganMonroe" & Rs_plots$day=="2021-06-17",]

Rs_plots_MMSF_2h <- Rs_plots_MMSF_2[Rs_plots_MMSF_2$Hole_Collar==1 ,c(37, 51, 53, 61)]
Rs_plots_MMSF_2nh <- Rs_plots_MMSF_2[Rs_plots_MMSF_2$Hole_Collar==0 ,c(37, 51, 53, 61)]

Rs_plots_MMSF_2_11 <- merge(Rs_plots_MMSF_2h, Rs_plots_MMSF_2nh, by = c("Pair"))
names(Rs_plots_MMSF_2_11)[4] <- "Holes"; names(Rs_plots_MMSF_2_11)[7] <- "NoHoles" 

plot(Rs_plots_MMSF_2_11$Holes, Rs_plots_MMSF_2_11$NoHoles, 
     xlim = c(0,4), ylim = c(0,4), cex = 2, pch = 16, col = as.factor(Rs_plots_MMSF_2_11$Symbiont.x))
abline(0,1)


Rs_plots_MMSF_3 <- Rs_plots[Rs_plots$Site=="MorganMonroe" & Rs_plots$day=="2021-06-24",]

Rs_plots_MMSF_3h <- Rs_plots_MMSF_3[Rs_plots_MMSF_3$Hole_Collar==1 ,c(37, 51, 53, 61)]
Rs_plots_MMSF_3nh <- Rs_plots_MMSF_3[Rs_plots_MMSF_3$Hole_Collar==0 ,c(37, 51, 53, 61)]

Rs_plots_MMSF_3_11 <- merge(Rs_plots_MMSF_3h, Rs_plots_MMSF_3nh, by = c("Pair"))
names(Rs_plots_MMSF_3_11)[4] <- "Holes"; names(Rs_plots_MMSF_3_11)[7] <- "NoHoles" 

plot(Rs_plots_MMSF_3_11$Holes, Rs_plots_MMSF_3_11$NoHoles, 
     xlim = c(0,4), ylim = c(0,4), cex = 2, pch = 16, col = as.factor(Rs_plots_MMSF_3_11$Symbiont.x))
abline(0,1)


Rs_plots_MMSF_4 <- Rs_plots[Rs_plots$Site=="MorganMonroe" & Rs_plots$day=="2021-07-08",]

Rs_plots_MMSF_4h <- Rs_plots_MMSF_4[Rs_plots_MMSF_4$Hole_Collar==1 ,c(37, 51, 53, 61)]
Rs_plots_MMSF_4nh <- Rs_plots_MMSF_4[Rs_plots_MMSF_4$Hole_Collar==0 ,c(37, 51, 53, 61)]

Rs_plots_MMSF_4_11 <- merge(Rs_plots_MMSF_4h, Rs_plots_MMSF_4nh, by = c("Pair"))
names(Rs_plots_MMSF_4_11)[4] <- "Holes"; names(Rs_plots_MMSF_4_11)[7] <- "NoHoles" 

plot(Rs_plots_MMSF_4_11$Holes, Rs_plots_MMSF_4_11$NoHoles, 
     xlim = c(0,4), ylim = c(0,4), cex = 2, pch = 16, col = as.factor(Rs_plots_MMSF_4_11$Symbiont.x))
abline(0,1)



Rs_plots_MMSF_5 <- Rs_plots[Rs_plots$Site=="MorganMonroe" & Rs_plots$day=="2021-08-06",]

Rs_plots_MMSF_5h <- Rs_plots_MMSF_5[Rs_plots_MMSF_5$Hole_Collar==1 ,c(37, 51, 53, 61)]
Rs_plots_MMSF_5nh <- Rs_plots_MMSF_5[Rs_plots_MMSF_5$Hole_Collar==0 ,c(37, 51, 53, 61)]

Rs_plots_MMSF_5_11 <- merge(Rs_plots_MMSF_5h, Rs_plots_MMSF_5nh, by = c("Pair"))
names(Rs_plots_MMSF_5_11)[4] <- "Holes"; names(Rs_plots_MMSF_5_11)[7] <- "NoHoles" 

plot(Rs_plots_MMSF_5_11$Holes, Rs_plots_MMSF_5_11$NoHoles, 
     xlim = c(0,4), ylim = c(0,4), cex = 2, pch = 16, col = as.factor(Rs_plots_MMSF_5_11$Symbiont.x))
points(Rs_plots_MMSF_4_11$Holes, Rs_plots_MMSF_4_11$NoHoles, 
       xlim = c(0,4), ylim = c(0,4), cex = 2, pch = 16, col = as.factor(Rs_plots_MMSF_4_11$Symbiont.x))
points(Rs_plots_MMSF_3_11$Holes, Rs_plots_MMSF_3_11$NoHoles, 
       xlim = c(0,4), ylim = c(0,4), cex = 2, pch = 16, col = as.factor(Rs_plots_MMSF_3_11$Symbiont.x))
points(Rs_plots_MMSF_2_11$Holes, Rs_plots_MMSF_2_11$NoHoles, 
       xlim = c(0,4), ylim = c(0,4), cex = 2, pch = 16, col = as.factor(Rs_plots_MMSF_2_11$Symbiont.x))
points(Rs_plots_MMSF_1_11$Holes, Rs_plots_MMSF_1_11$NoHoles, 
       xlim = c(0,4), ylim = c(0,4), cex = 2, pch = 16, col = as.factor(Rs_plots_MMSF_1_11$Symbiont.x))
abline(0,1)


##//Convert Respiration dat a to spatial dataframe
pts <- SpatialPointsDataFrame(coords = Rs_plots_MMSF_1[57:56], data = Rs_plots_MMSF_1,
                              proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))

##//Convert points to UTM
pts_UTM <- spTransform(pts, CRS("+proj=utm +zone=16 +ellps=WGS72 +towgs84=0,0,1.9,0,0,0.814,-0.38 +units=m +no_defs "))
pts_UTM

##//Inverse distance weighted plots
P <- pts_UTM
W <- MMSF_pheno_bound

##//Update bounding box
P@bbox <- W@bbox

##//Create an empty grid where n is the total number of cells
grd              <- as.data.frame(spsample(P, "regular", n=500000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

##//Add P's projection information to the empty grid
proj4string(P) <- proj4string(P) # Temp fix until new proj env is adopted
proj4string(grd) <- proj4string(P)

names(pts_UTM)
pts_UTM

###//Interpolate the grid cells
P.idw <- gstat::idw(Corr_LinFlux ~ 1, P, newdata=grd, idp = 10)

###//Convert to raster object then clip to boundary
r <- raster(P.idw)
r_flux1 <- r
r.m  <- mask(r, W)
r_flux1 <- r.m

##//Plot
tm_shape(r.m) + 
  tm_raster(n = 25, palette = topo.colors(5), auto.palette.mapping = FALSE,
            title="Efflux") + 
  tm_shape(P) + tm_dots(size=1.2, col = "grey") +
  #legend.outside.size(5)+
  tm_legend(legend.outside=TRUE)


############################################################################################
##############################################################################################

##//Convert Respiration dat a to spatial dataframe
pts <- SpatialPointsDataFrame(coords = Rs_plots_MMSF_2[57:56], data = Rs_plots_MMSF_2,
                              proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))

##//Convert points to UTM
pts_UTM <- spTransform(pts, CRS("+proj=utm +zone=16 +ellps=WGS72 +towgs84=0,0,1.9,0,0,0.814,-0.38 +units=m +no_defs "))
pts_UTM

##//Inverse distance weighted plots
P <- pts_UTM
W <- MMSF_pheno_bound

##//Update bounding box
P@bbox <- W@bbox

##//Create an empty grid where n is the total number of cells
grd              <- as.data.frame(spsample(P, "regular", n=500000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

##//Add P's projection information to the empty grid
proj4string(P) <- proj4string(P) # Temp fix until new proj env is adopted
proj4string(grd) <- proj4string(P)

names(pts_UTM)
pts_UTM[pts_UTM@data$n_holes==0,]

###//Interpolate the grid cells
P.idw <- gstat::idw(Corr_LinFlux ~ 1, P, newdata=grd, idp = 10)

###//Convert to raster object then clip to boundary
r <- raster(P.idw)
r_flux2 <- r
r.m  <- mask(r, W)
r_flux2 <- r.m
##//Plot
tm_shape(r.m) + 
  tm_raster(n = 25, palette = topo.colors(5), auto.palette.mapping = FALSE,
            title="Efflux") + 
  tm_shape(P) + tm_dots(size=1.2, col = "grey") +
  #legend.outside.size(5)+
  tm_legend(legend.outside=TRUE)

#####################################################################################
#########################################################################################

##//Convert Respiration dat a to spatial dataframe
pts <- SpatialPointsDataFrame(coords = Rs_plots_MMSF_3[57:56], data = Rs_plots_MMSF_3,
                              proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))

##//Convert points to UTM
pts_UTM <- spTransform(pts, CRS("+proj=utm +zone=16 +ellps=WGS72 +towgs84=0,0,1.9,0,0,0.814,-0.38 +units=m +no_defs "))
pts_UTM

##//Inverse distance weighted plots
P <- pts_UTM
W <- MMSF_pheno_bound

##//Update bounding box
P@bbox <- W@bbox

##//Create an empty grid where n is the total number of cells
grd              <- as.data.frame(spsample(P, "regular", n=500000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

##//Add P's projection information to the empty grid
proj4string(P) <- proj4string(P) # Temp fix until new proj env is adopted
proj4string(grd) <- proj4string(P)

names(pts_UTM)
pts_UTM[pts_UTM@data$n_holes==0,]

###//Interpolate the grid cells
P.idw <- gstat::idw(Corr_LinFlux ~ 1, P, newdata=grd, idp = 10)

###//Convert to raster object then clip to boundary
r <- raster(P.idw)
r_flux3 <- r
r.m  <- mask(r, W)
r_flux3 <- r.m
##//Plot
tm_shape(r.m) + 
  tm_raster(n = 25, palette = topo.colors(5), auto.palette.mapping = FALSE,
            title="Efflux") + 
  tm_shape(P) + tm_dots(size=1.2, col = "grey") +
  #legend.outside.size(5)+
  tm_legend(legend.outside=TRUE)

#####################################################################################
#########################################################################################
##//Convert Respiration dat a to spatial dataframe
pts <- SpatialPointsDataFrame(coords = Rs_plots_MMSF_4[57:56], data = Rs_plots_MMSF_4,
                              proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))

##//Convert points to UTM
pts_UTM <- spTransform(pts, CRS("+proj=utm +zone=16 +ellps=WGS72 +towgs84=0,0,1.9,0,0,0.814,-0.38 +units=m +no_defs "))
pts_UTM

##//Inverse distance weighted plots
P <- pts_UTM
W <- MMSF_pheno_bound

##//Update bounding box
P@bbox <- W@bbox

##//Create an empty grid where n is the total number of cells
grd              <- as.data.frame(spsample(P, "regular", n=500000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

##//Add P's projection information to the empty grid
proj4string(P) <- proj4string(P) # Temp fix until new proj env is adopted
proj4string(grd) <- proj4string(P)

names(pts_UTM)
pts_UTM[pts_UTM@data$n_holes==0,]

###//Interpolate the grid cells
P.idw <- gstat::idw(Corr_LinFlux ~ 1, P, newdata=grd, idp = 10)

###//Convert to raster object then clip to boundary
r <- raster(P.idw)
r_flux4 <- r
r.m  <- mask(r, W)
r_flux4 <- r.m
##//Plot
tm_shape(r.m) + 
  tm_raster(n = 25, palette = topo.colors(5), auto.palette.mapping = FALSE,
            title="Efflux") + 
  tm_shape(P) + tm_dots(size=1.2, col = "grey") +
  #legend.outside.size(5)+
  tm_legend(legend.outside=TRUE)

#####################################################################################
#########################################################################################
##//Convert Respiration dat a to spatial dataframe
pts <- SpatialPointsDataFrame(coords = Rs_plots_MMSF_5[57:56], data = Rs_plots_MMSF_5,
                              proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))

##//Convert points to UTM
pts_UTM <- spTransform(pts, CRS("+proj=utm +zone=16 +ellps=WGS72 +towgs84=0,0,1.9,0,0,0.814,-0.38 +units=m +no_defs "))
pts_UTM

##//Inverse distance weighted plots
P <- pts_UTM
W <- MMSF_pheno_bound

##//Update bounding box
P@bbox <- W@bbox

##//Create an empty grid where n is the total number of cells
grd              <- as.data.frame(spsample(P, "regular", n=500000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

##//Add P's projection information to the empty grid
proj4string(P) <- proj4string(P) # Temp fix until new proj env is adopted
proj4string(grd) <- proj4string(P)

names(pts_UTM)
pts_UTM[pts_UTM@data$n_holes==0,]

###//Interpolate the grid cells
P.idw <- gstat::idw(Corr_LinFlux ~ 1, P, newdata=grd, idp = 10)

###//Convert to raster object then clip to boundary
r <- raster(P.idw)
r_flux5 <- r
r.m  <- mask(r, W)
r_flux5 <- r.m
##//Plot
tm_shape(r.m) + 
  tm_raster(n = 25, palette = topo.colors(5), auto.palette.mapping = FALSE,
            title="Efflux") + 
  tm_shape(P) + tm_dots(size=1.2, col = "grey") +
  #legend.outside.size(5)+
  tm_legend(legend.outside=TRUE)

#####################################################################################
#########################################################################################

model_scales <- r_fungi * r_holes 

r.m  <- mask(model_scales, W)
##//Plot
tm_shape(r.m) + 
  tm_raster(n = 25, palette = topo.colors(5), auto.palette.mapping = FALSE,
            title="Scaled Efflux") + 
  tm_shape(P) + tm_dots(size=1.2, col = "grey") +
  #legend.outside.size(5)+
  tm_legend(legend.outside=TRUE)


plot(model_scales)
r.new = raster::resample(model_scales, r_flux5, "ngb")
plot(r.new)
model5 <- ((r.new * 0.01) * r_flux5) + r_flux5 

r.new = raster::resample(model_scales, r_flux4, "ngb")
model4 <- ((r.new * 0.01) * r_flux4) + r_flux4 

r.new = raster::resample(model_scales, r_flux3, "ngb")
model3 <- ((r.new * 0.01) * r_flux3) + r_flux3 

r.new = raster::resample(model_scales, r_flux2, "ngb")
model2 <- ((r.new * 0.01) * r_flux2) + r_flux2 

r.new = raster::resample(model_scales, r_flux1, "ngb")
model1 <- ((r.new * 0.01) * r_flux1) + r_flux1 


plot(model5)
plot(model4)
plot(model3)
plot(model2)
plot(model1)

EcM_Pdiff_2 <- abs(ECM_1 - ECM_2) / ((ECM_1 + ECM_2) / 2) * 100

pdiff_model1 <- abs(sum(model1@data@values, na.rm = TRUE) - sum(r_flux1@data@values, na.rm = TRUE)) /
  ((sum(model1@data@values, na.rm = TRUE) + sum(r_flux1@data@values, na.rm = TRUE)) / 2) * 100

pdiff_model2 <- abs(sum(model2@data@values, na.rm = TRUE) - sum(r_flux2@data@values, na.rm = TRUE)) /
  ((sum(model2@data@values, na.rm = TRUE) + sum(r_flux2@data@values, na.rm = TRUE)) / 2) * 100

pdiff_model3 <- abs(sum(model3@data@values, na.rm = TRUE) - sum(r_flux3@data@values, na.rm = TRUE)) /
  ((sum(model3@data@values, na.rm = TRUE) + sum(r_flux3@data@values, na.rm = TRUE)) / 2) * 100

pdiff_model4 <- abs(sum(model4@data@values, na.rm = TRUE) - sum(r_flux4@data@values, na.rm = TRUE)) /
  ((sum(model4@data@values, na.rm = TRUE) + sum(r_flux4@data@values, na.rm = TRUE)) / 2) * 100

pdiff_model5 <- abs(sum(model5@data@values, na.rm = TRUE) - sum(r_flux5@data@values, na.rm = TRUE)) /
  ((sum(model5@data@values, na.rm = TRUE) + sum(r_flux5@data@values, na.rm = TRUE)) / 2) * 100



pdiff_model2 <- abs(model2 - r_flux2) / ((model2 + r_flux2) / 2) * 100
pdiff_model3 <- abs(model3 - r_flux3) / ((model3 + r_flux3) / 2) * 100
pdiff_model4 <- abs(model4 - r_flux4) / ((model4 + r_flux4) / 2) * 100
pdiff_model5 <- abs(model5 - r_flux5) / ((model5 + r_flux5) / 2) * 100


mods1 <- raster::stack(r_flux1, model1)
plot(mods1)
fluxStack <- raster::stack(r_flux1, r_flux2, r_flux3, r_flux4, r_flux5)
modelStack <- raster::stack(model1, model2, model3, model4, model5)


plot(model5 - r_flux5)
plot(model3 - r_flux3)


plot(pdiff_model1)
plot(pdiff_model2)
plot(pdiff_model3)
plot(pdiff_model4)
plot(pdiff_model5)

diff1 <- abs(model1 - r_flux1)
diff2 <- abs(model2 - r_flux2)
diff3 <- abs(model3 - r_flux3)
diff4 <- abs(model4 - r_flux4)
diff5 <- abs(model5 - r_flux5)

avg1 <- ((model1 + r_flux1) / 2)
avg2 <- ((model2 + r_flux2) / 2)
avg3 <- ((model3 + r_flux3) / 2)
avg4 <- ((model4 + r_flux4) / 2)
avg5 <- ((model5 + r_flux5) / 2)


diff5mean <- max(diff5@data@values, na.rm = T)
diff4mean <- max(diff4@data@values, na.rm = T)
diff3mean <- max(diff3@data@values, na.rm = T)
diff2mean <- max(diff2@data@values, na.rm = T)
diff1mean <- max(diff1@data@values, na.rm = T)


avg5mean <- median(avg5@data@values, na.rm = T)
avg4mean <- median(avg4@data@values, na.rm = T)
avg3mean <- median(avg3@data@values, na.rm = T)
avg2mean <- median(avg2@data@values, na.rm = T)
avg1mean <- median(avg1@data@values, na.rm = T)

up5mean <- max(avg5@data@values, na.rm = T)
up4mean <- max(avg4@data@values, na.rm = T)
up3mean <- max(avg3@data@values, na.rm = T)
up2mean <- max(avg2@data@values, na.rm = T)
up1mean <- max(avg1@data@values, na.rm = T)

low5mean <- min(avg5@data@values, na.rm = T)
low4mean <- min(avg4@data@values, na.rm = T)
low3mean <- min(avg3@data@values, na.rm = T)
low2mean <- min(avg2@data@values, na.rm = T)
low1mean <- min(avg1@data@values, na.rm = T)



meandiffs5 <- diff5mean / avg5mean * 100
meandiffs4 <- diff4mean / avg4mean * 100
meandiffs3 <- diff3mean / avg3mean * 100
meandiffs2 <- diff2mean / avg2mean * 100
meandiffs1 <- diff1mean / avg1mean * 100

upperdiffs5 <- diff5mean / low5mean * 100
upperdiffs4 <- diff4mean / low4mean * 100
upperdiffs3 <- diff3mean / low3mean * 100
upperdiffs2 <- diff2mean / low2mean * 100
upperdiffs1 <- diff1mean / low1mean * 100

lowerdiffs5 <- diff5mean / up5mean * 100
lowerdiffs4 <- diff4mean / up4mean * 100
lowerdiffs3 <- diff3mean / up3mean * 100
lowerdiffs2 <- diff2mean / up2mean * 100
lowerdiffs1 <- diff1mean / up1mean * 100

unique(Rs_plots$day[Rs_plots$Site=="MorganMonroe"])

pct_df <- data.frame(Date = c("2021-06-11", "2021-06-17", "2021-06-24", "2021-07-08", "2021-08-06"),
                     PctDiff = c(meandiffs1, meandiffs2, meandiffs3, meandiffs4, meandiffs5),
                     LCIPctDiff = c(lowerdiffs1, lowerdiffs2, lowerdiffs3, lowerdiffs4, lowerdiffs5),
                     HCIPctDiff = c(upperdiffs1, upperdiffs2, upperdiffs3, upperdiffs4, upperdiffs5))



ggplot() +
  geom_point(data = pct_df,
               aes(x = Date, y = PctDiff), color = "black", 
               show.legend = TRUE,  size = 10, alpha = 1, shape = 16) +
  geom_errorbar(data = pct_df, aes(x = Date, ymin = LCIPctDiff, ymax = HCIPctDiff), width = 0.25)+
  # scale_color_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  # scale_fill_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  xlab('Date') +
  ylab(expression(paste("Efflux Increases [%]" ))) +
  #annotate("text", x = Rs_plots$Week[1], y = 70, label = "b)    ", size = 20) +
  theme(legend.position="NA",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30),
        axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=0.7))


#########################################################################################################
############################################################################################################

diff5mean <- mean(diff5@data@values, na.rm = T)
diff4mean <- mean(diff4@data@values, na.rm = T)
diff3mean <- mean(diff3@data@values, na.rm = T)
diff2mean <- mean(diff2@data@values, na.rm = T)
diff1mean <- mean(diff1@data@values, na.rm = T)


avg5mean <- mean(avg5@data@values, na.rm = T)
avg4mean <- mean(avg4@data@values, na.rm = T)
avg3mean <- mean(avg3@data@values, na.rm = T)
avg2mean <- mean(avg2@data@values, na.rm = T)
avg1mean <- mean(avg1@data@values, na.rm = T)

up5mean <- max(avg5@data@values, na.rm = T)
up4mean <- max(avg4@data@values, na.rm = T)
up3mean <- max(avg3@data@values, na.rm = T)
up2mean <- max(avg2@data@values, na.rm = T)
up1mean <- max(avg1@data@values, na.rm = T)

low5mean <- min(avg5@data@values, na.rm = T)
low4mean <- min(avg4@data@values, na.rm = T)
low3mean <- min(avg3@data@values, na.rm = T)
low2mean <- min(avg2@data@values, na.rm = T)
low1mean <- min(avg1@data@values, na.rm = T)



meandiffs5 <- diff5mean / avg5mean * 100
meandiffs4 <- diff4mean / avg4mean * 100
meandiffs3 <- diff3mean / avg3mean * 100
meandiffs2 <- diff2mean / avg2mean * 100
meandiffs1 <- diff1mean / avg1mean * 100

upperdiffs5 <- diff5mean / low5mean * 100
upperdiffs4 <- diff4mean / low4mean * 100
upperdiffs3 <- diff3mean / low3mean * 100
upperdiffs2 <- diff2mean / low2mean * 100
upperdiffs1 <- diff1mean / low1mean * 100

lowerdiffs5 <- diff5mean / up5mean * 100
lowerdiffs4 <- diff4mean / up4mean * 100
lowerdiffs3 <- diff3mean / up3mean * 100
lowerdiffs2 <- diff2mean / up2mean * 100
lowerdiffs1 <- diff1mean / up1mean * 100

unique(Rs_plots$day[Rs_plots$Site=="MorganMonroe"])

pct_df <- data.frame(Date = c("2021-06-11", "2021-06-17", "2021-06-24", "2021-07-08", "2021-08-06"),
                     PctDiff = c(meandiffs1, meandiffs2, meandiffs3, meandiffs4, meandiffs5),
                     LCIPctDiff = c(lowerdiffs1, lowerdiffs2, lowerdiffs3, lowerdiffs4, lowerdiffs5),
                     HCIPctDiff = c(upperdiffs1, upperdiffs2, upperdiffs3, upperdiffs4, upperdiffs5))



ggplot() +
  geom_point(data = pct_df,
             aes(x = Date, y = PctDiff), color = "black", 
             show.legend = TRUE,  size = 10, alpha = 1, shape = 16) +
  geom_errorbar(data = pct_df, aes(x = Date, ymin = LCIPctDiff, ymax = HCIPctDiff), width = 0.25)+
  # scale_color_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  # scale_fill_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  xlab('Date') +
  ylab(expression(paste("Efflux Increases [%]" ))) +
  #annotate("text", x = Rs_plots$Week[1], y = 70, label = "b)    ", size = 20) +
  theme(legend.position="NA",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30),
        axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=0.7))



r.m  <- mask(pdiff_model1, W)

median(pdiff_model1@data@values)
median(pdiff_model2@data@values, na.rm = T)
median(pdiff_model3@data@values, na.rm = T)
median(pdiff_model4@data@values, na.rm = T)
median(pdiff_model5@data@values, na.rm = T)

r.m  <- mask(model5, W)
r.m  <- mask(pdiff_model5, W)
r.m  <- mask(pdiff_model1, W)
r.m  <- mask(pdiff_model1, W)
r.m  <- mask(pdiff_model1, W)

##//Plot
tm_shape(r.m) + 
  tm_raster(n = 25, palette = topo.colors(5), auto.palette.mapping = FALSE,
            title="Scaled Efflux") + 
  tm_shape(P) + tm_dots(size=1.2, col = "grey") +
  #legend.outside.size(5)+
  tm_legend(legend.outside=TRUE)



r.m  <- mask(model4, W)
r.m  <- mask(pdiff_model4, W)

##//Plot
tm_shape(r.m) + 
  tm_raster(n = 25, palette = topo.colors(5), auto.palette.mapping = FALSE,
            title="Pct Diff. Scaled Efflux") + 
  tm_shape(P) + tm_dots(size=1.2, col = "grey") +
  #legend.outside.size(5)+
  tm_legend(legend.outside=TRUE)

r.m  <- mask(model3, W)
##//Plot
tm_shape(r.m) + 
  tm_raster(n = 25, palette = topo.colors(5), auto.palette.mapping = FALSE,
            title="Scaled Efflux") + 
  tm_shape(P) + tm_dots(size=1.2, col = "grey") +
  #legend.outside.size(5)+
  tm_legend(legend.outside=TRUE)


r.m  <- mask(model2, W)
##//Plot
tm_shape(r.m) + 
  tm_raster(n = 25, palette = topo.colors(5), auto.palette.mapping = FALSE,
            title="Scaled Efflux") + 
  tm_shape(P) + tm_dots(size=1.2, col = "grey") +
  #legend.outside.size(5)+
  tm_legend(legend.outside=TRUE)


r.m  <- mask(model1, W)
##//Plot
tm_shape(r.m) + 
  tm_raster(n = 25, palette = topo.colors(5), auto.palette.mapping = FALSE,
            title="Scaled Efflux") + 
  tm_shape(P) + tm_dots(size=1.2, col = "grey") +
  #legend.outside.size(5)+
  tm_legend(legend.outside=TRUE)



##//Convert Respiration dat a to spatial dataframe
pts <- SpatialPointsDataFrame(coords = Rs_plots_MMSF_1[57:56], data = Rs_plots_MMSF_1,
                              proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))

##//Convert points to UTM
pts_UTM <- spTransform(pts, CRS("+proj=utm +zone=16 +ellps=WGS72 +towgs84=0,0,1.9,0,0,0.814,-0.38 +units=m +no_defs "))
pts_UTM

#//Extract raster data from points
e <- raster::extract(model_scales, pts_UTM@coords, df = TRUE)

#// Add the Extracted data to points
pts_UTM@data <- cbind(pts_UTM@data,e)

plot(model_scales)
plot(pts_UTM[pts_UTM@data$n_holes==1,],
     cex = pts_UTM@data$Corr_LinFlux , add = TRUE, col = "blue", pch = 16)










Q <- MMSF_collars
Q@coords




plot(MMSF_collars)
names(Rs_plots_MMSF_1)

MMSF_collars2 <- merge(MMSF_collars, Rs_plots_MMSF_1, by = "Collar") 
plot(MMSF_collars2, col = "blue", cex = 2)

##//Inverse distance weighted plots
P <- MMSF_collars2
W <- MMSF_pheno_bound

##//Update bounding box
P@bbox <- W@bbox

##//Create an empty grid where n is the total number of cells
grd              <- as.data.frame(spsample(P, "regular", n=500000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

##//Add P's projection information to the empty grid
proj4string(P) <- proj4string(P) # Temp fix until new proj env is adopted
proj4string(grd) <- proj4string(P)
# 
plot(grd)

names(P)
plot(P)
##//Interpolate the grid cells
P.idw <- gstat::idw(Corr_LinFlux ~ 1, P, newdata=grd, idp = 10.0)

##//Convert to raster object then clip to boundary
r <- raster(P.idw)
r_holes_var <- r
r.m     <- mask(r, W)

##//Plot
tm_shape(r.m) + 
  tm_raster(n = 40,palette = topo.colors(8), auto.palette.mapping = FALSE,
            title="Cicada Hole Variation per Count") + 
  tm_shape(P) + tm_dots(size = 1.2, col = "white") +
  tm_legend(legend.outside=TRUE)







#//Put points into spatial points dataframe ie feature class
pts <- SpatialPointsDataFrame(coords = Rs_plots_MMSF_1[57:56], data = Rs_plots_MMSF_1,
                              proj4string = my.CRS)
plot(pts)

pts@coords

#//Extract raster data from points
e <- extract(r_holes,MMSF_collars@coords, df = TRUE)

#// Add the Extracted data to points
dim(e)
dim(pts)
pts@data <- cbind(pts@data,e)
dim(pts)
View((pts))


plot(driver_stack[[1]])
plot(pts@coords, cex = pts@data$Corr_LinFlux , add = TRUE, col = "blue", pch = 16)



names(Rs_plots_MMSF_1)


my.CRS <- MMSF_pheno@proj4string@projargs

Rs_plots_MMSF_1_shp <- st_as_sf(Rs_plots_MMSF_1, coords = c("Longitude", "Latitude"), crs = my.CRS)

plot(Rs_plots_MMSF_1_shp$geometry)
##//Update bounding box
P@bbox <- W@bbox

##//Create an empty grid where n is the total number of cells
grd              <- as.data.frame(spsample(P, "regular", n=500000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

##//Add P's projection information to the empty grid
proj4string(P) <- proj4string(P) # Temp fix until new proj env is adopted
proj4string(grd) <- proj4string(P)
