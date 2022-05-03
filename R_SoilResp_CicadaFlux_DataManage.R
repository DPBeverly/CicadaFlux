##//Data managmenet for MiniMet Systems at MMSF in conjunction with Sapflux and stem psychrometers

###///This script is ugly and brute force but is functional, will be converted to a loop based approach prior to publication
##//Package management
library(readxl)
library(ggplot2)
library(tidyverse)
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

theme_set(theme_minimal())
GoAvsGo <- c("#6F263D", "#236192", "#A2AAAD", "#000000")


##//Where is the data
setwd("D:/Dropbox/Projects/Indiana/Data/CicadaFlux/SoilFlux/RawData")


##//List of files in the directory
Flist <- dir(pattern = ".81x")

##//Complied dataframe
DOut <- data.frame()
Fdat <- data.frame()
#for(j in 1:length(Flist)){
##//Reading in the files by lines so that we can rip apart the irregular vetor formats
text <- readLines(Flist[1])
length(Flist)
##//Pulling meta data and results
maybe <-  text %>% 
  enframe(name = NULL) %>%
  filter(str_detect(value, "^.")) %>%   ##// Regular expression grabbing infromation for each line with two  vectors 
  separate(value, sep = "\t", into = c("Parameter", "Value"))

##//LiCor Data and Meta data
LiCor_dat <- data.frame(File = maybe$Value[maybe$Parameter=="File Name:"],
                      Collar = maybe$Value[maybe$Parameter=="Comments:"],
                      Obs = as.numeric(maybe$Value[maybe$Parameter=="Obs#:"]),
                      VCham = as.numeric(maybe$Value[maybe$Parameter=="Vcham:"]), 
                      Offset = as.numeric(maybe$Value[maybe$Parameter=="Offset:"]),
                      Area = as.numeric(maybe$Value[maybe$Parameter=="Area:"]),
                      VTotal = as.numeric(maybe$Value[maybe$Parameter=="Vtotal:"]),
                      FitStatus = maybe$Value[maybe$Parameter=="CrvFitStatus:"],
                      ExpFlux = as.numeric(maybe$Value[maybe$Parameter=="Exp_Flux:"]),
                      ExpFluxCV = as.numeric(maybe$Value[maybe$Parameter=="Exp_FluxCV:"]),
                      Exp_dCdt = as.numeric(maybe$Value[maybe$Parameter=="Exp_dCdry/dt:"]),
                      ExpR2 = as.numeric(maybe$Value[maybe$Parameter=="Exp_R2:"]),
                      LinFlux = as.numeric(maybe$Value[maybe$Parameter=="Lin_Flux:"]),
                      LinFluxCV = as.numeric(maybe$Value[maybe$Parameter=="Lin_FluxCV:"]),
                      Lin_dCdt = as.numeric(maybe$Value[maybe$Parameter=="Lin_dCdry/dt:"]),
                      LinR2 = as.numeric(maybe$Value[maybe$Parameter=="Lin_R2:"]),
                      LinFluxSE = as.numeric(maybe$Value[maybe$Parameter=="Lin_SE:"]))

###//Raw fluxes to long format dataframe
maybeData <-  text %>% 
  enframe(name = NULL) %>%
  filter(str_detect(value, "^\\d")) %>%  ##// Regular expression searching for start of line begining with a number 
  separate(value, sep = "\t", into = c("Type",	"Etime",	"Date",	"Tcham",	"Pressure",	"H2O", "CO2",	"Cdry",	"Tbench",	"RH",	"Tboard",	"Vin",
                                       "CO2ABS",	"H2OABS",	"Hour", "DOY", "RAWCO2", "RAWCO2REF",	"RAWH2O",	"RAWH2OREF"))

##//Updating the classes
maybeData$Date_ct <- as.POSIXct(strptime(maybeData$Date, format = "%Y-%m-%d %H:%M:%S"), tz="EST")
maybeData$Type <- as.numeric(maybeData$Type); maybeData$Etime <- as.numeric(maybeData$Etime); maybeData$Tcham <- as.numeric(maybeData$Tcham);
maybeData$Pressure <- as.numeric(maybeData$Pressure); maybeData$H2O <- as.numeric(maybeData$H2O); maybeData$CO2 <- as.numeric(maybeData$CO2);
maybeData$Cdry <- as.numeric(maybeData$Cdry); maybeData$Tbench <- as.numeric(maybeData$Tbench); maybeData$RH <- as.numeric(maybeData$RH);
maybeData$Tboard <- as.numeric(maybeData$Tboard); maybeData$Vin <- as.numeric(maybeData$Vin); maybeData$CO2ABS <- as.numeric(maybeData$CO2ABS);
maybeData$H2OABS <- as.numeric(maybeData$H2OABS); maybeData$Hour <- as.numeric(maybeData$Hour); maybeData$DOY <- as.numeric(maybeData$DOY);
maybeData$RAWCO2 <- as.numeric(maybeData$RAWCO2); maybeData$RAWCO2REF <- as.numeric(maybeData$RAWCO2REF);
maybeData$RAWH2O <- as.numeric(maybeData$RAWH2O); maybeData$RAWH2OREF <- as.numeric(maybeData$RAWH2OREF);


##//Reconstructing data to be in long format 
##//Using time of last measurement to seperate observations
endMes <- maybeData$Date_ct[maybeData$Type==4]


  tests <- maybeData[maybeData$Date_ct<=endMes[1],]
  tests$Collar <- LiCor_dat$Collar[1]
  tests$File <- LiCor_dat$File[1]
  tests$OBS <- LiCor_dat$Obs[1]
  tests$VCham <- LiCor_dat$VCham[1]
  tests$Offset <- LiCor_dat$Offset[1]
  tests$Area <- LiCor_dat$Area[1]
  tests$VTotal <- LiCor_dat$VTotal[1]
  tests$ExpFlux <- LiCor_dat$ExpFlux[1]
  tests$ExpFluxCV <- LiCor_dat$ExpFluxCV[1]
  tests$Exp_dCdt <- LiCor_dat$Exp_dCdt[1]
  tests$ExpR2 <- LiCor_dat$ExpR2[1]
  tests$LinFlux <- LiCor_dat$LinFlux[1]
  tests$LinFluxCV <- LiCor_dat$LinFluxCV[1]
  tests$Lin_dCdt <- LiCor_dat$Lin_dCdt[1]
  tests$LinR2 <- LiCor_dat$LinR2[1]
  tests$LinFluxSE <- LiCor_dat$LinFluxSE[1]
  
  DOut <- rbind(DOut, tests)
  
for(i in 2:length(endMes)){

tests <- maybeData[maybeData$Date_ct<=endMes[i] & maybeData$Date_ct>endMes[i-1],]
tests$Collar <- LiCor_dat$Collar[i]
tests$File <- LiCor_dat$File[i]
tests$OBS <- LiCor_dat$Obs[i]
tests$VCham <- LiCor_dat$VCham[i]
tests$Offset <- LiCor_dat$Offset[i]
tests$Area <- LiCor_dat$Area[i]
tests$VTotal <- LiCor_dat$VTotal[i]
tests$ExpFlux <- LiCor_dat$ExpFlux[i]
tests$ExpFluxCV <- LiCor_dat$ExpFluxCV[i]
tests$Exp_dCdt <- LiCor_dat$Exp_dCdt[i]
tests$ExpR2 <- LiCor_dat$ExpR2[i]
tests$LinFlux <- LiCor_dat$LinFlux[i]
tests$LinFluxCV <- LiCor_dat$LinFluxCV[i]
tests$Lin_dCdt <- LiCor_dat$Lin_dCdt[i]
tests$LinR2 <- LiCor_dat$LinR2[i]
tests$LinFluxSE <- LiCor_dat$LinFluxSE[i]


DOut <- rbind(DOut, tests)


}

Fdat <- rbind(Fdat, DOut)



#############################################################################################################

##//Reading in the files by lines so that we can rip apart the irregular vetor formats
text <- readLines(Flist[2])

##//Pulling meta data and results
maybe <-  text %>% 
  enframe(name = NULL) %>%
  filter(str_detect(value, "^.")) %>%   ##// Regular expression grabbing infromation for each line with two  vectors 
  separate(value, sep = "\t", into = c("Parameter", "Value"))

##//LiCor Data and Meta data
LiCor_dat <- data.frame(File = maybe$Value[maybe$Parameter=="File Name:"],
                        Collar = maybe$Value[maybe$Parameter=="Comments:"],
                        Obs = as.numeric(maybe$Value[maybe$Parameter=="Obs#:"]),
                        VCham = as.numeric(maybe$Value[maybe$Parameter=="Vcham:"]), 
                        Offset = as.numeric(maybe$Value[maybe$Parameter=="Offset:"]),
                        Area = as.numeric(maybe$Value[maybe$Parameter=="Area:"]),
                        VTotal = as.numeric(maybe$Value[maybe$Parameter=="Vtotal:"]),
                        FitStatus = maybe$Value[maybe$Parameter=="CrvFitStatus:"],
                        ExpFlux = as.numeric(maybe$Value[maybe$Parameter=="Exp_Flux:"]),
                        ExpFluxCV = as.numeric(maybe$Value[maybe$Parameter=="Exp_FluxCV:"]),
                        Exp_dCdt = as.numeric(maybe$Value[maybe$Parameter=="Exp_dCdry/dt:"]),
                        ExpR2 = as.numeric(maybe$Value[maybe$Parameter=="Exp_R2:"]),
                        LinFlux = as.numeric(maybe$Value[maybe$Parameter=="Lin_Flux:"]),
                        LinFluxCV = as.numeric(maybe$Value[maybe$Parameter=="Lin_FluxCV:"]),
                        Lin_dCdt = as.numeric(maybe$Value[maybe$Parameter=="Lin_dCdry/dt:"]),
                        LinR2 = as.numeric(maybe$Value[maybe$Parameter=="Lin_R2:"]),
                        LinFluxSE = as.numeric(maybe$Value[maybe$Parameter=="Lin_SE:"]))

###//Raw fluxes to long format dataframe
maybeData <-  text %>% 
  enframe(name = NULL) %>%
  filter(str_detect(value, "^\\d")) %>%  ##// Regular expression searching for start of line begining with a number 
  separate(value, sep = "\t", into = c("Type",	"Etime",	"Date",	"Tcham",	"Pressure",	"H2O", "CO2",	"Cdry",	"Tbench",	"RH",	"Tboard",	"Vin",
                                       "CO2ABS",	"H2OABS",	"Hour", "DOY", "RAWCO2", "RAWCO2REF",	"RAWH2O",	"RAWH2OREF"))

##//Updating the classes
maybeData$Date_ct <- as.POSIXct(strptime(maybeData$Date, format = "%Y-%m-%d %H:%M:%S"), tz="EST")
maybeData$Type <- as.numeric(maybeData$Type); maybeData$Etime <- as.numeric(maybeData$Etime); maybeData$Tcham <- as.numeric(maybeData$Tcham);
maybeData$Pressure <- as.numeric(maybeData$Pressure); maybeData$H2O <- as.numeric(maybeData$H2O); maybeData$CO2 <- as.numeric(maybeData$CO2);
maybeData$Cdry <- as.numeric(maybeData$Cdry); maybeData$Tbench <- as.numeric(maybeData$Tbench); maybeData$RH <- as.numeric(maybeData$RH);
maybeData$Tboard <- as.numeric(maybeData$Tboard); maybeData$Vin <- as.numeric(maybeData$Vin); maybeData$CO2ABS <- as.numeric(maybeData$CO2ABS);
maybeData$H2OABS <- as.numeric(maybeData$H2OABS); maybeData$Hour <- as.numeric(maybeData$Hour); maybeData$DOY <- as.numeric(maybeData$DOY);
maybeData$RAWCO2 <- as.numeric(maybeData$RAWCO2); maybeData$RAWCO2REF <- as.numeric(maybeData$RAWCO2REF);
maybeData$RAWH2O <- as.numeric(maybeData$RAWH2O); maybeData$RAWH2OREF <- as.numeric(maybeData$RAWH2OREF);


##//Reconstructing data to be in long format 
##//Using time of last measurement to seperate observations
endMes <- maybeData$Date_ct[maybeData$Type==4]


tests <- maybeData[maybeData$Date_ct<=endMes[1],]
tests$Collar <- LiCor_dat$Collar[1]
tests$File <- LiCor_dat$File[1]
tests$OBS <- LiCor_dat$Obs[1]
tests$VCham <- LiCor_dat$VCham[1]
tests$Offset <- LiCor_dat$Offset[1]
tests$Area <- LiCor_dat$Area[1]
tests$VTotal <- LiCor_dat$VTotal[1]
tests$ExpFlux <- LiCor_dat$ExpFlux[1]
tests$ExpFluxCV <- LiCor_dat$ExpFluxCV[1]
tests$Exp_dCdt <- LiCor_dat$Exp_dCdt[1]
tests$ExpR2 <- LiCor_dat$ExpR2[1]
tests$LinFlux <- LiCor_dat$LinFlux[1]
tests$LinFluxCV <- LiCor_dat$LinFluxCV[1]
tests$Lin_dCdt <- LiCor_dat$Lin_dCdt[1]
tests$LinR2 <- LiCor_dat$LinR2[1]
tests$LinFluxSE <- LiCor_dat$LinFluxSE[1]

DOut <- rbind(DOut, tests)

for(i in 2:length(endMes)){
    
  tests <- maybeData[maybeData$Date_ct<=endMes[i] & maybeData$Date_ct>endMes[i-1],]
  tests$Collar <- LiCor_dat$Collar[i]
  tests$File <- LiCor_dat$File[i]
  tests$OBS <- LiCor_dat$Obs[i]
  tests$VCham <- LiCor_dat$VCham[i]
  tests$Offset <- LiCor_dat$Offset[i]
  tests$Area <- LiCor_dat$Area[i]
  tests$VTotal <- LiCor_dat$VTotal[i]
  tests$ExpFlux <- LiCor_dat$ExpFlux[i]
  tests$ExpFluxCV <- LiCor_dat$ExpFluxCV[i]
  tests$Exp_dCdt <- LiCor_dat$Exp_dCdt[i]
  tests$ExpR2 <- LiCor_dat$ExpR2[i]
  tests$LinFlux <- LiCor_dat$LinFlux[i]
  tests$LinFluxCV <- LiCor_dat$LinFluxCV[i]
  tests$Lin_dCdt <- LiCor_dat$Lin_dCdt[i]
  tests$LinR2 <- LiCor_dat$LinR2[i]
  tests$LinFluxSE <- LiCor_dat$LinFluxSE[i]
  
  
  DOut <- rbind(DOut, tests)
  
  
}

Fdat <- rbind(Fdat, DOut)


#############################################################################################################

##//Reading in the files by lines so that we can rip apart the irregular vetor formats
text <- readLines(Flist[3])

##//Pulling meta data and results
maybe <-  text %>% 
  enframe(name = NULL) %>%
  filter(str_detect(value, "^.")) %>%   ##// Regular expression grabbing infromation for each line with two  vectors 
  separate(value, sep = "\t", into = c("Parameter", "Value"))

##//LiCor Data and Meta data
LiCor_dat <- data.frame(File = maybe$Value[maybe$Parameter=="File Name:"],
                        Collar = maybe$Value[maybe$Parameter=="Comments:"],
                        Obs = as.numeric(maybe$Value[maybe$Parameter=="Obs#:"]),
                        VCham = as.numeric(maybe$Value[maybe$Parameter=="Vcham:"]), 
                        Offset = as.numeric(maybe$Value[maybe$Parameter=="Offset:"]),
                        Area = as.numeric(maybe$Value[maybe$Parameter=="Area:"]),
                        VTotal = as.numeric(maybe$Value[maybe$Parameter=="Vtotal:"]),
                        FitStatus = maybe$Value[maybe$Parameter=="CrvFitStatus:"],
                        ExpFlux = as.numeric(maybe$Value[maybe$Parameter=="Exp_Flux:"]),
                        ExpFluxCV = as.numeric(maybe$Value[maybe$Parameter=="Exp_FluxCV:"]),
                        Exp_dCdt = as.numeric(maybe$Value[maybe$Parameter=="Exp_dCdry/dt:"]),
                        ExpR2 = as.numeric(maybe$Value[maybe$Parameter=="Exp_R2:"]),
                        LinFlux = as.numeric(maybe$Value[maybe$Parameter=="Lin_Flux:"]),
                        LinFluxCV = as.numeric(maybe$Value[maybe$Parameter=="Lin_FluxCV:"]),
                        Lin_dCdt = as.numeric(maybe$Value[maybe$Parameter=="Lin_dCdry/dt:"]),
                        LinR2 = as.numeric(maybe$Value[maybe$Parameter=="Lin_R2:"]),
                        LinFluxSE = as.numeric(maybe$Value[maybe$Parameter=="Lin_SE:"]))

###//Raw fluxes to long format dataframe
maybeData <-  text %>% 
  enframe(name = NULL) %>%
  filter(str_detect(value, "^\\d")) %>%  ##// Regular expression searching for start of line begining with a number 
  separate(value, sep = "\t", into = c("Type",	"Etime",	"Date",	"Tcham",	"Pressure",	"H2O", "CO2",	"Cdry",	"Tbench",	"RH",	"Tboard",	"Vin",
                                       "CO2ABS",	"H2OABS",	"Hour", "DOY", "RAWCO2", "RAWCO2REF",	"RAWH2O",	"RAWH2OREF"))

##//Updating the classes
maybeData$Date_ct <- as.POSIXct(strptime(maybeData$Date, format = "%Y-%m-%d %H:%M:%S"), tz="EST")
maybeData$Type <- as.numeric(maybeData$Type); maybeData$Etime <- as.numeric(maybeData$Etime); maybeData$Tcham <- as.numeric(maybeData$Tcham);
maybeData$Pressure <- as.numeric(maybeData$Pressure); maybeData$H2O <- as.numeric(maybeData$H2O); maybeData$CO2 <- as.numeric(maybeData$CO2);
maybeData$Cdry <- as.numeric(maybeData$Cdry); maybeData$Tbench <- as.numeric(maybeData$Tbench); maybeData$RH <- as.numeric(maybeData$RH);
maybeData$Tboard <- as.numeric(maybeData$Tboard); maybeData$Vin <- as.numeric(maybeData$Vin); maybeData$CO2ABS <- as.numeric(maybeData$CO2ABS);
maybeData$H2OABS <- as.numeric(maybeData$H2OABS); maybeData$Hour <- as.numeric(maybeData$Hour); maybeData$DOY <- as.numeric(maybeData$DOY);
maybeData$RAWCO2 <- as.numeric(maybeData$RAWCO2); maybeData$RAWCO2REF <- as.numeric(maybeData$RAWCO2REF);
maybeData$RAWH2O <- as.numeric(maybeData$RAWH2O); maybeData$RAWH2OREF <- as.numeric(maybeData$RAWH2OREF);


##//Reconstructing data to be in long format 
##//Using time of last measurement to seperate observations
endMes <- maybeData$Date_ct[maybeData$Type==4]

tests <- maybeData[maybeData$Date_ct<=endMes[1],]
tests$Collar <- LiCor_dat$Collar[1]
tests$File <- LiCor_dat$File[1]
tests$OBS <- LiCor_dat$Obs[1]
tests$VCham <- LiCor_dat$VCham[1]
tests$Offset <- LiCor_dat$Offset[1]
tests$Area <- LiCor_dat$Area[1]
tests$VTotal <- LiCor_dat$VTotal[1]
tests$ExpFlux <- LiCor_dat$ExpFlux[1]
tests$ExpFluxCV <- LiCor_dat$ExpFluxCV[1]
tests$Exp_dCdt <- LiCor_dat$Exp_dCdt[1]
tests$ExpR2 <- LiCor_dat$ExpR2[1]
tests$LinFlux <- LiCor_dat$LinFlux[1]
tests$LinFluxCV <- LiCor_dat$LinFluxCV[1]
tests$Lin_dCdt <- LiCor_dat$Lin_dCdt[1]
tests$LinR2 <- LiCor_dat$LinR2[1]
tests$LinFluxSE <- LiCor_dat$LinFluxSE[1]

DOut <- rbind(DOut, tests)

for(i in 2:length(endMes)){
  
  tests <- maybeData[maybeData$Date_ct<=endMes[i] & maybeData$Date_ct>endMes[i-1],]
  tests$Collar <- LiCor_dat$Collar[i]
  tests$File <- LiCor_dat$File[i]
  tests$OBS <- LiCor_dat$Obs[i]
  tests$VCham <- LiCor_dat$VCham[i]
  tests$Offset <- LiCor_dat$Offset[i]
  tests$Area <- LiCor_dat$Area[i]
  tests$VTotal <- LiCor_dat$VTotal[i]
  tests$ExpFlux <- LiCor_dat$ExpFlux[i]
  tests$ExpFluxCV <- LiCor_dat$ExpFluxCV[i]
  tests$Exp_dCdt <- LiCor_dat$Exp_dCdt[i]
  tests$ExpR2 <- LiCor_dat$ExpR2[i]
  tests$LinFlux <- LiCor_dat$LinFlux[i]
  tests$LinFluxCV <- LiCor_dat$LinFluxCV[i]
  tests$Lin_dCdt <- LiCor_dat$Lin_dCdt[i]
  tests$LinR2 <- LiCor_dat$LinR2[i]
  tests$LinFluxSE <- LiCor_dat$LinFluxSE[i]
  
  
  DOut <- rbind(DOut, tests)
  
  
}

Fdat <- rbind(Fdat, DOut)

#############################################################################################################

##//Reading in the files by lines so that we can rip apart the irregular vetor formats
text <- readLines(Flist[4])

##//Pulling meta data and results
maybe <-  text %>% 
  enframe(name = NULL) %>%
  filter(str_detect(value, "^.")) %>%   ##// Regular expression grabbing infromation for each line with two  vectors 
  separate(value, sep = "\t", into = c("Parameter", "Value"))

##//LiCor Data and Meta data
LiCor_dat <- data.frame(File = maybe$Value[maybe$Parameter=="File Name:"],
                        Collar = maybe$Value[maybe$Parameter=="Comments:"],
                        Obs = as.numeric(maybe$Value[maybe$Parameter=="Obs#:"]),
                        VCham = as.numeric(maybe$Value[maybe$Parameter=="Vcham:"]), 
                        Offset = as.numeric(maybe$Value[maybe$Parameter=="Offset:"]),
                        Area = as.numeric(maybe$Value[maybe$Parameter=="Area:"]),
                        VTotal = as.numeric(maybe$Value[maybe$Parameter=="Vtotal:"]),
                        FitStatus = maybe$Value[maybe$Parameter=="CrvFitStatus:"],
                        ExpFlux = as.numeric(maybe$Value[maybe$Parameter=="Exp_Flux:"]),
                        ExpFluxCV = as.numeric(maybe$Value[maybe$Parameter=="Exp_FluxCV:"]),
                        Exp_dCdt = as.numeric(maybe$Value[maybe$Parameter=="Exp_dCdry/dt:"]),
                        ExpR2 = as.numeric(maybe$Value[maybe$Parameter=="Exp_R2:"]),
                        LinFlux = as.numeric(maybe$Value[maybe$Parameter=="Lin_Flux:"]),
                        LinFluxCV = as.numeric(maybe$Value[maybe$Parameter=="Lin_FluxCV:"]),
                        Lin_dCdt = as.numeric(maybe$Value[maybe$Parameter=="Lin_dCdry/dt:"]),
                        LinR2 = as.numeric(maybe$Value[maybe$Parameter=="Lin_R2:"]),
                        LinFluxSE = as.numeric(maybe$Value[maybe$Parameter=="Lin_SE:"]))

###//Raw fluxes to long format dataframe
maybeData <-  text %>% 
  enframe(name = NULL) %>%
  filter(str_detect(value, "^\\d")) %>%  ##// Regular expression searching for start of line begining with a number 
  separate(value, sep = "\t", into = c("Type",	"Etime",	"Date",	"Tcham",	"Pressure",	"H2O", "CO2",	"Cdry",	"Tbench",	"RH",	"Tboard",	"Vin",
                                       "CO2ABS",	"H2OABS",	"Hour", "DOY", "RAWCO2", "RAWCO2REF",	"RAWH2O",	"RAWH2OREF"))

##//Updating the classes
maybeData$Date_ct <- as.POSIXct(strptime(maybeData$Date, format = "%Y-%m-%d %H:%M:%S"), tz="EST")
maybeData$Type <- as.numeric(maybeData$Type); maybeData$Etime <- as.numeric(maybeData$Etime); maybeData$Tcham <- as.numeric(maybeData$Tcham);
maybeData$Pressure <- as.numeric(maybeData$Pressure); maybeData$H2O <- as.numeric(maybeData$H2O); maybeData$CO2 <- as.numeric(maybeData$CO2);
maybeData$Cdry <- as.numeric(maybeData$Cdry); maybeData$Tbench <- as.numeric(maybeData$Tbench); maybeData$RH <- as.numeric(maybeData$RH);
maybeData$Tboard <- as.numeric(maybeData$Tboard); maybeData$Vin <- as.numeric(maybeData$Vin); maybeData$CO2ABS <- as.numeric(maybeData$CO2ABS);
maybeData$H2OABS <- as.numeric(maybeData$H2OABS); maybeData$Hour <- as.numeric(maybeData$Hour); maybeData$DOY <- as.numeric(maybeData$DOY);
maybeData$RAWCO2 <- as.numeric(maybeData$RAWCO2); maybeData$RAWCO2REF <- as.numeric(maybeData$RAWCO2REF);
maybeData$RAWH2O <- as.numeric(maybeData$RAWH2O); maybeData$RAWH2OREF <- as.numeric(maybeData$RAWH2OREF);


##//Reconstructing data to be in long format 


##//Using time of last measurement to seperate observations
endMes <- maybeData$Date_ct[maybeData$Type==4]


tests <- maybeData[maybeData$Date_ct<=endMes[1],]
tests$Collar <- LiCor_dat$Collar[1]
tests$File <- LiCor_dat$File[1]
tests$OBS <- LiCor_dat$Obs[1]
tests$VCham <- LiCor_dat$VCham[1]
tests$Offset <- LiCor_dat$Offset[1]
tests$Area <- LiCor_dat$Area[1]
tests$VTotal <- LiCor_dat$VTotal[1]
tests$ExpFlux <- LiCor_dat$ExpFlux[1]
tests$ExpFluxCV <- LiCor_dat$ExpFluxCV[1]
tests$Exp_dCdt <- LiCor_dat$Exp_dCdt[1]
tests$ExpR2 <- LiCor_dat$ExpR2[1]
tests$LinFlux <- LiCor_dat$LinFlux[1]
tests$LinFluxCV <- LiCor_dat$LinFluxCV[1]
tests$Lin_dCdt <- LiCor_dat$Lin_dCdt[1]
tests$LinR2 <- LiCor_dat$LinR2[1]
tests$LinFluxSE <- LiCor_dat$LinFluxSE[1]

DOut <- rbind(DOut, tests)

for(i in 2:length(endMes)){
  
  tests <- maybeData[maybeData$Date_ct<=endMes[i] & maybeData$Date_ct>endMes[i-1],]
  tests$Collar <- LiCor_dat$Collar[i]
  tests$File <- LiCor_dat$File[i]
  tests$OBS <- LiCor_dat$Obs[i]
  tests$VCham <- LiCor_dat$VCham[i]
  tests$Offset <- LiCor_dat$Offset[i]
  tests$Area <- LiCor_dat$Area[i]
  tests$VTotal <- LiCor_dat$VTotal[i]
  tests$ExpFlux <- LiCor_dat$ExpFlux[i]
  tests$ExpFluxCV <- LiCor_dat$ExpFluxCV[i]
  tests$Exp_dCdt <- LiCor_dat$Exp_dCdt[i]
  tests$ExpR2 <- LiCor_dat$ExpR2[i]
  tests$LinFlux <- LiCor_dat$LinFlux[i]
  tests$LinFluxCV <- LiCor_dat$LinFluxCV[i]
  tests$Lin_dCdt <- LiCor_dat$Lin_dCdt[i]
  tests$LinR2 <- LiCor_dat$LinR2[i]
  tests$LinFluxSE <- LiCor_dat$LinFluxSE[i]
  
  
  DOut <- rbind(DOut, tests)
  
  
}

Fdat <- rbind(Fdat, DOut)

##############################################################################################################################
#############################################################################################################

##//Reading in the files by lines so that we can rip apart the irregular vetor formats
text <- readLines(Flist[5])

##//Pulling meta data and results
maybe <-  text %>% 
  enframe(name = NULL) %>%
  filter(str_detect(value, "^.")) %>%   ##// Regular expression grabbing infromation for each line with two  vectors 
  separate(value, sep = "\t", into = c("Parameter", "Value"))

##//LiCor Data and Meta data
LiCor_dat <- data.frame(File = maybe$Value[maybe$Parameter=="File Name:"],
                        Collar = maybe$Value[maybe$Parameter=="Comments:"],
                        Obs = as.numeric(maybe$Value[maybe$Parameter=="Obs#:"]),
                        VCham = as.numeric(maybe$Value[maybe$Parameter=="Vcham:"]), 
                        Offset = as.numeric(maybe$Value[maybe$Parameter=="Offset:"]),
                        Area = as.numeric(maybe$Value[maybe$Parameter=="Area:"]),
                        VTotal = as.numeric(maybe$Value[maybe$Parameter=="Vtotal:"]),
                        FitStatus = maybe$Value[maybe$Parameter=="CrvFitStatus:"],
                        ExpFlux = as.numeric(maybe$Value[maybe$Parameter=="Exp_Flux:"]),
                        ExpFluxCV = as.numeric(maybe$Value[maybe$Parameter=="Exp_FluxCV:"]),
                        Exp_dCdt = as.numeric(maybe$Value[maybe$Parameter=="Exp_dCdry/dt:"]),
                        ExpR2 = as.numeric(maybe$Value[maybe$Parameter=="Exp_R2:"]),
                        LinFlux = as.numeric(maybe$Value[maybe$Parameter=="Lin_Flux:"]),
                        LinFluxCV = as.numeric(maybe$Value[maybe$Parameter=="Lin_FluxCV:"]),
                        Lin_dCdt = as.numeric(maybe$Value[maybe$Parameter=="Lin_dCdry/dt:"]),
                        LinR2 = as.numeric(maybe$Value[maybe$Parameter=="Lin_R2:"]),
                        LinFluxSE = as.numeric(maybe$Value[maybe$Parameter=="Lin_SE:"]))

###//Raw fluxes to long format dataframe
maybeData <-  text %>% 
  enframe(name = NULL) %>%
  filter(str_detect(value, "^\\d")) %>%  ##// Regular expression searching for start of line begining with a number 
  separate(value, sep = "\t", into = c("Type",	"Etime",	"Date",	"Tcham",	"Pressure",	"H2O", "CO2",	"Cdry",	"Tbench",	"RH",	"Tboard",	"Vin",
                                       "CO2ABS",	"H2OABS",	"Hour", "DOY", "RAWCO2", "RAWCO2REF",	"RAWH2O",	"RAWH2OREF"))

##//Updating the classes
maybeData$Date_ct <- as.POSIXct(strptime(maybeData$Date, format = "%Y-%m-%d %H:%M:%S"), tz="EST")
maybeData$Type <- as.numeric(maybeData$Type); maybeData$Etime <- as.numeric(maybeData$Etime); maybeData$Tcham <- as.numeric(maybeData$Tcham);
maybeData$Pressure <- as.numeric(maybeData$Pressure); maybeData$H2O <- as.numeric(maybeData$H2O); maybeData$CO2 <- as.numeric(maybeData$CO2);
maybeData$Cdry <- as.numeric(maybeData$Cdry); maybeData$Tbench <- as.numeric(maybeData$Tbench); maybeData$RH <- as.numeric(maybeData$RH);
maybeData$Tboard <- as.numeric(maybeData$Tboard); maybeData$Vin <- as.numeric(maybeData$Vin); maybeData$CO2ABS <- as.numeric(maybeData$CO2ABS);
maybeData$H2OABS <- as.numeric(maybeData$H2OABS); maybeData$Hour <- as.numeric(maybeData$Hour); maybeData$DOY <- as.numeric(maybeData$DOY);
maybeData$RAWCO2 <- as.numeric(maybeData$RAWCO2); maybeData$RAWCO2REF <- as.numeric(maybeData$RAWCO2REF);
maybeData$RAWH2O <- as.numeric(maybeData$RAWH2O); maybeData$RAWH2OREF <- as.numeric(maybeData$RAWH2OREF);


##//Reconstructing data to be in long format 
##//Using time of last measurement to seperate observations
endMes <- maybeData$Date_ct[maybeData$Type==4]

tests <- maybeData[maybeData$Date_ct<=endMes[1],]
tests$Collar <- LiCor_dat$Collar[1]
tests$File <- LiCor_dat$File[1]
tests$OBS <- LiCor_dat$Obs[1]
tests$VCham <- LiCor_dat$VCham[1]
tests$Offset <- LiCor_dat$Offset[1]
tests$Area <- LiCor_dat$Area[1]
tests$VTotal <- LiCor_dat$VTotal[1]
tests$ExpFlux <- LiCor_dat$ExpFlux[1]
tests$ExpFluxCV <- LiCor_dat$ExpFluxCV[1]
tests$Exp_dCdt <- LiCor_dat$Exp_dCdt[1]
tests$ExpR2 <- LiCor_dat$ExpR2[1]
tests$LinFlux <- LiCor_dat$LinFlux[1]
tests$LinFluxCV <- LiCor_dat$LinFluxCV[1]
tests$Lin_dCdt <- LiCor_dat$Lin_dCdt[1]
tests$LinR2 <- LiCor_dat$LinR2[1]
tests$LinFluxSE <- LiCor_dat$LinFluxSE[1]

DOut <- rbind(DOut, tests)

for(i in 2:length(endMes)){
  
  tests <- maybeData[maybeData$Date_ct<=endMes[i] & maybeData$Date_ct>endMes[i-1],]
  tests$Collar <- LiCor_dat$Collar[i]
  tests$File <- LiCor_dat$File[i]
  tests$OBS <- LiCor_dat$Obs[i]
  tests$VCham <- LiCor_dat$VCham[i]
  tests$Offset <- LiCor_dat$Offset[i]
  tests$Area <- LiCor_dat$Area[i]
  tests$VTotal <- LiCor_dat$VTotal[i]
  tests$ExpFlux <- LiCor_dat$ExpFlux[i]
  tests$ExpFluxCV <- LiCor_dat$ExpFluxCV[i]
  tests$Exp_dCdt <- LiCor_dat$Exp_dCdt[i]
  tests$ExpR2 <- LiCor_dat$ExpR2[i]
  tests$LinFlux <- LiCor_dat$LinFlux[i]
  tests$LinFluxCV <- LiCor_dat$LinFluxCV[i]
  tests$Lin_dCdt <- LiCor_dat$Lin_dCdt[i]
  tests$LinR2 <- LiCor_dat$LinR2[i]
  tests$LinFluxSE <- LiCor_dat$LinFluxSE[i]
  
  
  DOut <- rbind(DOut, tests)
  
  
}

Fdat <- rbind(Fdat, DOut)
##############################################################################################################
##############################################################################################################

##############################################################################################################################
#############################################################################################################

##//Reading in the files by lines so that we can rip apart the irregular vetor formats
text <- readLines(Flist[6])

##//Pulling meta data and results
maybe <-  text %>% 
  enframe(name = NULL) %>%
  filter(str_detect(value, "^.")) %>%   ##// Regular expression grabbing infromation for each line with two  vectors 
  separate(value, sep = "\t", into = c("Parameter", "Value"))

##//LiCor Data and Meta data
LiCor_dat <- data.frame(File = maybe$Value[maybe$Parameter=="File Name:"],
                        Collar = maybe$Value[maybe$Parameter=="Comments:"],
                        Obs = as.numeric(maybe$Value[maybe$Parameter=="Obs#:"]),
                        VCham = as.numeric(maybe$Value[maybe$Parameter=="Vcham:"]), 
                        Offset = as.numeric(maybe$Value[maybe$Parameter=="Offset:"]),
                        Area = as.numeric(maybe$Value[maybe$Parameter=="Area:"]),
                        VTotal = as.numeric(maybe$Value[maybe$Parameter=="Vtotal:"]),
                        FitStatus = maybe$Value[maybe$Parameter=="CrvFitStatus:"],
                        ExpFlux = as.numeric(maybe$Value[maybe$Parameter=="Exp_Flux:"]),
                        ExpFluxCV = as.numeric(maybe$Value[maybe$Parameter=="Exp_FluxCV:"]),
                        Exp_dCdt = as.numeric(maybe$Value[maybe$Parameter=="Exp_dCdry/dt:"]),
                        ExpR2 = as.numeric(maybe$Value[maybe$Parameter=="Exp_R2:"]),
                        LinFlux = as.numeric(maybe$Value[maybe$Parameter=="Lin_Flux:"]),
                        LinFluxCV = as.numeric(maybe$Value[maybe$Parameter=="Lin_FluxCV:"]),
                        Lin_dCdt = as.numeric(maybe$Value[maybe$Parameter=="Lin_dCdry/dt:"]),
                        LinR2 = as.numeric(maybe$Value[maybe$Parameter=="Lin_R2:"]),
                        LinFluxSE = as.numeric(maybe$Value[maybe$Parameter=="Lin_SE:"]))

###//Raw fluxes to long format dataframe
maybeData <-  text %>% 
  enframe(name = NULL) %>%
  filter(str_detect(value, "^\\d")) %>%  ##// Regular expression searching for start of line begining with a number 
  separate(value, sep = "\t", into = c("Type",	"Etime",	"Date",	"Tcham",	"Pressure",	"H2O", "CO2",	"Cdry",	"Tbench",	"RH",	"Tboard",	"Vin",
                                       "CO2ABS",	"H2OABS",	"Hour", "DOY", "RAWCO2", "RAWCO2REF",	"RAWH2O",	"RAWH2OREF"))

##//Updating the classes
maybeData$Date_ct <- as.POSIXct(strptime(maybeData$Date, format = "%Y-%m-%d %H:%M:%S"), tz="EST")
maybeData$Type <- as.numeric(maybeData$Type); maybeData$Etime <- as.numeric(maybeData$Etime); maybeData$Tcham <- as.numeric(maybeData$Tcham);
maybeData$Pressure <- as.numeric(maybeData$Pressure); maybeData$H2O <- as.numeric(maybeData$H2O); maybeData$CO2 <- as.numeric(maybeData$CO2);
maybeData$Cdry <- as.numeric(maybeData$Cdry); maybeData$Tbench <- as.numeric(maybeData$Tbench); maybeData$RH <- as.numeric(maybeData$RH);
maybeData$Tboard <- as.numeric(maybeData$Tboard); maybeData$Vin <- as.numeric(maybeData$Vin); maybeData$CO2ABS <- as.numeric(maybeData$CO2ABS);
maybeData$H2OABS <- as.numeric(maybeData$H2OABS); maybeData$Hour <- as.numeric(maybeData$Hour); maybeData$DOY <- as.numeric(maybeData$DOY);
maybeData$RAWCO2 <- as.numeric(maybeData$RAWCO2); maybeData$RAWCO2REF <- as.numeric(maybeData$RAWCO2REF);
maybeData$RAWH2O <- as.numeric(maybeData$RAWH2O); maybeData$RAWH2OREF <- as.numeric(maybeData$RAWH2OREF);


##//Reconstructing data to be in long format 
##//Using time of last measurement to seperate observations
endMes <- maybeData$Date_ct[maybeData$Type==4]

tests <- maybeData[maybeData$Date_ct<=endMes[1],]
tests$Collar <- LiCor_dat$Collar[1]
tests$File <- LiCor_dat$File[1]
tests$OBS <- LiCor_dat$Obs[1]
tests$VCham <- LiCor_dat$VCham[1]
tests$Offset <- LiCor_dat$Offset[1]
tests$Area <- LiCor_dat$Area[1]
tests$VTotal <- LiCor_dat$VTotal[1]
tests$ExpFlux <- LiCor_dat$ExpFlux[1]
tests$ExpFluxCV <- LiCor_dat$ExpFluxCV[1]
tests$Exp_dCdt <- LiCor_dat$Exp_dCdt[1]
tests$ExpR2 <- LiCor_dat$ExpR2[1]
tests$LinFlux <- LiCor_dat$LinFlux[1]
tests$LinFluxCV <- LiCor_dat$LinFluxCV[1]
tests$Lin_dCdt <- LiCor_dat$Lin_dCdt[1]
tests$LinR2 <- LiCor_dat$LinR2[1]
tests$LinFluxSE <- LiCor_dat$LinFluxSE[1]

DOut <- rbind(DOut, tests)

for(i in 2:length(endMes)){
  
  tests <- maybeData[maybeData$Date_ct<=endMes[i] & maybeData$Date_ct>endMes[i-1],]
  tests$Collar <- LiCor_dat$Collar[i]
  tests$File <- LiCor_dat$File[i]
  tests$OBS <- LiCor_dat$Obs[i]
  tests$VCham <- LiCor_dat$VCham[i]
  tests$Offset <- LiCor_dat$Offset[i]
  tests$Area <- LiCor_dat$Area[i]
  tests$VTotal <- LiCor_dat$VTotal[i]
  tests$ExpFlux <- LiCor_dat$ExpFlux[i]
  tests$ExpFluxCV <- LiCor_dat$ExpFluxCV[i]
  tests$Exp_dCdt <- LiCor_dat$Exp_dCdt[i]
  tests$ExpR2 <- LiCor_dat$ExpR2[i]
  tests$LinFlux <- LiCor_dat$LinFlux[i]
  tests$LinFluxCV <- LiCor_dat$LinFluxCV[i]
  tests$Lin_dCdt <- LiCor_dat$Lin_dCdt[i]
  tests$LinR2 <- LiCor_dat$LinR2[i]
  tests$LinFluxSE <- LiCor_dat$LinFluxSE[i]
  
  
  DOut <- rbind(DOut, tests)
  
  
}

Fdat <- rbind(Fdat, DOut)
##############################################################################################################
##############################################################################################################


##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################

#Fdat


#############################################################################################################
#str(Fdat)
#testCSV <- write.csv(Fdat, "CicadaFluxOut.csv")
Fdat$UnqMeas <- as.character(paste(Fdat$File, Fdat$Collar, sep = "_"))
MyMeas <- unique(Fdat$UnqMeas)
class(Fdat$UnqMeas)
length(MyMeas)
##//Remove burn-in data
Rs_RData_raw <- as.data.frame(Fdat[Fdat$Etime>5 & Fdat$Etime<=110,])


plot(as.factor(Rs_RData_raw$UnqMeas) , Rs_RData_raw$Cdry)

plot(Rs_RData_raw$Collar, Rs_RData_raw$Cdry)

#str(cicada)
#View(cicada[cicada$Collar=="GWC17",])


#write.csv(cicada[cicada$Collar=="GWC17",], "CicadaFluxOut_17.csv")

#cicada <- read.csv("CicadaFluxOut.csv")
#plot(cicada$Etime[cicada$Collar=="GWC02"], cicada$Cdry[cicada$Collar=="GWC02"])
i = 2
f = 3
plot(Rs_RData_raw$Cdry[Rs_RData_raw$UnqMeas==MyMeas[f]]~Rs_RData_raw$Etime[Rs_RData_raw$UnqMeas==MyMeas[f]], 
     col =  Rs_RData_raw$Collar[Rs_RData_raw$UnqMeas==MyMeas[f]])


cc <- Rs_RData_raw$Collar=="GWC18"
plot(Rs_RData_raw$Cdry[cc]~Rs_RData_raw$Etime[cc])


length(Rs_RData_raw$Cdry[Rs_RData_raw$Collar=="GWC17"])
dCdt_frame <- data.frame()

for(i in 1:length(MyMeas)){
##//Simple linear regression
DcDt_mod <- lm(Rs_RData_raw$Cdry[Rs_RData_raw$UnqMeas==MyMeas[i]]~Rs_RData_raw$Etime[Rs_RData_raw$UnqMeas==MyMeas[i]] )

##//R2 for model fits
LIN_R2 <- summary(DcDt_mod)$r.squared

##//Model Coeffecients
INT_CO2 <- DcDt_mod$coefficients[1]
dCdtm <- DcDt_mod$coefficients[2]

##//Cred Intervals
INT_CO2_LCI <- confint(DcDt_mod)[1]
INT_CO2_HCI <- confint(DcDt_mod)[3]
dCdt_LCIm <-  confint(DcDt_mod)[2]
dCdt_HCIm <-  confint(DcDt_mod)[4]

dCdt_frame <- rbind(dCdt_frame, data.frame(File = Rs_RData_raw$File[Rs_RData_raw$UnqMeas==MyMeas[i]][1],
                                           Collar = Rs_RData_raw$Collar[Rs_RData_raw$UnqMeas==MyMeas[i]][1],
                                           LinReg_R2 = LIN_R2,
                                           CO2_t0 = INT_CO2,
                                           CO2_t0_LCI = INT_CO2_LCI,
                                           CO2_t0_HCI = INT_CO2_HCI,
                                           dCdt = dCdtm,
                                           dCdt_LCI = dCdt_LCIm,
                                           dCdt_HCI = dCdt_HCIm))

}


dCdt_frame

dCdt_frame$UnqMeas <- as.character(paste(dCdt_frame$File, dCdt_frame$Collar, sep = "_"))


Rs_RData_sum <- Rs_RData_raw %>%
                dplyr::mutate(day = as.Date(Date_ct, format="%d-%m-%Y")) %>%
                dplyr::group_by(day, UnqMeas) %>% # group by the day column ##//mean looks good
                dplyr::summarise_if(is.numeric, median, na.rm = TRUE) %>%  
                na.omit

Rs_RData_sum <- inner_join(Rs_RData_sum, dCdt_frame, by = "UnqMeas")

Rs_Mdata <- read.csv("D:/Dropbox/Projects/Indiana/Data/CicadaFlux/RespMetaData.csv")
Rs_Sdata <- read.csv("D:/Dropbox/Projects/Indiana/Data/CicadaFlux/SoilTempMoisture/SoilT_VWC_CicadaFlux_2021.csv")

Rs_Sdata <- Rs_Sdata %>%
  dplyr::mutate(day = as.Date(Rs_Sdata$Date, format="%m/%d/%Y"))

Rs_data <- inner_join(Rs_RData_sum, Rs_Sdata, by = c("day", "Collar"))
Rs_data <- inner_join(Rs_data, Rs_Mdata, by = c("Site", "Collar"))

###//Recalculate the fluxes

R <- 8.314 #Pa m3 K-1 mol-1

Rs_data$VCollar <- Rs_data$Corr_offset_cm * Rs_data$Area

Rs_data$Corr_VTotal <- Rs_data$VCollar + Rs_data$VCham


Rs_data$Corr_LinFlux <- (10 * Rs_data$Corr_VTotal * Rs_data$Pressure * (1 - Rs_data$H2O / 10000)) /   
                                        ( R * Rs_data$Area * (Rs_data$Tcham +273.15)) * Rs_data$dCdt


Rs_data$Corr_LinFlux_LCI <- (10 * Rs_data$Corr_VTotal * Rs_data$Pressure * (1 - Rs_data$H2O / 10000)) /   
  ( R * Rs_data$Area * (Rs_data$Tcham +273.15)) * Rs_data$dCdt_LCI

Rs_data$Corr_LinFlux_HCI <- (10 * Rs_data$Corr_VTotal * Rs_data$Pressure * (1 - Rs_data$H2O / 10000)) /   
  ( R * Rs_data$Area * (Rs_data$Tcham +273.15)) * Rs_data$dCdt_HCI



length(Rs_data$UnqMeas)
length(Rs_plots$UnqMeas)




Rs_plots <- Rs_data[Rs_data$LinReg_R2>=0.85 & Rs_data$Corr_LinFlux>0,]

Rs_bad <- Rs_data[Rs_data$LinReg_R2<0.85 ,]

Rs_bad <- Rs_data[Rs_data$Corr_LinFlux<0 ,]


# 38 R2 removed


length(Rs_bad$Cdry[Rs_bad$Site=="KentFarm"])
length(Rs_bad$Cdry[Rs_bad$Site=="MorganMonroe"])
length(Rs_bad$Cdry[Rs_bad$Site=="GriffyWoods"])




plot(Rs_plots$LinFlux[Rs_plots$Corr_LinFlux<=6], Rs_plots$Corr_LinFlux[Rs_plots$Corr_LinFlux<=6])
abline(0,1)

plot(Rs_plots$dCdt_LCI[Rs_plots$Corr_LinFlux<=6], Rs_plots$Corr_LinFlux[Rs_plots$Corr_LinFlux<=6])

names(Rs_plots)
##//LiCOR vs Corrected Lin Flux
LinFluxComp <- ggplot() +
  geom_abline(slope = 1, intercept = 0, size = 3, alpha = 0.5, linetype = "dashed") + 
  geom_point(data = Rs_plots[Rs_plots$Corr_LinFlux<=6,], aes(x = LinFlux, y = Corr_LinFlux, color = Site), show.legend = TRUE,  size=4) +
  
  #geom_point(data = stp3[cc1,], aes(x=delta_TLeaf , y= QY_FP.x, color=Spp), show.legend = FALSE,  size=2) +
  geom_errorbar(data = Rs_plots[Rs_plots$Corr_LinFlux<=6,], aes(x = LinFlux , ymin = Corr_LinFlux_HCI,
                                     ymax = Corr_LinFlux_LCI, color = Site), show.legend = FALSE,
                                     size = 1, width = .15)+
  #geom_abline(slope = 1, intercept = 0, size = 3) + 
  xlab(expression(paste("LiCOR Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  ylab(expression(paste("Corr. Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  xlim(0,5)+
  ylim(0,5)+
  theme(legend.position = "top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30))

LinFluxComp


##//LiCOR vs Corrected Lin Flux
LinFluxCompSym <- ggplot() +
  geom_abline(slope = 1, intercept = 0, size = 3, alpha = 0.5, linetype = "dashed") + 
  geom_point(data = Rs_plots[Rs_plots$Corr_LinFlux<=6,], aes(x = LinFlux, y = Corr_LinFlux, color = Symbiont), show.legend = TRUE,  size=4) +
  
  #geom_point(data = stp3[cc1,], aes(x=delta_TLeaf , y= QY_FP.x, color=Spp), show.legend = FALSE,  size=2) +
  geom_errorbar(data = Rs_plots[Rs_plots$Corr_LinFlux<=6,], aes(x = LinFlux , ymin = Corr_LinFlux_HCI,
                                                                ymax = Corr_LinFlux_LCI, color = Symbiont), show.legend = FALSE,
                size = 1, width = .15)+
  #geom_abline(slope = 1, intercept = 0, size = 3) + 
  xlab(expression(paste("LiCOR Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  ylab(expression(paste("Corr. Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  scale_color_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  xlim(0,5)+
  ylim(0,5)+
  theme(legend.position = "top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30))

LinFluxCompSym


Rs_plots$Site_Pairs <-as.character(paste(Rs_plots$Site, Rs_plots$Pair, sep = "_"))
str(Rs_plots)

Rs_plots_sum <- Rs_plots[Rs_plots$Corr_LinFlux<=6,] %>%
  #dplyr::mutate(day = as.Date(Date_ct, format="%d-%m-%Y")) %>%
  dplyr::group_by(Site_Pairs, Symbiont) %>% # group by the day column ##//mean looks good
  dplyr::summarise_if(is.numeric, median, na.rm = TRUE) %>%  
  na.omit


colors = c("#4D54E8", "#ECC01D", "#A2A197")
names(Rs_plots_sum)


LinFluxDensity_Symbiont <- ggplot() +
  #geom_abline(slope = 1, intercept = 0, size = 3, alpha = 0.5, linetype = "dashed") + 
  geom_point(data = Rs_plots_sum, aes(x = Hole_Plot_m2, y = Corr_LinFlux, color = Symbiont), 
             show.legend = TRUE,  size = 4, position = position_dodge(0.5)) +
  
  #geom_point(data = stp3[cc1,], aes(x=delta_TLeaf , y= QY_FP.x, color=Spp), show.legend = FALSE,  size=2) +
  geom_errorbar(data = Rs_plots_sum, aes(x = Hole_Plot_m2 , ymin = Corr_LinFlux_HCI,
                                                            ymax = Corr_LinFlux_LCI, color = Symbiont), show.legend = FALSE,
                size = 1, width = .15, position = position_dodge(0.5))+
  scale_color_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  #geom_abline(slope = 1, intercept = 0, size = 3) + 
  xlab('Holes m2') +
  ylab(expression(paste("Corr. Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
 # xlim(0,5)+
 # ylim(0,5)+
  theme(legend.position = "top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30))

LinFluxDensity_Symbiont


names(Rs_plots_sum)

ggplot() +
  #geom_abline(slope = 1, intercept = 0, size = 3, alpha = 0.5, linetype = "dashed") + 
  geom_point(data = Rs_plots_sum, aes(x = SoilT_C, y = Corr_LinFlux, color = Symbiont), 
             show.legend = TRUE,  size = 4, position = position_dodge(0.5)) +
  
  #geom_point(data = stp3[cc1,], aes(x=delta_TLeaf , y= QY_FP.x, color=Spp), show.legend = FALSE,  size=2) +
  geom_errorbar(data = Rs_plots_sum, aes(x = SoilT_C , ymin = Corr_LinFlux_HCI,
                                         ymax = Corr_LinFlux_LCI, color = Symbiont), show.legend = FALSE,
                size = 1, width = .15, position = position_dodge(0.5))+
  #geom_abline(slope = 1, intercept = 0, size = 3) + 
  scale_color_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  xlab('Soil Temp. [C] ') +
  ylab(expression(paste("Corr. Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  # xlim(0,5)+
  # ylim(0,5)+
  theme(legend.position = "top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30))


ggplot() +
  #geom_abline(slope = 1, intercept = 0, size = 3, alpha = 0.5, linetype = "dashed") + 
  geom_point(data = Rs_plots_sum, aes(x = SoilT_C, y = Corr_LinFlux, color = Symbiont), 
             show.legend = TRUE,  size = 4) +
  
  #geom_point(data = stp3[cc1,], aes(x=delta_TLeaf , y= QY_FP.x, color=Spp), show.legend = FALSE,  size=2) +
  geom_errorbar(data = Rs_plots_sum, aes(x = SoilT_C , ymin = Corr_LinFlux_HCI,
                                         ymax = Corr_LinFlux_LCI, color = Symbiont), show.legend = FALSE,
                size = 1, width = .1)+
  #geom_abline(slope = 1, intercept = 0, size = 3) + 
  scale_color_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  xlab('Soil Temp. [C] ') +
  ylab(expression(paste("Corr. Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  # xlim(0,5)+
  # ylim(0,5)+
  theme(legend.position = "top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30))



ggplot() +
  #geom_abline(slope = 1, intercept = 0, size = 3, alpha = 0.5, linetype = "dashed") + 
  geom_point(data = Rs_plots[Rs_plots$Corr_LinFlux<=6,], aes(x = SoilT_C, y = Corr_LinFlux, color = Site), 
             show.legend = TRUE,  size = 4) +
  
  #geom_point(data = stp3[cc1,], aes(x=delta_TLeaf , y= QY_FP.x, color=Spp), show.legend = FALSE,  size=2) +
  geom_errorbar(data = Rs_plots[Rs_plots$Corr_LinFlux<=6,], aes(x = SoilT_C , ymin = Corr_LinFlux_HCI,
                                         ymax = Corr_LinFlux_LCI, color = Site), show.legend = FALSE,
                size = 1, width = .1)+
  #geom_abline(slope = 1, intercept = 0, size = 3) + 
 # scale_color_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  xlab('Soil Temp. [C] ') +
  ylab(expression(paste("Corr. Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  # xlim(0,5)+
  # ylim(0,5)+
  theme(legend.position = "top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30))


mean(Rs_plots$SoilT_C[Rs_plots$Corr_LinFlux<=6 & Rs_plots$Site=="KentFarm"])
mean(Rs_plots$SoilT_C[Rs_plots$Corr_LinFlux<=6 & Rs_plots$Site=="MorganMonroe"])
mean(Rs_plots$SoilT_C[Rs_plots$Corr_LinFlux<=6 & Rs_plots$Site=="GriffyWoods"])


ggplot() +
  #geom_abline(slope = 1, intercept = 0, size = 3, alpha = 0.5, linetype = "dashed") + 
  geom_point(data = Rs_plots_sum, aes(x = SoilVWC_pct, y = Corr_LinFlux, color = Symbiont), 
             show.legend = TRUE,  size = 4, position = position_dodge(0.5)) +
  
  #geom_point(data = stp3[cc1,], aes(x=delta_TLeaf , y= QY_FP.x, color=Spp), show.legend = FALSE,  size=2) +
  geom_errorbar(data = Rs_plots_sum, aes(x = SoilVWC_pct , ymin = Corr_LinFlux_HCI,
                                         ymax = Corr_LinFlux_LCI, color = Symbiont), show.legend = FALSE,
                size = 1, width = .75, position = position_dodge(0.5))+
  #geom_abline(slope = 1, intercept = 0, size = 3) + 
  scale_color_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  xlab('Soil VWC [%] ') +
  ylab(expression(paste("Corr. Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  # xlim(0,5)+
  # ylim(0,5)+
  theme(legend.position = "top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30))

ggplot() +
  #geom_abline(slope = 1, intercept = 0, size = 3, alpha = 0.5, linetype = "dashed") + 
  geom_point(data = Rs_plots_sum, aes(x = n_holes, y = Corr_LinFlux, color = Symbiont), 
             show.legend = TRUE,  size = 4, position = position_dodge(0.5)) +
  
  #geom_point(data = stp3[cc1,], aes(x=delta_TLeaf , y= QY_FP.x, color=Spp), show.legend = FALSE,  size=2) +
  geom_errorbar(data = Rs_plots_sum, aes(x = n_holes , ymin = Corr_LinFlux_HCI,
                                         ymax = Corr_LinFlux_LCI, color = Symbiont), show.legend = FALSE,
                size = 1, width = .15, position = position_dodge(0.5))+
  #geom_abline(slope = 1, intercept = 0, size = 3) + 
  scale_color_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  xlab('Soil Temp. [C] ') +
  ylab(expression(paste("Corr. Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  # xlim(0,5)+
  # ylim(0,5)+
  theme(legend.position = "top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30))




ggplot() +
  #geom_abline(slope = 1, intercept = 0, size = 3, alpha = 0.5, linetype = "dashed") + 
  geom_point(data = Rs_plots[Rs_plots$Corr_LinFlux<=6,], aes(x = as.factor(n_holes), y = Corr_LinFlux, color = Symbiont), 
             show.legend = TRUE,  size = 4, position = position_dodge(0.5)) +
  
  #geom_point(data = stp3[cc1,], aes(x=delta_TLeaf , y= QY_FP.x, color=Spp), show.legend = FALSE,  size=2) +
  geom_errorbar(data = Rs_plots[Rs_plots$Corr_LinFlux<=6,], aes(x =as.factor(n_holes), ymin = Corr_LinFlux_HCI,
                                         ymax = Corr_LinFlux_LCI, color = Symbiont), show.legend = FALSE,
                size = 1, width = .15, position = position_dodge(0.5))+
  #geom_abline(slope = 1, intercept = 0, size = 3) + 
  scale_color_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  xlab('Number of Holes') +
  ylab(expression(paste("Corr. Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  # xlim(0,5)+
  # ylim(0,5)+
  theme(legend.position = "top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30))



ggplot() +
  geom_boxplot(data = Rs_plots[Rs_plots$Corr_LinFlux<=6,], aes(x = as.factor(n_holes), y = Corr_LinFlux, fill = Symbiont), color = "black", 
               show.legend = TRUE,  size = 2, alpha = 0.4, position = position_dodge(0.5)) +
  scale_color_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  scale_fill_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  xlab('Number of Holes') +
  ylab(expression(paste("Corr. Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  theme(legend.position="top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30))


ggplot() +
  geom_boxplot(data = Rs_plots[Rs_plots$Corr_LinFlux<=6 & Rs_plots$Symbiont=="AM",], aes(x = as.factor(n_holes), y = Corr_LinFlux, fill = Site), color = "black", 
               show.legend = TRUE,  size = 2, alpha = 0.4, position = position_dodge(0.5)) +
  scale_color_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  scale_fill_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  xlab('Number of Holes') +
  ylab(expression(paste("Corr. Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  theme(legend.position="top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30))

ggplot() +
  geom_boxplot(data = Rs_plots[Rs_plots$Corr_LinFlux<=6 & Rs_plots$Symbiont=="ECM",], aes(x = as.factor(n_holes), y = Corr_LinFlux, fill = Site), color = "black", 
               show.legend = TRUE,  size = 2, alpha = 0.4, position = position_dodge(0.5)) +
  scale_color_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  scale_fill_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  xlab('Number of Holes') +
  ylab(expression(paste("Corr. Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  theme(legend.position="top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30))



ggplot() +
geom_boxplot(data = Rs_plots[Rs_plots$Corr_LinFlux<=6,], aes(x = Site, y = Corr_LinFlux, fill = Symbiont), color = "black", 
             show.legend = TRUE,  size = 2, alpha = 0.4, position = position_dodge(0.5)) +
  scale_color_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  scale_fill_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  xlab('Site') +
  ylab(expression(paste("Corr. Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  theme(legend.position="top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30))



LinFluxSite <- ggplot() +
  #geom_abline(slope = 1, intercept = 0, size = 3, alpha = 0.5, linetype = "dashed") + 
  geom_point(data = Rs_plots[Rs_plots$Corr_LinFlux<=6,], aes(x = Site, y = Corr_LinFlux, color = Site), 
             show.legend = TRUE,  size = 4, position = position_dodge(0.5)) +
  
  #geom_point(data = stp3[cc1,], aes(x=delta_TLeaf , y= QY_FP.x, color=Spp), show.legend = FALSE,  size=2) +
  geom_errorbar(data = Rs_plots[Rs_plots$Corr_LinFlux<=6,], aes(x = Hole_Plot_m2 , ymin = Corr_LinFlux_HCI,
                                                                ymax = Corr_LinFlux_LCI, color = Site), show.legend = FALSE,
                size = 1, width = .15, position = position_dodge(0.5))+
  #geom_abline(slope = 1, intercept = 0, size = 3) + 
  xlab('Holes m2') +
  ylab('Corrected Efflux') +
  # xlim(0,5)+
  # ylim(0,5)+
  theme(legend.position = "top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30))

LinFluxSite



LinFluxSymbiont <- ggplot() +
  #geom_abline(slope = 1, intercept = 0, size = 3, alpha = 0.5, linetype = "dashed") + 
  geom_point(data = Rs_plots[Rs_plots$Corr_LinFlux<=6,], aes(x = Hole_Collar, y = Corr_LinFlux, color = Site), 
             show.legend = TRUE,  size = 4, position = position_dodge(0.5)) +
  
  #geom_point(data = stp3[cc1,], aes(x=delta_TLeaf , y= QY_FP.x, color=Spp), show.legend = FALSE,  size=2) +
  geom_errorbar(data = Rs_plots[Rs_plots$Corr_LinFlux<=6,], aes(x = Hole_Collar , ymin = Corr_LinFlux_HCI,
                                                                ymax = Corr_LinFlux_LCI, color = Site), show.legend = FALSE,
                size = 1, width = .15, position = position_dodge(0.5))+
  #geom_abline(slope = 1, intercept = 0, size = 3) + 
  xlab('Holes m2') +
 # ylab('Corrected Efflux') +
  
  #xlab(expression(paste("Predawn  ", psi ["L"], "  [MPa]"))) +
  ylab(expression(paste("Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  # xlim(0,5)+
  # ylim(0,5)+
  theme(legend.position = "top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30))

LinFluxSymbiont




##//Getting me some packages
library(aspace)
library(bigleaf)
library(tidyverse)
library(rstan)
library(brms)
library(modelr)
library(sjPlot)
library(sjstats)
library(RColorBrewer)
library(tidybayes)
theme_set(theme_minimal())

###// Get me some data 
##// Five Min. met data from greenhouse summarized to daily 

hist(Rs_plots$Corr_LinFlux[Rs_plots$Corr_LinFlux<=6])
##//Bayesian approan
##// an uniformed prior for simple linear regression
my.prior <- set_prior("uniform(-30,30)", class = "b")
my.prior <- set_prior("normal(0.45,0.08)", class = "b")

Rs_plots$Cicada <- as.factor(Rs_plots$Hole_Collar)

names(Rs_plots)

##//Modeling resistance of canopy
M1 <- brm(Corr_LinFlux ~ Hole_Collar + Symbiont + Site,
            data = Rs_plots[Rs_plots$Corr_LinFlux<=6,], prior = my.prior,
            family =  "gamma",
            iter = 30000, save_all_pars = TRUE, seed = 1234,
            chains = 4,
            # cores = 4,
            control = list(adapt_delta = 0.99, max_treedepth = 15))


summary(M1, prob = 0.94)
bayes_R2(M1, prob = c(0.03, 0.97))
#plot(M1)

plot_model(M1, type = "pred", terms = c("Hole_Collar", "Symbiont", "Site"), show.p = TRUE,
           show.data = TRUE, value.size = 25,  dot.size = 2,   line.size = 2, 
           vline.color = 15, jitter = 0.1,   title = NULL, colors = c("#4D54E8", "#ECC01D", "#A2A197"),
           axis.title = expression(paste("Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  )),
           bpe = "mean", bpe.style = "dot", ci.lvl = 0.89, value.offset = .3,
           wrap.title = 1000,
           wrap.labels = 50,
           axis.lim = NULL) + 
  theme_bw()




plot_model(M1, type = "pred", terms = c("Hole_Collar", "Site"), show.p = TRUE,
           show.data = TRUE, dot.size = 2) + 
  theme_bw()



plot_model(M1, type = "pred", terms = c("Hole_Collar", "Symbiont"), show.p = TRUE,
           show.data = TRUE, dot.size = 2) + 
  theme_bw()

plot_model(M1, type = "pred", terms = c("Site", "Symbiont"), show.p = TRUE,
           show.data = TRUE, dot.size = 2) + 
  theme_bw()


plot_model(M1, type = "pred", terms = c("Hole_Collar", "Site"), show.p = TRUE,
           show.data = TRUE, dot.size = 2) + 
  theme_bw()

plot_model(M1, type = "pred", terms = c("Hole_Collar", "Symbiont"), show.p = TRUE,
           show.data = TRUE, dot.size = 2) + 
  theme_bw()

plot_model(M1, type = "pred", terms = c("Site", "Symbiont"), show.p = TRUE,
           show.data = TRUE, dot.size = 2) + 
  theme_bw()

##//Modeling resistance of canopy
M1_r <- brm(Corr_LinFlux ~ Hole_Collar + Symbiont + (1|Site),
          data = Rs_plots[Rs_plots$Corr_LinFlux<=6,], prior = my.prior,
          family =  "gamma",
          iter = 30000, save_all_pars = TRUE, seed = 1234,
          chains = 4,
          # cores = 4,
          control = list(adapt_delta = 0.99, max_treedepth = 15))


summary(M1_r, prob = 0.94)

bayes_R2(M1_r, prob = c(0.03, 0.97))


plot_model(M1_r, type = "pred", terms = c("Hole_Collar", "Symbiont", "Site"), show.p = TRUE,
           show.data = TRUE, value.size = 25,  dot.size = 2,   line.size = 2,
           vline.color = 15, jitter = 0.1,   title = NULL, colors = c("#4D54E8", "#ECC01D", "#A2A197"),
           axis.title = expression(paste("Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  )),
           bpe = "mean", bpe.style = "dot", ci.lvl = 0.89, value.offset = .3,
           wrap.title = 1000,
           wrap.labels = 50,
           axis.lim = NULL) + 
  theme_bw()

plot_model(M1_r, type = "pred", terms = c("Hole_Collar", "Site"), show.p = TRUE,
           show.data = TRUE, dot.size = 2) + 
  theme_bw()

plot_model(M1_r, type = "pred", terms = c("Hole_Collar", "Symbiont"), show.p = TRUE,
           show.data = TRUE, dot.size = 2) + 
  theme_bw()

plot_model(M1_r, type = "pred", terms = c("Site", "Symbiont"), show.p = TRUE,
           show.data = TRUE, dot.size = 2) + 
  theme_bw()


##//Modeling resistance of canopy
M2_r <- brm(Corr_LinFlux ~ Cicada + Symbiont + (1|Site),
            data = Rs_plots[Rs_plots$Corr_LinFlux<=6,], prior = my.prior,
            family =  "gamma",
            iter = 30000, save_all_pars = TRUE, seed = 1234,
            chains = 4,
            # cores = 4,
            control = list(adapt_delta = 0.99, max_treedepth = 15))


summary(M2_r, prob = 0.94)

bayes_R2(M2_r, prob = c(0.03, 0.97))

plot_model(M2_r, type = "pred", terms = c("Cicada", "Site"), show.p = TRUE,
           show.data = TRUE, dot.size = 2, ) + 
  theme_bw()

library("ggplot2")
library("ggsci")

plot_model(M2_r, type = "pred", terms = c("Cicada", "Site", "Symbiont"), show.p = TRUE,
           show.data = TRUE,value.size = 2,  dot.size = 2,   line.size = 2,
           vline.color = 2, jitter = 0.5,   title = NULL, colors = c("#4D54E8", "#ECC01D", "#A2A197"),
           axis.title = expression(paste("Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  )),
           axis.labels = c("x", "C"),
           legend.title = "My Sites",
           wrap.title = 100,
           wrap.labels = 50,
           axis.lim = NULL,) + 
  theme_bw()


plot_model(M2_r, type = "pred", terms = c("Cicada", "Symbiont", "Site"), show.p = TRUE,
           show.data = TRUE, value.size = 25,  dot.size = 2,   line.size = 2,
           vline.color = 15, jitter = 0.8,   title = NULL, colors = c("#4D54E8", "#ECC01D", "#A2A197"),
           axis.title = expression(paste("Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  )),
           bpe = "mean", bpe.style = "dot", ci.lvl = 0.89, value.offset = .3,
           wrap.title = 1000,
           wrap.labels = 50,
           axis.lim = NULL) + 
  theme_bw()




plot_model(M2_r, type = "pred", terms = c("Cicada", "Symbiont"), show.p = TRUE,
           show.data = TRUE, dot.size = 2) + 
  theme_bw()

plot_model(M2_r, type = "pred", terms = c("Site", "Symbiont"), show.p = TRUE,
           show.data = TRUE, dot.size = 2) + 
  theme_bw()

get_model_data(
  M2_r, type = "pred", terms = c("Site", "Symbiont", "Cicada"), 
  ci.lvl = 0.89,)


Rs_data$n_holes

hist(Rs_data$Corr_LinFlux[Rs_data$Corr_LinFlux<=6])

##//Modeling resistance of canopy
M3_r <- brm(Corr_LinFlux ~ n_holes + Symbiont + (1|Site),
            data = Rs_plots[Rs_plots$Corr_LinFlux<=6,], prior = my.prior,
            family =  "gamma",
            iter = 30000, save_all_pars = TRUE, seed = 1234,
            chains = 4,
            # cores = 4,
            control = list(adapt_delta = 0.99, max_treedepth = 15))


summary(M3_r, prob = 0.89)

bayes_R2(M3_r, prob = c(0.055, 0.945))

plot_model(M3_r, type = "pred", terms = c("n_holes", "Site"), 
           show.data = TRUE, dot.size = 2, show.p = TRUE,
           line.size = 2,
           vline.color = 2, jitter = 0.1,   title = NULL, 
           axis.title = expression(paste("Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  )),
           axis.labels = c("x", "C"),
           legend.title = "My Sites",
           wrap.title = 100,
           wrap.labels = 50,
           axis.lim = NULL) + 
  theme_bw()

library("ggplot2")
library("ggsci")

plot_model(M3_r, type = "pred", terms = c("n_holes", "Symbiont", "Site"), show.p = TRUE,
           show.data = TRUE,value.size = 2,  dot.size = 2,   line.size = 2,
           vline.color = 2, jitter = 0.1,   title = NULL, colors = c("#4D54E8", "#ECC01D", "#A2A197"),
           axis.title = expression(paste("Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  )),
           axis.labels = c("x", "C"),
           legend.title = "Symbiont",
           wrap.title = 100,
           wrap.labels = 50,
           axis.lim = NULL,) + 
  theme_bw()


plot_model(M2_r, type = "pred", terms = c("Cicada", "Symbiont", "Site"), show.p = TRUE,
           show.data = TRUE, value.size = 25,  dot.size = 2,   line.size = 2,
           vline.color = 15, jitter = 0.8,   title = NULL, colors = c("#4D54E8", "#ECC01D", "#A2A197"),
           axis.title = expression(paste("Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  )),
           bpe = "mean", bpe.style = "dot", ci.lvl = 0.89, value.offset = .3,
           wrap.title = 1000,
           wrap.labels = 50,
           axis.lim = NULL) + 
  theme_bw()


#####################################################################################################
#################################################################
plot(Rs_plots$n_holes, Rs_plots$SoilT_C)

median(Rs_plots$Corr_LinFlux[Rs_plots$Corr_LinFlux<=6 & Rs_plots$Symbiont=="AM"])

median(Rs_plots$Corr_LinFlux[Rs_plots$Corr_LinFlux<=6 & Rs_plots$Symbiont=="ECM"])


AM_0 <- median(Rs_plots$Corr_LinFlux[Rs_plots$Corr_LinFlux<=6 & Rs_plots$Symbiont=="AM" & Rs_plots$n_holes==0])
ECM_0 <- median(Rs_plots$Corr_LinFlux[Rs_plots$Corr_LinFlux<=6 & Rs_plots$Symbiont=="ECM" & Rs_plots$n_holes==0])

AM_1 <- median(Rs_plots$Corr_LinFlux[Rs_plots$Corr_LinFlux<=6 & Rs_plots$Symbiont=="AM" & Rs_plots$n_holes==1])
ECM_1 <- median(Rs_plots$Corr_LinFlux[Rs_plots$Corr_LinFlux<=6 & Rs_plots$Symbiont=="ECM" & Rs_plots$n_holes==1])

AM_2 <- median(Rs_plots$Corr_LinFlux[Rs_plots$Corr_LinFlux<=6 & Rs_plots$Symbiont=="AM" & Rs_plots$n_holes==2])
ECM_2 <- median(Rs_plots$Corr_LinFlux[Rs_plots$Corr_LinFlux<=6 & Rs_plots$Symbiont=="ECM" & Rs_plots$n_holes==2])

All_0 <- median(Rs_plots$Corr_LinFlux[Rs_plots$Corr_LinFlux<=6 & Rs_plots$n_holes==0])

All_1 <- median(Rs_plots$Corr_LinFlux[Rs_plots$Corr_LinFlux<=6 & Rs_plots$n_holes==1])

All_2 <- median(Rs_plots$Corr_LinFlux[Rs_plots$Corr_LinFlux<=6 & Rs_plots$n_holes==2])



##//Modeling resistance of canopy
M4_r <- brm(Corr_LinFlux ~ n_holes + SoilT_C + Symbiont + (1|Site),
            data = c, prior = my.prior,
            family =  "gamma",
            iter = 30000, save_all_pars = TRUE, seed = 1234,
            chains = 4,
            # cores = 4,
            control = list(adapt_delta = 0.99, max_treedepth = 15))


summary(M4_r, prob = 0.89)

bayes_R2(M4_r, prob = c(0.055, 0.945))

plot_model(M4_r, type = "pred", terms = c("n_holes", "Site"), 
           show.data = TRUE, dot.size = 2, show.p = TRUE,
           line.size = 2,
           vline.color = 2, jitter = 0.1,   title = NULL, 
           axis.title = expression(paste("Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  )),
           axis.labels = c("x", "C"),
           legend.title = "My Sites",
           wrap.title = 100,
           wrap.labels = 50,
           axis.lim = NULL) + 
  theme_bw()

plot_model(M4_r, type = "pred", terms = c("Site", "n_holes", "SoilT_C"), 
           show.data = TRUE, dot.size = 2, show.p = TRUE,
           line.size = 2,
           vline.color = 2, jitter = 0.25,   title = NULL, 
           axis.title = expression(paste("Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  )),
           axis.labels = c("x", "C"),
           legend.title = "My Sites",
           wrap.title = 100,
           wrap.labels = 50,
           axis.lim = NULL) + 
  theme_bw()



plot_model(M4_r, type = "pred", terms = c( "SoilT_C", "n_holes", "Symbiont"), 
           show.data = TRUE, dot.size = 2, show.p = TRUE,
           line.size = 2,
           vline.color = 2, jitter = 0.25,   title = NULL, 
           axis.title = expression(paste("Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  )),
           axis.labels = c("x", "C"),
           legend.title = "# Holes",
           wrap.title = 100,
           wrap.labels = 50,
           axis.lim = NULL) + 
  theme_bw()










##//Modeling resistance of canopy
M2 <- brm(Corr_LinFlux ~ Hole_Collar * Symbiont + Site,
          data = Rs_plots[Rs_plots$Corr_LinFlux<=6,], prior = my.prior,
          family =  "gamma",
          iter = 30000, save_all_pars = TRUE, seed = 1234,
          chains = 4,
          # cores = 4,
          control = list(adapt_delta = 0.99, max_treedepth = 15))


summary(M2, prob = 0.94)
bayes_R2(M2, prob = c(0.03, 0.97))
#plot(M1)

plot_model(M2, type = "pred", terms = c("Hole_Collar", "Site"), show.p = TRUE,
           show.data = TRUE, dot.size = 2) + 
  theme_bw()

plot_model(M2, type = "pred", terms = c("Hole_Collar", "Symbiont"), show.p = TRUE,
           show.data = TRUE, dot.size = 2) + 
  theme_bw()

plot_model(M2, type = "pred", terms = c("Site", "Symbiont"), show.p = TRUE,
           show.data = TRUE, dot.size = 2) + 
  theme_bw()

##//Modeling resistance of canopy
M3 <- brm(Corr_LinFlux ~ Hole_Collar + Symbiont * Site,
          data = Rs_plots[Rs_plots$Corr_LinFlux<=6,], prior = my.prior,
          family =  "gamma",
          iter = 30000, save_all_pars = TRUE, seed = 1234,
          chains = 4,
          # cores = 4,
          control = list(adapt_delta = 0.99, max_treedepth = 15))


summary(M3, prob = 0.94)
bayes_R2(M3, prob = c(0.03, 0.97))
#plot(M1)

plot_model(M3, type = "pred", terms = c("Hole_Collar", "Site"), show.p = TRUE,
           show.data = TRUE, dot.size = 2) + 
  theme_bw()

plot_model(M3, type = "pred", terms = c("Hole_Collar", "Symbiont"), show.p = TRUE,
           show.data = TRUE, dot.size = 2) + 
  theme_bw()

plot_model(M3, type = "pred", terms = c("Site", "Symbiont"), show.p = TRUE,
           show.data = TRUE, dot.size = 2) + 
  theme_bw()

##///Do the model selection WAIC
M1 <- add_criterion(M1, "waic")
M1_r <- add_criterion(M1_r, "waic")
M2_r <- add_criterion(M2_r, "waic")

M2 <- add_criterion(M2, "waic")
M3 <- add_criterion(M3, "waic")

waic(M1); waic(M2); waic(M3); waic(M1_r); waic(M2_r)
M1_loo <- loo(M1, reloo = T); M2_loo <- loo(M2, reloo = T); M3_loo <- loo(M3, reloo = T);

M1_r_loo <- loo(M1_r, reloo = T); M2_r_loo <- loo(M2_r, reloo = T);

# compare both models
loo_compare(M1, M1_r, M2_r, M2, M3, criterion = "waic")
loo_compare(M1_loo, M1_r_loo, M2_r_loo, M2_loo, M3_loo)


library(tidyverse)
daily_mean_TLeaf <- metm[metm$PPFD_avg>=50 & metm$PPFD_avg<=1000,] %>%
  dplyr::mutate(day = as.Date(TIMESTAMP_ct, format="%Y-%m-%d")) %>%
  dplyr::group_by(day) %>% # group by the day column ##//mean looks good
  dplyr::summarise(delta_TLeaf_Pine_Water = mean(delta_TLeaf_Pine_Water),
                   delta_TLeaf_Pine_Drought = mean(delta_TLeaf_Pine_Drought ),
                   delta_TLeaf_Aspen_Water= mean(delta_TLeaf_Aspen_Water),
                   delta_TLeaf_Aspen_Drought = mean(delta_TLeaf_Aspen_Drought),
                   delta_TLeaf_Pine_Water_cor = mean(delta_TLeaf_Pine_Water_cor),
                   delta_TLeaf_Pine_Drought_cor = mean(delta_TLeaf_Pine_Drought_cor),
                   delta_TLeaf_Aspen_Water_cor = mean(delta_TLeaf_Aspen_Water_cor),
                   delta_TLeaf_Aspen_Drought_cor = mean(delta_TLeaf_Aspen_Drought_cor)) %>%  # calculate the SUM of all precipitation that occurred on each day
  na.omit()

class(Rs_RData_raw)
Rs_RData_raw[Rs_RData_raw$UnqMeas==as.character(MyMeas[4]),]

ggplot() + 
  geom_point(data = Rs_RData_raw, aes(x = Etime, y = Cdry, color = UnqMeas), size = 4, alpha = .4) +
  theme(legend.position = "NA",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30),
        axis.text.x = element_text(angle = 45))


##//Unique measurements
myCollars <- unique(Rs_RData_mods$Collar)
myFiles <- unique(Rs_RData_mods$File)

##//Model priors (uninformed)
#my.prior <- set_prior("uniform(100,1000)", class = "b")
my.prior <- set_prior("normal(480,25)", class = "b")

hist(Rs_RData_mods$Cdry[Rs_RData_mods$Collar==myCollars[k] & Rs_RData_mods$File==myFiles[j]])

k = 1
j = 2

hist(Rs_RData_mods$Cdry[Rs_RData_mods$Collar==myCollars[k] & Rs_RData_mods$File==myFiles[j]])


########################################################################################################
dfOUT2 <- data.frame()
#for (i in 1:length(ID1)){
for(j in 1:length(myFiles)){
  
myCollars <- unique(Rs_RData_mods$Collar[Rs_RData_mods$File==myFiles[j]])

   for(k in 1:length(myCollars)){
 #   for (i in 1:length(ID1)){
      #k = 22
      #j = 2
      
      
      ##//Modeling resistance of canopy
      dCdT <- brm(Cdry ~ Etime,
                  data = Rs_RData_mods[Rs_RData_mods$Collar==myCollars[k] & Rs_RData_mods$File==myFiles[j],], prior = my.prior,
                  family =  "normal",
                  iter = 30000, seed = 1234,
                  chains = 4,  silent = TRUE,
                  cores = 4,
                  control = list(adapt_delta = 0.99, max_treedepth = 15))
      
      
      BayesR2 <- bayes_R2(dCdT, prob = c(0.03, 0.97))
      
      dCdT_MLE <- posterior_summary(dCdT)[2]
      dCdT_Q2.5 <- posterior_summary(dCdT)[10]
      dCdT_Q97.5 <- posterior_summary(dCdT)[14]
      
      Int_MLE <- posterior_summary(dCdT)[1]
      Int_Q2.5 <- posterior_summary(dCdT)[9]
      Int_Q97.5 <- posterior_summary(dCdT)[13]
      
      
      dfOUT2 <- rbind(dfOUT2, data.frame(
        Collar = myCollars[k],
        File = myFiles[j],
        BayesR2 = BayesR2[1],
        
        dCdT_MLE = dCdT_MLE,
        dCdT_Q2.5 = dCdT_Q2.5,
        dCdT_Q97.5 = dCdT_Q97.5,
        
        Int_MLE = Int_MLE,
        Int_Q2.5 = Int_Q2.5,
        Int_Q97.5 = Int_Q97.5))
    }
}





Fmod <- lm(delta_TLeaf_Pine_Water_mean ~ delta_TLeaf_Pine_Drought_mean, data = Met_dd)
summary(Fmod)

##//Bayesian approan
##// an uniformed prior for simple linear regression
my.prior <- set_prior("uniform(-30,30)", class = "b")
my.prior <- set_prior("normal(0.45,0.08)", class = "b")

##// Histagram to estimate data distribution
hist(Met_dd$delta_TLeaf_Pine_Water_mean)

##//Modeling resistance of canopy
Bmod <- brm(delta_TLeaf_Pine_Water_mean ~ delta_TLeaf_Pine_Drought_mean,
            data = Met_dd, prior = my.prior,
            family =  "normal",
            iter = 30000, save_all_pars = TRUE, seed = 1234,
            chains = 4,
            # cores = 4,
            control = list(adapt_delta = 0.99, max_treedepth = 15))


summary(Bmod, prob = 0.94)
bayes_R2(Bmod, prob = c(0.03, 0.97))
plot(Bmod)




########################################################################################################
dfOUT2 <- data.frame()
#for (i in 1:length(ID1)){
for(j in 1:length(ID2)){
  for(k in 1:length(ID3)){
    for (i in 1:length(ID1)){
      #i = 1
      #j = 2
      ##//MLE for parameters
      PD_LWP_MLE <- mledist(iso_area$LWP_PD[iso_area$Position==ID1[i] & iso_area$Month==ID2[j] &
                                              iso_area$Species==ID3[k] & !is.na(iso_area$LWP_PD)==T],
                            "norm")
      MD_LWP_MLE <- mledist(iso_area$LWP_MD_Avg[iso_area$Position==ID1[i] & iso_area$Month==ID2[j] & 
                                                  iso_area$Species==ID3[k]& !is.na(iso_area$LWP_PD)==T],
                            "norm")
      
      ##//Cred. Intervals
      Cred_PD_LWP <- ci(iso_area$LWP_PD[iso_area$Position==ID1[i] & iso_area$Month==ID2[j] & 
                                          iso_area$Species==ID3[k]& !is.na(iso_area$LWP_PD)==T],
                        ci = 0.94)
      Cred_MD_LWP <- ci(iso_area$LWP_MD_Avg[iso_area$Position==ID1[i] & iso_area$Month==ID2[j] &
                                              iso_area$Species==ID3[k]& !is.na(iso_area$LWP_PD)==T],
                        ci = 0.94) 
      
      dfOUT2 <- rbind(dfOUT2, data.frame(
        Spp = ID3[k],
        Month = ID2[j],
        Position = ID1[i],
        
        PD_LWP_MLE = PD_LWP_MLE$estimate[1],
        MD_LWP_MLE = MD_LWP_MLE$estimate[1],
        
        PD_LDI = Cred_PD_LWP$CI_low,
        PD_HDI = Cred_PD_LWP$CI_high,
        MD_LDI = Cred_MD_LWP$CI_low,
        MD_HDI = Cred_MD_LWP$CI_high))
    }
  }
}




KS_HQ_May_D_post <- posterior_samples(KS_HQ_May_D)
KS_HQ_June_D_post <- posterior_samples(KS_HQ_June_D)
KS_HQ_July_D_post <- posterior_samples(KS_HQ_July_D)




length(ColNames)
Rs_RData <- str_split(Rs_Collar$V1,"Comments:\t")[[1]][2]



# test input
cat("DW", file = Flist[1])


grep("DW", readLines(Flist[1]), value = TRUE)
## [1] "a 1"


Rs <- read.csv("./SoilFlux/CleanData/GW26May2021_Extracted.csv")

names(Rs)[5] <- "Collar" 

Ts <- read.csv("./SoilTempMoisture/GriffyWoods_20210526.csv")

toy <- inner_join(Rs,Ts, by = "Collar")

plot(toy$ST_C, toy$Exp_Flux)
plot(toy$ST_C, toy$Lin_Flux)
plot(toy$Lin_R2)

strsplit(toy$Collar,"C")
toy$CollarNum <- as.numeric(unlist(str_split(toy$Collar,"GWC")))[!is.na(as.numeric(unlist(str_split(toy$Collar,"GWC"))))]

is.even <- function(x) x %% 2 == 0
is.odd <- function(x) x %% 2 != 0

toy$Trt[is.odd(toy$CollarNum)] <- "Holes"
toy$Trt[is.even(toy$CollarNum)] <- "NoHoles"

pairs <- toy$CollarNum

str(toy)
toy$Trt <- as.factor((toy$Trt))
plot(toy$Lin_R2, toy$Lin_Flux)

names(toy)

ggplot() +

  geom_violin(data = toy[toy$Lin_R2>=0.9,], aes(x = Trt, y = Lin_Flux, fill = Trt, color = Trt), 
              trim = TRUE, alpha = 0.3,
              position = position_dodge(width = 0.5)) +
  
  geom_boxplot(data = toy[toy$Lin_R2>=0.9,], aes(x = Trt, y = Lin_Flux, fill = Trt),width = 0.1, color = "black",
               position = position_dodge(width = 0.5), size = 1, alpha = 0.5)+
  
  scale_color_manual("Cicadas", values = c("#a6611a", "#80cdc1", "#018571")) +
  scale_fill_manual("Cicadas", values = c("#a6611a", "#80cdc1", "#018571")) +
 # ylim(0,5)+
  ylab(expression(paste("R" ["S"] , " [",mu, "mol " , " CO" ["2"], "  m"^"-2 ", "s"^"-1", "]"))) +
  xlab("Cicada Status")+
  theme(legend.position="NA",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Cicadas",size=30),
        legend.text=element_text(size=30))


##//Data is split by North and South systems
##//List of files in directory
Nlist <- dir("./North")
Slist <- dir("./South")

##//Empty dataframe