##//Data managmenet for MiniMet Systems at MMSF in conjunction with Sapflux and stem psychrometers
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
theme_set(theme_minimal())

##//Where is the data
setwd("D:/Dropbox/Projects/Indiana/Data/CicadaFlux/SoilFlux/RawData")


##//List of files in the directory
Flist <- dir(pattern = ".81x")


##//Complied dataframe
DOut <- data.frame()
Fdat <- data.frame()
for(j in 1:length(Flist)){
##//Reading in the files by lines so that we can rip apart the irregular vetor formats
text <- readLines(Flist[j])

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
endMes <- maybeData$Date[maybeData$Type==4]

for(i in 1:length(endMes)){

tests <- maybeData[maybeData$Date<=endMes[i],]
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

}



plot(tests$Etime, tests$Cdry)
plot(DOut$Etime, DOut$Cdry, col = as.numeric(DOut$Collar))

plot(DOut$OBS, DOut$Cdry, col = as.numeric(DOut$Collar))

str(maybeData)

LiCor_dat$Collar

 cbind(maybeData[maybeData$Type==4,], LiCor_dat$Collar)
unique(maybeData$Type==4)

plot(maybeData$Etime[maybeData$Type<=3], maybeData$Cdry[maybeData$Type<=3])

tail(maybeData)


###########################################################################################################

n.readLines("path/filename.txt" , n = 5, skip = 2)

readline("Type")


text = readLines("D:/Dropbox/Projects/Indiana/Data/CicadaFlux/SoilFlux/RawData/Testing/DW28May2021CicadaHole.81x")
dat <- read.table("D:/Dropbox/Projects/Indiana/Data/CicadaFlux/SoilFlux/RawData/Testing/DW28May2021CicadaHole.81x", sep = "\t" )
#line()

library(dplyr)
library(tidyr)

data.frame(text = text) %>%
  separate_rows(text, sep = "\t", convert = TRUE) %>%
  extract(text, c("File Name", "Comments"), regex = ":\t[ ]*\\S+") #%>%
  filter(!Comments %in% c("Vcham:", "Offset:")) 
#  group_by(Comments) %>%
#  mutate(id = 1:n()) %>%
#  spread(File Name, Comments) %>%
#  select(-id)

  
  library(tidyselect)
  separate_rows(text[1],  sep = "\t", convert = TRUE)
  
  # It takes its argument by expression:
  vars_pull(letters, c)
  
  # Negative numbers select from the end:
  vars_pull(letters, -3)
  
  # You can unquote variables:
  var <- 10
  vars_pull(letters, !! var)
  vars_pull(text, 1)
  
  vars_select(
    test,
    ...,
    .include = character(),
    .exclude = character(),
    .strict = TRUE
  )
  
  # For better printing
  iris <- as_tibble(iris)
  texts <- as_tibble(text)
  
  texts %>% pivot_longer(Type)
  
  texts %>% select(!(name:mass))

  starwars
  
  df <- tibble(lines = readLines("D:/Dropbox/Projects/Indiana/Data/CicadaFlux/SoilFlux/RawData/Testing/DW28May2021CicadaHole.81x"))
  
  
  library(tidyverse)
  
  sample_data <- readLines("Sample.txt")
  
  sample_data %>% 
    enframe(name = NULL) %>%
    filter(str_detect(value, "^\\d{2}-")) %>% 
    separate(value, sep = "\\s+", into = c("cert_id", "cust_id", "sites"))

sprintf()

fruit <- c("apple", "banana", "pear", "pinapple")
str_detect(fruit, "a")
str_detect(fruit, "^a")
str_detect(fruit, "a$")
str_detect(fruit, "b")
str_detect(fruit, "[aeiou]")

# Also vectorised over pattern
str_detect("aecfg", letters)

# Returns TRUE if the pattern do NOT match
str_detect(fruit, "^p", negate = TRUE)
  
ids <- do.call(rbind, regmatches(regexec(pattern = 'Comments:\t[ ]*\\S+', text = text), x = text))
ASIN <- do.call(rbind, regmatches(regexec(pattern = 'ASIN:\\s+', text = text), x = text))
title <- do.call(rbind, regmatches(regexec(pattern = 'title:\\s+', text = text), x = text))


Collar <- str_extract(text, 'Comments:\t[ ]*\\S+')
FileName <- str_extract(text, 'File Name:\t[ ]*\\S+')

ASIN <- str_extract(text, 'ASIN:[ ]*\\S+')
title <- str_extract(text, 'title:[ ]*\\S+')
group <- str_extract(text, 'group:[ ]*\\S+')
similar <- str_extract(text, 'similar:[ ]*\\S+')
rating <- str_extract(text, 'avg rating:[ ]*\\S+')

df <- data.frame(
  x = 1:3,
  y = c("a", "d,e,f", "g,h"),
  z = c("1", "2,3,4", "5,6"),
  stringsAsFactors = FALSE
)
separate_rows(df, y, z, convert = TRUE)

df <- data.frame(x = c(NA, "a-b", "a-d", "b-c", "d-e"))
df %>% extract(x, "A")
df %>% extract(x, c("A", "B"), "([[:alnum:]]+)-([[:alnum:]]+)")

# If no match, NA:
df %>% extract(x, c("A", "B"), "([a-d]+)-([a-d]+)")


library(stringr)
shopping_list <- c("apples x4", "bag of flour", "bag of sugar", "milk x2")
str_extract(shopping_list, "\\d")
str_extract(shopping_list, "[a-z]+")
str_extract(shopping_list, "[a-z]{1,4}")
str_extract(shopping_list, "\\b[a-z]{1,4}\\b")

str_extract_all(shopping_list, "[a-z]+")
str_extract_all(shopping_list, "\\b[a-z]+\\b")
str_extract_all(shopping_list, "\\d")

str_extract_all(shopping_list, "\\b[a-z]+\\b", simplify = TRUE)
str_extract_all(shopping_list, "\\d", simplify = TRUE)

# Extract all words
str_extract_all("This is, suprisingly, a sentence.", boundary("word"))

Collar <- str_extract(text, 'Comments:\t')
Collar
Collar <- str_extract(text, '\\d')



Clist <- find_pattern_in(
  "Comments",
  basedir = "D:/Dropbox/Projects/Indiana/Data/CicadaFlux/SoilFlux/RawData",
  dir_recursive = FALSE,
  reader = readLines,
  include.comments = FALSE,
  comment.char = NULL,
  use.OS = FALSE,
  file_pattern = Flist[1],
  file_contents_perl = FALSE,
  file_contents_fixed = FALSE,
  file_contents_ignore_case = FALSE,
  file.ext = NULL,
  which_lines =  "all")


Collar

Clist <- find_pattern_in(
  "Comments",
  basedir = c,
  dir_recursive = FALSE,
  reader = readLines,
  include.comments = FALSE,
  comment.char = NULL,
  use.OS = FALSE,
 # file_pattern = ".81x",
  file_contents_perl = FALSE,
  file_contents_fixed = FALSE,
  file_contents_ignore_case = FALSE,
  file.ext = NULL,
  which_lines =  "all")




# Extract all words
str_extract_all("This is, suprisingly, a sentence.", boundary("word"))


Collar <- ifelse(Collar=="NA", "Data", Collar)

class(Collar)
row(Collar[!is.na(Collar)])

ASIN <- str_extract(text, 'ASIN:[ ]*\\S+')
title <- str_extract(text, 'title:[ ]*\\S+')
group <- str_extract(text, 'group:[ ]*\\S+')
similar <- str_extract(text, 'similar:[ ]*\\S+')
rating <- str_extract(text, 'avg rating:[ ]*\\S+')

####
##//Collars
Clist <- find_pattern_in(
  "Comments",
  basedir = "D:/Dropbox/Projects/Indiana/Data/CicadaFlux/SoilFlux/RawData",
  dir_recursive = FALSE,
  reader = readLines,
  include.comments = FALSE,
  comment.char = NULL,
  use.OS = FALSE,
  file_pattern = ".81x",
  file_contents_perl = FALSE,
  file_contents_fixed = FALSE,
  file_contents_ignore_case = FALSE,
  file.ext = NULL,
  which_lines =  "all")
##//Start of measurements
Mlist <- find_pattern_in(
  "Type",
  basedir = "D:/Dropbox/Projects/Indiana/Data/CicadaFlux/SoilFlux/RawData",
  dir_recursive = FALSE,
  reader = readLines,
  include.comments = FALSE,
  comment.char = NULL,
  use.OS = FALSE,
  file_pattern = ".81x",
  file_contents_perl = FALSE,
  file_contents_fixed = FALSE,
  file_contents_ignore_case = FALSE,
  file.ext = NULL,
  which_lines =  "all")


##//The raw data
Yikes2 <- read.table(Mlist$file[i], header = FALSE, sep = "\t", skip = Mlist$line_no[i]+1, nrows = 137)





EndMlist <- find_pattern_in(
  "Exp_Flux",
  basedir = "D:/Dropbox/Projects/Indiana/Data/CicadaFlux/SoilFlux/RawData",
  dir_recursive = FALSE,
  reader = readLines,
  include.comments = FALSE,
  comment.char = NULL,
  use.OS = FALSE,
  file_pattern = ".81x",
  file_contents_perl = FALSE,
  file_contents_fixed = FALSE,
  file_contents_ignore_case = FALSE,
  file.ext = NULL,
  which_lines =  "all")


##//The raw data
##///CCCloser to what we need


dats <- read.table(EndMlist$file[i], header = FALSE, sep = "\t", skip = EndMlist$line_no[i]-140, nrows = 137)
dats <- read.table(EndMlist$file[i], header = FALSE, sep = "\t", skip = EndMlist$line_no[i]-140, nrows = 137)



plot(Yikes2$V7~Yikes2$V2)

EndMlist <- find_pattern_in(
  "CrvFitStatus",
  basedir = "D:/Dropbox/Projects/Indiana/Data/CicadaFlux/SoilFlux/RawData",
  dir_recursive = FALSE,
  reader = readLines,
  include.comments = FALSE,
  comment.char = NULL,
  use.OS = FALSE,
  file_pattern = ".81x",
  file_contents_perl = FALSE,
  file_contents_fixed = FALSE,
  file_contents_ignore_case = FALSE,
  file.ext = NULL,
  which_lines =  "all")
EndMlist <- find_pattern_in(
  "CrvFitStatus",
  basedir = "D:/Dropbox/Projects/Indiana/Data/CicadaFlux/SoilFlux/RawData",
  dir_recursive = FALSE,
  reader = readLines,
  include.comments = FALSE,
  comment.char = NULL,
  use.OS = FALSE,
  file_pattern = ".81x",
  file_contents_perl = FALSE,
  file_contents_fixed = FALSE,
  file_contents_ignore_case = FALSE,
  file.ext = NULL,
  which_lines =  "all")
EndMlist <- find_pattern_in(
  "CrvFitStatus",
  basedir = "D:/Dropbox/Projects/Indiana/Data/CicadaFlux/SoilFlux/RawData",
  dir_recursive = FALSE,
  reader = readLines,
  include.comments = FALSE,
  comment.char = NULL,
  use.OS = FALSE,
  file_pattern = ".81x",
  file_contents_perl = FALSE,
  file_contents_fixed = FALSE,
  file_contents_ignore_case = FALSE,
  file.ext = NULL,
  which_lines =  "all")
EndMlist <- find_pattern_in(
  "Exp_FluxCV",
  basedir = "D:/Dropbox/Projects/Indiana/Data/CicadaFlux/SoilFlux/RawData",
  dir_recursive = FALSE,
  reader = readLines,
  include.comments = FALSE,
  comment.char = NULL,
  use.OS = FALSE,
  file_pattern = ".81x",
  file_contents_perl = FALSE,
  file_contents_fixed = FALSE,
  file_contents_ignore_case = FALSE,
  file.ext = NULL,
  which_lines =  "all")


#Mlist$file[1]
length(Clist$file)
length(Mlist$file)
length(EndMlist$file)



#Flist <- dir()

Rs_FileName <- c()
Rs_Collar <- c()
Rs_RData <- data.frame()
#i = 1

for(i in 1:length(Mlist$file)){

##/// Start extracting varables and parameters
##//File name
#i = 1
Rs_File <- read.table(Mlist$file[i], header = FALSE, sep = " ", skip = 1, nrows = 1)
Rs_FileName <- str_split(Rs_File$V2,"Name:\t")[[1]][2]

##//Collar
Rs_Collars <- read.table(Clist$file[i], header = FALSE, sep = " ", skip = Clist$line_no[i]-1, nrows = 1)
Rs_Collar <- str_split(Rs_Collars$V1,"Comments:\t")[[1]][2]


##//The raw data
Rs_RData2 <- read.table(Mlist$file[i], header = FALSE, sep = "\t", skip = Mlist$line_no[i]+1, nrows = 137)

##//Column Names
ColNames <- c("Type",	"Etime",	"Date",	"Tcham",	"Pressure",	"H2O", "CO2",	"Cdry",	"Tbench",	"RH",	"Tboard",	"Vin",
                "CO2ABS",	"H2OABS",	"Hour", "DOY", "RAWCO2", "RAWCO2REF",	"RAWH2O",	"RAWH2OREF")

##//Combining dataframes names, files, and collars 
colnames(Rs_RData2) <- ColNames 

Rs_RData2$Collar <- Rs_Collar
Rs_RData2$File <- Rs_FileName

##//Merge dataframes back together
Rs_RData <- rbind(Rs_RData, Rs_RData2)

}

##//Plot all measurements
plot(Rs_RData$Etime[Rs_RData$Etime>15 & Rs_RData$Etime<=100], Rs_RData$Cdry[Rs_RData$Etime>15 & Rs_RData$Etime<=100])



unique(Rs_RData$Collar)
unique(Rs_RData$File)


##//Remove burn-in data
Rs_RData_mods <- Rs_RData[Rs_RData$Etime>15 & Rs_RData$Etime<=100,]

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