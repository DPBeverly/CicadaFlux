##//Getting me some packages
##//Package management
library(bigleaf)
library(tidybayes)
library(rstan)
library(brms)
library(modelr)
library(sjPlot)
library(sjstats)
library(RColorBrewer)
library(lubridate)
library(readxl)
library(tidyverse)
library(hutils)
library(dplyr)
library(plyr)
library(tidyr)
library(tidyselect)
library(stringr)
library(matrixStats)
library(knitr)
library(car)
library(corrplot)
library(hysteresis)
library(gridExtra)
library(ggpubr)
library(TreeDep)
library(emmeans)
theme_set(theme_classic())
GoAvsGo <- c("#6F263D", "#236192", "#A2AAAD", "#000000")
cicadaColors <- c( "#191919", "#943b37" , "#fc0d03")
##//Grab micromet data that we can clean psychrometers with respect to VPD
##//AmeriFlux download
dat_46m <- read.csv("D:/Dropbox/Projects/Indiana/Data/CicadaFlux/AMF_US-MMS_BASE-BADM_21-5/AMF_US-MMS_BASE_HR_21-5.csv", 
                    skip = 2, na.strings = -9999)

names(dat_46m)

dat_46m$TIMESTAMP_END2 <- dat_46m$TIMESTAMP_END * 100
dat_46m$TIMESTAMP <- ymd_hms(dat_46m$TIMESTAMP_END2, tz = "EST")

##//Make if posixct
dat_46m$TIMESTAMP_ct <- as.POSIXct(strptime(dat_46m$TIMESTAMP, "%Y-%m-%d %H:%M:%S"))

##//Convert to Hours 
dat_46m$Hours <- strptime(substr(dat_46m$TIMESTAMP_ct, 12, 16), "%H:%M")


range(dat_46m$TIMESTAMP_ct, na.rm = T)

cc <- dat_46m$TIMESTAMP_ct>"2021-01-01 00:00:00 EST" #& dat_46m$USTAR_1_1_1>0.5 & dat_46m$PPFD_IN_1_1_1<15
plot(dat_46m$TS_2_1_1[cc], dat_46m$FC_1_1_1[cc])
plot(dat_46m$SWC_1_1_1[cc], dat_46m$FC_1_1_1[cc])

PreCicada2 <- dat_46m[dat_46m$TIMESTAMP_ct>"2005-01-01 00:00:00 EST" & dat_46m$TIMESTAMP_ct<"2021-01-01 00:00:00 EST",]
PreCicada <- dat_46m[dat_46m$TIMESTAMP_ct>"2011-01-01 00:00:00 EST" & dat_46m$TIMESTAMP_ct<"2021-01-01 00:00:00 EST",]
Cicada <- dat_46m[dat_46m$TIMESTAMP_ct>"2021-01-01 00:00:00 EST" & dat_46m$TIMESTAMP_ct<"2022-01-01 00:00:00 EST",]
Cicada_04 <- dat_46m[dat_46m$TIMESTAMP_ct>"2004-01-01 00:00:00 EST" & dat_46m$TIMESTAMP_ct<"2005-01-01 00:00:00 EST",]


PreCicada$doy <- as.numeric(strftime(PreCicada$TIMESTAMP_ct, format = "%j"))
PreCicada2$doy <- as.numeric(strftime(PreCicada2$TIMESTAMP_ct, format = "%j"))
Cicada$doy <- as.numeric(strftime(Cicada$TIMESTAMP_ct, format = "%j"))
Cicada_04$doy <- as.numeric(strftime(Cicada_04$TIMESTAMP_ct, format = "%j"))



plot(PreCicada2$doy, PreCicada2$FC_1_1_1)
points(Cicada$doy, Cicada$FC_1_1_1, col = "red")
points(Cicada_04$doy, Cicada_04$FC_1_1_1, col = "cyan")

# plot(PreCicada2$doy, cumsum(PreCicada2$FC_1_1_1))
# points(Cicada$doy, Cicada$FC_1_1_1, col = "red")
# points(Cicada_04$doy, Cicada_04$FC_1_1_1, col = "cyan")



cc <- PreCicada$FC_1_1_1> -50 & PreCicada$FC_1_1_1< 50 & PreCicada$USTAR_1_1_1>0.29 & PreCicada$PPFD_IN_1_1_1>500

d_pre = PreCicada[cc,] %>%
  #dplyr::mutate(TIMESTAMP_agg = floor_date(TIMESTAMP_ct, unit = "day"), ) %>%
  dplyr::group_by(doy) %>%
  dplyr::summarise_if(is.numeric, median, na.rm=T) 

cc <- PreCicada2$FC_1_1_1> -50 & PreCicada2$FC_1_1_1< 50 & PreCicada2$USTAR_1_1_1>0.29 & PreCicada2$PPFD_IN_1_1_1>500

d_pre2 = PreCicada2[cc,] %>%
  #dplyr::mutate(TIMESTAMP_agg = floor_date(TIMESTAMP_ct, unit = "day"), ) %>%
  dplyr::group_by(doy) %>%
  dplyr::summarise_if(is.numeric, median, na.rm=T) 



cc <- Cicada$FC_1_1_1> -50 & Cicada$FC_1_1_1< 50 & Cicada$USTAR_1_1_1>0.29 & Cicada$PPFD_IN_1_1_1>500
d_cicada = Cicada[cc,] %>%
  #  dplyr::mutate(TIMESTAMP_agg = floor_date(TIMESTAMP_ct, unit = "day"), ) %>%
  dplyr::group_by(doy) %>%
  dplyr::summarise_if(is.numeric, median, na.rm=T) 

cc <- Cicada_04$FC_1_1_1> -50 & Cicada_04$FC_1_1_1< 50 & Cicada_04$USTAR_1_1_1>0.29 & Cicada_04$PPFD_IN_1_1_1>500
d_cicada_04 = Cicada_04[cc,] %>%
  #  dplyr::mutate(TIMESTAMP_agg = floor_date(TIMESTAMP_ct, unit = "day"), ) %>%
  dplyr::group_by(doy) %>%
  dplyr::summarise_if(is.numeric, median, na.rm=T) 

plot(d_pre2$doy, d_pre2$FC_1_1_1)
plot(d_cicada_04$doy, d_cicada_04$FC_1_1_1, col = "blue")
points(d_cicada$doy, d_cicada$FC_1_1_1, col = "red")
points(d_pre2$doy, d_pre2$FC_1_1_1)


plot(d_pre2$doy, cumsum(d_pre2$FC_1_1_1))
points(d_cicada$doy, cumsum(d_cicada$FC_1_1_1), col = "red")
points(d_cicada_04$doy, cumsum(d_cicada_04$FC_1_1_1), col = "blue")

plot(d_cicada_04$doy, cumsum(d_cicada_04$FC_1_1_1), col = "blue")
points(d_cicada$doy, cumsum(d_cicada$FC_1_1_1), col = "red")
points(d_pre2$doy, cumsum(d_pre2$FC_1_1_1))




plot(d_pre$doy, d_pre$FC_1_1_1, type = "b")
plot(d_pre2$doy, d_pre2$FC_1_1_1, type = "b")
points(d_cicada$doy, d_cicada$FC_1_1_1, type = "b", col = "red")
points(d_cicada_04$doy, d_cicada_04$FC_1_1_1, type = "b", col = "blue")

plot(d_pre$TS_2_1_1, d_pre$FC_1_1_1, type = "b")
points(d_cicada$TS_2_1_1, d_cicada$FC_1_1_1, type = "b", col = "red")
points(d_cicada_04$TS_2_1_1, d_cicada_04$FC_1_1_1, type = "b", col = "blue")







cc <- PreCicada$FC_1_1_1> -50 & PreCicada$FC_1_1_1< 50 & PreCicada$USTAR_1_1_1>0.29 & PreCicada$PPFD_IN_1_1_1<5

d_pre = PreCicada[cc,] %>%
  #dplyr::mutate(TIMESTAMP_agg = floor_date(TIMESTAMP_ct, unit = "day"), ) %>%
  dplyr::group_by(doy) %>%
  dplyr::summarise_if(is.numeric, median, na.rm=T) 

cc <- PreCicada2$FC_1_1_1> -50 & PreCicada2$FC_1_1_1< 50 & PreCicada2$USTAR_1_1_1>0.29 & PreCicada2$PPFD_IN_1_1_1<5

d_pre2 = PreCicada2[cc,] %>%
  #dplyr::mutate(TIMESTAMP_agg = floor_date(TIMESTAMP_ct, unit = "day"), ) %>%
  dplyr::group_by(doy) %>%
  dplyr::summarise_if(is.numeric, median, na.rm=T) 



cc <- Cicada$FC_1_1_1> -50 & Cicada$FC_1_1_1< 50 & Cicada$USTAR_1_1_1>0.29 & Cicada$PPFD_IN_1_1_1<5
d_cicada = Cicada[cc,] %>%
#  dplyr::mutate(TIMESTAMP_agg = floor_date(TIMESTAMP_ct, unit = "day"), ) %>%
  dplyr::group_by(doy) %>%
  dplyr::summarise_if(is.numeric, median, na.rm=T) 

cc <- Cicada_04$FC_1_1_1> -50 & Cicada_04$FC_1_1_1< 50 & Cicada_04$USTAR_1_1_1>0.29 & Cicada_04$PPFD_IN_1_1_1<5
d_cicada_04 = Cicada_04[cc,] %>%
  #  dplyr::mutate(TIMESTAMP_agg = floor_date(TIMESTAMP_ct, unit = "day"), ) %>%
  dplyr::group_by(doy) %>%
  dplyr::summarise_if(is.numeric, median, na.rm=T) 



plot(d_pre$doy, d_pre$FC_1_1_1, type = "b")
plot(d_pre2$doy, d_pre2$FC_1_1_1, type = "b")
points(d_cicada$doy, d_cicada$FC_1_1_1, type = "b", col = "red")
points(d_cicada_04$doy, d_cicada_04$FC_1_1_1, type = "b", col = "blue")

plot(d_pre2$TS_2_1_1, d_pre2$FC_1_1_1, type = "b")
points(d_cicada$TS_2_1_1, d_cicada$FC_1_1_1, type = "b", col = "red")
points(d_cicada_04$TS_2_1_1, d_cicada_04$FC_1_1_1, type = "b", col = "blue")

plot(d_pre2$doy, d_pre2$LW_OUT_1_1_1, type = "b")
plot(d_cicada$doy, d_cicada$LW_OUT_1_1_1, type = "b")
plot(d_cicada_04$doy, d_cicada_04$LW_OUT_1_1_1, type = "b")


predays <- unique(PreCicada2$doy)
for(i in 2:length(predays)){ 
PreCicada2$DDG <- ((min(PreCicada2$TA_1_1_1[PreCicada2$doy==predays[i]], na.rm = T) + 
                  max(PreCicada2$TA_1_1_1[PreCicada2$doy==predays[i]], na.rm = T)) / 2) - 7 

}
plot(PreCicada2$doy, PreCicada2$DDG)
##//Extra test frames
cc <- PreCicada2$FC_1_1_1> -50 & PreCicada2$FC_1_1_1< 50 & PreCicada2$USTAR_1_1_1>0.29 #& PreCicada2$PPFD_IN_1_1_1<5

d_pre22 = PreCicada2[cc,] %>%
  #dplyr::mutate(TIMESTAMP_agg = floor_date(TIMESTAMP_ct, unit = "day"), ) %>%
  dplyr::group_by(doy) %>%
  dplyr::summarise_if(is.numeric, median, na.rm=T) 



cc <- Cicada$FC_1_1_1> -50 & Cicada$FC_1_1_1< 50 & Cicada$USTAR_1_1_1>0.29 #& Cicada$PPFD_IN_1_1_1<5
d_cicada2 = Cicada[cc,] %>%
  #  dplyr::mutate(TIMESTAMP_agg = floor_date(TIMESTAMP_ct, unit = "day"), ) %>%
  dplyr::group_by(doy) %>%
  dplyr::summarise_if(is.numeric, median, na.rm=T) 

cc <- Cicada_04$FC_1_1_1> -50 & Cicada_04$FC_1_1_1< 50 & Cicada_04$USTAR_1_1_1>0.29 #& Cicada_04$PPFD_IN_1_1_1<5
d_cicada_042 = Cicada_04[cc,] %>%
  #  dplyr::mutate(TIMESTAMP_agg = floor_date(TIMESTAMP_ct, unit = "day"), ) %>%
  dplyr::group_by(doy) %>%
  dplyr::summarise_if(is.numeric, median, na.rm=T) 



d_environ_comp <- rbind(data.frame(Treatment = "PreCicada_16Y",
                                   Flux = d_pre22$FC_1_1_1[d_pre22$doy>100 & d_pre22$doy<200], 
                                   SoilT = d_pre22$TS_2_1_1[d_pre22$doy>100 & d_pre22$doy<200],
                                   SoilVWC = d_pre22$SWC_1_1_1[d_pre22$doy>100 & d_pre22$doy<200],
                                   Rad_out = d_pre22$LW_OUT_1_1_1[d_pre22$doy>100 & d_pre22$doy<200],
                                   AirT = d_pre22$TA_1_1_1[d_pre22$doy>100 & d_pre22$doy<200],
                                   DDG ),
                        data.frame(Treatment = "Cicada_21",
                                   Flux = d_cicada2$FC_1_1_1[d_cicada2$doy>100 & d_cicada2$doy<200], 
                                   SoilT = d_cicada2$TS_2_1_1[d_cicada2$doy>100 & d_cicada2$doy<200],
                                   SoilVWC = d_cicada2$SWC_1_1_1[d_cicada2$doy>100 & d_cicada2$doy<200],
                                   Rad_out = d_cicada2$LW_OUT_1_1_1[d_cicada2$doy>100 & d_cicada2$doy<200],
                                   AirT = d_cicada2$TA_1_1_1[d_cicada2$doy>100 & d_cicada2$doy<200]),
                        data.frame(Treatment = "Cicada_04",
                                   Flux = d_cicada_042$FC_1_1_1[d_cicada_042$doy>100 & d_cicada_042$doy<200], 
                                   SoilT = d_cicada_042$TS_2_1_1[d_cicada_042$doy>100 & d_cicada_042$doy<200],
                                   SoilVWC = d_cicada_042$SWC_1_1_1[d_cicada_042$doy>100 & d_cicada_042$doy<200],
                                   Rad_out = d_cicada_042$LW_OUT_1_1_1[d_cicada_042$doy>100 & d_cicada_042$doy<200],
                                   AirT = d_cicada_042$TA_1_1_1[d_cicada_042$doy>100 & d_cicada_042$doy<200]))


plot(as.factor(d_environ_comp$Treatment), 
     d_environ_comp$Flux)

NEEMod <- lm(d_environ_comp$Flux~as.factor(d_environ_comp$Treatment))
summary(NEEMod)

plot(as.factor(d_environ_comp$Treatment), 
     d_environ_comp$SoilT)

SoilTMod <- lm(d_environ_comp$SoilT~as.factor(d_environ_comp$Treatment))
summary(SoilTMod)


plot(as.factor(d_environ_comp$Treatment), 
     d_environ_comp$SoilVWC)

SoilTMod <- lm(d_environ_comp$SoilT~as.factor(d_environ_comp$Treatment))
summary(SoilTMod)

plot(as.factor(d_environ_comp$Treatment), 
     d_environ_comp$AirT)

AirTMod <- lm(d_environ_comp$AirT~as.factor(d_environ_comp$Treatment))
summary(AirTMod)


plot(as.factor(d_environ_comp$Treatment), 
     d_environ_comp$Rad_out)

RadMod <- lm(d_environ_comp$Rad_out~as.factor(d_environ_comp$Treatment))
summary(RadMod)




cc <- Cicada$FC_1_1_1> -50 & Cicada$FC_1_1_1< 50 & Cicada$USTAR_1_1_1>0.29 & Cicada$PPFD_IN_1_1_1<5
Cicada_sumz = Cicada[cc,]  %>% 
  dplyr::group_by(doy)  %>% 
  dplyr::summarize(FC_Std_21 = sd(FC_1_1_1, na.rm = T), 
                   FC_Md_21 = median(FC_1_1_1, na.rm = T),
                   ST_Std_21 = sd(TS_2_1_1, na.rm = T), 
                   ST_Md_21 = median(TS_2_1_1, na.rm = T)  )

Cicada_sumz$FC_UVar_21 <- Cicada_sumz$FC_Md_21 + Cicada_sumz$FC_Std_21
Cicada_sumz$FC_LVar_21 <- Cicada_sumz$FC_Md_21 - Cicada_sumz$FC_Std_21

Cicada_sumz$ST_UVar_21 <- Cicada_sumz$ST_Md_21 + Cicada_sumz$ST_Std_21
Cicada_sumz$ST_LVar_21 <- Cicada_sumz$ST_Md_21 - Cicada_sumz$ST_Std_21


cc <- Cicada_04$FC_1_1_1> -50 & Cicada_04$FC_1_1_1< 50 & Cicada_04$USTAR_1_1_1>0.29 & Cicada_04$PPFD_IN_1_1_1<5
Cicada_04_sumz = Cicada_04[cc,]  %>% 
  dplyr::group_by(doy)  %>% 
  dplyr::summarize(FC_Std_04 = sd(FC_1_1_1, na.rm = T), 
                   FC_Md_04 = median(FC_1_1_1, na.rm = T),
                   ST_Std_04 = sd(TS_2_1_1, na.rm = T), 
                   ST_Md_04 = median(TS_2_1_1, na.rm = T)  )

Cicada_04_sumz$FC_UVar_04 <- Cicada_04_sumz$FC_Md_04 + Cicada_04_sumz$FC_Std_04
Cicada_04_sumz$FC_LVar_04 <- Cicada_04_sumz$FC_Md_04 - Cicada_04_sumz$FC_Std_04

Cicada_04_sumz$ST_UVar_04 <- Cicada_04_sumz$ST_Md_04 + Cicada_04_sumz$ST_Std_04
Cicada_04_sumz$ST_LVar_04 <- Cicada_04_sumz$ST_Md_04 - Cicada_04_sumz$ST_Std_04



PreCicada[cc,]
cc <- PreCicada$FC_1_1_1> -10 & PreCicada$FC_1_1_1< 20 & PreCicada$USTAR_1_1_1>0.29 & PreCicada$PPFD_IN_1_1_1<5
plot(PreCicada$FC_1_1_1[cc]~PreCicada$TS_2_1_1[cc])
plot(PreCicada$FC_1_1_1[cc]~PreCicada$doy[cc])


cc <- PreCicada$FC_1_1_1> -50 & PreCicada$FC_1_1_1< 50 & PreCicada$USTAR_1_1_1>0.29 & PreCicada$PPFD_IN_1_1_1<5

PreCicada_sumz = PreCicada[cc,]  %>% 
  dplyr::group_by(doy)  %>% 
  dplyr::summarize(FC_Std_10y = sd(FC_1_1_1, na.rm = T), 
                   FC_Md_10y = median(FC_1_1_1, na.rm = T),
                   ST_Std_10y = sd(TS_2_1_1, na.rm = T), 
                   ST_Md_10y = median(TS_2_1_1, na.rm = T)  )

PreCicada_sumz$FC_UVar_10y <- PreCicada_sumz$FC_Md_10y + PreCicada_sumz$FC_Std_10y
PreCicada_sumz$FC_LVar_10y <- PreCicada_sumz$FC_Md_10y - PreCicada_sumz$FC_Std_10y

PreCicada_sumz$ST_UVar_10y <- PreCicada_sumz$ST_Md_10y + PreCicada_sumz$ST_Std_10y
PreCicada_sumz$ST_LVar_10y <- PreCicada_sumz$ST_Md_10y - PreCicada_sumz$ST_Std_10y


cc <- PreCicada2$FC_1_1_1> -50 & PreCicada2$FC_1_1_1< 50 & PreCicada2$USTAR_1_1_1>0.29 & PreCicada2$PPFD_IN_1_1_1<5

PreCicada2_sumz = PreCicada2[cc,]  %>% 
  dplyr::group_by(doy)  %>% 
  dplyr::summarize(FC_Std_15y = sd(FC_1_1_1, na.rm = T), 
                   FC_Md_15y = median(FC_1_1_1, na.rm = T),
                   ST_Std_15y = sd(TS_2_1_1, na.rm = T), 
                   ST_Md_15y = median(TS_2_1_1, na.rm = T)  )

PreCicada2_sumz$FC_UVar_15y <- PreCicada2_sumz$FC_Md_15y + PreCicada2_sumz$FC_Std_15y
PreCicada2_sumz$FC_LVar_15y <- PreCicada2_sumz$FC_Md_15y - PreCicada2_sumz$FC_Std_15y

PreCicada2_sumz$ST_UVar_15y <- PreCicada2_sumz$ST_Md_15y + PreCicada2_sumz$ST_Std_15y
PreCicada2_sumz$ST_LVar_15y <- PreCicada2_sumz$ST_Md_15y - PreCicada2_sumz$ST_Std_15y


##// Eddy Flux figures
##// Day in which soil temperature reaches 68 F

plot(PreCicada2_sumz$doy,
     PreCicada2_sumz$ST_Md_15y)
abline(17.7, 0)
abline(v = TriggerAVG)

points(Cicada_04_sumz$doy, 
       Cicada_04_sumz$ST_Md_04, col = "red", pch = 15)
abline(v = 162, col = "red")

points(Cicada_sumz$doy, 
       Cicada_sumz$ST_Md_21, col = "darkred", pch = 15)
abline(v = 144, col = "darkred")

TriggerAVG <- mean(PreCicada2_sumz$doy[PreCicada2_sumz$doy<200 & PreCicada2_sumz$ST_Md_15y>17.5  & PreCicada2_sumz$ST_Md_15y<18], na.rm = T)
Trigger04 <- 162
Trigger21 <- 144

##// Average == 153.6667
##// 2021  == 144

ggplot() +
  geom_ribbon(data = PreCicada2_sumz, aes(x = doy, ymin = FC_LVar_15y, ymax = FC_UVar_15y),
              fill = "grey70", alpha = 0.4) + 

  geom_line(data = PreCicada2_sumz, aes(x = doy, y = FC_Md_15y),
            color = "black", size = 2, linetype = 1, alpha = 0.2) +
  geom_line(data = Cicada_sumz, aes(x = doy, y = FC_Md_21), color = "darkred",
            size = 1) +
  # 
  geom_line(data = Cicada_04_sumz, aes(x = doy, y = FC_Md_04), color = "red",
            size = 1) +
  
  geom_vline(xintercept = TriggerAVG, col = "black", linetype = 4, size = 2, alpha = 0.3) +
  geom_vline(xintercept = Trigger04, col = "red", linetype = 2, size = 2, alpha = 0.4) +
  geom_vline(xintercept = Trigger21, col = "darkred", linetype = 2, size = 2, alpha = 0.4) +
  

  xlab(expression(paste("Day of Year"))) +
  ylab(expression(paste("R" ["ECO"] , " [",mu, "mol " , " CO" ["2"], "  m"^"-2 ", "s"^"-1", "]"))) +
  ylim(-05,12)+
  xlim(0,200)+
  annotate("text", x = 80, y = -3, label = "2005-2020", color =  "grey70", size = 20) +
  annotate("text", x = 100, y = 7.5, label = "2021", color =  "darkred", size = 20) +
  annotate("text", x = 50, y = 5.5, label = "2004", color =  "red", size = 20) +
  #annotate("text", x = 3.5, y = 10.5, label = "camera", size = 10) +
  theme(legend.position = "NA",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species", size=30),
        legend.text=element_text(size=30)) 

Cicada_Comps <- inner_join(Cicada_sumz,  PreCicada_sumz, by = "doy")
Cicada_Comps2 <- inner_join(Cicada_Comps,  Cicada_04_sumz, by = "doy")
Cicada_Comps2 <- inner_join(Cicada_Comps2,  PreCicada2_sumz, by = "doy")
plot(Cicada_Comps$FC_Md.x[Cicada_Comps$doy>50 & Cicada_Comps$doy<200]~ 
     Cicada_Comps$FC_Md.y[Cicada_Comps$doy>50 & Cicada_Comps$doy<200])
abline(0,1)

comp_data <- rbind(data.frame(Treatment = "PreCicada_10Y",
                        Flux = Cicada_Comps2$FC_Md_10y[Cicada_Comps2$doy>50 & Cicada_Comps2$doy<200], 
                        SoilT = Cicada_Comps2$ST_Md_10y[Cicada_Comps2$doy>50 & Cicada_Comps2$doy<200]),
                   data.frame(Treatment = "PreCicada_16Y",
                              Flux = Cicada_Comps2$FC_Md_15y[Cicada_Comps2$doy>50 & Cicada_Comps2$doy<200], 
                              SoilT = Cicada_Comps2$ST_Md_15y[Cicada_Comps2$doy>50 & Cicada_Comps2$doy<200]),
                   
                   data.frame(Treatment = "Cicada_21",
                        Flux = Cicada_Comps2$FC_Md_21[Cicada_Comps2$doy>50 & Cicada_Comps2$doy<200],
                        SoilT = Cicada_Comps2$ST_Md_21[Cicada_Comps2$doy>50 & Cicada_Comps2$doy<200]),
                   data.frame(Treatment = "Cicada_04",
                              Flux = Cicada_Comps2$FC_Md_04[Cicada_Comps2$doy>50 & Cicada_Comps2$doy<200],
                              SoilT = Cicada_Comps2$ST_Md_04[Cicada_Comps2$doy>50 & Cicada_Comps2$doy<200]))

comp_data <- rbind(data.frame(Treatment = "PreCicada_16Y",
                              Flux = Cicada_Comps2$FC_Md_15y[Cicada_Comps2$doy>50 & Cicada_Comps2$doy<200], 
                              SoilT = Cicada_Comps2$ST_Md_15y[Cicada_Comps2$doy>50 & Cicada_Comps2$doy<200]),
                   
                   data.frame(Treatment = "Cicada_21",
                              Flux = Cicada_Comps2$FC_Md_21[Cicada_Comps2$doy>50 & Cicada_Comps2$doy<200],
                              SoilT = Cicada_Comps2$ST_Md_21[Cicada_Comps2$doy>50 & Cicada_Comps2$doy<200]),
                   data.frame(Treatment = "Cicada_04",
                              Flux = Cicada_Comps2$FC_Md_04[Cicada_Comps2$doy>50 & Cicada_Comps2$doy<200],
                              SoilT = Cicada_Comps2$ST_Md_04[Cicada_Comps2$doy>50 & Cicada_Comps2$doy<200]))



Cicada_effects <- lm(comp_data$Flux~ comp_data$Treatment)
summary(Cicada_effects)

Cicada_effects2 <- lm(comp_data$SoilT ~ comp_data$Treatment)
summary(Cicada_effects2)


##// No difference in FC between the 10 year mean and the cicada year
plot(Cicada_Comps$FC_Md.x[Cicada_Comps$doy>50 & Cicada_Comps$doy<200]~ 
     Cicada_Comps$FC_Md.y[Cicada_Comps$doy>50 & Cicada_Comps$doy<200])
abline(0,1)


plot(density(comp_data$SoilT[comp_data$Treatment=="Cicada_04"], na.rm = T))#, comp_data$SoilT[comp_data$Treatment=="Cicada_04"])
lines(density(comp_data$SoilT[comp_data$Treatment=="Cicada_21"], na.rm = T), col = "red")#, comp_data$SoilT[comp_data$Treatment=="Cicada_04"])


###//Model for testing differences between years
hist(comp_data$Flux)
##//Uniformed Prior
my.prior <- set_prior("uniform(-30000,30000)", class = "b")
# Reco_mod1 <- brm(
#   Flux ~ Treatment,
#     data = comp_data,
#     prior = my.prior,
#     family =  "normal",
#     iter = 2000,
#     chains = 4,
#     cores = 4,
#     control = list(adapt_delta = 0.90, max_treedepth = 15))
# save(Reco_mod1, file = "D:/Dropbox/Projects/Indiana/Data/CicadaFlux/ModelSaves/Reco_mod1.rda")
# rm(Rs_mod3)
##//Load Model output
load("D:/Dropbox/Projects/Indiana/Data/CicadaFlux/ModelSaves/Reco_mod1.rda")
##//Model summaries
summary(Reco_mod1, prob = 0.94)
bayes_R2(Reco_mod1, prob = c(0.03, 0.97))

##//Post hoc tests
#get the adjusted means
Reco_mod1_em <- emmeans (Reco_mod1,  ~ Treatment)
Reco_mod1_em

#get all possible contrasts
cont <- contrast(Reco_mod1_em, "tukey")
cont

#get the posterior draws from the contrasts
cont_posterior <- gather_emmeans_draws(cont)

#plot
ggplot(cont_posterior,
       aes(y = contrast, x = .value)) +
  geom_halfeyeh() +
  # facet_wrap(~wool) +
  geom_vline(xintercept = 0, color = "red", lty = 2)


#need to figure out how to make this look all posterior-y
emmip(Reco_mod1,  ~ Treatment, CIs = TRUE, cov.reduce = range)


plot(comp_data$Flux ~ as.factor(comp_data$Treatment))

##Next question: How about the Respiration sensitivity to temperature at the ecosystem scale
ggplot() +
  geom_ribbon(data = PreCicada_sumz, aes(x = ST_Md_10y , ymin = FC_LVar_10y, ymax = FC_UVar_10y), fill = "grey70") + 
  geom_point(data = PreCicada_sumz, aes(x = ST_Md_10y , y = FC_Md_10y), color = "black") + 
  geom_point(data = Cicada_sumz, aes(x = ST_Md_21, y = FC_Md_21), color = "darkred", 
            size = 2) +
  geom_point(data = Cicada_04_sumz, aes(x = ST_Md_04, y = FC_Md_04), color = "blue", 
             size = 2) +
  geom_vline(xintercept = 149, col = "black", linetype = 2, size = 2) +
  geom_vline(xintercept = 144, col = "red", linetype = 2, size = 2) +
  
  xlab(expression(paste("Soil Temperature [C]"))) +
  ylab(expression(paste("R" ["ECO"] , " [",mu, "mol " , " CO" ["2"], "  m"^"-2 ", "s"^"-1", "]"))) +
  ylim(-0,12)+
  xlim(0, 25)+
  annotate("text", x = 80, y = -3, label = "2011-2020", color =  "grey70", size = 20) +
  annotate("text", x = 100, y = 7.5, label = "2021", color =  "darkred", size = 20) +
  
  #annotate("text", x = 3.5, y = 10.5, label = "camera", size = 10) +
  theme(legend.position = "NA",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species", size=30),
        legend.text=element_text(size=30)) 


plot(density(Cicada_sumz$ST_Md_21, na.rm = T))
lines(density(Cicada_04_sumz$ST_Md_04, na.rm = T), col = "red")


##//Q10 models
##//10 year Average dataset

##//Model function (updated with year syntax)
formula <- bf(
  FC_Md_10y ~ asym * exp(scale * ST_Md_10y),
  scale ~ 1,
  asym ~ 1,
  sigma ~ 1,
  nl = TRUE)

# quick and dirty priors. you need priors to even fit a model
##//Prior must be positive
prior1 <- c(
  prior(normal(0.2, 1), nlpar = "asym"),
  # the population average slope is constrained to be positive
  prior(normal(0.0699, 1), nlpar = "scale"))


hist(log(PreCicada_sumz$FC_Md_10y[PreCicada_sumz$doy>50 & PreCicada_sumz$doy<200  & PreCicada_sumz$FC_Md_10y>0]))


plot(PreCicada_sumz$FC_Md_10y[PreCicada_sumz$doy>50 & PreCicada_sumz$doy<200  & PreCicada_sumz$FC_Md_10y>0]~
       PreCicada_sumz$ST_Md_10y[PreCicada_sumz$doy>50 & PreCicada_sumz$doy<200 & PreCicada_sumz$FC_Md_10y>0])

range(PreCicada_sumz$ST_Md_10y, na.rm = T)
cc <- PreCicada_sumz$doy>50 & PreCicada_sumz$doy<200 & PreCicada_sumz$FC_Md_10y>0 & PreCicada_sumz$ST_Md_10y<23
# fit_PreCicada_10y <- brm(
#   formula,
#   data = PreCicada_sumz[cc,],
#   prior = prior1,
#   family =  "lognormal",
#   iter = 10000,
#   chains = 3,
#   #cores = 4,
#   control = list(adapt_delta = 0.90, max_treedepth = 15))
# save(fit_PreCicada_10y, file = "D:/Dropbox/Projects/Indiana/Data/CicadaFlux/ModelSaves/fit_PreCicada_10y.rda")

#fit_PreCicada_10y <- 
  load( file = "D:/Dropbox/Projects/Indiana/Data/CicadaFlux/ModelSaves/fit_PreCicada_10y.rda")

summary(fit_PreCicada_10y, prob = 0.89)
bayes_R2(fit_PreCicada_10y, prob = c(0.055, 0.89))
plot_model(fit_PreCicada_10y, type = "pred", terms = c("ST_Md_10y"), show.data = TRUE) + 
theme_bw()

##//16 year data set

##//Model function (updated with year syntax)
formula <- bf(
  FC_Md_15y ~ asym * exp(scale * ST_Md_15y),
  scale ~ 1,
  asym ~ 1,
  sigma ~ 1,
  nl = TRUE)

# quick and dirty priors. you need priors to even fit a model
##//Prior must be positive
prior1 <- c(
  prior(normal(0.2, 1), nlpar = "asym"),
  # the population average slope is constrained to be positive
  prior(normal(0.0699, 1), nlpar = "scale"))


hist(log(PreCicada2_sumz$FC_Md_15y[PreCicada2_sumz$doy>50 & 
                                     PreCicada2_sumz$doy<200  & 
                                     PreCicada2_sumz$FC_Md_15y>0]))


plot(PreCicada2_sumz$FC_Md_15y[PreCicada2_sumz$doy>50 & 
                                PreCicada2_sumz$doy<200 & 
                                PreCicada2_sumz$FC_Md_15y>0]~
       PreCicada2_sumz$ST_Md_15y[PreCicada2_sumz$doy>50 & 
                                  PreCicada2_sumz$doy<200 & 
                                  PreCicada2_sumz$FC_Md_15y>0])

range(PreCicada2_sumz$ST_Md_15y, na.rm = T)
cc <- PreCicada2_sumz$doy>50 & PreCicada2_sumz$doy<200 & PreCicada2_sumz$FC_Md_15y>0 & PreCicada2_sumz$ST_Md_15y<23
# fit_PreCicada_16y <- brm(
#   formula,
#   data = PreCicada2_sumz[cc,],
#   prior = prior1,
#   family =  "lognormal",
#   iter = 10000,
#   chains = 3,
#   #cores = 4,
#   control = list(adapt_delta = 0.90, max_treedepth = 15))
# save(fit_PreCicada_16y, file = "D:/Dropbox/Projects/Indiana/Data/CicadaFlux/ModelSaves/fit_PreCicada_16y.rda")
# 
#fit_PreCicada_16y <- 
load( file = "D:/Dropbox/Projects/Indiana/Data/CicadaFlux/ModelSaves/fit_PreCicada_16y.rda")

summary(fit_PreCicada_16y, prob = 0.89)
bayes_R2(fit_PreCicada_16y, prob = c(0.055, 0.89))
plot_model(fit_PreCicada_16y, type = "pred", terms = c("ST_Md_15y"), show.data = TRUE) + 
  theme_bw()


##//2021 model
##//Model function (updated with year syntax)
formula <- bf(
  FC_Md_21 ~ asym * exp(scale * ST_Md_21),
  scale ~ 1,
  asym ~ 1,
  sigma ~ 1,
  nl = TRUE)

# quick and dirty priors. you need priors to even fit a model
##//Prior must be positive
prior1 <- c(
  prior(normal(0.2, 1), nlpar = "asym"),
  # the population average slope is constrained to be positive
  prior(normal(0.0699, 1), nlpar = "scale"))

cc <- Cicada_sumz$doy>50 & Cicada_sumz$doy<200 & Cicada_sumz$FC_Md_21>0
hist(log(Cicada_sumz$FC_Md_21[cc]))

# fit_Cicada_21 <- brm(
#   formula,
#   data = Cicada_sumz[cc,],
#   prior = prior1,
#   family =  "lognormal",
#   iter = 10000,
#   chains = 3,
#   #cores = 4,
#   control = list(adapt_delta = 0.90, max_treedepth = 15))
# save(fit_Cicada_21, file = "D:/Dropbox/Projects/Indiana/Data/CicadaFlux/ModelSaves/fit_Cicada_21_RECO.rda")
# #fit_Cicada_21 <- 
  load(file = "D:/Dropbox/Projects/Indiana/Data/CicadaFlux/ModelSaves/fit_Cicada_21_RECO.rda")

summary(fit_Cicada_21, prob = 0.89)
bayes_R2(fit_Cicada_21, prob = c(0.055, 0.89))
plot_model(fit_Cicada_21, type = "pred", terms = c("ST_Md_21"), show.data = TRUE) + 
  theme_bw()

##//2004 model
##//Model function (updated with year syntax)
formula <- bf(
  FC_Md_04 ~ asym * exp(scale * ST_Md_04),
  scale ~ 1,
  asym ~ 1,
  sigma ~ 1,
  nl = TRUE)

# quick and dirty priors. you need priors to even fit a model
##//Prior must be positive
prior1 <- c(
  prior(normal(0.2, 1), nlpar = "asym"),
  # the population average slope is constrained to be positive
  prior(normal(0.0699, 1), nlpar = "scale"))

cc <- Cicada_04_sumz$doy>50 & Cicada_04_sumz$doy<200 & Cicada_04_sumz$FC_Md_04>0
hist(log(Cicada_04_sumz$FC_Md_04[cc]))

# fit_Cicada_04 <- brm(
#   formula,
#   data = Cicada_04_sumz[cc,],
#   prior = prior1,
#   family =  "lognormal",
#   iter = 10000,
#   chains = 3,
#   #cores = 4,
#   control = list(adapt_delta = 0.90, max_treedepth = 15))
# save(fit_Cicada_04, file = "D:/Dropbox/Projects/Indiana/Data/CicadaFlux/ModelSaves/fit_Cicada_04_RECO.rda")
#fit_Cicada_04 <- 
  load(file = "D:/Dropbox/Projects/Indiana/Data/CicadaFlux/ModelSaves/fit_Cicada_04_RECO.rda")

summary(fit_Cicada_04, prob = 0.89)
bayes_R2(fit_Cicada_04, prob = c(0.055, 0.89))
plot_model(fit_Cicada_04, type = "pred", terms = c("ST_Md_04"), show.data = TRUE) + 
  theme_bw()


##//Calculate Q10
fit_cicada_21_ex <- prepare_predictions(fit_Cicada_21)
EC_cicada_21_q10 <- exp(10 * fit_cicada_21_ex$nlpars$scale$fe$b)

##//Calculate Q10
fit_cicada_04_ex <- prepare_predictions(fit_Cicada_04)
EC_cicada_04_q10 <- exp(10 * fit_cicada_04_ex$nlpars$scale$fe$b)

fit_Precicada_10_ex <- prepare_predictions(fit_PreCicada_10y)
EC_Precicada_10_q10 <- exp(10 * fit_Precicada_10_ex$nlpars$scale$fe$b)

fit_Precicada_16_ex <- prepare_predictions(fit_PreCicada_16y)
EC_Precicada_16_q10 <- exp(10 * fit_Precicada_16_ex$nlpars$scale$fe$b)


EC_Q10_d <- rbind(data.frame(Treatment = "10 Yr Avg",
                             Q10 = sample(EC_Precicada_10_q10, size = 2000)),
                  data.frame(Treatment = "16 Yr Avg",
                             Q10 = sample(EC_Precicada_16_q10, size = 2000)),
                  data.frame(Treatment = "2021",
                             Q10 = sample(EC_cicada_21_q10, size = 2000)),
                  data.frame(Treatment = "2004",
                             Q10 = sample(EC_cicada_04_q10, size = 2000)))
names(EC_Q10_d)[2] <- "Q10"

ggplot() +
  geom_violin(data = EC_Q10_d[EC_Q10_d$Treatment!="10 Yr Avg",],
              aes(y = Q10, x = Treatment, fill = Treatment), color = "black",
               show.legend = TRUE, trim = T, size = 1, alpha = 0.7) +
  geom_boxplot(data = EC_Q10_d[EC_Q10_d$Treatment!="10 Yr Avg",], 
               aes(y = Q10, x = Treatment, fill = Treatment), color = "black",
              show.legend = FALSE,  size = 1.5, alpha = 0.1, varwidth = 0.1) +
  
  xlab('') +
  ylab(expression(paste("R" ["ECO "],  "Q" ["10 "] ))) +
#  annotate("text", x = EC_Q10_d$Treatment[1], y = 6, label = "b)                ", size = 20) +
  #scale_color_manual(values = GoAvsGo) +
  scale_fill_manual(values =  c("#191919", "#fc0d03", "#943b37")) + 
  theme(legend.position= "NA",
        axis.text=element_text(size=30),
        axis.title=element_text(size=40),
        legend.title=element_text("Treatment",size=30),
        legend.text=element_text(size=30))

Q10Plot


EC_Q10_sum = EC_Q10_d %>%
  #  dplyr::mutate(TIMESTAMP_agg = floor_date(TIMESTAMP_ct, unit = "day"), ) %>%
  dplyr::group_by(Treatment) %>%
  dplyr::summarise_if(is.numeric, median, na.rm=T) 

(abs(2.86-3.85) / ((2.86+3.85)/2) )*100
##//2004 29.5082 % increase in Q10 relative to 16 year average 
(abs(2.86-3.57) / ((2.86+3.57)/2) )*100
##//2021 22.08398 % increase in Q10 relative to 16 year average


##//Are Q10s different
my.prior <- set_prior("uniform(-30000,30000)", class = "b")
##//Model
Q10_mod2 <- brm(
    Q10 ~ Treatment,
    data = EC_Q10_d,
    prior = my.prior,
    family =  "normal",
    iter = 2000,
    chains = 4,
    # cores = 4,
    control = list(adapt_delta = 0.90, max_treedepth = 15))
save(Q10_mod2, file = "D:/Dropbox/Projects/Indiana/Data/CicadaFlux/ModelSaves/Q10_mod2_all.rda")
#Q10_mod2 <- 
  load("D:/Dropbox/Projects/Indiana/Data/CicadaFlux/ModelSaves/Q10_mod2_all.rda")
summary(Q10_mod2)

##//Test posteriors
hypQ10 = hypothesis(
  Q10_mod2,
  hypothesis = c(
    "Intercept = Intercept + TreatmentCicada2004",
    "Intercept = Intercept + TreatmentCicada2021"
  ))
hypQ10

library(emmeans)
##//Post hoc tests
#get the adjusted means
Q10_mod2_em <- emmeans (Q10_mod2,  ~ Treatment)
Q10_mod2_em

#get all possible contrasts
cont <- contrast(Q10_mod2_em, "tukey")
cont

#get the posterior draws from the contrasts
cont_posterior <- gather_emmeans_draws(cont)

#plot
ggplot(cont_posterior,
       aes(y = contrast, x = .value)) +
  geom_halfeyeh() +
  # facet_wrap(~wool) +
  geom_vline(xintercept = 0, color = "red", lty = 2)


#need to figure out how to make this look all posterior-y
emmip(Q10_mod2, ~ Treatment, CIs = TRUE, cov.reduce = range)




ggplot() +
  geom_violin(data = comp_data, aes(y = SoilT, x = Treatment, fill = Treatment), color = "black",
              show.legend = TRUE,  size = 1, alpha = 0.7) +
  geom_boxplot(data = comp_data, aes(y = SoilT, x = Treatment), color = "black",
               show.legend = FALSE,  size = 0.1, alpha = 0.1, varwidth = 0.2) +
  
  xlab('Treatment') +
  ylab(expression(paste("R" ["ECO "],  "Q" ["10 "] ))) +
  #  annotate("text", x = EC_Q10_d$Treatment[1], y = 6, label = "b)                ", size = 20) +
  #scale_color_manual(values = GoAvsGo) +
  scale_fill_manual(values = c("grey70", "darkred", "red", "pink")) + 
  theme(legend.position= "NA",
        axis.text=element_text(size=30),
        axis.title=element_text(size=40),
        legend.title=element_text("Treatment",size=30),
        legend.text=element_text(size=30))


##//Calculate Q10
fit_nohole_ex <- prepare_predictions(fit_nohole)
survey_nohole_q10 <- exp(10 * fit_nohole_ex$nlpars$scale$fe$b)





plot(Cicada_sumz$doy, Cicada_sumz$FC_Md)


cc <- PreCicada$FC_1_1_1> -50 & PreCicada$FC_1_1_1< 50 & PreCicada$USTAR_1_1_1>0.29 & PreCicada$PPFD_IN_1_1_1<10

d_pre = PreCicada[cc,] %>%
  #dplyr::mutate(TIMESTAMP_agg = floor_date(TIMESTAMP_ct, unit = "day"), ) %>%
  dplyr::group_by(doy) %>%
  dplyr::summarise_if(is.numeric, mean, na.rm=T) 


cc <- Cicada$FC_1_1_1> -50 & Cicada$FC_1_1_1< 50 & Cicada$USTAR_1_1_1>0.29 & Cicada$PPFD_IN_1_1_1<10
d_cicada = Cicada[cc,] %>%
  #  dplyr::mutate(TIMESTAMP_agg = floor_date(TIMESTAMP_ct, unit = "day"), ) %>%
  dplyr::group_by(doy) %>%
  dplyr::summarise_if(is.numeric, mean, na.rm=T) 

ff <- d_pre$doy>100 & d_pre$doy<200  
gg <- d_cicada$doy>100 & d_cicada$doy<200  

plot(d_pre$TS_2_1_1[ff], d_pre$FC_1_1_1[ff], type = "b", ylim = c(-1,20), xlim = c(1,30))
points(d_cicada$TS_2_1_1[gg], d_cicada$FC_1_1_1[gg], type = "b", col = "red")





abline(v=149)
abline(v=144, col = "red", lwd = 2)

d_pre2 = d_pre %>%
  #dplyr::mutate(TIMESTAMP_agg = floor_date(TIMESTAMP_ct, unit = "day"), ) %>%
  dplyr::group_by(doy) %>%
  dplyr::summarise_if(is.numeric, median, na.rm=T) 

d_cicada2 = d_cicada %>%
  #dplyr::mutate(TIMESTAMP_agg = floor_date(TIMESTAMP_ct, unit = "day"), ) %>%
  dplyr::group_by(doy) %>%
  dplyr::summarise_if(is.numeric, median, na.rm=T) 


plot(d_pre2$doy, d_pre2$FC_1_1_1)

d_pre2 = PreCicada %>%
  dplyr::mutate(TIMESTAMP_agg = floor_date(TIMESTAMP_ct, unit = "day"), ) %>%
  dplyr::group_by(TIMESTAMP_agg) %>%
  dplyr::summarise_if(is.numeric, sum, na.rm=T) 

d_cicada2 = Cicada %>%
  dplyr::mutate(TIMESTAMP_agg = floor_date(TIMESTAMP_ct, unit = "day"), ) %>%
  dplyr::group_by(TIMESTAMP_agg) %>%
  dplyr::summarise_if(is.numeric, sum, na.rm=T) 




d_pre3 = d_pre2 %>%
  #dplyr::mutate(TIMESTAMP_agg = floor_date(TIMESTAMP_ct, unit = "day"), ) %>%
  dplyr::group_by(doy) %>%
  dplyr::summarise_if(is.numeric, mean, na.rm=T) 

d_cicada3 = d_cicada2 %>%
  #dplyr::mutate(TIMESTAMP_agg = floor_date(TIMESTAMP_ct, unit = "day"), ) %>%
  dplyr::group_by(doy) %>%
  dplyr::summarise_if(is.numeric, mean, na.rm=T) 



cc <- d_cicada$doy> 100 & d_cicada$doy < 200 & d_cicada$TS_2_1_1 > 10.7778 & d_cicada$TS_2_1_1 < 24.7778
dd <- d_pre$doy> 100 & d_pre$doy < 200 & d_pre$TS_2_1_1 > 10.7778 & d_pre$TS_2_1_1 < 24.7778

plot(d_pre$doy[dd], d_pre$FC_1_1_1[dd], ylim = c(-10, 8), col = "black", pch = 15, type = "b")
points(d_cicada$doy[cc], d_cicada$FC_1_1_1[cc], col = "red", pch = 15, type = "b")
abline(v=149)
abline(v=144, col = "red", lwd = 2)


plot(d_pre$doy, d_pre$FC_1_1_1, col = "black", pch = 15, type = "b")
points(d_cicada$doy, d_cicada$FC_1_1_1, col = "red", pch = 15, type = "b")
abline(v=149)
abline(v=144, col = "red", lwd = 2)

plot(d_pre2$doy, d_pre2$FC_1_1_1, col = "black", pch = 15, type = "b")
points(d_cicada2$doy, d_cicada2$FC_1_1_1, col = "red", pch = 15, type = "b")
abline(v=149)
abline(v=144, col = "red", lwd = 2)


plot(d_pre3$doy, d_pre3$FC_1_1_1, col = "black", pch = 15, type = "b")
points(d_cicada3$doy, d_cicada3$FC_1_1_1, col = "red", pch = 15, type = "b")
abline(v=149)
abline(v=144, col = "red", lwd = 2)





plot(d_pre$doy, d_pre$FC_1_1_1, ylim = c(-8, 8), type = "b")
points(d_cicada$doy, d_cicada$FC_1_1_1, col = "red", pch = 15, type = "b")
abline(v=149)
abline(v=144, col = "red", lwd = 2)

d_pre$doy[d_pre$TS_2_1_1 > 17.7778]
d_cicada$doy[d_cicada$TS_2_1_1 > 17.7778]
