Rs_plots
library(ggExtra)

##//Idea is to make pairwise figure

Rs_plots$Site_Pairs <-as.character(paste(Rs_plots$Site, Rs_plots$Pair, sep = "_"))


names(Rs_plots)
Rs_plots_sum <- Rs_plots[Rs_plots$Corr_LinFlux<=6,] %>%
  #dplyr::mutate(day = as.Date(Date_ct, format="%d-%m-%Y")) %>%
  dplyr::group_by(Date, Site_Pairs, Hole_Collar, Symbiont) %>% # group by the day column ##//mean looks good
  dplyr::summarise_if(is.numeric, median, na.rm = TRUE) %>%  
  na.omit

names(Rs_plots_sum)
Holes <- Rs_plots_sum[Rs_plots_sum$Hole_Collar==1, c(1:4, 45:46, 56:58)]
NoHoles <- Rs_plots_sum[Rs_plots_sum$Hole_Collar==0, c(1:4, 45:46, 56:58)]

Figs <- inner_join(Holes, NoHoles, by = c("Date", "Site_Pairs", "Symbiont"))
names(Figs)
names(Figs)[7] <- "Corr_LinFluxHoles"
names(Figs)[8] <- "Corr_LinFluxHoles_LCI"
names(Figs)[9] <- "Corr_LinFluxHoles_HCI"

names(Figs)[13] <- "Corr_LinFluxNoHoles"
names(Figs)[14] <- "Corr_LinFluxNoHoles_LCI"
names(Figs)[15] <- "Corr_LinFluxNoHoles_HCI"


plot(Figs$Corr_LinFluxHoles~ Figs$Corr_LinFluxNoHoles)
abline(0,1)

plot(Figs$Corr_LinFluxHoles~ Figs$Corr_LinFluxNoHoles)


Figs$Date

ECM_Figs_m1 <- lm(Figs$Corr_LinFluxHoles[Figs$Symbiont=="ECM" & Figs$SoilVWC_pct.x<30] ~
                    Figs$Corr_LinFluxNoHoles[Figs$Symbiont=="ECM" & Figs$SoilVWC_pct.x<30])
AM_Figs_m1 <- lm(Figs$Corr_LinFluxHoles[Figs$Symbiont=="AM" & Figs$SoilVWC_pct.x<30] ~ 
                   Figs$Corr_LinFluxNoHoles[Figs$Symbiont=="AM" & Figs$SoilVWC_pct.x<30])


p <- ggplot() +
  geom_abline(slope = 1, intercept = 0, size = 3, linetype = 2, color = "black") +
  # geom_abline(slope = ECM_Figs_m1$coefficients[2], intercept = ECM_Figs_m1$coefficients[1], 
  #             size = 3, linetype = 2, color = GoAvsGo[2]) +
  # geom_abline(slope = AM_Figs_m1$coefficients[2], intercept = AM_Figs_m1$coefficients[1], 
  #             size = 3, linetype = 2, color = GoAvsGo[1]) +
  
  
  geom_point(data = Figs[Figs$SoilVWC_pct.x<40,], aes(x = Corr_LinFluxNoHoles, y = Corr_LinFluxHoles, color = Symbiont), 
             show.legend = TRUE,  size = 14, alpha = 0.8) + 
  scale_color_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 

  #geom_abline(slope = 1, intercept = 0, size = 3) + 
  xlab(expression(paste("No Hole Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  ylab(expression(paste("Hole Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  xlim(0,5)+
  ylim(0,5)+
  theme(legend.position = "top",
        axis.text=element_text(size=50),
        axis.title=element_text(size=45),
        legend.title=element_text("Species",size=50),
        legend.text=element_text(size=50))


p2 <- ggMarginal(p, data = Figs[Figs$SoilVWC_pct.x<40,], type="densigram")

ggplot() +
  geom_abline(slope = 1, intercept = 0, size = 3, linetype = 2, color = "black") +
  # geom_abline(slope = ECM_Figs_m1$coefficients[2], intercept = ECM_Figs_m1$coefficients[1], 
  #             size = 3, linetype = 2, color = GoAvsGo[2]) +
  # geom_abline(slope = AM_Figs_m1$coefficients[2], intercept = AM_Figs_m1$coefficients[1], 
  #             size = 3, linetype = 2, color = GoAvsGo[1]) +
  
  
  geom_point(data = Figs[Figs$SoilVWC_pct.x>40,], aes(x = Corr_LinFluxNoHoles, y = Corr_LinFluxHoles, color = Symbiont), 
             show.legend = TRUE,  size = 14, alpha = 0.8) + 
  scale_color_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  
  #geom_abline(slope = 1, intercept = 0, size = 3) + 
  xlab(expression(paste("No Hole Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  ylab(expression(paste("Hole Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  xlim(0,5)+
  ylim(0,5)+
  theme(legend.position = "NA",
        axis.text=element_text(size=50),
        axis.title=element_text(size=45),
        legend.title=element_text("Species",size=50),
        legend.text=element_text(size=50))



ggplot() +
  geom_abline(slope = 1, intercept = 0, size = 3, linetype = 2, color = "black") +
  # geom_abline(slope = ECM_Figs_m1$coefficients[2], intercept = ECM_Figs_m1$coefficients[1], 
  #             size = 3, linetype = 2, color = GoAvsGo[2]) +
  # geom_abline(slope = AM_Figs_m1$coefficients[2], intercept = AM_Figs_m1$coefficients[1], 
  #             size = 3, linetype = 2, color = GoAvsGo[1]) +
  
  
  geom_point(data = Figs, aes(x = Corr_LinFluxNoHoles, y = Corr_LinFluxHoles, color = Symbiont), 
             show.legend = TRUE,  size = 14, alpha = 0.8) + 
  scale_color_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  
  #geom_abline(slope = 1, intercept = 0, size = 3) + 
  xlab(expression(paste("No Hole Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  ylab(expression(paste("Hole Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  xlim(0,5)+
  ylim(0,5)+
  theme(legend.position = "top",
        axis.text=element_text(size=50),
        axis.title=element_text(size=45),
        legend.title=element_text("Species",size=50),
        legend.text=element_text(size=50))



Figs_m1 <- lm(Figs$Corr_LinFluxHoles ~ Figs$Corr_LinFluxNoHoles)

ggplot() +
  geom_abline(slope = 1, intercept = 0, size = 3, linetype = 2, color = "black") +
  geom_abline(slope = Figs_m1$coefficients[2], intercept = Figs_m1$coefficients[1],
              size = 5, linetype = 2, color = GoAvsGo[3]) +

  geom_point(data = Figs, aes(x = Corr_LinFluxNoHoles, y = Corr_LinFluxHoles), 
             color = GoAvsGo[3], show.legend = TRUE,  size = 5, alpha = 0.8) + 
  scale_color_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  
  #geom_abline(slope = 1, intercept = 0, size = 3) + 
  xlab(expression(paste("No Hole Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  ylab(expression(paste("Hole Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  # xlim(0,5)+
  # ylim(0,5)+
  theme(legend.position = "top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30))

Figs$Date[c(1:49)]
Figs_Early <- Figs[c(1:49),]
Figs_Early$Date

Figs_Late <- Figs[c(50:144),]



ggplot() +
  geom_abline(slope = 1, intercept = 0, size = 3, linetype = 2, color = "black") +
  # geom_abline(slope = Figs_m1$coefficients[2], intercept = Figs_m1$coefficients[1],
  #             size = 5, linetype = 2, color = GoAvsGo[3]) +
  
  geom_point(data = Figs_Early[Figs_Early$SoilVWC_pct.x<30,], aes(x = Corr_LinFluxNoHoles, y = Corr_LinFluxHoles), 
             color = GoAvsGo[3], show.legend = TRUE,  size = 5, alpha = 0.8) + 
  
  geom_point(data = Figs_Late[Figs_Late$SoilVWC_pct.x<30,], aes(x = Corr_LinFluxNoHoles, y = Corr_LinFluxHoles), 
             color = GoAvsGo[4], show.legend = TRUE,  size = 5, alpha = 0.8) + 
  
  #scale_color_manual(values = GoAvsGo)+ 
  
  #geom_abline(slope = 1, intercept = 0, size = 3) + 
  xlab(expression(paste("No Hole Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  ylab(expression(paste("Hole Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  # xlim(0,5)+
  # ylim(0,5)+
  theme(legend.position = "top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30))


ggplot() +
  geom_abline(slope = 1, intercept = 0, size = 3, linetype = 2, color = "black") +
  # geom_abline(slope = Figs_m1$coefficients[2], intercept = Figs_m1$coefficients[1],
  #             size = 5, linetype = 2, color = GoAvsGo[3]) +
  
  geom_point(data = Figs_Early[Figs_Early$SoilVWC_pct.x>30,], aes(x = Corr_LinFluxNoHoles, y = Corr_LinFluxHoles), 
             color = GoAvsGo[3], show.legend = TRUE,  size = 5, alpha = 0.8) + 
  
  geom_point(data = Figs_Late[Figs_Late$SoilVWC_pct.x>30,], aes(x = Corr_LinFluxNoHoles, y = Corr_LinFluxHoles), 
             color = GoAvsGo[4], show.legend = TRUE,  size = 5, alpha = 0.8) + 
  
  #scale_color_manual(values = GoAvsGo)+ 
  
  #geom_abline(slope = 1, intercept = 0, size = 3) + 
  xlab(expression(paste("No Hole Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  ylab(expression(paste("Hole Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  # xlim(0,5)+
  # ylim(0,5)+
  theme(legend.position = "top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30))






ggplot() +
  geom_abline(slope = 1, intercept = 0, size = 3, linetype = 2, color = "black") +
  # geom_abline(slope = Figs_m1$coefficients[2], intercept = Figs_m1$coefficients[1],
  #             size = 5, linetype = 2, color = GoAvsGo[3]) +
  
  geom_point(data = Figs_Early, aes(x = Corr_LinFluxNoHoles, y = Corr_LinFluxHoles), 
             color = GoAvsGo[3], show.legend = TRUE,  size = 5, alpha = 0.8) + 
  
  geom_point(data = Figs_Late, aes(x = Corr_LinFluxNoHoles, y = Corr_LinFluxHoles), 
             color = GoAvsGo[4], show.legend = TRUE,  size = 5, alpha = 0.8) + 
  
  #scale_color_manual(values = GoAvsGo)+ 
  
  #geom_abline(slope = 1, intercept = 0, size = 3) + 
  xlab(expression(paste("No Hole Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  ylab(expression(paste("Hole Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  # xlim(0,5)+
  # ylim(0,5)+
  theme(legend.position = "top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30))







AllResid <- resid(lm(Figs$Corr_LinFluxHoles[Figs$SoilVWC_pct.x<30] - 
                                 Figs$Corr_LinFluxNoHoles[Figs$SoilVWC_pct.x<30] ~ 0))
plot(AllResid, Figs$SoilVWC_pct.y[Figs$SoilVWC_pct.x<30])
plot(AllResid, Figs$SoilT_C.x[Figs$SoilVWC_pct.x<30])

plot(AllResid~ as.factor(Figs$Date[Figs$SoilVWC_pct.x<30]))
plot(Figs_Late$AllResid~ as.factor(Figs_Late$Symbiont))


ggplot() +
  geom_abline(slope = 1, intercept = 0, size = 3, linetype = 2, color = "black") +
  # geom_abline(slope = Figs_m1$coefficients[2], intercept = Figs_m1$coefficients[1],
  #             size = 5, linetype = 2, color = GoAvsGo[3]) +
  
  geom_point(data = Figs[Figs$SoilT_C.x<30,], aes(x = Corr_LinFluxNoHoles, y = Corr_LinFluxHoles, 
                                                      color = Symbiont), 
              show.legend = TRUE,  size = 5, alpha = 0.8) + 
  

  scale_color_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  
  #geom_abline(slope = 1, intercept = 0, size = 3) + 
  xlab(expression(paste("No Hole Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  ylab(expression(paste("Hole Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  # xlim(0,5)+
  # ylim(0,5)+
  theme(legend.position = "top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30))




hist(log(Rs_plots$Corr_LinFlux))
##//Modeling
my.prior <- set_prior("uniform(-30,30)", class = "b")

Bmod <- brm(Corr_LinFlux ~ n_holes*SoilVWC_pct,
            data = Rs_plots, prior = my.prior,
            family =  "lognormal",
            iter = 30000, seed = 1234,
            save_pars = save_pars(all = TRUE),
            chains = 4,
            # cores = 4,
            control = list(adapt_delta = 0.99, max_treedepth = 15))


summary(Bmod, prob = 0.94)

bayes_R2(Bmod, prob = c(0.03, 0.97))
plot_model(Bmod, type = "pred", terms = c("n_holes", "SoilVWC_pct")) + 
  theme_bw()

plot_model(Bmod, type = "pred", terms = c("SoilVWC_pct", "n_holes")) + 
  theme_bw()


plot_model(Bmod, type = "emm", terms = c("n_holes", "SoilVWC_pct")) + 
  theme_bw()

plot_model(Bmod, type = "int", terms = c("n_holes", "SoilVWC_pct")) + 
  theme_bw()

plot_model(Bmod, type = "int", terms = c("SoilVWC_pct")) + 
  theme_bw()

################################################################
str(Rs_plots)
Rs_plots$n_holes_f <- as.factor(Rs_plots$n_holes)
##//Modeling
my.prior <- set_prior("uniform(-30,30)", class = "b")
Bmod_all <- brm(Corr_LinFlux ~ SoilVWC_pct*n_holes_f + Symbiont + SoilT_C + (1|Site),
            data = Rs_plots[Rs_plots$Corr_LinFlux<6,], prior = my.prior,
            family =  "lognormal",
            iter = 30000, seed = 1234,
            save_pars = save_pars(all = TRUE),
            chains = 4,
            # cores = 4,
            control = list(adapt_delta = 0.99, max_treedepth = 15))
summary(Bmod_all, prob = 0.89)

#bayes_R2(Bmod_all, prob = c(0.03, 0.97))
plot_model(Bmod_all, type = "pred", terms = c("n_holes_f", "SoilVWC_pct")) + 
  theme_bw()

plot_model(Bmod_all, type = "pred", terms = c("SoilVWC_pct", "n_holes_f")) + 
  theme_bw()

plot_model(Bmod_all, type = "pred", terms = c("Symbiont", "SoilVWC_pct")) + 
  theme_bw()

plot_model(Bmod_all, type = "pred", terms = c("SoilVWC_pct", "Symbiont")) + 
  theme_bw()

plot_model(Bmod_all, type = "emm", terms = c("n_holes_f", "SoilVWC_pct")) + 
  theme_bw()
plot_model(Bmod_all, type = "emm", terms = c("SoilVWC_pct", "n_holes_f")) + 
  theme_bw()
plot_model(Bmod_all, type = "emm", terms = c("SoilVWC_pct", "n_holes_f", "Symbiont")) + 
  theme_bw()



#################################################
Bmod_all2 <- brm(Corr_LinFlux ~ SoilVWC_pct*n_holes + Symbiont + SoilT_C + (1|Site),
                data = Rs_plots[Rs_plots$Corr_LinFlux<6,], prior = my.prior,
                family =  "lognormal",
                iter = 30000, seed = 1234,
                save_pars = save_pars(all = TRUE),
                chains = 4,
                # cores = 4,
                control = list(adapt_delta = 0.99, max_treedepth = 15))
summary(Bmod_all2, prob = 0.89)

#bayes_R2(Bmod_all, prob = c(0.03, 0.97))
plot_model(Bmod_all2, type = "pred", terms = c("n_holes", "SoilVWC_pct")) + 
  theme_bw()

plot_model(Bmod_all2, type = "pred", terms = c("Symbiont", "n_holes", "SoilVWC_pct")) + 
  theme_bw()


plot_model(Bmod_all2, type = "pred", terms = c("SoilVWC_pct", "n_holes")) + 
  theme_bw()

plot_model(Bmod_all2, type = "pred", terms = c("Symbiont", "SoilVWC_pct")) + 
  theme_bw()

plot_model(Bmod_all2, type = "pred", terms = c("SoilVWC_pct", "Symbiont")) + 
  theme_bw()

plot_model(Bmod_all2, type = "pred", terms = c("SoilVWC_pct", "Symbiont", "n_holes")) + 
  theme_bw()


plot_model(Bmod_all2, type = "emm", terms = c("n_holes", "SoilVWC_pct")) + 
  theme_bw()
plot_model(Bmod_all2, type = "emm", terms = c("SoilVWC_pct", "n_holes")) + 
  theme_bw()
plot_model(Bmod_all2, type = "emm", terms = c("SoilVWC_pct", "n_holes", "Symbiont")) + 
  theme_bw()



#################################################
Bmod_all3 <- brm(Corr_LinFlux ~ SoilVWC_pct*n_holes + Symbiont + (1|Site),
                 data = Rs_plots[Rs_plots$Corr_LinFlux<6,], prior = my.prior,
                 family =  "lognormal",
                 iter = 30000, seed = 1234,
                 save_pars = save_pars(all = TRUE),
                 chains = 4,
                 # cores = 4,
                 control = list(adapt_delta = 0.99, max_treedepth = 15))
summary(Bmod_all3, prob = 0.89)

#bayes_R2(Bmod_all, prob = c(0.03, 0.97))
plot_model(Bmod_all3, type = "pred", terms = c("n_holes", "SoilVWC_pct")) + 
  theme_bw()

plot_model(Bmod_all3, type = "pred", terms = c("Symbiont", "n_holes", "SoilVWC_pct")) + 
  theme_bw()


plot_model(Bmod_all3, type = "pred", terms = c("SoilVWC_pct", "n_holes")) + 
  theme_bw()

plot_model(Bmod_all3, type = "pred", terms = c("Symbiont", "SoilVWC_pct")) + 
  theme_bw()

plot_model(Bmod_all3, type = "pred", terms = c("SoilVWC_pct", "Symbiont")) + 
  theme_bw()

plot_model(Bmod_all3, type = "pred", terms = c("SoilVWC_pct", "Symbiont", "n_holes")) + 
  theme_bw()


plot_model(Bmod_all3, type = "emm", terms = c("n_holes", "SoilVWC_pct")) + 
  theme_bw()
plot_model(Bmod_all3, type = "emm", terms = c("SoilVWC_pct", "n_holes")) + 
  theme_bw()
plot_model(Bmod_all3, type = "emm", terms = c("SoilVWC_pct", "n_holes", "Symbiont")) + 
  theme_bw()


(loo1 <- loo(Bmod_all))
(loo2 <- loo(Bmod_all2))
(loo3 <- loo(Bmod_all3))


# model with an additional varying intercept for subjects
# compare both models
loo_compare(loo1, loo2, loo3)
loo_compare(loo1, loo2)


############################################################


Rs_plots
ggplot() +
  
  geom_point(data = Rs_plots[Rs_plots$Symbiont=="AM",],
              aes(x = SoilVWC_pct, y = Corr_LinFlux, fill = n_holes_f, color = n_holes_f), 
              alpha = 0.3, size = 5,
              position = position_dodge(width = 0.5)) +
  
  
  scale_color_manual("Holes", values = GoAvsGo) +
  scale_fill_manual("Holes", values = GoAvsGo) +
  # ylim(0,5)+
  ylab(expression(paste("R" ["S"] , " [",mu, "mol " , " CO" ["2"], "  m"^"-2 ", "s"^"-1", "]"))) +
  xlab("Soil Moisture [% VWC]")+
  theme(legend.position="NA",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Cicadas",size=30),
        legend.text=element_text(size=30))


ggplot() +
  
  geom_point(data = Rs_plots[Rs_plots$Corr_LinFlux<6,],
             aes(x = SoilVWC_pct, y = Corr_LinFlux, 
                 fill = n_holes_f, color = n_holes_f,
                 shape = Symbiont), 
             alpha = 0.3, size = 5,
             position = position_dodge(width = 0.5)) +
  
  
  scale_color_manual("Holes", values = GoAvsGo) +
  scale_fill_manual("Holes", values = GoAvsGo) +
  # ylim(0,5)+
  ylab(expression(paste("R" ["S"] , " [",mu, "mol " , " CO" ["2"], "  m"^"-2 ", "s"^"-1", "]"))) +
  xlab("Soil Moisture [% VWC]")+
  theme(legend.position="NA",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Cicadas",size=30),
        legend.text=element_text(size=30))






##//Modeling
my.prior <- set_prior("uniform(-30,30)", class = "b")

Bmod_AM <- brm(Corr_LinFlux ~ SoilVWC_pct*n_holes_f + SoilT_C,
                data = Rs_plots[Rs_plots$Symbiont=="AM",], prior = my.prior,
                family =  "lognormal",
                iter = 30000, seed = 1234,
                save_pars = save_pars(all = TRUE),
                chains = 4,
                # cores = 4,
                control = list(adapt_delta = 0.99, max_treedepth = 15))
summary(Bmod_AM, prob = 0.94)

#bayes_R2(Bmod_all, prob = c(0.03, 0.97))
plot_model(Bmod_AM, type = "pred", terms = c("n_holes_f", "SoilVWC_pct")) + 
  theme_bw()

plot_model(Bmod_AM, type = "pred", terms = c("SoilVWC_pct", "n_holes_f")) + 
  theme_bw()

plot_model(Bmod_all, type = "emm", terms = c("n_holes_f", "SoilVWC_pct")) + 
  theme_bw()
plot_model(Bmod_AM, type = "emm", terms = c("SoilVWC_pct", "n_holes_f")) + 
  theme_bw()


##//Modeling
my.prior <- set_prior("uniform(-30,30)", class = "b")

Bmod_ECM <- brm(Corr_LinFlux ~ SoilVWC_pct*n_holes_f + SoilT_C,
                data = Rs_plots[Rs_plots$Symbiont=="ECM",], prior = my.prior,
                family =  "lognormal",
                iter = 30000, seed = 1234,
                save_pars = save_pars(all = TRUE),
                chains = 4,
                # cores = 4,
                control = list(adapt_delta = 0.99, max_treedepth = 15))
summary(Bmod_ECM, prob = 0.94)

#bayes_R2(Bmod_all, prob = c(0.03, 0.97))
plot_model(Bmod_ECM, type = "pred", terms = c("n_holes_f", "SoilVWC_pct")) + 
  theme_bw()

plot_model(Bmod_ECM, type = "pred", terms = c("SoilVWC_pct", "n_holes_f")) + 
  theme_bw()

plot_model(Bmod_ECM, type = "emm", terms = c("n_holes_f", "SoilVWC_pct")) + 
  theme_bw()
plot_model(Bmod_ECM, type = "emm", terms = c("SoilVWC_pct", "n_holes_f")) + 
  theme_bw()








plot_model(Bmod_all, type = "int", terms = c("n_holes_f", "SoilVWC_pct", "Symbiont")) + 
  theme_bw()

plot_model(Bmod_all, type = "int", terms = c("SoilT_C", "n_holes_f")) + 
  theme_bw()


############################################################################



plot(Rs_plots$Corr_LinFlux~Rs_plots$SoilVWC_pct)

