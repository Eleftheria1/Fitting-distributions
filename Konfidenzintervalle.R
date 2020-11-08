#################################################################################################
#
#                                    Konfidenzintervalle
#
#################################################################################################
#benötigte Pakete
library(tidyverse)
library(forecast)
library(ggfortify)
library("fitdistrplus")
library("ggpubr")
library("caret")
library("lubridate")
library("fGarch")
library("nimble")

#Dafür muss Parameterschätzung ausgeführt worden sein



##############################################################################
#                           Normalverteilung
##############################################################################

mod_norm
# KI für mu nach Fisher
(norm_lower <- mod_norm$estimate[1] - mod_norm$sd[1] * qnorm(0.975))
(norm_upper <- mod_norm$estimate[1] + mod_norm$sd[1] * qnorm(0.975))

#profile likelihood
res_norm <- tibble(try_vals = seq(-0.0025,0.004,length.out = 1000),
                   log_restricted = sapply(try_vals,function(val){
                     fitdist(bit_data$log_diff,distr = "norm",fix.arg = list(mean = val))$loglik}),
                   inside = if_else(2*(mod_norm$loglik - log_restricted) <= qchisq(0.95,df = 1),TRUE,FALSE))

ggplot(res_norm,aes(x = try_vals,y = 1, col = inside)) +
  geom_point()

# KI nach Profile Likelihood
dplyr::filter(res_norm,inside == TRUE) %>%
  summarise(min = min(try_vals),
            max = max(try_vals))

##############################################################################
#                           t Verteilung
##############################################################################

mod_t

# KI für mu nach Fisher
(t_lower <- mod_t$estimate[1] - mod_t$sd[1] * qnorm(0.975))
(t_upper <- mod_t$estimate[1] + mod_t$sd[1] * qnorm(0.975))


#profile likelihood
res_t <- tibble(try_vals = seq(-0.0025,0.004,length.out = 1000),
                   log_restricted = sapply(try_vals,function(val){
                     fitdist(bit_data$log_diff,distr = "std",  start = list(sd = 0.1,
                                                                            nu = 2.01),
                             fix.arg = list(mean = val))$loglik}),
                   inside = if_else(2*(mod_t$loglik - log_restricted) <= qchisq(0.95,df = 1),TRUE,FALSE))

ggplot(res_t,aes(x = try_vals,y = 1, col = inside)) +
  geom_point()

# KI nach Profile Likelihood
dplyr::filter(res_t,inside == TRUE) %>%
  summarise(min = min(try_vals),
            max = max(try_vals))

##############################################################################
#                           Generalisierte Fehlerverteilung
##############################################################################

mod_ged

# KI für mu nach Fisher
(ged_lower <- mod_ged$estimate[1] - mod_ged$sd[1] * qnorm(0.975))
(ged_upper <- mod_ged$estimate[1] + mod_ged$sd[1] * qnorm(0.975))

#profile likelihood
res_ged <- tibble(try_vals = seq(-0.0025,0.004,length.out = 1000),
                log_restricted = sapply(try_vals,function(val){
                  fitdist(bit_data$log_diff,distr = "ged",  start = list(sd = 0.1,
                                                                         nu = 2.01),
                          fix.arg = list(mean = val))$loglik}),
                inside = if_else(2*(mod_ged$loglik - log_restricted) <= qchisq(0.95,df = 1),TRUE,FALSE))

ggplot(res_ged,aes(x = try_vals,y = 1, col = inside)) +
  geom_point()

# KI nach Profile Likelihood
dplyr::filter(res_ged,inside == TRUE) %>%
  summarise(min = min(try_vals),
            max = max(try_vals))


  
##############################################################################
#                           Doppelexponentialverteilung
##############################################################################

mod_dexp
# KI für mu nach Fisher
(dexp_lower <- mod_dexp$estimate[1] - mod_dexp$sd[1] * qnorm(0.975))
(dexp_upper <- mod_dexp$estimate[1] + mod_dexp$sd[1] * qnorm(0.975))

#profile likelihood
res_dexp <- tibble(try_vals = seq(-0.0025,0.004,length.out = 1000),
                  log_restricted = sapply(try_vals,function(val){
                    fitdist(bit_data$log_diff,distr = "dexp",  start = list(scale = 0.01),
                            fix.arg = list(location = val))$loglik}),
                  inside = if_else(2*(mod_dexp$loglik - log_restricted) <= qchisq(0.95,df = 1),TRUE,FALSE))

ggplot(res_dexp,aes(x = try_vals,y = 1, col = inside)) +
  geom_point()

# KI nach Profile Likelihood
dplyr::filter(res_dexp,inside == TRUE) %>%
  summarise(min = min(try_vals),
            max = max(try_vals))








# alle profile likelihoods zusammen
ggplot() + 
  geom_point(data = res_norm,aes(x = try_vals,y = 1, col = inside), shape = 15) +
  geom_point(data = res_t,aes(x = try_vals,y = 0, col = inside), shape = 15) +
  geom_point(data = res_ged,aes(x = try_vals,y = -1, col = inside), shape = 15) +
  geom_point(data = res_dexp,aes(x = try_vals,y = -2, col = inside), shape = 15) +
  geom_rect(aes(xmin = norm_lower,xmax = norm_upper,ymin = 0.85,ymax = 0.9,
                fill = "blue4"))+
  geom_rect(aes(xmin = t_lower,xmax = t_upper,ymin = -0.15,ymax = -0.1,
                fill = "blue4"))+
  geom_rect(aes(xmin = ged_lower,xmax = ged_upper,ymin = -1.15,ymax = -1.1,
                fill = "blue4"))+
  geom_rect(aes(xmin = dexp_lower,xmax = dexp_upper,ymin = -2.15,ymax = -2.1,
                fill = "blue4"))+
  annotate(geom = "text",x = 0.0013, y = 1.25,
           label = "Normalverteilung",hjust = 0)+
  annotate(geom = "text",x = 0.0013, y = 0.25,
           label = "t-Verteilung",hjust = 0)+
  annotate(geom = "text",x = 0.0013, y = -0.67, 
           label = "Generalisierte \nFehlerverteilung",hjust = 0)+
  annotate(geom = "text",x = 0.0013, y = -1.75,
           label = "Doppelexponentialverteilung",hjust = 0)+
  geom_vline(xintercept = 0,col = "black", linetype = "dotted")+
  labs(x = "Möglicher Wertebereich für \u03BC",y = "")+
  scale_color_manual(values = c("red3","steelblue2"),labels = c("Nullhypothese abgelehnt","Nullhypothese beibehalten"),name ="")+
  scale_fill_manual(values = "blue4",labels = c("KI nach Fisher"),name ="")+
  theme_bw()+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(),
        legend.key.height = unit(0.01, units = "cm"),
        legend.key.width = unit(0.1, units = "cm"))
