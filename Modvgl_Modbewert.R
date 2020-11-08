#################################################################################################
#
#                                 Modellvergleich mittels AIC/BIC
#                         Modellbewertung mittel KS-Test und Kreuzvalidierung
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

# Dafür muss Parameterschätzung ausgeführt worden sein

#### AIC und BIC der Modelle
##############################################################################
#                           Normalverteilung
##############################################################################
aic_norm <- mod_norm$aic
bic_norm <- mod_norm$bic


##############################################################################
#                           t-Verteilung
##############################################################################
aic_t <- mod_t$aic
bic_t <- mod_t$bic


##############################################################################
#                           Doppelexponentialverteilung
##############################################################################
aic_dexp <- mod_dexp$aic
bic_dexp <- mod_dexp$bic

##############################################################################
#                           Generalisierte Fehlerverteilung
##############################################################################
aic_ged <- mod_ged$aic
bic_ged <- mod_ged$bic


##############################################################################
#                           Gaußsche Mischverteilung
##############################################################################


mod_mixnorm_1 <- fitdist(bit_data$log_diff,distr = "normMix",
                       fix.arg = list(alpha = 0.6),
                       start = list(mu1 = 0,mu2 = 0,
                                    sig1 = 0.1,sig2 = 0.001))
mod_mixnorm_2<- fitdist(bit_data$log_diff,distr = "normMix",
                       fix.arg = list(alpha = 0.5),
                       start = list(mu1 = 0,mu2 = 0,
                                    sig1 = 0.1,sig2 = 0.001))

aic_mix1 <- mod_mixnorm_1$aic
bic_mix1 <- mod_mixnorm_1$bic
aic_mix2 <- mod_mixnorm_2$aic
bic_mix2 <- mod_mixnorm_2$bic

##############################################################################
#                           Robuste Schätzung
##############################################################################
aic_robust <- -2*sum(log(dnorm(bit_data$log_diff, mean = mean_trimmed, sd = mad))) + 2*2
bic_robust <- -2*sum(log(dnorm(bit_data$log_diff, mean = mean_trimmed, sd = mad))) + log(length(bit_data$log_diff)) * 2

# Liste der AIC und BIC 
mod_vgl_aic <- tibble(aic_norm, aic_t, aic_dexp, aic_ged, aic_mix1, aic_mix2, aic_robust)
mod_vgl_bic <- tibble(bic_norm, bic_t, bic_dexp, bic_ged, bic_mix1, bic_mix2, bic_robust)


