#################################################################################################
#
#                                    Likelihood-Quotienten Tests
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

#Dafür muss Parameterschaetzung ausgeführt worden sein


#### likelihood quotienten test für mu
ll_mean <- 2*(mod_norm$loglik - sum(log(dnorm(bit_data$log_diff,
                                              mean = mean_trimmed,
                                              sd = mod_norm$estimate[2]))))

#### likelihood quotienten test für sigma
ll_sd <- 2*(mod_norm$loglik - sum(log(dnorm(bit_data$log_diff,
                                            mean = mod_norm$estimate[1],
                                            sd = mad))))

#### likelihood quotienten test für mu = 0
ll_nullrend <- 2*(mod_norm$loglik - sum(log(dnorm(bit_data$log_diff,
                                                  mean = 0,
                                                  sd = mod_norm$estimate[2])))) 
#H0 kann nicht abgelehnt werden, d.h man kann davon ausgehen, dass man im Mittel keine Rendite erzielt

### Vergleich mit Quantil der chi^2-Verteilung
ll_mean >= qchisq(df = 1,p = 0.95)   # H0 wird NICHT abgelehnt
ll_sd >= qchisq(df = 1,p = 0.95)    #H0 wird abgelehnt
