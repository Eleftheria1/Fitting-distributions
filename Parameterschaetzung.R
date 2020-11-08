#################################################################################################
#
#                                    Parameterschätzungen
#                                     Fit Distributions
#                                        Grafiken
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

#Dafür muss Robuste_schaetzer ausgeführt worden sein

##### Zunächst Lösung der Parameterschätzung per Hand
#### ml schätzer für mu und sigma auf basis der normalverteilung

mean(bit_data$log_diff) # sehr geringe nettorendite
sqrt(1/length(bit_data$log_diff) * sum((bit_data$log_diff - mean(bit_data$log_diff))^2))
sd(bit_data$log_diff) # im vergleich erwartungstreuer schätzer kaum unterschiedlich

#### Varianz des ml schätzers für mu
(sd(bit_data$log_diff)^2)/length(bit_data$log_diff)
# Standardabweichung dementsprechend:
sqrt((sd(bit_data$log_diff)^2)/length(bit_data$log_diff))



#### Jetzt Parameterschätzung und Fitdistribution mit R
##############################################################################
#                           Normalverteilung
##############################################################################
mod_norm <- fitdist(bit_data$log_diff,distr = "norm")
mod_norm$estimate

# mit nur einer normmalverteilung
col_vec_norm1 <- "chocolate2"
ggplot(bit_data) +
  geom_histogram(aes( x = log_diff, y = ..density..),
                 bins = 100,
                 alpha = 0.5)+
  stat_function(fun = "dnorm",
                args = list(mean = mod_norm$estimate[1],
                            sd = mod_norm$estimate[2]),
                col = col_vec_norm1,
                size = 0.8) +
  annotate(geom = "text",x = 0.11,y = 17.5,
           label = paste0("\u03BC = ",round(mod_norm$estimate[1],4),
                         "\n\u03C3 = ",round(mod_norm$estimate[2],4)),
           col = col_vec_norm1,
           hjust = 0,
           size = 3.5)+
  labs(x = "Nettorendite", y = "Dichte",
       subtitle = "Normalverteilung") +
  theme_minimal() +
  theme(axis.line = element_line(color = "black"),
        panel.grid.minor = element_blank(),
        plot.subtitle = element_text(hjust = 0.5))

# Normalverteilung mit robusten Schätzungen im Vergleich
col_vec_norm2 <- c("norm" = "chocolate2",
                   "norm_trim" = "chocolate4")
ggplot(bit_data) +
  geom_histogram(aes( x = log_diff, y = ..density..),
                 bins = 100,
                 alpha = 0.5)+
  stat_function(fun = "dnorm",   # erste verteilung
                args = list(mean = mod_norm$estimate[1],
                            sd = mod_norm$estimate[2]),
                aes(col = "norm")) +
  stat_function(fun = "dnorm",    # zweite Verteilung
                args = list(mean = mean_trimmed,
                            sd = mad),
                aes(col = "norm_trim")) +
  annotate(geom = "text",x = 0.11,y = 17,col = col_vec_norm2["norm"],
           label = paste0("\u03BC = ",round(mod_norm$estimate[1],4),
                          "\n\u03C3 =  ",round(mod_norm$estimate[2],4)),
           hjust = 0)+
  annotate(geom = "text",x = 0.11,y = 13,col = col_vec_norm2["norm_trim"],
           label = paste0("\u03BC = ",round(mean_trimmed,4),
                          "\n\u03C3 =  ",round(mad,4)),
           hjust = 0)+
  scale_color_manual(name = "Verteilungen:",
                     values = col_vec_norm2,
                     labels = c("ML Schätzung","Robuste Schätzung"))+
  labs(x = "Nettorendite", y = "Dichte") +
  theme_minimal() +
  theme(axis.line = element_line(color = "black"),
        panel.grid.minor = element_blank())


##############################################################################
#                           t verteilung
##############################################################################
mod_t <- fitdist(bit_data$log_diff,distr = "std",
                 start = list(mean = -0.1,
                              sd = 0.001,
                              nu = 2.1))
mod_t$estimate
summary(mod_t)

col_vec_t1 <- "green4"
ggplot(bit_data) +
  geom_histogram(aes( x = log_diff, y = ..density..),
                 bins = 100,
                 alpha = 0.5)+
  stat_function(fun = "dstd",
                args = list(mean = mod_t$estimate[1],
                            sd = mod_t$estimate[2],
                            nu = mod_t$estimate[3]),
                col = col_vec_t1,
                size = 0.8) +
  annotate(geom = "text",x = 0.15,y = 16.5,
           label = paste0("\u03BC = ",round(mod_t$estimate[1],4),
                          "\n\u03C3 = ",round(mod_t$estimate[2],4),
                          "\n\u03BD = ",round(mod_t$estimate[3],4)),
           col = col_vec_t1,
           size = 3.5)+
  labs(x = "Nettorendite", y = "Dichte",
       subtitle = "t-Verteilung") +
  theme_minimal() +
  theme(axis.line = element_line(color = "black"),
        panel.grid.minor = element_blank(),
        plot.subtitle = element_text(hjust = 0.5))


##############################################################################
#                   Generalisierten Fehlerverteilung
##############################################################################
mod_ged <- fitdist(bit_data$log_diff,distr = "ged",
                 start = list(mean = -0.1,
                              sd = 0.001,
                              nu = 1))
mod_ged$estimate


col_vec_ged1 <- "magenta4"
ggplot(bit_data) +
  geom_histogram(aes( x = log_diff, y = ..density..),
                 bins = 100,
                 alpha = 0.5)+
  stat_function(fun = "dged",
                args = list(mean = mod_ged$estimate[1],
                            sd = mod_ged$estimate[2],
                            nu = mod_ged$estimate[3]),
                col = col_vec_ged1,
                size = 0.8) +
  annotate(geom = "text",x = 0.11,y = 16.5,
           label = paste0("\u03BC = ",round(mod_ged$estimate[1],4),
                          "\n\u03C3 = ",round(mod_ged$estimate[2],4),
                          "\n\u03BD = ",round(mod_ged$estimate[3],4)),
           col = col_vec_ged1, hjust = 0,
           size = 3.5)+
  labs(x = "Nettorendite", y = "Dichte",
       subtitle = "Generalisierte Fehlerverteilung") +
  theme_minimal() +
  theme(axis.line = element_line(color = "black"),
        panel.grid.minor = element_blank(),
        plot.subtitle = element_text(hjust = 0.5))



##############################################################################
#                   Doppelexponentialverteilung
##############################################################################
mod_dexp <- fitdist(bit_data$log_diff,distr = "dexp",
                   start = list(location = -0.1,
                                scale = 0.001))
mod_dexp$estimate


col_vec_dexp1 <- "cyan3"
ggplot(bit_data) +
  geom_histogram(aes(x = log_diff, y = ..density..),
                 bins = 100,
                 alpha = 0.5)+
  stat_function(fun = "dged",
                args = list(mean = mod_dexp$estimate[1],
                            sd = mod_dexp$estimate[2]),
                col = col_vec_dexp1,
                size = 0.8) +
  annotate(geom = "text",x = 0.11,y = 17.5,
           label = paste0("\u03BC = ",round(mod_dexp$estimate[1],4),
                          "\n\u03C3 = ",round(mod_dexp$estimate[2],4)),
           col = col_vec_dexp1, hjust = 0,
           size = 3.5)+
  labs(x = "Nettorendite", y = "Dichte",
       subtitle = "Doppelexponentialverteilung") +
  theme_minimal() +
  theme(axis.line = element_line(color = "black"),
        panel.grid.minor = element_blank(),
        plot.subtitle = element_text(hjust = 0.5))




##############################################################################
#                   normal mixture models
##############################################################################
dnormMix <- function(x,mu1,mu2,sig1,sig2, alpha = 0.5){
  alpha <- alpha
  alpha * dnorm(x,mean = mu1, sd = sig1) + 
    (1 - alpha) * dnorm(x, mean = mu2, sd = sig2)
}

pnormMix <- function(q,mu1,mu2,sig1,sig2,lower.tail = TRUE, alpha = 0.5){
  alpha <- alpha
  alpha * pnorm(q,mean = mu1, sd = sig1,lower.tail = lower.tail) + 
    (1 - alpha) * pnorm(q, mean = mu2, sd = sig2,lower.tail = lower.tail)
}


plot_seq <- seq(from = min(bit_data$log_diff),
                to = max(bit_data$log_diff),
                length.out = 10000)
estimate_matrix <- sapply((4:6)/10,function(alph){
  mod_mixnorm <- fitdist(bit_data$log_diff,distr = "normMix",
                       fix.arg = list(alpha = alph),
                       start = list(mu1 = 0,mu2 = 0,
                                    sig1 = 0.1,sig2 = 0.001))
  mod_mixnorm$estimate
})

plot_est_mat <- apply(estimate_matrix,2,function(est_vec){
  dnormMix(plot_seq,mu1 = est_vec[1],mu2 = est_vec[2],
           sig1 = est_vec[3],sig2 = est_vec[4])
})
colnames(plot_est_mat) <- paste((4:6)/10)
plot_est_mat <- as_tibble(plot_est_mat)
plot_est_mat_tidy <- plot_est_mat %>%
  mutate(x = plot_seq) %>%
  pivot_longer(1:3,names_to = "alpha")



ggplot(bit_data) +
  geom_histogram(aes(x = log_diff, y = ..density..),
                 bins = 100,
                 alpha = 0.5)+
  geom_line(data = plot_est_mat_tidy, aes(x = x, y = value, col = alpha),
            size =0.8) +
  scale_color_manual(values = rainbow(3))+
  labs(x = "Nettorendite", y = "Dichte",
       subtitle = "Diskrete Gaußsche Mischverteilung") +
  theme_minimal() +
  theme(axis.line = element_line(color = "black"),
        panel.grid.minor = element_blank(),
        plot.subtitle = element_text(hjust = 0.5))





