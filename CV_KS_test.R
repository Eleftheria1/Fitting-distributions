#########################################################################
# 
#                           Kreuzvalidierung und KS-Test
#                     (nur für 2 bis 4 Parameter implementiert)
#
#########################################################################
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
library("broom")
library("goftest")


#Dafür muss Parameterschaetzung ausgeführt worden sein

cv_distfit_fun <- function(data_vec,
                           distname, #format: norm, t, exp, gamma
                           start = NULL, #format: list of starting values
                           n_folds = 10,
                           show_flist = TRUE,
                           seed = 69){
  # Kreuzvalidierung iniziieren mit Folds
  set.seed(seed)
  folds_ind <- createFolds(data_vec, k = n_folds, list = F)
  folds_list <- list()
  
  # iterieren über folds -----------------------------------------------------------------------
  for(fold in 1:n_folds){
    # training und test data splitten und Modell auf Trainingsdaten fitten ---------------
    training_data <- data_vec[folds_ind != fold]
    testing_data <- data_vec[folds_ind == fold]
    mod_train <- fitdist(data = training_data, distr = distname,
                         start = start)
    
    
    # KS-test 
    dist_name <- str_c("p",distname,sep = "")
    if(length(mod_train$estimate) == 2){
       tidy_ks_test <- tidy(ks.test(x = testing_data,
                                 y = dist_name,
                                 mod_train$estimate[1], # the parameters estimated by the model on the training data
                                 mod_train$estimate[2],
                                 alternative = "two.sided",exact = FALSE))
    } else if(length(mod_train$estimate) == 3){
      tidy_ks_test <- tidy(ks.test(x = testing_data,
                                   y = dist_name,
                                   mod_train$estimate[1], # the parameters estimated by the model on the training data
                                   mod_train$estimate[2],
                                   mod_train$estimate[3],
                                   alternative = "two.sided",exact = FALSE))
    } else if(length(mod_train$estimate) == 4){
      tidy_ks_test <- tidy(ks.test(x = testing_data,
                                   y = dist_name,
                                   mod_train$estimate[1], # the parameters estimated by the model on the training data
                                   mod_train$estimate[2],
                                   mod_train$estimate[3],
                                   mod_train$estimate[4],
                                   alternative = "two.sided",exact = FALSE))
    }else{stop("not implemented for that number of parameters!")}
   
    crit_val <- sqrt(-0.5 * log(0.5*0.05))/sqrt(length(testing_data))
    ks_res <- ifelse(tidy_ks_test$statistic > crit_val,"Ablehnen","Beibehalten")


    # fold list füllen mit fold Ergebnissen --------------------------------------------------
    folds_list[[fold]] <- list(params = mod_train$estimate,
                               sd_params = mod_train$sd,
                               ks_statistic = tidy_ks_test$statistic,
                               ks_res = ks_res,
                               ks_pval = tidy_ks_test$p.value)
    
  }
  # Mittelwert über Folds kriegen ----------------------------------------------------
  if(length(mod_train$estimate) > 1){
    avg_params <- rowMeans(sapply(folds_list, function(list){list$params}))
  } else {
    avg_params <- mean(sapply(folds_list, function(list){list$params}))
  }
  
  if(length(mod_train$estimate) > 1){
    avg_sd_params <- rowMeans(sapply(folds_list, function(list){list$sd_params}))
  } else {
    avg_sd_params <- mean(sapply(folds_list, function(list){list$sd_params}))
  }
  
  avg_list <- list(
    params = avg_params,
    sd_params = avg_sd_params,
    ks_statistic = mean(sapply(folds_list, function(list){list$ks_statistic})),
    ks_overall_res = ifelse(mean(sapply(folds_list, function(list){list$ks_statistic})) > (sqrt(-0.5 * log(0.5*0.05))/sqrt(length(data_vec)/n_folds)),
                            "Ablehnen","Beibehalten"),
    ks_pval = mean(sapply(folds_list, function(list){list$ks_pval}))
  )
  
  # Endergebnisse in Liste  widergeben -----------------------------------------------------------
  if(!show_flist){
    return(avg_list)
  } else {
    return(list(
      avg_list = avg_list,
      folds_list = folds_list
    ))
  }
}

#########################################################################
#                       Hilffunktion für Visualisierung
#########################################################################
# tidy format kriegen für plotting:
prep_plot_data <- function(cv_distfit_list,
                           distname = "default"){
  res <- sapply(1:10, function(fold){
    c(ks_statistic = cv_distfit_list$folds_list[[fold]][[3]],
      ks_res = cv_distfit_list$folds_list[[fold]][[4]])
  })
  res <- t(res) %>%
    as_tibble(.name_repair = "minimal") %>%
    mutate(ks_statistic = as.numeric(ks_statistic),
           mean_metric = mean(ks_statistic),
           dist = rep(distname,10))
  
  return(res)
}



#########################################################################
#                            ecdfs and ks tests
#########################################################################
crit_val_full <- sqrt(-0.5 * log(0.5*0.05))/sqrt(length(bit_data$log_diff))
# Normalverteilung -----------------------------------------------------
ks_norm_full <- c(gofstat(mod_norm)$ks, gofstat(mod_norm)$kstest)

cv_ks_norm <- cv_distfit_fun(bit_data$log_diff, distname = "norm")
plot_cv_data_norm <- prep_plot_data(cv_ks_norm,distname = "Normalverteilung")



ecdf_norm <- ggplot(bit_data,aes(x = log_diff)) +
  stat_function(fun = "pnorm", args = list(mean = mod_norm$estimate[1],
                                           sd = mod_norm$estimate[2]),
                size = 0.8, alpha = 0.5) +
  stat_ecdf(col = "chocolate2",size = 0.8)+
  labs(x = "Nettorendite", y = "Empirische Verteilungsfunktion",
       subtitle = "Normalverteilung")+
  theme_minimal() +
  theme(axis.line = element_line(color = "black"),
        panel.grid.minor = element_blank(),
        plot.subtitle = element_text(hjust = 0.5))
ecdf_norm


# Student-t Verteilung -------------------------------------------------
ks_t_full <- c(gofstat(mod_norm)$ks, gofstat(mod_norm)$kstest)

cv_ks_t <- cv_distfit_fun(bit_data$log_diff, distname = "std",start = list(mean = -0.1,
                                                                           sd = 0.001,
                                                                           nu = 2.1))
plot_cv_data_t <- prep_plot_data(cv_ks_t,distname = "Student-t Verteilung")

ecdf_t <- ggplot(bit_data,aes(x = log_diff)) +
  stat_function(fun = "pstd", args = list(mean = mod_t$estimate[1],
                                          sd = mod_t$estimate[2],
                                          nu = mod_t$estimate[3]),
                size = 0.8, alpha = 0.5) +
  stat_ecdf(col = "green4",size = 0.8)+
  labs(x = "Nettorendite", y = "Empirische Verteilungsfunktion",
       subtitle = "Student-t Verteilung")+
  theme_minimal() +
  theme(axis.line = element_line(color = "black"),
        panel.grid.minor = element_blank(),
        plot.subtitle = element_text(hjust = 0.5))
ecdf_t

# Generalisierte Fehlerverteilung ----------------------------------------------------------
ks_ged_full <- c(gofstat(mod_ged)$ks, gofstat(mod_ged)$kstest)

cv_ks_ged <- cv_distfit_fun(bit_data$log_diff, distname = "ged",start = list(mean = 0,
                                                                           sd = 0.03,
                                                                           nu = 0.7))
plot_cv_data_ged <- prep_plot_data(cv_ks_ged,distname = "Generalisierte Fehlerverteilung")


ecdf_ged <- ggplot(bit_data,aes(x = log_diff)) +
  stat_function(fun = "pged", args = list(mean = mod_ged$estimate[1],
                                          sd = mod_ged$estimate[2],
                                          nu = mod_ged$estimate[3]),
                size = 0.8, alpha = 0.5) +
  stat_ecdf(col = "magenta4",size = 0.8)+
  labs(x = "Nettorendite", y = "Empirische Verteilungsfunktion",
       subtitle = "Generalisierte Fehlerverteilung")+
  theme_minimal() +
  theme(axis.line = element_line(color = "black"),
        panel.grid.minor = element_blank(),
        plot.subtitle = element_text(hjust = 0.5))
ecdf_ged


# Doppeltexponentialverteilung ----------------------------------------------------------
ks_dexp_full <- c(gofstat(mod_dexp)$ks, gofstat(mod_dexp)$kstest)

cv_ks_dexp <- cv_distfit_fun(bit_data$log_diff, distname = "dexp",start = list(location = 0.001,
                                                                             scale = 0.03))
plot_cv_data_dexp <- prep_plot_data(cv_ks_dexp,distname = "Doppeltexponentialverteilung")

ecdf_dexp <- ggplot(bit_data,aes(x = log_diff)) +
  stat_function(fun = "pdexp", args = list(location = mod_dexp$estimate[1],
                                           scale = mod_dexp$estimate[2]),
                size = 0.8, alpha = 0.5) +
  stat_ecdf(col = "cyan3",size = 0.8)+
  labs(x = "Nettorendite", y = "Empirische Verteilungsfunktion",
       subtitle = "Doppeltexponentialverteilung")+
  theme_minimal() +
  theme(axis.line = element_line(color = "black"),
        panel.grid.minor = element_blank(),
        plot.subtitle = element_text(hjust = 0.5))
ecdf_dexp

# Diskrete Gaußsche Mischverteilung -------------------------------------------------
mod_mixnorm <- fitdist(bit_data$log_diff,distr = "normMix",
                       fix.arg = list(alpha = 0.5),
                       start = list(mu1 = 0,mu2 = 0,
                                    sig1 = 0.1,sig2 = 0.001))
ks_mixnorm_full <- c(gofstat(mod_mixnorm)$ks, gofstat(mod_mixnorm)$kstest)

cv_ks_mixnorm <- cv_distfit_fun(bit_data$log_diff, distname = "normMix",start = list(mu1 = 0,mu2 = 0,
                                                                                     sig1 = 0.1,sig2 = 0.001))
plot_cv_data_mixnorm <- prep_plot_data(cv_ks_mixnorm,distname = "Diskrete Gaußsche \nMischverteilung (\u03B1 = 0.5)")

ecdf_mixnorm <- ggplot(bit_data,aes(x = log_diff)) +
  stat_function(fun = "pnormMix", args = list(mu1 = mod_mixnorm$estimate[1],
                                              mu2 = mod_mixnorm$estimate[2],
                                              sig1 = mod_mixnorm$estimate[3],
                                              sig2 = mod_mixnorm$estimate[4]),
                size = 0.8, alpha = 0.5) +
  stat_ecdf(col = "mediumblue",size = 0.8)+
  labs(x = "Nettorendite", y = "Empirische Verteilungsfunktion",
       subtitle = "Diskrete Gaußsche Mischverteilung")+
  theme_minimal() +
  theme(axis.line = element_line(color = "black"),
        panel.grid.minor = element_blank(),
        plot.subtitle = element_text(hjust = 0.5))
ecdf_mixnorm

# alle ecdfs zusammen
gridExtra::grid.arrange(ecdf_norm + labs(x = "",y = ""),
                        ecdf_t + labs(x = "",y = ""),
                        ecdf_ged + labs(x = "",y = ""),
                        ecdf_dexp + labs(x = "",y = ""),
                        ecdf_mixnorm + labs(x = "",y = ""),
                        nrow = 3) 

# cv visualisierung ------------------------------------------------------
full_cv_data <- plot_cv_data_norm %>%
  bind_rows(plot_cv_data_t) %>%
  bind_rows(plot_cv_data_ged) %>%
  bind_rows(plot_cv_data_dexp) %>%
  bind_rows(plot_cv_data_mixnorm)




ggplot(full_cv_data) +
  geom_point(aes(x = as.factor(dist), y = ks_statistic, col = as.factor(ks_res)),
             shape = 16,alpha = 0.4)+
  geom_point(aes(x = as.factor(dist), y = mean_metric, col = as.factor(ks_res)),
             shape = 5,size = 3)+
  geom_hline(yintercept = sqrt(-0.5 * log(0.5*0.05))/sqrt(length(bit_data$log_diff)/10),
             linetype = "longdash", col = "grey35")+
  annotate(geom = "text",x = 2.1, y = 0.095,label = "Nullhypothese Beibehalten",
           col = "grey35", hjust = 0,size = 3.5)+
  annotate(geom = "text",x = 2.1, y = 0.105,label = "Nullhypothese Ablehnen",
           col = "grey35", hjust = 0,size = 3.5)+
  scale_color_manual(name = "Nullhypothese",values = c("red3","royalblue1"))+
  labs(x = "", y = "KS Teststatistik",
       subtitle = "Vergleich der KS Teststatistiken")+
  theme_minimal() +
  theme(axis.line = element_line(color = "black"),
        panel.grid.minor = element_blank(),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 315,vjust = 1,hjust = 0))












