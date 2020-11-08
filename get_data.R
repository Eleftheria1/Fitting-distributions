#################################################################################################
#
#                                     get the data right
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

#Daten einlesen
setwd("C:/Users/Eleftheria/OneDrive/Desktop/5FS/Finanzoekonometrieseminar")
bit_data <- read_csv("BTC-USD.csv")



# passende Spaltennamen hinzufügen
new_colnames <- colnames(bit_data)
new_colnames[6] <- "adj_close"
colnames(bit_data) <- new_colnames


# plot autocorrelation
autoplot(acf(bit_data$adj_close, plot = F)) # standard error ist 2/sqrt(n) 
                                            # Klar, autocorrelation ist riesig

# lag 1 Spalte mit den diffs hinzufügen
bit_data <- bit_data %>%
  mutate(adj_diff = c(0,diff(adj_close)),
         perc_diff = adj_close/(adj_close + adj_diff),
         log_diff = c(0,diff(log(adj_close))))

# plot autocorrelation erneut
autoplot(acf(bit_data$adj_diff, plot = F))

(auto_plot <- autoplot(acf(bit_data$log_diff, plot = F)))

auto_plot +
  scale_y_continuous(limits = c(NA,0.1)) +
  scale_x_continuous(breaks = c(1,(1:10)*5)) +
  theme_minimal() +
  theme(axis.line = element_line(color = "black"),
        panel.grid.minor = element_blank())
# Passt!

# Unterschiede visualisieren
ggplot(bit_data) +
  geom_histogram(aes( x = adj_diff, y = ..density..),bins = 50)
ggplot(bit_data) +
  geom_histogram(aes( x = log_diff, y = ..density..),bins = 50)
#Transformation auf jeden Fall sinnvoll!











