#################################################################################################
#
#                                    Robuste Schätzung
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

#Dafür muss zunächst get_data ausgefühert werden


#### ml schätzer für mu und sigma auf basis der normalverteilung

mean(bit_data$log_diff) # sehr geringe nettorendite
sqrt(1/length(bit_data$log_diff) * sum((bit_data$log_diff - mean(bit_data$log_diff))^2))
sd(bit_data$log_diff) # im vergleich erwartungstreuer schätzer kaum unterschiedlich

#### Varianz des ml schätzers für mu
(sd(bit_data$log_diff)^2)/length(bit_data$log_diff)
# Standardabweichung dementsprechend:
sqrt((sd(bit_data$log_diff)^2)/length(bit_data$log_diff))


#### getrimmter mittelwert 
sorted <- sort(bit_data$log_diff)
k <- floor(length(bit_data$log_diff)*0.15)
sorted <- sorted[-c((length(sorted)-k+1):length(sorted))]
trimmed <- sorted[-c(1:k)]
mean_trimmed <- mean(trimmed)

#### getrimmte standardabwweichung MAD
mad <- 1.486 * median(abs(bit_data$log_diff - median(bit_data$log_diff)))

### Median
median <- median(bit_data$log_diff)

