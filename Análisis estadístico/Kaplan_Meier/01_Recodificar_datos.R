
############################
## 01_Recodificar_datos.R ##
############################

# Cargar librerías
library(survival) 
library(KMsurv) 
library(survMisc) 
library(survminer) 
library(ggfortify) 
library(flexsurv) 
library(actuar) 
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)
library(xts)
library(plyr)
library(dplyr)
library(writexl)
library(knitr)
library(moments)
library(fitdistrplus)
library(logspline)

# Carga de datos
dataset_cov = read.csv("Resultados/04.01.23/final_cov_040123.csv")
dataset_cov = dataset_cov[c("Caso", "Edad", "Sexo")]
dataset_corr = read.csv("Resultados/04.01.23/final_dataset_corr.csv")
dataset = merge(dataset_corr, dataset_cov, by = "Caso")   

# Filtrar para adultos
dataset = dataset %>% filter(Edad >= 18)
dataset$Sexo = as.factor(dataset$Sexo)
dataset$d = as.factor(dataset$d)

# Variable dicotómica para el desenlace (alta, fallecido o censurado)
dataset$outcome = dataset$death_binaria
dataset$outcome[dataset$outcome == 1] = "Fallecido"

for (i in 1:nrow(dataset)) {
  if (dataset$d[i] == 0) {
    dataset$outcome[i] = "Censurado"
  }
}

dataset$outcome[dataset$outcome == 0] = "Alta"

# Crear la variable de censura (evento de interés: muerte)
for (k in 1:nrow(dataset)) {
  if (dataset$outcome[k] == "Alta" || dataset$outcome[k] == "Censurado" ) {
    dataset$d[k] = 0
  } else {dataset$d[k] = 1}
}

# Base de datos de pacientes criticos cronicos
cronicos = dataset %>%  filter(t >= 21)
# Base de datos de pacientes criticos
criticos = dataset %>%  filter(t < 21)

# Variable binaria (críticos y críticos crónicos)
dataset$Cronico = NA

for (i in 1:nrow(dataset)) {
  
  if (dataset$t_UCI[i] >= 21) {
    dataset$Cronico[i] = "Crónico"
  }else{
    dataset$Cronico[i] = "Crítico"
  }
  
}

# Establecer fecha de censura (120 días)
for (k in 1:nrow(dataset)) {
  if (dataset$t[k] > 120) {
    dataset$t[k] = 120
    dataset$d[k] = 0
  }
}


# Construir base de datos de recepción para la prueba Mantel-Cox 
mantel_cox = data.frame(variable = c("All by sex", "All by age",
                                     "All by chronic", "CCI by sex",
                                     "CCI by age", "No-CCI by sex",
                                     "No-CCI by age"),
                        chi_sqr = rep(NA, 7), p_value = rep(NA, 7))

rm(dataset_corr, dataset_cov, i,k)


