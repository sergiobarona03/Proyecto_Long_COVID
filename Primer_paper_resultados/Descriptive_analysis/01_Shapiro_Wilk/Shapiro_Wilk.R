
###############################
## Análisis estadístico [1]  ##
##    (Shapiro-Wilk test)    ##
###############################

library(readxl)
library(writexl)
library(dplyr)
library(ggplot2)
library(moments)
library(knitr)
library(kableExtra)

# Cargar datos
dataset = read.csv("Paper_resultados/Dataset/Output/cov_final_dataset.csv")

# Base de datos de recepción
normal = data.frame(Variable = c("Edad", "ICU_LoS", "Onset_Adm"), 
                    Skewness = c(NA,NA,NA), Kurtosis=c(NA,NA,NA),
                    W= c(NA,NA,NA), p_value= c(NA, NA,NA))

normal$Skewness[1] = skewness(dataset$age)
normal$Kurtosis[1] = kurtosis(dataset$age)
normal$W[1] = as.numeric(shapiro.test(dataset$age)$statistic)
normal$p_value[1] = as.numeric(shapiro.test(dataset$age)$p.value)
  
normal$Skewness[2] = skewness(dataset$t_UCI)
normal$Kurtosis[2] = kurtosis(dataset$t_UCI)
normal$W[2] = as.numeric(shapiro.test(dataset$t_UCI)$statistic)
normal$p_value[2] = as.numeric(shapiro.test(dataset$t_UCI)$p.value)

normal$Skewness[3] = skewness(dataset$t_so)
normal$Kurtosis[3] = kurtosis(dataset$t_so)
normal$W[3] = as.numeric(shapiro.test(dataset$t_so)$statistic)
normal$p_value[3] = as.numeric(shapiro.test(dataset$t_so)$p.value)

# Guardar en formato Latex
for (i in 2:(ncol(normal)-1)) {
  normal[,i] = round(normal[,i], digits = 4)
}

normal_ltx = kable(normal, , 
                   caption = "Shapiro-Wilk normality test for continuous variables", 
                   format = "latex")

writeLines(normal_ltx,
           "Paper_resultados/Descriptive_analysis/01_Shapiro_Wilk/normal.tex")
