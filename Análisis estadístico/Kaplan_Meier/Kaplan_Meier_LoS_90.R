############################
## Kaplan-Meier (ICU LoS) ##
############################

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
setwd("/Users/sergiobarona03/Desktop/Proyecto_Long_COVID/")
dataset_cov = read.csv("Resultados/04.01.23/final_cov_040123.csv")
dataset_cov = dataset_cov[c("Caso", "Edad", "Sexo")]
dataset_corr = read.csv("Resultados/04.01.23/final_dataset_corr.csv")

dataset = merge(dataset_corr, dataset_cov, by = "Caso")   

# Adultos
dataset = dataset %>% filter(Edad >= 18)
dataset$Sexo = as.factor(dataset$Sexo)
dataset$d = as.factor(dataset$d)

# Outcome
dataset$outcome = dataset$death_binaria
dataset$outcome[dataset$outcome == 1] = "Fallecido"

for (i in 1:nrow(dataset)) {
  if (dataset$d[i] == 0) {
    dataset$outcome[i] = "Censurado"
  }
}

dataset$outcome[dataset$outcome == 0] = "Alta"

# Pacientes criticos cronicos
cronicos = dataset %>%  filter(t >= 21)
# Pacientes criticos
criticos = dataset %>%  filter(t < 21)

# Variable binaria (críticos y críticos crónicos)
dataset$Cronico = NA

for (i in 1:nrow(dataset)) {
  
  if (dataset$t[i] >= 21) {
    dataset$Cronico[i] = "Crónico"
  }else{
    dataset$Cronico[i] = "Crítico"
  }
  
}

# Establecer fecha de censura (90 días)

for (k in 1:nrow(dataset)) {
  if (dataset$t[k] > 90) {
    dataset$t[k] = 90
    dataset$d[k] = 0
  }
}

##############################################
##############################################
## I. Estimación general sin subpoblaciones ##
##############################################
##############################################

dataset_km = dataset[c("Caso", "t", "d")]
colnames(dataset_km) = c("Caso", "Tiempo", "Censura")

dataset_km$Censura = as.character(dataset_km$Censura)
dataset_km$Censura[dataset_km$Censura == "2"] = "0"
dataset_km$Censura = as.numeric(dataset_km$Censura)


df_km <- survival::survfit(Surv(dataset_km$Tiempo, 
                                dataset_km$Censura) ~ 1, data = dataset_km, 
                           type = "kaplan-meier", error = "tsiatis", 
                           conf.type = "log-log", conf.int = 0.95)


outcome_1 = with(df_km, data.frame(time, n.risk,
                                   n.event, surv, std.err,
                                   lower, upper))

colnames(outcome_1) = c("time", "n.risk", "n.event", "surv", "std.err", "lower 95% CI", 
                        "upper 95% CI")


# Guardar los resultados
writexl::write_xlsx(outcome_1, 
                    "Análisis estadístico/Kaplan_Meier/90_días/Estimación_general/Outcome.xlsx")

png(file = "Análisis estadístico/Kaplan_Meier/90_días/Estimación_general/Curva.png",
    width = 558, height = 407)

ggsurvplot(fit = df_km, data = dataset_km, conf.int = T, 
           xlab = "Time since admission to ICU (days)", 
           ylab = "Survival probability",
           legend.title = " ",
           palette = "lightgray", 
           risk.table = TRUE,
           tables.height = 0.2,
           tables.theme = theme_cleantable(),
           ggthem = theme_bw()) 


dev.off()

###############################
## I.Crónicos (T≥21 vs T<21) ##
###############################

dataset_km_CCI = dataset[c("Caso", "t", "d", "Cronico")]
colnames(dataset_km_CCI) = c("Caso", "Tiempo", "Censura", "CCI")

# Añadir fila de censura (auxiliar)
aux_row = data.frame(Caso = 999999, Tiempo = 90, 
                     Censura = 0, 
                     CCI = "Crítico")

dataset_km_CCI = rbind(dataset_km_CCI, aux_row)

# Estimador de Kaplan-Meier
dataset_km_CCI$Censura = as.character(dataset_km_CCI$Censura)
dataset_km_CCI$Censura[dataset_km_CCI$Censura == "2"] = "0"
dataset_km_CCI$Censura = as.numeric(dataset_km_CCI$Censura)

dataset_km_CCI$CCI = as.factor(dataset_km_CCI$CCI)

df_km_CCI <- survfit(Surv(dataset_km_CCI$Tiempo,
                          dataset_km_CCI$Censura) ~ CCI, 
                     data = dataset_km_CCI, 
                     type = "kaplan-meier", 
                     error = "tsiatis", 
                     conf.type = "log-log", conf.int = 0.99)

# Pacientes críticos
outcome_CCI_1 = with(df_km_CCI[1], 
                     data.frame(time, n.risk, n.event, 
                                surv, std.err, lower, upper))

colnames(outcome_CCI_1) = c("time", "n.risk", "n.event", 
                            "surv", "std.err", "lower 95% CI", 
                            "upper 95% CI")

writexl::write_xlsx(outcome_CCI_1, 
                    "Análisis estadístico/Kaplan_Meier/90_días/Estimación_crónicos/General/Outcome_críticos.xlsx")

# Pacientes crónicos
outcome_CCI_2 = with(df_km_CCI[2], 
                     data.frame(time, n.risk, n.event, 
                                surv, std.err, lower, upper))

colnames(outcome_CCI_2) = c("time", "n.risk", "n.event", 
                            "surv", "std.err", "lower 95% CI", 
                            "upper 95% CI")

writexl::write_xlsx(outcome_CCI_2, 
                    "Análisis estadístico/Kaplan_Meier/90_días/Estimación_crónicos/General/Outcome_crónicos.xlsx")


png(file = "Análisis estadístico/Kaplan_Meier/90_días/Estimación_crónicos/General/Curva.png",
    width = 558, height = 407)

ggsurvplot(fit = df_km_CCI, data = dataset_km_CCI, 
           conf.int = T, 
           xlab = "Time since admission to ICU (days)", 
           ylab = "Survival probability", 
           legend.title = "Prolonged ICU Stay",
           legend.labs = c("Critical illness", "Chronic illness"),
           risk.table = FALSE,
           palette = c("black", "gray"),
           ggthem = theme_bw(),
           pval = TRUE,
           pval.method = TRUE)

dev.off()


