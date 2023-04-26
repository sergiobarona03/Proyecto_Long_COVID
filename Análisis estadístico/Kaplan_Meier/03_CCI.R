#####################################
##  03. Estimador de Kaplan-Meier  ##
##  (Pacientes críticos crónicos)  ##     
#####################################
library(here)
setwd("/Users/sergiobarona03/Desktop/Proyecto_Long_COVID/")
source(here::here("Análisis estadístico/Kaplan_Meier","02_Casos_totales.R"))

########################
## A. CCI según sexo  ##
########################
# Base de datos de entrada
dataset_km_cronico = dataset_km[c("Caso", "Tiempo",
                                  "Censura", "CCI", "Sexo")]
aux_rows = data.frame(Caso = c(9998,9999), Tiempo = c(120, 120), 
                      Censura = c(0, 0), CCI = c("Crítico", "Crítico"), 
                      Sexo = c(0, 1))
dataset_km_cronico = rbind(dataset_km_cronico, aux_rows)

# Seleccionar subpoblación CCI
dataset_km_cronico = dataset_km_cronico %>% filter(CCI == "Crónico")

# Estimador de Kaplan-Meier
df_km_cronico <- survfit(Surv(dataset_km_cronico$Tiempo,
                              dataset_km_cronico$Censura) ~ Sexo, 
                         data = dataset_km_cronico, 
                         type = "kaplan-meier", 
                         error = "tsiatis", 
                         conf.type = "log-log", conf.int = 0.99)

# CCI con sexo femenino
outcome_cronico_f = with(df_km_cronico[1], 
                         data.frame(time, n.risk, n.event, 
                                    surv, std.err, lower, upper))
colnames(outcome_cronico_f) = c("time", "n.risk", "n.event", 
                                "surv", "std.err", "lower 95% CI", 
                                "upper 95% CI")
writexl::write_xlsx(outcome_cronico_f, 
                    "Análisis estadístico/Kaplan_Meier/120_días/Estimación_crónicos/Sexo/Outcome_female.xlsx")

# CCI con sexo masculino
outcome_cronico_m = with(df_km_cronico[2], 
                         data.frame(time, n.risk, n.event, 
                                    surv, std.err, lower, upper))
colnames(outcome_cronico_m) = c("time", "n.risk", "n.event", 
                                "surv", "std.err", "lower 95% CI", 
                                "upper 95% CI")
writexl::write_xlsx(outcome_cronico_m, 
                    "Análisis estadístico/Kaplan_Meier/120_días/Estimación_crónicos/Sexo/Outcome_male.xlsx")

# Curvas de supervivencia estimadas
png(file = "Análisis estadístico/Kaplan_Meier/120_días/Estimación_crónicos/Sexo/Curva.png",
    width = 558, height = 407)
ggsurvplot(fit = df_km_cronico, data = dataset_km_cronico, 
           conf.int = F, 
           xlab = "Time since admission to ICU (days)", 
           ylab = "Survival probability", 
           legend.title = "Prolonged ICU Stay",
           risk.table = FALSE,
           palette = c("black", "lightgray"),
           legend.labs = c("Male", "Female"),
           ggthem = theme_bw())
dev.off()

# Test log-rank
mantel_CCI_sex = survdiff(Surv(dataset_km_cronico$Tiempo,
                               dataset_km_cronico$Censura) ~ Sexo, 
                          data = dataset_km_cronico, rho = 0)
m = which(mantel_cox$variable == "CCI by sex")
mantel_cox$chi_sqr[m] = round(mantel_CCI_sex$chisq, digits = 2)
mantel_cox$p_value[m] = round(mantel_CCI_sex$pvalue, digits = 2)

rm(df_km_cronico, dataset_km_cronico, mantel_CCI_sex)

#######################
## B. CCI según edad ##
#######################
# Base de datos de entrada
dataset_km_cronico = dataset_km[c("Caso", "Tiempo", "Censura", 
                               "CCI", "Group")]
aux_rows = data.frame(Caso = c(9998, 9999), Tiempo = c(120, 120), 
                      Censura = c(0, 0),CCI = c("Crítico", "Crítico"), 
                      Group = c("[18,65)","[65,Inf]"))
dataset_km_cronico = rbind(dataset_km_cronico, aux_rows)

# Seleccionar la subpoblación CCI
dataset_km_cronico = dataset_km_cronico %>% filter(CCI == "Crónico")

# Estimador de Kaplan-Meier
df_km_cronico <- survfit(Surv(dataset_km_cronico$Tiempo,
                              dataset_km_cronico$Censura) ~ Group, 
                         data = dataset_km_cronico, 
                         type = "kaplan-meier", 
                         error = "tsiatis", 
                         conf.type = "log-log", conf.int = 0.99)

# Grupo 1: CCI de [18, 65)
outcome_cronico_g1 = with(df_km_cronico[1], data.frame(time, n.risk, n.event, 
                                     surv, std.err, lower, upper))
colnames(outcome_cronico_g1) = c("time", "n.risk", "n.event", 
                                 "surv", "std.err", "lower 95% CI", 
                                 "upper 95% CI")
writexl::write_xlsx(outcome_cronico_g1, 
                    "Análisis estadístico/Kaplan_Meier/120_días/Estimación_crónicos/Edad/Outcome_g1.xlsx")

# Grupo 2: CCI de [65, Inf)
outcome_cronico_g2 = with(df_km_cronico[2], 
                          data.frame(time, n.risk, n.event, 
                                     surv, std.err, lower, upper))
colnames(outcome_cronico_g2) = c("time", "n.risk", "n.event", 
                                 "surv", "std.err", "lower 95% CI", 
                                 "upper 95% CI")
writexl::write_xlsx(outcome_cronico_g2, 
                    "Análisis estadístico/Kaplan_Meier/120_días/Estimación_crónicos/Edad/Outcome_g2.xlsx")

# Curvas de supervivencia estimadas
png(file = "Análisis estadístico/Kaplan_Meier/120_días/Estimación_crónicos/Edad/Curva.png",
    width = 558, height = 407)
ggsurvplot(fit = df_km_cronico, data = dataset_km_cronico, 
           conf.int = F, 
           xlab = "Time since admission to ICU (days)", 
           ylab = "Survival probability", 
           legend.title = "Prolonged ICU Stay",
           risk.table = FALSE,
           palette = c("black", "gray", "lightgray"),
           legend.labs = c("[18,65)", "[65,Inf)"),
           ggthem = theme_bw())
dev.off()

# Test log-rank
mantel_CCI_age = survdiff(Surv(dataset_km_cronico$Tiempo,
                               dataset_km_cronico$Censura) ~ Group, 
                          data = dataset_km_cronico, rho = 0)
m = which(mantel_cox$variable == "CCI by age")
mantel_cox$chi_sqr[m] = round(mantel_CCI_age$chisq, digits = 2)
mantel_cox$p_value[m] = round(mantel_CCI_age$pvalue, digits = 2)

rm(df_km_cronico, dataset_km_cronico, mantel_CCI_age, aux_row, aux_rows,
   outcome_cronico_f, outcome_cronico_m, outcome_cronico_g1,
   outcome_cronico_g2)

