###################################
## 02. Estimador de Kaplan-Meier ##
##    (Casos totales)            ##
###################################
library(here)
setwd("/Users/sergiobarona03/Desktop/Proyecto_Long_COVID/")
source(here::here("Análisis estadístico/Kaplan_Meier","01_Recodificar_datos.R"))

############################################
## Preparar base de datos con covariables ##
############################################
dataset_km = dataset[c("Caso", "t", "d", "Cronico", "Sexo", "Edad")]
colnames(dataset_km) = c("Caso", "Tiempo", "Censura",
                         "CCI", "Sexo", "Edad")

dataset_km$CCI = as.factor(dataset_km$CCI)
dataset_km$Sexo = as.factor(dataset_km$Sexo)
dataset_km$Censura = as.character(dataset_km$Censura)
dataset_km$Censura[dataset_km$Censura == "2"] = "0"
dataset_km$Censura = as.numeric(dataset_km$Censura)

# Construir grupos etarios
dataset_km = dataset_km %>% mutate(Group = cut(Edad, 
                                               breaks = c(18, 65, Inf),
                                               include.lowest = TRUE,
                                               right = FALSE))
dataset_km$Group = as.factor(dataset_km$Group)

##############################################
## A. Estimación general sin subpoblaciones ##
##############################################
df_km <- survival::survfit(Surv(dataset_km$Tiempo, 
                                dataset_km$Censura) ~ 1, data = dataset_km, 
                           type = "kaplan-meier", error = "tsiatis", 
                           conf.type = "log-log", conf.int = 0.95)

outcome_1 = with(df_km, data.frame(time, n.risk,
                                   n.event, surv, std.err,
                                   lower, upper))
colnames(outcome_1) = c("time", "n.risk", "n.event", "surv", "std.err", "lower 95% CI", 
                        "upper 95% CI")

# Guardar la salida del modelo
writexl::write_xlsx(outcome_1, 
                    "Análisis estadístico/Kaplan_Meier/120_días/Estimación_general/Outcome.xlsx")

# Guardar la curva de supervivencia estimada
png(file = "Análisis estadístico/Kaplan_Meier/120_días/Estimación_general/Curva.png",
    width = 558, height = 407)
ggsurvplot(fit = df_km, data = dataset_km, conf.int = T, 
           xlab = "Time since admission to ICU (days)", 
           ylab = "Survival probability",
           legend.title = "",
           palette = "lightgray",
           tables.theme = theme_cleantable(),
           ggthem = theme_bw()) 
dev.off()
rm(outcome_1)

##################################
## B. Subpoblaciones según sexo ##
##################################
df_km_sex <- survfit(Surv(dataset_km$Tiempo,
                      dataset_km$Censura) ~ Sexo, 
                 data = dataset_km, 
                 type = "kaplan-meier", 
                 error = "tsiatis", 
                 conf.type = "log-log", conf.int = 0.99)

# Pacientes críticos no-crónicos
outcome_f = with(df_km_sex[1], 
                 data.frame(time, n.risk, n.event, 
                            surv, std.err, lower, upper))
colnames(outcome_f) = c("time", "n.risk", "n.event", 
                        "surv", "std.err", "lower 95% CI", 
                        "upper 95% CI")
writexl::write_xlsx(outcome_f, 
                    "Análisis estadístico/Kaplan_Meier/120_días/Estimación_general/Sexo/Outcome_female.xlsx")

# Pacientes críticos crónicos
outcome_m = with(df_km_sex[2], 
                 data.frame(time, n.risk, n.event, 
                            surv, std.err, lower, upper))
colnames(outcome_m) = c("time", "n.risk", "n.event", 
                        "surv", "std.err", "lower 95% CI", 
                        "upper 95% CI")
writexl::write_xlsx(outcome_m, 
                    "Análisis estadístico/Kaplan_Meier/120_días/Estimación_general/Sexo/Outcome_male.xlsx")

# Guardar la curva de supervivencia estimada
png(file = "Análisis estadístico/Kaplan_Meier/120_días/Estimación_general/Sexo/Curva.png",
    width = 558, height = 407)
ggsurvplot(fit = df_km_sex, data = dataset_km, 
           conf.int = F, 
           xlab = "Time since admission to ICU (days)", 
           ylab = "Survival probability", 
           legend.title = "Prolonged ICU Stay",
           risk.table = FALSE,
           palette = c("black", "lightgray"),
           legend.labs = c("Male", "Female"),
           ggthem = theme_bw())
dev.off()

# Prueba log-rank
mantel_all_sex = survdiff(Surv(dataset_km$Tiempo, dataset_km$Censura) ~ Sexo, 
                          data = dataset_km, rho = 0)
m = which(mantel_cox$variable == "All by sex")
mantel_cox$chi_sqr[m] = round(mantel_all_sex$chisq, digits = 2)
mantel_cox$p_value[m] = round(mantel_all_sex$pvalue, digits = 2)

rm(df_km_sex, outcome_m, outcome_f, mantel_all_sex)
################################################
## C. Estimación general según grupos etarios ##
################################################
df_km_age <- survfit(Surv(dataset_km$Tiempo,
                      dataset_km$Censura) ~ Group, 
                 data = dataset_km, 
                 type = "kaplan-meier", 
                 error = "tsiatis", 
                 conf.type = "log-log", conf.int = 0.99)

# Grupo 1 [18, 65)
outcome_g1 = with(df_km_age[1], data.frame(time, n.risk, n.event, 
                             surv, std.err, lower, upper))
colnames(outcome_g1) = c("time", "n.risk", "n.event", 
                         "surv", "std.err", "lower 95% CI", 
                         "upper 95% CI")
writexl::write_xlsx(outcome_g1, 
                    "Análisis estadístico/Kaplan_Meier/120_días/Estimación_general/Edad/Outcome_g1.xlsx")

# Grupo 2 [65, Inf)
outcome_g2 = with(df_km_age[2], data.frame(time, n.risk, n.event, 
                             surv, std.err, lower, upper))
colnames(outcome_g2) = c("time", "n.risk", "n.event", 
                         "surv", "std.err", "lower 95% CI", 
                         "upper 95% CI")
writexl::write_xlsx(outcome_g2, 
                    "Análisis estadístico/Kaplan_Meier/120_días/Estimación_general/Edad/Outcome_g2.xlsx")

# Guardar la curva de supervivencia estimada
png(file = "Análisis estadístico/Kaplan_Meier/120_días/Estimación_general/Edad/Curva.png",
    width = 558, height = 407)
ggsurvplot(fit = df_km_age, data = dataset_km, 
           conf.int = F, 
           xlab = "Time since admission to ICU (days)", 
           ylab = "Survival probability", 
           legend.title = "Prolonged ICU Stay",
           risk.table = FALSE,
           palette = c("black", "gray", "lightgray"),
           legend.labs = c("[18,65)", "[65,Inf)"),
           ggthem = theme_bw())
dev.off()

# Prueba log-rank
mantel_all_age = survdiff(Surv(dataset_km$Tiempo, dataset_km$Censura) ~ Group, 
                          data = dataset_km, rho = 0)
m = which(mantel_cox$variable == "All by age")
mantel_cox$chi_sqr[m] = round(mantel_all_age$chisq, digits = 2)
mantel_cox$p_value[m] = round(mantel_all_age$pvalue, digits = 2)

rm(df_km_age, outcome_g1, outcome_g2, mantel_all_age)
#####################################################
## D. Subpoblaciones según crónicos (T≥21 vs T<21) ##
#####################################################
dataset_km_CCI = dataset_km[c("Caso", "Tiempo", "Censura", "CCI")]
aux_row = data.frame(Caso = 9999, Tiempo = 120, Censura = 0, CCI = "Crítico")
dataset_km_CCI = rbind(dataset_km_CCI, aux_row)

df_km_CCI <- survfit(Surv(dataset_km_CCI$Tiempo,
                          dataset_km_CCI$Censura) ~ CCI, 
                     data = dataset_km_CCI, 
                     type = "kaplan-meier", 
                     error = "tsiatis", 
                     conf.type = "log-log", conf.int = 0.99)

# Resultado para pacientes críticos
outcome_CCI_1 = with(df_km_CCI[1], 
                     data.frame(time, n.risk, n.event, 
                                surv, std.err, lower, upper))
colnames(outcome_CCI_1) = c("time", "n.risk", "n.event", 
                            "surv", "std.err", "lower 95% CI", 
                            "upper 95% CI")
writexl::write_xlsx(outcome_CCI_1, 
                    "Análisis estadístico/Kaplan_Meier/120_días/Estimación_crónicos/General/Outcome_críticos.xlsx")

# Resultado para pacientes crónicos
outcome_CCI_2 = with(df_km_CCI[2], 
                     data.frame(time, n.risk, n.event, 
                                surv, std.err, lower, upper))
colnames(outcome_CCI_2) = c("time", "n.risk", "n.event", 
                            "surv", "std.err", "lower 95% CI", 
                            "upper 95% CI")
writexl::write_xlsx(outcome_CCI_2, 
                    "Análisis estadístico/Kaplan_Meier/120_días/Estimación_crónicos/General/Outcome_crónicos.xlsx")

# Guardar curva de supervivencia estimada
png(file = "Análisis estadístico/Kaplan_Meier/120_días/Estimación_crónicos/General/Curva.png",
    width = 558, height = 407)
ggsurvplot(fit = df_km_CCI, data = dataset_km_CCI, 
           conf.int = F, 
           xlab = "Time since admission to ICU (days)", 
           ylab = "Survival probability", 
           legend.title = "Prolonged ICU Stay",
           legend.labs = c("Critical illness", "Chronic illness"),
           risk.table = FALSE,
           palette = c("black", "lightgray"),
           ggthem = theme_bw())
dev.off()

# Prueba log-rank
mantel_all_chronic = survdiff(Surv(dataset_km_CCI$Tiempo, dataset_km_CCI$Censura) ~ CCI, 
                              data = dataset_km_CCI, rho = 0)
m = which(mantel_cox$variable == "All by chronic")
mantel_cox$chi_sqr[m] = round(mantel_all_chronic$chisq, digits = 2)
mantel_cox$p_value[m] = round(mantel_all_chronic$pvalue, digits = 2)

rm(df_km_CCI, dataset_km_CCI, outcome_CCI_1, 
   outcome_CCI_2, mantel_all_chronic)
