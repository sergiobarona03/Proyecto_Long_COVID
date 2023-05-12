###################################
## 02. Estimador de Kaplan-Meier ##
##    (Casos totales)            ##
###################################
library(here)
library(survminer)
library(survMisc)
library(survival)
source(here::here("Paper_resultados/Descriptive_analysis/05_Chi_2_Fisher/", "Fisher_test.R"))

############################################
## Preparar base de datos con covariables ##
############################################
dataset_km = dataset[c("Caso", "sex", "age", "CCI", "t_UCI", "d")]
colnames(dataset_km) = c("Caso", "Sex", "Age",
                         "CCI", "Time", "Censored")

dataset_km$CCI = as.factor(dataset_km$CCI)
dataset_km$Sex = as.factor(dataset_km$Sex)
dataset_km$Censored = as.numeric(dataset_km$Censored)

# Construir grupos etarios
dataset_km = dataset_km %>% mutate(Group = cut(Age, 
                                               breaks = c(18, 65, Inf),
                                               include.lowest = TRUE,
                                               right = FALSE))
dataset_km$Group = as.factor(dataset_km$Group)

# Base de datos de recepcion (Mantel-Cox)
mantel_cox = read_excel("Paper_resultados/Kaplan_Meier/Output/Mantel_Cox/Estructura_Mantel_Cox.xlsx")
mantel_cox = as.data.frame(mantel_cox)
##############################################
## A. Estimación general sin subpoblaciones ##
##############################################
df_km <- survival::survfit(Surv(dataset_km$Time, 
                                dataset_km$Censored) ~ 1, data = dataset_km, 
                           type = "kaplan-meier", error = "tsiatis", 
                           conf.type = "log-log", conf.int = 0.95)

outcome_1 = with(df_km, data.frame(time, n.risk,
                                   n.event, surv, std.err,
                                   lower, upper))
colnames(outcome_1) = c("time", "n.risk", "n.event", "surv", "std.err", "lower 95% CI", 
                        "upper 95% CI")

# Guardar la salida del modelo
writexl::write_xlsx(outcome_1, 
                    "Paper_resultados/Kaplan_Meier/Output/Total/General/Outcome.xlsx")

# Guardar la curva de supervivencia estimada
png(file = "Paper_resultados/Kaplan_Meier/Output/Total/General/Curva.png",
    width = 558, height = 407)
ggsurvplot(fit = df_km, data = dataset_km, conf.int = T, 
           xlab = "Time since admission to ICU (days)", 
           ylab = "Survival probability",
           legend.title = "",
           palette = "lightgray",
           tables.theme = theme_cleantable(),
           ggthem = theme_bw()) 
dev.off()
rm(outcome_1, df_km)

##################################
## B. Subpoblaciones según sexo ##
##################################
df_km_sex <- survfit(Surv(dataset_km$Time,
                      dataset_km$Censored) ~ Sex, 
                 data = dataset_km, 
                 type = "kaplan-meier", 
                 error = "tsiatis", 
                 conf.type = "log-log", conf.int = 0.99)

# Pacientes con sexo femenino
outcome_f = with(df_km_sex[1], 
                 data.frame(time, n.risk, n.event, 
                            surv, std.err, lower, upper))
colnames(outcome_f) = c("time", "n.risk", "n.event", 
                        "surv", "std.err", "lower 95% CI", 
                        "upper 95% CI")
writexl::write_xlsx(outcome_f, 
                    "Paper_resultados/Kaplan_Meier/Output/Total/Sex/Outcome_female.xlsx")

# Pacientes con sexo masculino
outcome_m = with(df_km_sex[2], 
                 data.frame(time, n.risk, n.event, 
                            surv, std.err, lower, upper))
colnames(outcome_m) = c("time", "n.risk", "n.event", 
                        "surv", "std.err", "lower 95% CI", 
                        "upper 95% CI")
writexl::write_xlsx(outcome_m, 
                    "Paper_resultados/Kaplan_Meier/Output/Total/Sex/Outcome_male.xlsx")

# Guardar la curva de supervivencia estimada
png(file = "Paper_resultados/Kaplan_Meier/Output/Total/Sex/Curva.png",
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
mantel_all_sex = survdiff(Surv(dataset_km$Time, dataset_km$Censored) ~ Sex, 
                          data = dataset_km, rho = 0)
m = which(mantel_cox$variable == "All by sex")
mantel_cox$chi_sqr[m] = round(mantel_all_sex$chisq, digits = 2)
mantel_cox$p_value[m] = round(mantel_all_sex$pvalue, digits = 2)

rm(df_km_sex, outcome_m, outcome_f, mantel_all_sex)
################################################
## C. Estimación general según grupos etarios ##
################################################
df_km_age <- survfit(Surv(dataset_km$Time,
                      dataset_km$Censored) ~ Group, 
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
                    "Paper_resultados/Kaplan_Meier/Output/Total/Age/Outcome_g1.xlsx")

# Grupo 2 [65, Inf)
outcome_g2 = with(df_km_age[2], data.frame(time, n.risk, n.event, 
                             surv, std.err, lower, upper))
colnames(outcome_g2) = c("time", "n.risk", "n.event", 
                         "surv", "std.err", "lower 95% CI", 
                         "upper 95% CI")
writexl::write_xlsx(outcome_g2, 
                    "Paper_resultados/Kaplan_Meier/Output/Total/Age/Outcome_g2.xlsx")

# Guardar la curva de supervivencia estimada
png(file = "Paper_resultados/Kaplan_Meier/Output/Total/Age/Curva.png",
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
mantel_all_age = survdiff(Surv(dataset_km$Time, dataset_km$Censored) ~ Group, 
                          data = dataset_km, rho = 0)
m = which(mantel_cox$variable == "All by age")
mantel_cox$chi_sqr[m] = round(mantel_all_age$chisq, digits = 2)
mantel_cox$p_value[m] = round(mantel_all_age$pvalue, digits = 2)

rm(df_km_age, outcome_g1, outcome_g2, mantel_all_age)

################################################
## D. Subpoblaciones según CCI (T≥21 vs T<21) ##
################################################
dataset_km_CCI = dataset_km[c("Caso", "Time", "Censored", "CCI")]
aux_row = data.frame(Caso = 9999, Time = 100, Censored = 0, CCI = "non-CCI")
dataset_km_CCI = rbind(dataset_km_CCI, aux_row)

df_km_CCI <- survfit(Surv(dataset_km_CCI$Time,
                          dataset_km_CCI$Censored) ~ CCI, 
                     data = dataset_km_CCI, 
                     type = "kaplan-meier", 
                     error = "tsiatis", 
                     conf.type = "log-log", conf.int = 0.99)

# Resultado para pacientes crónicos
outcome_CCI_1 = with(df_km_CCI[1], 
                     data.frame(time, n.risk, n.event, 
                                surv, std.err, lower, upper))
colnames(outcome_CCI_1) = c("time", "n.risk", "n.event", 
                            "surv", "std.err", "lower 95% CI", 
                            "upper 95% CI")
writexl::write_xlsx(outcome_CCI_1, 
                    "Paper_resultados/Kaplan_Meier/Output/Total/CCI/Outcome_CCI.xlsx")

# Resultado para pacientes críticos
outcome_CCI_2 = with(df_km_CCI[2], 
                     data.frame(time, n.risk, n.event, 
                                surv, std.err, lower, upper))
colnames(outcome_CCI_2) = c("time", "n.risk", "n.event", 
                            "surv", "std.err", "lower 95% CI", 
                            "upper 95% CI")
writexl::write_xlsx(outcome_CCI_2, 
                    "Paper_resultados/Kaplan_Meier/Output/Total/CCI/Outcome_Non_CCI.xlsx")

# Guardar curva de supervivencia estimada
png(file = "Paper_resultados/Kaplan_Meier/Output/Total/CCI/Curva.png",
    width = 558, height = 407)
ggsurvplot(fit = df_km_CCI, data = dataset_km_CCI, 
           conf.int = F, 
           xlab = "Time since admission to ICU (days)", 
           ylab = "Survival probability", 
           legend.title = "Prolonged ICU Stay",
           legend.labs = c("Chronic illness", "Critical illness"),
           risk.table = FALSE,
           palette = c("black", "lightgray"),
           ggthem = theme_bw())
dev.off()

# Prueba log-rank
mantel_all_chronic = survdiff(Surv(dataset_km_CCI$Time, dataset_km_CCI$Censored) ~ CCI, 
                              data = dataset_km_CCI, rho = 0)
m = which(mantel_cox$variable == "All by chronic")
mantel_cox$chi_sqr[m] = round(mantel_all_chronic$chisq, digits = 2)
mantel_cox$p_value[m] = round(mantel_all_chronic$pvalue, digits = 2)

rm(df_km_CCI, dataset_km_CCI, outcome_CCI_1, 
   outcome_CCI_2, mantel_all_chronic)
