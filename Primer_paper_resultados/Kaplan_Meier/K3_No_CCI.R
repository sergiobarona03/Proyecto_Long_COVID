########################################
##  04. Estimador de Kaplan-Meier     ##
##  (Pacientes críticos no-crónicos)  ##     
########################################
library(here)
setwd("/Users/sergiobarona03/Desktop/Proyecto_Long_COVID/")
source(here::here("Análisis estadístico/Kaplan_Meier","03_CCI.R"))

###########################
## A. no-CCI según sexo  ##
###########################
# Base de datos de entrada
dataset_km_critico = dataset_km[c("Caso", "Tiempo",
                                  "Censura", "CCI", "Sexo")]
aux_rows = data.frame(Caso = c(9998,9999), Tiempo = c(120, 120), 
                      Censura = c(0, 0), 
                      CCI = c("Crítico", "Crítico"), 
                      Sexo = c(0, 1))
dataset_km_critico = rbind(dataset_km_critico, aux_rows)

# Seleccionar la subpoblación no-CCI
dataset_km_critico = dataset_km_critico %>% filter(CCI == "Crítico")

# Estimador de Kaplan-Meier
df_km_critico <- survfit(Surv(dataset_km_critico$Tiempo,
                              dataset_km_critico$Censura) ~ Sexo, 
                         data = dataset_km_critico, 
                         type = "kaplan-meier", 
                         error = "tsiatis", 
                         conf.type = "log-log", conf.int = 0.99)

# no-CCI con sexo femenino
outcome_critico_f = with(df_km_critico[1],data.frame(time, n.risk, n.event, 
                                    surv, std.err, lower, upper))
colnames(outcome_critico_f) = c("time", "n.risk", "n.event", 
                                "surv", "std.err", "lower 95% CI", 
                                "upper 95% CI")
writexl::write_xlsx(outcome_critico_f, 
                    "Análisis estadístico/Kaplan_Meier/120_días/Estimación_críticos/Sexo/Outcome_female.xlsx")

# no-CCI con sexo masculino
outcome_critico_m = with(df_km_critico[2],data.frame(time, n.risk, n.event, 
                                    surv, std.err, lower, upper))
colnames(outcome_critico_m) = c("time", "n.risk", "n.event", 
                                "surv", "std.err", "lower 95% CI", 
                                "upper 95% CI")
writexl::write_xlsx(outcome_critico_m, 
                    "Análisis estadístico/Kaplan_Meier/120_días/Estimación_críticos/Sexo/Outcome_male.xlsx")

# Curvas de supervivencia estimadas
png(file = "Análisis estadístico/Kaplan_Meier/120_días/Estimación_críticos/Sexo/Curva.png",
    width = 558, height = 407)
ggsurvplot(fit = df_km_critico, data = dataset_km_critico, 
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
mantel_no_CCI_sex = survdiff(Surv(dataset_km_critico$Tiempo,
                                  dataset_km_critico$Censura) ~ Sexo, 
                             data = dataset_km_critico, rho = 0)
m = which(mantel_cox$variable == "No-CCI by sex")
mantel_cox$chi_sqr[m] = round(mantel_no_CCI_sex$chisq, digits = 2)
mantel_cox$p_value[m] = round(mantel_no_CCI_sex$pvalue, digits = 2)

rm(df_km_critico, dataset_km_critico, mantel_no_CCI_sex)

############################
## B. No-CCI según edad   ##
############################
# Base de datos de entrada
dataset_km_critico = dataset_km[c("Caso", "Tiempo", "Censura", 
                                  "CCI", "Group")]
aux_rows = data.frame(Caso = c(9998, 9999), Tiempo = c(120, 120), 
                      Censura = c(0, 0), 
                      CCI = c("Crítico", "Crítico"), 
                      Group = c("[18,65)", 
                                "[65,Inf]"))
dataset_km_critico = rbind(dataset_km_critico, aux_rows)

# Seleccionar subpoblación no-CCI
dataset_km_critico = dataset_km_critico %>% filter(CCI == "Crítico")

# Estimador de Kaplan-Meier
df_km_critico <- survfit(Surv(dataset_km_critico$Tiempo,
                              dataset_km_critico$Censura) ~ Group, 
                         data = dataset_km_critico, 
                         type = "kaplan-meier", 
                         error = "tsiatis", 
                         conf.type = "log-log", conf.int = 0.99)

# Grupo 1 [18, 65)
outcome_critico_g1 = with(df_km_critico[1], 
                          data.frame(time, n.risk, n.event, 
                                     surv, std.err, lower, upper))
colnames(outcome_critico_g1) = c("time", "n.risk", "n.event", 
                                 "surv", "std.err", "lower 95% CI", 
                                 "upper 95% CI")
writexl::write_xlsx(outcome_critico_g1, 
                    "Análisis estadístico/Kaplan_Meier/120_días/Estimación_críticos/Edad/Outcome_g1.xlsx")

# Grupo 2 [65, Inf)
outcome_critico_g2 = with(df_km_critico[2], 
                          data.frame(time, n.risk, n.event, 
                                     surv, std.err, lower, upper))
colnames(outcome_critico_g2) = c("time", "n.risk", "n.event", 
                                 "surv", "std.err", "lower 95% CI", 
                                 "upper 95% CI")
writexl::write_xlsx(outcome_critico_g2, 
                    "Análisis estadístico/Kaplan_Meier/120_días/Estimación_críticos/Edad/Outcome_g2.xlsx")

# Curvas de supervivencia estimadas
png(file = "Análisis estadístico/Kaplan_Meier/120_días/Estimación_críticos/Edad/Curva.png",
    width = 558, height = 407)
ggsurvplot(fit = df_km_critico, data = dataset_km_critico, 
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
mantel_no_CCI_age = survdiff(Surv(dataset_km_critico$Tiempo,
                                  dataset_km_critico$Censura) ~ Group, 
                             data = dataset_km_critico, rho = 0)
m = which(mantel_cox$variable == "No-CCI by age")
mantel_cox$chi_sqr[m] = round(mantel_no_CCI_age$chisq, digits = 2)
mantel_cox$p_value[m] = round(mantel_no_CCI_age$pvalue, digits = 2)

rm(df_km_critico, dataset_km_critico, mantel_no_CCI_age,
   aux_rows, outcome_critico_f, outcome_critico_m,
   outcome_critico_g1, outcome_critico_g2)

# Guardar la prueba de Mantel-Cox
writexl::write_xlsx(mantel_cox,  
                    "Análisis estadístico/Kaplan_Meier/120_días/Mantel_cox/Outcome_Mantel_Cox.xlsx")



