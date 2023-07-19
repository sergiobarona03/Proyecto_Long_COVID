#####################################
##  03. Estimador de Kaplan-Meier  ##
##  (Pacientes críticos crónicos)  ##     
#####################################
library(here)
library(survminer)
library(survMisc)
library(survival)
source(here::here("Paper_resultados/Kaplan_Meier/", "K1_Casos_totales.R"))

########################
## A. CCI según sexo  ##
########################
# Base de datos de entrada
dataset_km_CCI = dataset_km[c("Caso", "Time",
                                  "Censored", "CCI", "Sex")]

# Seleccionar subpoblación CCI
dataset_km_CCI = dataset_km_CCI %>% filter(CCI == "CCI")

# Estimador de Kaplan-Meier
df_km_CCI <- survfit(Surv(dataset_km_CCI$Time,
                          dataset_km_CCI$Censored) ~ Sex, 
                         data = dataset_km_CCI, 
                         type = "kaplan-meier", 
                         error = "tsiatis", 
                         conf.type = "log-log", conf.int = 0.99)

# CCI con sexo masculino
outcome_CCI_m = with(df_km_CCI[1], 
                         data.frame(time, n.risk, n.event, 
                                    surv, std.err, lower, upper))
colnames(outcome_CCI_m) = c("time", "n.risk", "n.event", 
                                "surv", "std.err", "lower 95% CI", 
                                "upper 95% CI")
writexl::write_xlsx(outcome_CCI_m, 
                    "Paper_resultados/Kaplan_Meier/Output/CCI/Sex/Outcome_male.xlsx")

# CCI con sexo femenino
outcome_CCI_f = with(df_km_CCI[2], 
                         data.frame(time, n.risk, n.event, 
                                    surv, std.err, lower, upper))
colnames(outcome_CCI_f) = c("time", "n.risk", "n.event", 
                                "surv", "std.err", "lower 95% CI", 
                                "upper 95% CI")
writexl::write_xlsx(outcome_CCI_f, 
                    "Paper_resultados/Kaplan_Meier/Output/CCI/Sex/Outcome_female.xlsx")

# Curvas de supervivencia estimadas
p = ggsurvplot(fit = df_km_CCI, data = dataset_km_CCI, 
           conf.int = F, 
           xlab = "Time since admission to ICU (days)", 
           ylab = "Survival probability", 
           legend.title = "Chronic Critical Illness",
           risk.table = FALSE,
           palette = c("black", "lightgray"),
           legend.labs = c("Male", "Female"),
           ggthem = theme_bw())
p_dml = rvg::dml(ggobj = p$plot)
officer::read_pptx() %>% officer::add_slide() %>%
  officer::ph_with(p_dml, ph_location()) %>%
  base::print("Paper_resultados/Kaplan_Meier/Output/CCI/Sex/Curva.pptx")
rm(p, p_dml)



# Test log-rank
mantel_CCI_sex = survdiff(Surv(dataset_km_CCI$Time,
                               dataset_km_CCI$Censored) ~ Sex, 
                          data = dataset_km_CCI, rho = 0)
m = which(mantel_cox$variable == "CCI by sex")
mantel_cox$chi_sqr[m] = round(mantel_CCI_sex$chisq, digits = 2)
mantel_cox$p_value[m] = round(mantel_CCI_sex$pvalue, digits = 2)

rm(df_km_CCI, dataset_km_CCI, mantel_CCI_sex)

#######################
## B. CCI según edad ##
#######################
# Base de datos de entrada
dataset_km_CCI = dataset_km[c("Caso", "Time", "Censored", 
                               "CCI", "Group")]
aux_rows = data.frame(Caso = c(9998, 9999), Time = c(90, 90), 
                      Censored = c(0, 0),CCI = c("CCI", "CCI"), 
                      Group = c("[18,65)","[65,Inf]"))
dataset_km_CCI = rbind(dataset_km_CCI, aux_rows)

# Seleccionar la subpoblación CCI
dataset_km_CCI = dataset_km_CCI %>% filter(CCI == "CCI")

# Estimador de Kaplan-Meier
df_km_CCI <- survfit(Surv(dataset_km_CCI$Time,
                          dataset_km_CCI$Censored) ~ Group, 
                         data = dataset_km_CCI, 
                         type = "kaplan-meier", 
                         error = "tsiatis", 
                         conf.type = "log-log", conf.int = 0.99)

# Grupo 1: CCI de [18, 65)
outcome_CCI_g1 = with(df_km_CCI[1], data.frame(time, n.risk, n.event, 
                                     surv, std.err, lower, upper))
colnames(outcome_CCI_g1) = c("time", "n.risk", "n.event", 
                                 "surv", "std.err", "lower 95% CI", 
                                 "upper 95% CI")
writexl::write_xlsx(outcome_CCI_g1, 
                    "Paper_resultados/Kaplan_Meier/Output/CCI/Age/Outcome_g1.xlsx")

# Grupo 2: CCI de [65, Inf)
outcome_CCI_g2 = with(df_km_CCI[2], 
                          data.frame(time, n.risk, n.event, 
                                     surv, std.err, lower, upper))
colnames(outcome_CCI_g2) = c("time", "n.risk", "n.event", 
                                 "surv", "std.err", "lower 95% CI", 
                                 "upper 95% CI")
writexl::write_xlsx(outcome_CCI_g2, 
                    "Paper_resultados/Kaplan_Meier/Output/CCI/Age/Outcome_g2.xlsx")

# Curvas de supervivencia estimadas

p = ggsurvplot(fit = df_km_CCI, data = dataset_km_CCI, 
           conf.int = F, 
           xlab = "Time since admission to ICU (days)", 
           ylab = "Survival probability", 
           legend.title = "Chronic Critical Illness",
           risk.table = FALSE,
           palette = c("black", "gray"),
           legend.labs = c("[18,65)", "[65,Inf)"),
           ggthem = theme_bw())
p_dml = rvg::dml(ggobj = p$plot)
officer::read_pptx() %>% officer::add_slide() %>%
  officer::ph_with(p_dml, ph_location()) %>%
  base::print("Paper_resultados/Kaplan_Meier/Output/CCI/Age/Curva.pptx")
rm(p, p_dml)

# Test log-rank
mantel_CCI_age = survdiff(Surv(dataset_km_CCI$Time,
                               dataset_km_CCI$Censored) ~ Group, 
                          data = dataset_km_CCI, rho = 0)
m = which(mantel_cox$variable == "CCI by age")
mantel_cox$chi_sqr[m] = round(mantel_CCI_age$chisq, digits = 2)
mantel_cox$p_value[m] = round(mantel_CCI_age$pvalue, digits = 2)

rm(dataset_km_CCI, df_km_CCI, mantel_CCI_age, aux_row, aux_rows,
   outcome_CCI_f, outcome_CCI_m, outcome_CCI_g1,
   outcome_CCI_g2)

