########################################
##  04. Estimador de Kaplan-Meier     ##
##  (Pacientes críticos no-crónicos)  ##     
########################################
library(here)
source(here::here("Paper_resultados/Kaplan_Meier/", "K2_CCI.R"))

###########################
## A. no-CCI según sexo  ##
###########################
# Base de datos de entrada
dataset_km_non_CCI = dataset_km[c("Caso", "Time",
                                  "Censored", "CCI", "Sex")]
# Seleccionar la subpoblación no-CCI
dataset_km_non_CCI = dataset_km_non_CCI %>% filter(CCI == "non-CCI")

# Estimador de Kaplan-Meier
df_km_non_CCI <- survfit(Surv(dataset_km_non_CCI$Time,
                              dataset_km_non_CCI$Censored) ~ Sex, 
                         data = dataset_km_non_CCI, 
                         type = "kaplan-meier", 
                         error = "tsiatis", 
                         conf.type = "log-log", conf.int = 0.99)

# no-CCI con sexo masculino
outcome_non_CCI_m = with(df_km_non_CCI[1],data.frame(time, n.risk, n.event, 
                                    surv, std.err, lower, upper))
colnames(outcome_non_CCI_m) = c("time", "n.risk", "n.event", 
                                "surv", "std.err", "lower 95% CI", 
                                "upper 95% CI")
writexl::write_xlsx(outcome_non_CCI_m, 
                    "Paper_resultados/Kaplan_Meier/Output/Non_CCI/Sex/Outcome_male.xlsx")

# no-CCI con sexo femenino
outcome_non_CCI_f = with(df_km_non_CCI[2],data.frame(time, n.risk, n.event, 
                                    surv, std.err, lower, upper))
colnames(outcome_non_CCI_f) = c("time", "n.risk", "n.event", 
                                "surv", "std.err", "lower 95% CI", 
                                "upper 95% CI")
writexl::write_xlsx(outcome_non_CCI_f, 
                    "Paper_resultados/Kaplan_Meier/Output/Non_CCI/Sex/Outcome_female.xlsx")

# Curvas de supervivencia estimadas

p = ggsurvplot(fit = df_km_non_CCI, data = dataset_km_non_CCI, 
           conf.int = F, 
           xlab = "Time since admission to ICU (days)", 
           ylab = "Survival probability", 
           legend.title = "Non-Chronic Illness",
           risk.table = FALSE,
           palette = c("black", "lightgray"),
           legend.labs = c("Male", "Female"),
           ggthem = theme_bw())
p_dml = rvg::dml(ggobj = p$plot)
officer::read_pptx() %>% officer::add_slide() %>%
   officer::ph_with(p_dml, ph_location()) %>%
   base::print("Paper_resultados/Kaplan_Meier/Output/Non_CCI/Sex/Curva.pptx")
rm(p, p_dml)

# Test log-rank
mantel_no_CCI_sex = survdiff(Surv(dataset_km_non_CCI$Time,
                                  dataset_km_non_CCI$Censored) ~ Sex, 
                             data = dataset_km_non_CCI, rho = 0)
m = which(mantel_cox$variable == "No-CCI by sex")
mantel_cox$chi_sqr[m] = round(mantel_no_CCI_sex$chisq, digits = 2)
mantel_cox$p_value[m] = round(mantel_no_CCI_sex$pvalue, digits = 2)

rm(df_km_non_CCI, dataset_km_non_CCI, mantel_no_CCI_sex,
      outcome_non_CCI_f, outcome_non_CCI_m,
      outcome_non_CCI_g1, outcome_non_CCI_g2)

############################
## B. No-CCI según edad   ##
############################
# Base de datos de entrada
dataset_km_non_CCI = dataset_km[c("Caso", "Time", "Censored", 
                                  "CCI", "Group")]

# Seleccionar subpoblación no-CCI
dataset_km_non_CCI = dataset_km_non_CCI %>% filter(CCI == "non-CCI")

# Estimador de Kaplan-Meier
df_km_non_CCI <- survfit(Surv(dataset_km_non_CCI$Time,
                              dataset_km_non_CCI$Censored) ~ Group, 
                         data = dataset_km_non_CCI, 
                         type = "kaplan-meier", 
                         error = "tsiatis", 
                         conf.type = "log-log", conf.int = 0.99)

# Grupo 1 [18, 65)
outcome_non_CCI_g1 = with(df_km_non_CCI[1], 
                          data.frame(time, n.risk, n.event, 
                                     surv, std.err, lower, upper))
colnames(outcome_non_CCI_g1) = c("time", "n.risk", "n.event", 
                                 "surv", "std.err", "lower 95% CI", 
                                 "upper 95% CI")
writexl::write_xlsx(outcome_non_CCI_g1, 
                    "Paper_resultados/Kaplan_Meier/Output/Non_CCI/Age/Outcome_g1.xlsx")

# Grupo 2 [65, Inf)
outcome_non_CCI_g2 = with(df_km_non_CCI[2], 
                          data.frame(time, n.risk, n.event, 
                                     surv, std.err, lower, upper))
colnames(outcome_non_CCI_g2) = c("time", "n.risk", "n.event", 
                                 "surv", "std.err", "lower 95% CI", 
                                 "upper 95% CI")
writexl::write_xlsx(outcome_non_CCI_g2, 
                    "Paper_resultados/Kaplan_Meier/Output/Non_CCI/Age/Outcome_g2.xlsx")

# Curvas de supervivencia estimadas
p = ggsurvplot(fit = df_km_non_CCI, data = dataset_km_non_CCI, 
           conf.int = F, 
           xlab = "Time since admission to ICU (days)", 
           ylab = "Survival probability", 
           legend.title = "Prolonged ICU Stay",
           risk.table = FALSE,
           palette = c("black", "gray"),
           legend.labs = c("[18,65)", "[65,Inf)"),
           ggthem = theme_bw())
p_dml = rvg::dml(ggobj = p$plot)
officer::read_pptx() %>% officer::add_slide() %>%
   officer::ph_with(p_dml, ph_location()) %>%
   base::print("Paper_resultados/Kaplan_Meier/Output/Non_CCI/Age/Curva.pptx")
rm(p, p_dml)

# Test log-rank
mantel_no_CCI_age = survdiff(Surv(dataset_km_non_CCI$Time,
                                  dataset_km_non_CCI$Censored) ~ Group, 
                             data = dataset_km_non_CCI, rho = 0)
m = which(mantel_cox$variable == "No-CCI by age")
mantel_cox$chi_sqr[m] = round(mantel_no_CCI_age$chisq, digits = 2)
mantel_cox$p_value[m] = round(mantel_no_CCI_age$pvalue, digits = 2)

rm(df_km_non_CCI, dataset_km_non_CCI, mantel_no_CCI_age,
   outcome_non_CCI_f, outcome_non_CCI_m,
   outcome_non_CCI_g1, outcome_non_CCI_g2)

# Guardar la prueba de Mantel-Cox
writexl::write_xlsx(mantel_cox,  
                    "Paper_resultados/Kaplan_Meier/Output/Mantel_cox/Outcome_Mantel_Cox.xlsx")



