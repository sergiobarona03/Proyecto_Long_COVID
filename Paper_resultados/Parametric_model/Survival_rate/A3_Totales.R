
########################################
## Modelo paramétrico: casos totales  ##
########################################

# construcción de grupos etarios

dataset_parametric = dataset %>% filter(age > 18)
dataset_parametric = dataset_parametric %>% mutate(Group = cut(age, 
                                               breaks = c(18, 65, Inf),
                                               include.lowest = TRUE,
                                               right = FALSE))
dataset_parametric$Group = as.character(dataset_parametric$Group)

dataset_parametric$Group_age = NA

for (i in 1:nrow(dataset_parametric)) {
  if (dataset_parametric$Group[i] == "[18,65)") {
    dataset_parametric$Group_age[i] = 1
  }
  
  if (dataset_parametric$Group[i] == "[65,Inf]") {
    dataset_parametric$Group_age[i] = 2
  }
}


dataset_parametric <- dataset_parametric %>% mutate(Group_age = factor(Group_age,
                                    levels = c(1, 2),
                                    labels = c("[18,65)", "[65,Inf]")))

###########################################
## Estimación diferenciada según la edad ##
###########################################

parametric_model_age = dataset_parametric %>% group_by(Group_age) %>% do(flex = flexsurvreg(Surv(t_UCI, d) ~ 1, data = ., dist = "gompertz"))
df_km_group <-  survfit(Surv(t_UCI, d) ~ Group, data = dataset_parametric)



# Grupo 1
parametric_model_age_1 = parametric_model_age[[2]][[1]] %>% summary(type = "survival") %>% data.frame
writexl::write_xlsx(parametric_model_age_1,
                    "Paper_resultados/Parametric_model/Survival_rate/Output/Total/Age/Outcome_g1.xlsx")

# Grupo 2:
parametric_model_age_2 = parametric_model_age[[2]][[2]] %>% summary(type = "survival") %>% data.frame
writexl::write_xlsx(parametric_model_age_2,
                    "Paper_resultados/Parametric_model/Survival_rate/Output/Total/Age/Outcome_g2.xlsx")

# Guardar la curva de supervivencia estimada
png(file = "Paper_resultados/Parametric_model/Survival_rate/Output/Total/Age/Curva.png",
    width = 558, height = 407)

plot(df_km_group,  main = "", xlab = "Time since admission to ICU (days)",
     ylab = "Survival probability", col = c("black", "azure4"))
lines(parametric_model_age$flex[[1]], ci = TRUE,   col = "black")
lines(parametric_model_age$flex[[2]], ci = TRUE,   col = "azure4")
legend("topright", legend = c("[18,65)", "[65,Inf]"), 
       lwd = 1:(2 + 1), 
       col = c("black", "azure4"), bty = "n", fill = c("black","azure4"),
       horiz = TRUE, inset = 0.08)

dev.off()

plot(df_km_group,  main = "", xlab = "Time since admission to ICU (days)",
     ylab = "Survival probability", col = c("black", "azure4"))
lines(parametric_model_age$flex[[1]], ci = TRUE,   col = "black")
lines(parametric_model_age$flex[[2]], ci = TRUE,   col = "azure4")
legend("topright", legend = c("[18,65)", "[65,Inf]"), 
       lwd = 1:(2 + 1), 
       col = c("black", "azure4"), bty = "n", fill = c("black","azure4"),
       horiz = TRUE, inset = 0.08)
                                                                                                                                                                   
##################################################
## Estimación diferenciada según CCI vs non-CCI ##
##################################################

parametric_model_status = dataset_parametric %>% group_by(CCI) %>% do(flex = flexsurvreg(Surv(t_UCI, d) ~ 1, data = ., dist = "gompertz"))
df_km_group <-  survfit(Surv(t_UCI, d) ~ CCI, data = dataset_parametric)



# Grupo 1
parametric_model_CCI = parametric_model_status[[2]][[1]] %>% summary(type = "survival") %>% data.frame
writexl::write_xlsx(parametric_model_CCI,
                    "Paper_resultados/Parametric_model/Survival_rate/Output/Total/CCI/Outcome_CCI.xlsx")

# Grupo 2:
parametric_model_non_CCI = parametric_model_status[[2]][[2]] %>% summary(type = "survival") %>% data.frame
writexl::write_xlsx(parametric_model_non_CCI,
                    "Paper_resultados/Parametric_model/Survival_rate/Output/Total/CCI/Outcome_Non_CCI.xlsx")

# Guardar la curva de supervivencia estimada
png(file = "Paper_resultados/Parametric_model/Survival_rate/Output/Total/CCI/Curva.png",
    width = 558, height = 407)

plot(df_km_group,  main = "", xlab = "Time since admission to ICU (days)",
     ylab = "Survival probability", col = c("black", "azure4"))
lines(parametric_model_age$flex[[1]], ci = TRUE,   col = "black")
lines(parametric_model_age$flex[[2]], ci = TRUE,   col = "azure4")
legend("topright", legend = c("CCI", "Non-CCI"), 
       lwd = 1:(2 + 1), 
       col = c("black", "azure4"), bty = "n", fill = c("black","azure4"),
       horiz = TRUE, inset = 0.08)

dev.off()

plot(df_km_group,  main = "", xlab = "Time since admission to ICU (days)",
     ylab = "Survival probability", col = c("black", "azure4"))
lines(parametric_model_age$flex[[1]], ci = TRUE,   col = "black")
lines(parametric_model_age$flex[[2]], ci = TRUE,   col = "azure4")
legend("topright", legend = c("CCI", "Non-CCI"), 
       lwd = 1:(2 + 1), 
       col = c("black", "azure4"), bty = "n", fill = c("black","azure4"),
       horiz = TRUE, inset = 0.08)


CCI = dataset %>% filter(CCI == "CCI")

CCI_parametric = flexsurvreg(Surv(t_UCI, d) ~ 1, data = dataset, dist = "gompertz") %>% summary(type = "survival") %>% data.frame

x =  c("osa", "bli", "usd", "ml")
y = c("mnu", "erd", "usd", "ml")
z =  c("ssu", "erd", "usd", "ml")

myl = list(A = x,B = y,C = z)

lapply(1:length(myl), function(n) setdiff(myl[[n]], unlist(myl[-n])))


