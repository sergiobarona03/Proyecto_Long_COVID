
########################################
## Modelo paramétrico: casos totales  ##
########################################

source(here::here("Paper_resultados/Parametric_model/Survival_rate/",
                  "A2_Selecting.R"))

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

dataset_parametric <- dataset_parametric %>% mutate(sex = factor(sex,
                                                                       levels = c("0", "1"),
                                                                       labels = c("Male", "Female")))


###########################################
## Estimación diferenciada según la edad ##
###########################################

parametric_model_sex = dataset_parametric %>% group_by(sex) %>% do(flex = flexsurvreg(Surv(t_UCI, d) ~ 1, data = ., dist = "gompertz"))
df_km_sex <-  survfit(Surv(t_UCI, d) ~ sex, data = dataset_parametric)



# Male
parametric_model_male = parametric_model_sex[[2]][[1]] %>% summary(type = "survival") %>% data.frame
writexl::write_xlsx(parametric_model_male,
                    "Paper_resultados/Parametric_model/Survival_rate/Output/Total/Sex/Outcome_male.xlsx")

# Female
parametric_model_female = parametric_model_sex[[2]][[2]] %>% summary(type = "survival") %>% data.frame
writexl::write_xlsx(parametric_model_female,
                    "Paper_resultados/Parametric_model/Survival_rate/Output/Total/Sex/Outcome_female.xlsx")

# Guardar la curva de supervivencia estimada
setEPS()
postscript("Paper_resultados/Parametric_model/Survival_rate/Output/Total/Sex/Curva.eps")
plot(df_km_sex,  main = "", xlab = "Time since admission to ICU (days)",
     ylab = "Survival probability", col = c("black", "azure4"))
lines(parametric_model_sex$flex[[1]], ci = TRUE,   col = "black")
lines(parametric_model_sex$flex[[2]], ci = TRUE,   col = "azure4")
legend("topright", legend = c("Male", "Female"), 
       lwd = 1:(2 + 1), 
       col = c("black", "azure4"), bty = "n", fill = c("black","azure4"),
       horiz = TRUE, inset = 0.08)
dev.off()

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
setEPS()
postscript("Paper_resultados/Parametric_model/Survival_rate/Output/Total/Age/Curva.eps")
plot(df_km_group,  main = "", xlab = "Time since admission to ICU (days)",
     ylab = "Survival probability", col = c("black", "azure4"))
lines(parametric_model_age$flex[[1]], ci = TRUE,   col = "black")
lines(parametric_model_age$flex[[2]], ci = TRUE,   col = "azure4")
legend("topright", legend = c("[18,65)", "[65,Inf]"), 
       lwd = 1:(2 + 1), 
       col = c("black", "azure4"), bty = "n", fill = c("black","azure4"),
       horiz = TRUE, inset = 0.08)
dev.off()


                                                                                                                                                                   
