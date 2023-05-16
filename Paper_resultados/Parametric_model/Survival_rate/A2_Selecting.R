
######################################
## Selección del modelo paramétrico ##
######################################

source(here::here("Paper_resultados/Parametric_model/Survival_rate/",
                  "A1_Parametric_models.R"))

################################################
## Comparación de las tasas de supervivencia  ##
##    (Salida de los modelos paramétricos)    ##
################################################
# gamma
gamma_model = flexg_gamma
colnames(gamma_model) = c("time", "gamma")

# gompertz
gompertz_model = flexg_gompertz[c("time", "est")]
colnames(gompertz_model) = c("time", "gompertz")

# exponencial
exp_model = flexg_exp[c("time","est")]
colnames(exp_model) = c("time", "exp")

# log-normal
lognormal_model = flexg_lnorm[c("time","est")]
colnames(lognormal_model) = c("time", "lnormal")

# log-logis
loglogis_model = flexg_loglogis[c("time","est")]
colnames(loglogis_model) = c("time", "loglogis")

# weibull
weibull_model = flexg_weibull[c("time","est")]
colnames(weibull_model) = c("time", "weibull")

comparar = merge(gamma_model, gompertz_model, by = "time")
comparar = merge(comparar, exp_model, by = "time")
comparar = merge(comparar, lognormal_model, by = "time")
comparar = merge(comparar, loglogis_model, by ="time")
comparar = merge(comparar, weibull_model, by = "time")

########################################## 
## Comparación de modelos paramétricos  ##
##########################################

# Curvas de supervivencia
dist_surv <- c("Weibull", "Log-normal",
                 "Generalized Gamma", "Log-Logistic",
                 "Gompertz", "Exponential") 
surv_input = list(flexg_weibull, flexg_lnorm, flexg_gamma,
                    flexg_loglogis, flexg_gompertz, flexg_exp)
names(surv_input) = dist_surv

png(file = "Paper_resultados/Parametric_model/Survival_rate/Output/Parametric_model_vs.png",
    width = 500, height = 450)

ggplot() + geom_line(aes(time, (est), 
                         col = names(surv_input)[1]), 
                     data = surv_input[[1]],
) + geom_line(aes(time, (est), 
                  col = names(surv_input)[2]), 
              data = surv_input[[2]],
) + geom_line(aes(time, (est), 
                  col = names(surv_input)[3]), 
              data = surv_input[[3]],
) + geom_line(aes(time, (est), 
                  col = names(surv_input)[4]), 
              data = surv_input[[4]],
) + geom_line(aes(time, (est), 
                  col = names(surv_input)[5]), 
              data = surv_input[[5]],
) + geom_line(aes(time, (est), 
                  col = names(surv_input)[6]), 
              data = surv_input[[6]],
) + geom_step(aes(time, (surv), col = "Kaplan-Meier"), data = non_par) +
    labs(x = "Time since admission to ICU (days)", 
         y = "Survival probability", col = "") + scale_color_viridis_d()+ theme(
             legend.position = c(0.8, 0.7), legend.background = element_blank()
  
                                                                                           )
dev.off()

# Funciones de riesgo acumulado
dist_hazard <- dist_surv 
hazard_input = dist_input
names(hazard_input) = dist_hazard

png(file = "Paper_resultados/Parametric_model/Survival_rate/Output/Hazard_model_vs.png",
    width = 500, height = 450)

ggplot() + geom_line(aes(time, -log(est), 
                         col = names(hazard_input)[1]), 
                     data = hazard_input[[1]],
) + geom_line(aes(time, -log(est), 
                  col = names(hazard_input)[2]), 
              data = hazard_input[[2]],
) + geom_line(aes(time, -log(est), 
                  col = names(hazard_input)[3]), 
              data = hazard_input[[3]],
) + geom_line(aes(time, -log(est), 
                  col = names(hazard_input)[4]), 
              data = hazard_input[[4]],
) + geom_line(aes(time, -log(est), 
                  col = names(hazard_input)[5]), 
              data = hazard_input[[5]],
) + geom_line(aes(time, -log(est), 
                  col = names(hazard_input)[6]), 
              data = hazard_input[[6]],
) + geom_step(aes(time, -log(surv), col = "Kaplan-Meier"), data = non_par) +
    labs(x = "Time since admission to ICU (days)", 
         y = "Cumulative Hazard", col = "") + scale_color_viridis_d() + theme(
             legend.position = c(0.8, 0.3), legend.background = element_blank()
         )

dev.off()
##########################################
## (Criterio de información de Akaike)  ##
##########################################
dist = c("weibull", "lnorm", "GenGamma.orig",
         "llogis", "gompertz", "exp")
data_surv <- Surv(dataset$t_UCI, dataset$d)
modelo <- sapply(dist, 
                 function(x) flexsurvreg(data_surv ~ 1, data = dataset, dist = x), 
                 USE.NAMES = T, simplify = F)

IC_model <- sapply(modelo, function(x) c(AIC = AIC(x), BIC = BIC(x), LogLik = logLik(x)), simplify = T)
IC_model[, order(IC_model["AIC", ])]


# Guardar en formato latex
IC_model_t = IC_model %>% as.matrix() %>% t() %>% as.data.frame()

write_xlsx(IC_model_t, 
           "Paper_resultados/Parametric_model/Survival_rate/Output/IC_model.xlsx")

IC_model_latex  = kable(IC_model_t, 
                            caption = "Criterios de información",
                            format = "latex")
writeLines(IC_model_latex,
           "Paper_resultados/Parametric_model/Survival_rate/Output/IC_model.tex")







