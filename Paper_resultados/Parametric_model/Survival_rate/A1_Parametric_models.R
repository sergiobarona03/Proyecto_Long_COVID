
###############################
## Primer borrador: análisis ##
##     de distribución       ##
###############################
source(here::here("Paper_resultados/Descriptive_analysis/05_Chi_2_Fisher/",
                  "Fisher_test.R"))

library(here)
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

############################################
## Modelo paramétrico gamma generalizado  ##
############################################
flexg_gamma <- flexsurvreg(Surv(t_UCI, d) ~ 1,
                           data = dataset, 
                           dist = "gengamma.orig") %>% summary(type = "survival") %>% data.frame

non_par <- survfit(Surv(t_UCI, d) ~ 1, data = dataset) %>% fortify

# Curva de supervivencia
ggplot() + geom_line(aes(time, est, col = "Paramétrico (Gamma generalizada)"),
                     data = flexg_gamma) + geom_step(aes(time,
                                                         surv, col = "No Paramétrico"), data =non_par) + labs(x = "Tiempo (Días)",
                                                                                                              y = "Probabilidad de Supervivencia", col = "Ajustes", 
                                                                                                          title = "Figura 7: Estimación paramétrica y no-paramétrica de la función de supervivencia")
# Función de riesgo acumulada
ggplot() + geom_line(aes(time, -log(est), col = "Paramétrico (Gamma generalizada)"), data = flexg_gamma) + geom_step(aes(time, -log(surv), col = "No-paramétrico"), data = non_par) +
  labs(x = "Tiempo (Días)", 
       y = "Riesgo Acumulado", col = "Ajustes", title = "Figura 8: Estimación paramétrica y no-paramétrica de la función de riesgo acumulado")


#######################################
## Modelo paramétrico log-logística  ##
#######################################

flexg_loglogis <- flexsurvreg(Surv(t_UCI, d) ~ 1, 
                              data = dataset, dist = "llogis") %>%
  summary(type = "survival") %>% data.frame

# Curva de supervivencia
ggplot() + geom_line(aes(time, est, col = "Paramétrico (Log-logística)"),
                     data = flexg_loglogis) + geom_step(aes(time,
                                                            surv, col = "No-paramétrico"),
                                                        data =non_par) + labs(x = "Tiempo (Días)",
                                                                              y = "Probabilidad de Supervivencia", col = "Ajustes")
# Función de riesgo acumulado
ggplot() + geom_line(aes(time, -log(est), col = "Paramétrico (Log-logística)"), data = flexg_loglogis) +
  geom_step(aes(time, -log(surv), col = "No-paramétrico"), data = non_par) +
  labs(x = "Tiempo (Días)", y = "Riesgo Acumulado", col = "Ajustes")

#######################################
## Modelo paramétrico de Gompertz    ##
#######################################
flexg_gompertz <- flexsurvreg(Surv(t_UCI, d) ~ 1, data = dataset, dist = "gompertz") %>% summary(type = "survival") %>% data.frame

# Curva de supervivencia
ggplot() + geom_line(aes(time, est, col = "Parametric model (Gompertz)"),
                     data = flexg_gompertz) + geom_step(aes(time,
                                                            surv, col = "Kaplan-Meier estimator"),
                                                        data =non_par) + labs(x = "Time since admission to ICU (days)",
                                                                              y = "Survival probability",
                                                                              col = "")
# Función de riesgo acumulado
ggplot() + geom_line(aes(time, -log(est), col = "Paramétrico (Gompertz)"), data = flexg_gompertz) +
  geom_step(aes(time, -log(surv), col = "No-paramétrico"), data = non_par) +
  labs(x = "Tiempo (Días)", y = "Riesgo Acumulado", col = "Ajustes")


#######################################
## Modelo paramétrico exponencial    ##
#######################################
flexg_exp <- flexsurvreg(Surv(t_UCI, d) ~ 1, data = dataset, dist = "exp") %>% summary(type = "survival") %>% data.frame

# Curva de supervivencia
ggplot() + geom_line(aes(time, est, col = "Parametric model (Exponential)"),
                     data = flexg_exp) + geom_step(aes(time,
                                                            surv, col = "Kaplan-Meier estimator"),
                                                        data =non_par) + labs(x = "Time since admission to ICU (days)",
                                                                              y = "Survival probability",
                                                                              col = "")
# Función de riesgo acumulado
ggplot() + geom_line(aes(time, -log(est), col = "Paramétrico (Exponential)"), data = flexg_exp) +
  geom_step(aes(time, -log(surv), col = "No-paramétrico"), data = non_par) +
  labs(x = "Tiempo (Días)", y = "Riesgo Acumulado", col = "Ajustes")



#######################################
## Modelo paramétrico log normal     ##
#######################################
flexg_lnorm <- flexsurvreg(Surv(t_UCI, d) ~ 1, data = dataset, dist = "lnorm") %>% summary(type = "survival") %>% data.frame

# Curva de supervivencia
ggplot() + geom_line(aes(time, est, col = "Parametric model (Log-Normal)"),
                     data = flexg_lnorm) + geom_step(aes(time,
                                                            surv, col = "Kaplan-Meier estimator"),
                                                        data =non_par) + labs(x = "Time since admission to ICU (days)",
                                                                              y = "Survival probability",
                                                                              col = "")
# Función de riesgo acumulado
ggplot() + geom_line(aes(time, -log(est), col = "Paramétrico (Log-Normal)"), data = flexg_lnorm) +
  geom_step(aes(time, -log(surv), col = "No-paramétrico"), data = non_par) +
  labs(x = "Tiempo (Días)", y = "Riesgo Acumulado", col = "Ajustes")


#######################################
## Modelo paramétrico Weibull        ##
#######################################
flexg_weibull <- flexsurvreg(Surv(t_UCI, d) ~ 1, data = dataset, dist = "weibull") %>% summary(type = "survival") %>% data.frame

# Curva de supervivencia
ggplot() + geom_line(aes(time, est, col = "Parametric model (Weibull)"),
                     data = flexg_lnorm) + geom_step(aes(time,
                                                         surv, col = "Kaplan-Meier estimator"),
                                                     data =non_par) + labs(x = "Time since admission to ICU (days)",
                                                                           y = "Survival probability",
                                                                           col = "")
# Función de riesgo acumulado
ggplot() + geom_line(aes(time, -log(est), col = "Paramétrico (Weibull)"), data = flexg_lnorm) +
  geom_step(aes(time, -log(surv), col = "No-paramétrico"), data = non_par) +
  labs(x = "Tiempo (Días)", y = "Riesgo Acumulado", col = "Ajustes")


