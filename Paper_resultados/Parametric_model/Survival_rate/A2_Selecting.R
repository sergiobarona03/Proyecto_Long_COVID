
######################################
## Selección del modelo paramétrico ##
######################################

source(here::here("Paper_resultados/Descriptive_analysis/05_Chi_2_Fisher/",
                  "Fisher_test.R"))

################################################
## Comparación de las tasas de supervivencia  ##
##    (Salida de los modelos paramétricos)    ##
################################################
# gamma
gamma_model = flexg_gamma[c("time", "est")]
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
## (Criterio de información de Akaike)  ##
##########################################

dist <- c("weibull", "lnorm", "GenGamma.orig", "llogis", "gompertz", "exp") 

data.Surv <- Surv(dataset$t_UCI, dataset$d)
modelo_general <- sapply(dist, function(x) flexsurvreg(data.Surv ~ 1, dist = x), USE.NAMES = T, simplify = F)

plot(modelo_general[[1]], ci = F, conf.int = F, lty = 2, 
     main = "Figura 28: ajuste Paramétrico",
     xlab = "Tiempo", ylab = "Probabilidad de Supervivencia")
for (i in 2:length(dist)) plot(modelo_general[[i]], ci = F, conf.int = F, add = T, col = i + 1, lty = i)
legend("bottomright", c("Kaplan-Meier", "Weibull", "Log-Normal","Gamma generalizada",
                        "Log-logística", "Gompertz", "Exponencial"), lty = 1:(length(dist) + 1), col = 1:(length(dist) + 1))




data_surv <- Surv(dataset$t_UCI, dataset$d)
modelo <- sapply(dist, 
                 function(x) flexsurvreg(data_surv ~ 1, data = dataset, dist = x), 
                 USE.NAMES = T, simplify = F)

IC_model <- sapply(modelo, function(x) c(AIC = AIC(x), BIC = BIC(x), LogLik = logLik(x)), simplify = T)
IC_model[, order(IC_model["AIC", ])]

kable(IC_model, caption = "Tabla 13: Criterios de información", col.names = c("Gamma generalizada", "Weibull", "Gompertz", "Log-Normal", "Log-Logística"))








