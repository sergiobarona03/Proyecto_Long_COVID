
############################
## Selección de la        ##
##   distribución teórica ##
############################
source(here::here("Paper_resultados/Parametric_model/LoS/",
                  "B2_Selecting.R"))

# De acuerdo con AIC & BIC, se selecciona un modelo
# fundamentado en la distribución gamma generalizada



#######################
## Casos poblacional ##
#######################
parametric_los = fitdist(dataset$t_UCI_desc, "gengamma",
                         start = list(mu = 1, sigma = 1, Q = 1))
# Parámetros estimados
parametros_poblacion = data.frame(estimate = parametric_los$estimate, sd = parametric_los$sd)
writexl::write_xlsx(parametros_poblacion, 
                      "Paper_resultados/Parametric_model/LoS/Output/Model_selection/Total/estimate.xlsx")

# Resultados (mediana e IQR estimados con IC)
quantile_function = function(x){qgengamma(x, mu = parametric_los$estimate[1],
            sigma = parametric_los$estimate[2],
          Q = parametric_los$estimate[3])}

quantile = data.frame(variable = c("Q1", "Q2", "Q3"),
                      distribution = c(NA, NA, NA),
                      CI_1 = c(NA, NA, NA),
                      CI_2 = c(NA, NA, NA),
                      sample = c(NA, NA, NA))

# Quantile function
q = c(0.25,0.5,0.75)
for (k in 1:3) {
  quantile$distribution[k] = quantile_function(q[k])
  quantile$sample[k] = quantile(dataset$t_UCI_desc, q[k])
}

writexl::write_xlsx(quantile, 
                    "Paper_resultados/Parametric_model/LoS/Output/Model_selection/Total/summary.xlsx")
rm(quantile, parametric_los)






