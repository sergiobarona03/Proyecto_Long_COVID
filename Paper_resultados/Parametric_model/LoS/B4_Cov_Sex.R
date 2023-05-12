
######################
## Casos según sexo ##
######################
source(here::here("Paper_resultados/Parametric_model/LoS/",
                  "B3_Total.R"))

# Sexo == 0 (masculino)
dataset_male = dataset %>% filter(sex == 0)
parametric_male = fitdist(dataset_male$t_UCI_desc, "gengamma",
                          start = list(mu = 1, sigma = 1, Q = 1))
parametros_male = data.frame(estimate = parametric_male$estimate, sd = parametric_male$sd)
writexl::write_xlsx(parametros_male, 
                    "Paper_resultados/Parametric_model/LoS/Output/Model_selection/Sex/estimate_male.xlsx")

quantile_male = data.frame(variable = c("Q1", "Q2", "Q3"),
                           distribution = c(NA, NA, NA),
                           CI_1 = c(NA, NA, NA),
                           CI_2 = c(NA, NA, NA),
                           sample = c(NA, NA, NA))
quantile_function = function(x){qgengamma(x, mu = parametric_male$estimate[1],
                                          sigma = parametric_male$estimate[2],
                                          Q = parametric_male$estimate[3])}
q = c(0.25,0.5,0.75)
for (k in 1:3) {
  quantile_male$distribution[k] = quantile_function(q[k])
  quantile_male$sample[k] = quantile(dataset_male$t_UCI_desc, q[k])
}

writexl::write_xlsx(quantile_male, 
                    "Paper_resultados/Parametric_model/LoS/Output/Model_selection/Sex/summary_male.xlsx")
rm(quantile_male, parametric_male)

# Sexo == 1 (femenino)
dataset_female = dataset %>% filter(sex == 1)
parametric_female = fitdist(dataset_female$t_UCI_desc, "gengamma",
                            start = list(mu = 1, sigma = 1, Q = 1))
parametros_female = data.frame(estimate = parametric_female$estimate, sd = parametric_female$sd)
writexl::write_xlsx(parametros_female, 
                    "Paper_resultados/Parametric_model/LoS/Output/Model_selection/Sex/estimate_female.xlsx")

quantile_female = data.frame(variable = c("Q1", "Q2", "Q3"),
                             distribution = c(NA, NA, NA),
                             CI_1 = c(NA, NA, NA),
                             CI_2 = c(NA, NA, NA),
                             sample = c(NA, NA, NA))
quantile_function = function(x){qgengamma(x, mu = parametric_female$estimate[1],
                                          sigma = parametric_female$estimate[2],
                                          Q = parametric_female$estimate[3])}
q = c(0.25,0.5,0.75)
for (k in 1:3) {
  quantile_female$distribution[k] = quantile_function(q[k])
  quantile_female$sample[k] = quantile(dataset_female$t_UCI_desc, q[k])
}

writexl::write_xlsx(quantile_female, 
                    "Paper_resultados/Parametric_model/LoS/Output/Model_selection/Sex/summary_female.xlsx")
rm(quantile_female, parametric_female)

# Prueba de Kolmogorov-Smirnov
ks_test = data.frame(variable = c("Sex","Age", "CCI"),
                     D= c(NA, NA, NA), p_value = c(NA, NA, NA))

ks_sex = ks.test(dataset_male$t_UCI_desc, dataset_female$t_UCI_desc)
ks_test$D[1] = ks_sex$statistic
ks_test$p_value[1] = ks_sex$p.value


