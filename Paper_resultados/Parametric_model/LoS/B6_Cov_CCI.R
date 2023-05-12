
######################
## Casos según CCI  ##
######################
source(here::here("Paper_resultados/Parametric_model/LoS/",
                  "B4_Cov_Sex.R"))

# group1 = CCI
dataset_CCI = dataset %>% filter(CCI == "CCI")
parametric_CCI = fitdist(dataset_CCI$t_UCI_desc, "gengamma",
                        start = list(mu = 0.1, sigma = 0.1, Q = 0.1))
parametros_CCI = data.frame(estimate = parametric_CCI$estimate,
                           sd = parametric_CCI$sd)

writexl::write_xlsx(parametros_CCI, 
                    "Paper_resultados/Parametric_model/LoS/Output/Model_selection/CCI/estimate_CCI.xlsx")

quantile_CCI = data.frame(variable = c("Q1", "Q2", "Q3"),
                         distribution = c(NA, NA, NA),
                         CI_1 = c(NA, NA, NA),
                         CI_2 = c(NA, NA, NA),
                         sample = c(NA, NA, NA))
quantile_function = function(x){qgengamma(x, mu = parametric_CCI$estimate[1],
                                          sigma = parametric_CCI$estimate[2],
                                          Q = parametric_CCI$estimate[3])}
q = c(0.25,0.5,0.75)
for (k in 1:3) {
  quantile_CCI$distribution[k] = quantile_function(q[k])
  quantile_CCI$sample[k] = quantile(dataset_CCI$t_UCI_desc, q[k])
}

writexl::write_xlsx(quantile_CCI, 
                    "Paper_resultados/Parametric_model/LoS/Output/Model_selection/CCI/summary_CCI.xlsx")
rm(quantile_CCI, parametric_CCI)

# group2 = Non-CCI
dataset_non_CCI = dataset %>% filter(CCI == "non-CCI")
parametric_non_CCI = fitdist(dataset_non_CCI$t_UCI_desc, "gengamma",
                         start = list(mu = 0.1, sigma = 0.1, Q = 0.1))
parametros_non_CCI = data.frame(estimate = parametric_non_CCI$estimate,
                            sd = parametric_non_CCI$sd)

writexl::write_xlsx(parametros_non_CCI, 
                    "Paper_resultados/Parametric_model/LoS/Output/Model_selection/CCI/estimate_non_CCI.xlsx")

quantile_non_CCI = data.frame(variable = c("Q1", "Q2", "Q3"),
                          distribution = c(NA, NA, NA),
                          CI_1 = c(NA, NA, NA),
                          CI_2 = c(NA, NA, NA),
                          sample = c(NA, NA, NA))
quantile_function = function(x){qgengamma(x, mu = parametric_non_CCI$estimate[1],
                                          sigma = parametric_non_CCI$estimate[2],
                                          Q = parametric_non_CCI$estimate[3])}
q = c(0.25,0.5,0.75)
for (k in 1:3) {
  quantile_non_CCI$distribution[k] = quantile_function(q[k])
  quantile_non_CCI$sample[k] = quantile(dataset_non_CCI$t_UCI_desc, q[k])
}

writexl::write_xlsx(quantile_non_CCI, 
                    "Paper_resultados/Parametric_model/LoS/Output/Model_selection/CCI/summary_non_CCI.xlsx")
rm(quantile_non_CCI, parametric_non_CCI)

# Kolmogorov-Smirnov test
ks_CCI = ks.test(dataset_CCI$t_UCI_desc, dataset_non_CCI$t_UCI_desc)
ks_test$D[3] = ks_CCI$statistic
ks_test$p_value[3] = ks_CCI$p.value

writexl::write_xlsx(ks_test, 
                    "Paper_resultados/Parametric_model/LoS/Output/Model_selection/Kolmogorov_Smirnov_test.xlsx")
