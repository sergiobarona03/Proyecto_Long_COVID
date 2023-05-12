######################
## Casos según edad ##
######################
source(here::here("Paper_resultados/Parametric_model/LoS/",
                  "B4_Cov_Sex.R"))

dataset = dataset %>% mutate(group = cut(age, 
                                               breaks = c(18, 65, Inf),
                                               include.lowest = TRUE,
                                               right = FALSE))
# age1 == [18,65) 
dataset_g1 = dataset %>% filter(group == "[18,65)")
parametric_g1 = fitdist(dataset_g1$t_UCI_desc, "gengamma.orig",
                        start = list(shape = 1, scale = 0.1, k = 1))
parametros_g1 = data.frame(estimate = parametric_g1$estimate,
                             sd = parametric_g1$sd)

writexl::write_xlsx(parametros_g1, 
                    "Paper_resultados/Parametric_model/LoS/Output/Model_selection/Age/estimate_g1.xlsx")

quantile_g1 = data.frame(variable = c("Q1", "Q2", "Q3"),
                           distribution = c(NA, NA, NA),
                           CI_1 = c(NA, NA, NA),
                           CI_2 = c(NA, NA, NA),
                           sample = c(NA, NA, NA))
quantile_function = function(x){qgengamma.orig(x, shape = parametric_g1$estimate[1],
                                          scale = parametric_g1$estimate[2],
                                          k = parametric_g1$estimate[3])}
q = c(0.25,0.5,0.75)
for (k in 1:3) {
  quantile_g1$distribution[k] = quantile_function(q[k])
  quantile_g1$sample[k] = quantile(dataset_g1$t_UCI_desc, q[k])
}

writexl::write_xlsx(quantile_g1, 
                    "Paper_resultados/Parametric_model/LoS/Output/Model_selection/Age/summary_g1.xlsx")
rm(quantile_g1, parametric_g1)

# age2 == [65,Inf) 
dataset_g2 = dataset %>% filter(group == "[65,Inf]")
parametric_g2 = fitdist(dataset_g2$t_UCI_desc, "gengamma.orig",
                        start = list(shape = 1, scale = 0.1, k = 1))
parametros_g2 = data.frame(estimate = parametric_g2$estimate,
                           sd = parametric_g2$sd)

writexl::write_xlsx(parametros_g2, 
                    "Paper_resultados/Parametric_model/LoS/Output/Model_selection/Age/estimate_g2.xlsx")

quantile_g2 = data.frame(variable = c("Q1", "Q2", "Q3"),
                         distribution = c(NA, NA, NA),
                         CI_1 = c(NA, NA, NA),
                         CI_2 = c(NA, NA, NA),
                         sample = c(NA, NA, NA))
quantile_function = function(x){qgengamma.orig(x, shape = parametric_g2$estimate[1],
                                          scale = parametric_g2$estimate[2],
                                          k = parametric_g2$estimate[3])}
q = c(0.25,0.5,0.75)
for (k in 1:3) {
  quantile_g2$distribution[k] = quantile_function(q[k])
  quantile_g2$sample[k] = quantile(dataset_g2$t_UCI_desc, q[k])
}


writexl::write_xlsx(quantile_g2, 
                    "Paper_resultados/Parametric_model/LoS/Output/Model_selection/Age/summary_g2.xlsx")
rm(quantile_g2, parametric_g2)

# Prueba de Kolmogorov-Smirnov
ks_age = ks.test(dataset_g1$t_UCI_desc, dataset_g2$t_UCI_desc)
ks_test$D[2] = ks_age$statistic
ks_test$p_value[2] = ks_age$p.value

