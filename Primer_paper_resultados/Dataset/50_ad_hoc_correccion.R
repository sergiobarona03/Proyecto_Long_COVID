
##############################
## 50_Ad_hoc_correccion_T.R ##
##############################
library(here)
source(here::here("Paper_resultados/Dataset", "40_covariables.R"))

fallecidos = (final_df %>% filter(!is.na(death)))$Caso
final_df$t_T_corr = as.character(NA)

# Condicional (correccion de datos finales)
for (k in fallecidos) {
  x_1 = which(final_df$Caso == k)
  
  if ((final_df$death[x_1] <= final_df$t_T[x_1]) & 
      (final_df$death[x_1] > final_df$t_0[x_1])) {
    final_df$t_T_corr[x_1] = as.character(final_df$death[x_1])
  }else{final_df$t_T_corr[x_1] = as.character(final_df$t_T[x_1])}
}


# Corrección de la fecha de muerte
for (k in fallecidos) {
  x_1 = which(final_df$Caso == k)
  final_df$t_T[x_1] = final_df$t_T_corr[x_1]
}



# Considérese que "if Alive v Censored, then d = 0; otherwise, d = 1"
final_df$d = NA
for (i in 1:nrow(final_df)) {
  if (final_df$outcome[i] == "Alive" || final_df$outcome[i] == "Censored") {
    final_df$d[i] = 0
  } else {final_df$d[i] = 1}
}


# Mantener variables de interes
final_df = final_df[c("Caso", "sex", "age", "outcome", "t_0", "t_T", "t_UCI", "t_so", "d")]

# guardar final data con covariables
write.csv(final_df, 
          "Paper_resultados/Dataset/Output/cov_final_dataset.csv", 
          row.names = FALSE)

rm(dataset_na, cov, input, larger, cas, uci, fallecidos, i, j, k, x_1)


