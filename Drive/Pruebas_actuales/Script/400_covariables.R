

#######################
## 400_covariables.R ##
#######################

library(here)
source(here::here("Script/Script_subsets", "000_librerias.R"))
source(here::here("Script/Script_subsets", "100_subsets.R"))
source(here::here("Script/Script_subsets", "110_recodificar.R"))
source(here::here("Script/Script_subsets", "200_input.R"))
source(here::here("Script/Script_subsets", "300_final_data.R"))

# Nota A: por simplicidad, recurro a la base de datos de la fecha final
larger_dataset = readRDS(here::here("Resultados/04.01.23", "larger_dataset.RDS"))
# Nota B: el dataset fue previamente recodificado según la funcion_w_n

# recuperar sexo y edad
cov = larger_dataset[c("Caso", "Edad", "Sexo")]
final_df_w = merge(final_df_z, cov, by = "Caso", all.x=T, all.y = F)

# recodificar el sexo y convertir la variable en factor
final_df_w$Sexo <- gsub(pattern = "\\s*(f|F)+\\s*", 
               replacement = "1", final_df_w$Sexo)

final_df_w$Sexo <- gsub(pattern = "\\s*(m|M)+\\s*", 
               replacement = "0", final_df_w$Sexo)

final_df_w$Sexo = as.factor(as.character(final_df_w$Sexo))

# guardar final data con covariables
write.csv(final_df_w, 
          "Resultados/04.01.23/final_cov_040123.csv", 
          row.names = FALSE)





