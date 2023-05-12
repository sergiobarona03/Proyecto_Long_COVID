
###############################
## 40_Recovering_variables.R ##
###############################
library(here)
source(here::here("Paper_resultados/Dataset", "30_final_data.R"))

# Seleccionar pacientes muertos
larger = readRDS("Paper_resultados/Dataset/Output/larger_dataset.RDS")

##############################
##############################
### Covariables: age & sex ###
##############################
##############################
cov = larger[c("Caso", "Edad", "Sexo")]
colnames(cov) = c("Caso", "age", "sex")
final_df = merge(final_df, cov, by = "Caso", all.x=T, all.y = F)

# recodificar el sexo y convertir la variable en factor
final_df$sex <- gsub(pattern = "\\s*(f|F)+\\s*", 
               replacement = "1", final_df$sex)
final_df$sex <- gsub(pattern = "\\s*(m|M)+\\s*", 
               replacement = "0", final_df$sex)
final_df$sex = as.factor(as.character(final_df$sex))


#####################################
#####################################
### Time from SO to ICU admission ###
#####################################
#####################################

casos_fallecidos = larger[c("Caso", "Inicio", "Muerte")]
casos_fallecidos$so =format(as.Date(casos_fallecidos$Inicio, format = "%d/%m/%Y"), "%Y-%m-%d")
casos_fallecidos$so = as.Date(casos_fallecidos$so)
casos_fallecidos$death = format(as.Date(casos_fallecidos$Muerte, format = "%d/%m/%Y"), "%Y-%m-%d")
casos_fallecidos$death = as.Date(casos_fallecidos$death)

final_df = merge(final_df, casos_fallecidos[c("Caso", "so", "death")], by = "Caso")

# Calcular: time from so to ICU admission
final_df$t_so = NA
for (j in 1:nrow(final_df)) {
  final_df$t_so[j] = difftime(as.character(final_df$t_0[j]),
                              as.character(final_df$so[j]), 
                                      units = "days")
}

rm(casos_fallecidos)



