
# Recovering variables: time from SO to ICU admission and boolean variable
setwd("D:\\Desktop\\Proyecto_Long_COVID\\")
larger = readRDS("Resultados\\04.01.23\\larger_dataset.RDS")

final_data = read.csv("Resultados\\04.01.23\\complete_final_data_040123.csv")


# Casos fallecidos (boolean variable) y Time from SO to ICU admission

larger <- larger %>% mutate_all(na_if,"")
casos_fallecidos = larger 
casos_fallecidos = casos_fallecidos[c("Caso", "Inicio", "Muerte")]

final_data_merge = merge(final_data, casos_fallecidos, by = "Caso")
final_data_merge = final_data_merge[c("Caso", "Inicio", "t_0", "t_T", "Muerte", "t", "d")]

final_data_merge$OS = format(as.Date(final_data_merge$Inicio, format = "%d/%m/%Y"), "%Y-%m-%d")
final_data_merge$Death = format(as.Date(final_data_merge$Muerte, format = "%d/%m/%Y"), "%Y-%m-%d")

fallecidos = (final_data_merge %>% filter(!is.na(Death)))$Caso

# Condicional (corrección de datos finales)
for (k in fallecidos) {
  x_1 = which(final_data_merge$Caso == k)
  
  if ((final_data_merge$Death[x_1] <= final_data_merge$t_T[x_1]) & 
      (final_data_merge$Death[x_1] > final_data_merge$t_0[x_1])) {
    final_data_merge$t_T_corr[x_1] = final_data_merge$Death[x_1]
  }else{final_data_merge$t_T_corr[x_1] = final_data_merge$t_T[x_1]}
}


# Datos con correciones en la observaciones
final_data_merge = final_data_merge[c("Caso", "t_0", "t_T_corr", "OS", "Death", "d")]

# Calcular longitudes
final_data_merge$t = NA
final_data_merge$t_OS = NA

for (j in 1:nrow(final_data_merge)) {

  final_data_merge$t[j] = difftime(final_data_merge$t_T_corr[j],final_data_merge$t_0[j], 
                                   units = "days")
  final_data_merge$t_OS[j] = difftime(final_data_merge$t_0[j],final_data_merge$OS[j], 
                                    units = "days")
    
}

final_data_merge$death_binaria = NA

for (j in fallecidos) {
  x_1 = which(final_data_merge$Caso == j)
  
  if (!is.na(final_data_merge$Death[x_1]) &
      (final_data_merge$Death[x_1] <= final_data_merge$t_T_corr[x_1]) & 
      (final_data_merge$Death[x_1] > final_data_merge$t_0[x_1])) {
    final_data_merge$death_binaria[x_1] = 1
  }else{
    final_data_merge$death_binaria[x_1] = 0
  }
  
}

final_data_merge$death_binaria[is.na(final_data_merge$death_binaria)] = 0


setwd("D:\\Desktop\\Proyecto_Long_COVID\\")
write.csv(final_data_merge, "Resultados\\04.01.23\\final_dataset_corr.csv", row.names = FALSE)



