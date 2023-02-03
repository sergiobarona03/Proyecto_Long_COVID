
#######################
## 300_final_data.R  ##
#######################

# Base de datos de recepción (Base de datos vacía)

final_df = data.frame(Caso = input$Caso,
                      t_0 = rep(NA,nrow(input)),
                      t_T = rep(NA,nrow(input)),
                      t = rep(NA, nrow(input)),
                      d = rep(NA,nrow(input)))

final_df$t_0 = ymd(final_df$t_0)
final_df$t_T = ymd(final_df$t_T)

# Extrarer t_0, t_T e información de censura

for (i in 1:nrow(input)) {
  
  print(i)
  
  # seleccionar subset del caso
  df_1 = input[i,]
  
  #número de fila para el caso
  x = which(final_df$Caso == as.numeric(df_1$Caso))
  
  # trasponer datos
  df_1_t = df_1[,-1]  %>% as.matrix()  %>% t()
  colnames(df_1_t) = "Date"
  df_1_t = as.data.frame(df_1_t)
  
  # llenar la información para el caso x

  final_df$t_0[x] = min(df_1_t$Date, na.rm = T)
  final_df$t_T[x] = max(df_1_t$Date, na.rm = T)
  
  # si min = max, entonces permanece en UCI
  
  if (final_df$t_0[x] == final_df$t_T[x]) {
    final_df$t_T[x] = NA
  }
  
  ######################################################
  ## Llenar la información para la variable censurada ##
  ######################################################
  
  # los datos censurados corresponden a censura por derecha
  
  # eliminar filas NA y no considerar Hospital_UCI
  df_2 = df_1_t
  df_2$Ubic = rownames(df_2)
  df_2 = df_2 %>% filter(Ubic != "Hospital_UCI")
  df_2 = na.omit(df_2)
  
  if ({"NO_COVID" %in% df_2$Ubic} || nrow(df_2) == 0) {
    final_df$d[x] = 0
  } else {
    final_df$d[x] = 1
  }
  
  
}


# Extrarer t (diferencia entre t_T y t_0). Fin de muestra = 2021-11-30

for (j in 1:nrow(final_df)) {
  print(j)
  # cambiar NA por el fin de la muestra
  if (is.na(final_df$t_T[j])) {
    final_df$t_T[j] = ymd("2021-11-30")
  }
  
  # calcular t (t_T - t_0)
  final_df$t[j] = difftime(final_df$t_T[j],final_df$t_0[j], units = "days")
  
}



# Forma final de la base de datos
final_df_z = final_df[c("Caso", "t", "d")]

write.csv(final_df_z, 
          "Resultados/04.01.23/final_data_040123.csv", 
          row.names = FALSE)







