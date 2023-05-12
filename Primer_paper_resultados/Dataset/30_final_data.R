
#######################
## 300_final_data.R  ##
#######################

source(here::here("Paper_resultados/Dataset", "20_input.R"))

# Base de datos de recepci?n (Base de datos vac?a)
final_df = data.frame(Caso = input$Caso,
                      t_0 = rep(NA,nrow(input)),
                      t_T = rep(NA,nrow(input)),
                      t_UCI = rep(NA, nrow(input)),
                      outcome = rep(NA,nrow(input)))

final_df$t_0 = ymd(final_df$t_0)
final_df$t_T = ymd(final_df$t_T)

# Extrarer t_0, t_T y variable "outcome"

for (i in 1:nrow(input)) {
  print(i)
  # seleccionar subset del caso
  df_1 = input[i,]
  
  # trasponer datos
  df_1_t = df_1[,-1]  %>% as.matrix()  %>% t()
  colnames(df_1_t) = "Date"
  df_1_t = as.data.frame(df_1_t)
  
  # llenar la inf. para el caso 
  final_df$t_0[which(final_df$Caso == as.numeric(df_1$Caso))] = min(df_1_t$Date, na.rm = T)
  final_df$t_T[which(final_df$Caso == as.numeric(df_1$Caso))] = max(df_1_t$Date, na.rm = T)
  
  # si min = max, entonces permanece en UCI
  if (final_df$t_0[which(final_df$Caso == as.numeric(df_1$Caso))] 
    == final_df$t_T[which(final_df$Caso == as.numeric(df_1$Caso))]) {
    final_df$t_T[which(final_df$Caso == as.numeric(df_1$Caso))] = NA
  }
  
  ######################
  ## Variable Outcome ##
  ######################
  # eliminar filas NA y no considerar Hospital_UCI
  df_2 = df_1_t
  df_2$Ubic = rownames(df_2)
  df_2 = df_2 %>% filter(Ubic != "Hospital_UCI")
  df_2 = na.omit(df_2)
  
  if (nrow(df_2)==0) {
    df_2 = data.frame(Date = NA, Ubic = "Censored")
  }
  if (df_2$Ubic == "Censored" || df_2$Ubic == "NO_COVID") {
    final_df$outcome[which(final_df$Caso == as.numeric(df_1$Caso))] = "Censored"
  } 
  if (df_2$Ubic %in% c("Casa", "Hospital", "Recuperado")) {
    final_df$outcome[which(final_df$Caso == as.numeric(df_1$Caso))] = "Alive"
  } 
  if (df_2$Ubic == "Fallecido") {
    final_df$outcome[which(final_df$Caso == as.numeric(df_1$Caso))] = "Dead"
  } 
}

# Extrarer t UCI (diferencia entre t_T y t_0). Fin de muestra = 2021-11-30
for (j in 1:nrow(final_df)) {
  # cambiar NA por el fin de la muestra
  if (is.na(final_df$t_T[j])) {
    final_df$t_T[j] = ymd("2021-11-30")
  }
  # calcular t (t_T - t_0)
  final_df$t_UCI[j] = difftime(final_df$t_T[j],final_df$t_0[j], units = "days")
}


# Forma final de la base de datos
write.csv(final_df, 
          "Paper_resultados/Dataset/Output/final_data.csv", 
          row.names = FALSE)

rm(df_1, df_1_t, df_1_t_2, df_1_t_3, df_2, ubic,
   j, k , y, z)



