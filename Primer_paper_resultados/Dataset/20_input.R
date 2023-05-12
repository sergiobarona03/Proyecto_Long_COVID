

##################
## 20_input.R  ##
##################

source(here::here("Paper_resultados/Dataset", "11_recodificar.R"))

# Base de datos de recepci?n (base de datos vacia)
input = data.frame(Caso = dataset_na$Caso,
                   Casa = rep(NA, nrow(dataset_na)),
                   Hospital = rep(NA, nrow(dataset_na)),
                   Fallecido = rep(NA, nrow(dataset_na)),
                   Hospital_UCI = rep(NA, nrow(dataset_na)),
                   Recuperado = rep(NA, nrow(dataset_na)),
                   NO_COVID = rep(NA, nrow(dataset_na)))

input = input %>%  mutate(Casa = ymd(Casa))
input = input %>%  mutate(Hospital = ymd(Hospital))
input = input %>%  mutate(Fallecido = ymd(Fallecido))
input = input %>%  mutate(Hospital_UCI = ymd(Hospital_UCI))
input = input %>%  mutate(Recuperado = ymd(Recuperado))
input = input %>%  mutate(NO_COVID = ymd(NO_COVID))

##############################################
## Determinar el momento de ingreso a UCI   ##
##############################################

for (k in 1:nrow(input)) {
  print(k)
  df_1 = dataset_na[k,]
  df_1 = df_1[c("Caso", uci, "u_11", 
                 "u_12", "u_13", 
                 "u_14", "u_15")]
  
  #eliminar columnas NA
  df_1 = df_1[, colSums(is.na(df_1)) != nrow(df_1)]
  colnames(df_1) = c("Caso", "Date")
 
  # seleccionar la fecha de ingreso a UCI
  input$Hospital_UCI[which(input$Caso == df_1$Caso)] = df_1$Date
}

#################################
## Determinar el momento y el  ##
##    lugar de salida de UCI   ##
#################################

for (j in 1:nrow(input)) {
  print(j)
  df_1 = dataset_na[j,]
  # eliminar UCI como ubicacion
  df_1 = df_1[setdiff(colnames(df_1), c(uci, "u_11", 
                                        "u_12", "u_13", 
                                        "u_14", "u_15"))]
  # seleccionarlo en input
  df_2 = input[which(input$Caso == as.numeric(df_1$Caso)),]
  # fecha de ingreso a UCI
  y = ymd(df_2$Hospital_UCI)
  
  #eliminar columnas NA
  df_1 = df_1[, colSums(is.na(df_1)) != nrow(df_1), drop = FALSE]
  
  # trasponer datos
  if (ncol(df_1) == 1) {
    df_1_t = data.frame(Caso = df_1$Caso, x_16 = NA)[,-1,drop = FALSE] %>% as.matrix()  %>% t()
  } else {
    df_1_t = df_1[,-1, drop = FALSE]  %>% as.matrix()  %>% t()
  }
  
  colnames(df_1_t) = "Date"
  df_1_t = as.data.frame(df_1_t)
  
  # seleccionar las fechas superiores a la fecha de ingreso a UCI
  df_1_t_2 = df_1_t %>% filter(Date > y)
  
  # seleccionar la fecha de salida de UCI (min. del subconjunto)
  
  if (nrow(df_1_t_2)==0) {
    z = NA
  } else {
    z = min(df_1_t_2$Date) 
  }
  
  # ¿cuál es la ubicación en z?
  
  if (nrow(df_1_t_2)==0) {
    df_1_t_3 = df_1_t
    ubic = substring(rownames(df_1_t_3),1,1)
    ubic = unique(ubic)
  } else {
    df_1_t_3 = df_1_t_2 %>% filter(Date == z)
    ubic = substring(rownames(df_1_t_3),1,1)
    ubic = unique(ubic)
  }
  
  
  # ubicarlo en input
  if (ubic == "c") {
    input$Casa[which(input$Caso == as.numeric(df_1$Caso))] = z
  }
  
  if (ubic == "h") {
    input$Hospital[which(input$Caso == as.numeric(df_1$Caso))] = z
  }
  
  if (ubic == "f") {
    input$Fallecido[which(input$Caso == as.numeric(df_1$Caso))] = z
  }
  
  if (ubic == "u") {
    input$Hospital_UCI[which(input$Caso == as.numeric(df_1$Caso))] = z
  }
  
  if (ubic == "r") {
    input$Recuperado[which(input$Caso == as.numeric(df_1$Caso))] = z
  }
  
  if (ubic == "n") {
    input$NO_COVID[which(input$Caso == as.numeric(df_1$Caso))] = z
  }
  
  if (ubic == "x") {
    input[which(input$Caso == as.numeric(df_1$Caso)),c(2,3,4,6,7)] = NA
  }
  
}



