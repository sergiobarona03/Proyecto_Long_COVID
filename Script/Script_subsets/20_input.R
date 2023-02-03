

##################
## 200_input.R  ##
##################

source(here::here("Script/Script_subsets", "11_recodificar.R"))

# Base de datos de recepción (base de datos vacía)

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
  
  # trasponer datos
  df_1_t = df_1[,-1]  %>% as.matrix()  %>% t()
  colnames(df_1_t) = "Date"
  df_1_t = as.data.frame(df_1_t)

  # convertir en variables fecha
  df_1_t = df_1_t %>%  mutate(Date = ymd(Date))
  
  # seleccionar el valor mínimo
  x = which(input$Caso == df_1$Caso)
  
  input$Hospital_UCI[x] = min(df_1_t$Date)
  
}

#################################
## Determinar el momento y el  ##
##    lugar de salida de UCI   ##
#################################


for (j in 1:nrow(input)) {
  
  print(j)

  df_1 = dataset_na[j,]
  
  # eliminar UCI como ubicacion
  drop = c(uci, "u_11", 
           "u_12", "u_13", 
           "u_14", "u_15")
  
  keep = setdiff(colnames(df_1), drop)
  df_1 = df_1[keep]
  
  #definir caso
  caso_x = as.numeric(df_1$Caso)
  
  # ubicación del caso en input
  x = which(input$Caso == caso_x)
  
  # seleccionarlo en input
  df_2 = input[x,]
  
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
  
  # seleccionar la fecha de salida de UCI (mínimo del subconjunto)
  
  if (nrow(df_1_t_2)==0) {
    z = NA
  } else {
    z = min(df_1_t_2$Date) 
  }
  
  # se responde a la pregunta por la ubicación en z
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
    input$Casa[x] = z
  }
  
  if (ubic == "h") {
    input$Hospital[x] = z
  }
  
  if (ubic == "f") {
    input$Fallecido[x] = z
  }
  
  if (ubic == "u") {
    input$Hospital_UCI[x] = z
  }
  
  if (ubic == "r") {
    input$Recuperado[x] = z
  }
  
  if (ubic == "n") {
    input$NO_COVID[x] = z
  }
  
  if (ubic == "x") {
    input[x,c(2,3,4,6,7)] = NA
  }
  
  if (is.na(ubic)) {
    input[x,c(2,3,4,6,7)] = NA
  }
  
  
}



