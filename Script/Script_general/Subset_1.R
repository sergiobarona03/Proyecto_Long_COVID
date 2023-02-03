#######################################
#######################################
## CONSTRUCCION DE LA BASE DE DATOS  ##
##        PARA SUBSET_1              ##
##    [2020-11-30: 2021-03-31]       ##
#######################################
#######################################

#El preambulo
install.packages("readxl")
install.packages("xts")
install.packages("plyr")
install.packages("dplyr")
install.packages("archive")
install.packages("data.table")
install.packages("lubridate")

library(readxl)
library(xts)
library(plyr)
library(dplyr)
library(archive)
library(data.table)
library(lubridate)

############################
### BUCLE PARA CARGAR ######
###    ARCHIVOS       ######
############################

#Definir el periodo
t_0 <- as.Date("2020-11-30")  
t_T <- as.Date("2021-03-31")  
fechas <- seq(from=t_0, to=t_T, by=1)
fechas <- fechas[-which(fechas == "2020-12-31")]
fechas <- fechas[-which(fechas == "2021-01-04")]

N <- length(fechas)

#Definir los formatos diferenciados
zip_files = c(as.Date("2020-11-30"), 
              as.Date("2021-02-11"), as.Date("2021-08-23"), as.Date("2021-09-13"), 
              seq(from=as.Date("2021-09-14"), to=as.Date("2021-09-19"), by=1), 
              "2021-10-31", seq(from=as.Date("2021-10-04"), to=as.Date("2021-10-18"), by=1),
              seq(from=as.Date("2021-10-25"), to=as.Date("2021-10-30"), by=1), 
              "2021-11-01", "2021-11-07", 
              seq(from=as.Date("2021-11-16"), to=as.Date("2021-11-21"), by=1), 
              seq(from=as.Date("2021-11-23"), to=as.Date("2021-11-26"), by=1), 
              seq(from=as.Date("2021-11-29"), to=as.Date("2021-11-30"), by=1)
)

format = vector()

for (k in 1:length(fechas)){
  if(fechas[k]== zip_files[1] ||
     fechas[k]== zip_files[2] ||
     fechas[k]== zip_files[3] ||
     fechas[k]== zip_files[4] ||
     fechas[k]== zip_files[5] ||
     fechas[k]== zip_files[6] ||
     fechas[k]== zip_files[7] ||
     fechas[k]== zip_files[8] ||
     fechas[k]== zip_files[9] ||
     fechas[k]== zip_files[10] ||
     fechas[k]== zip_files[11] ||
     fechas[k]== zip_files[12] ||
     fechas[k]== zip_files[13] ||
     fechas[k]== zip_files[14] ||
     fechas[k]== zip_files[15] ||
     fechas[k]== zip_files[16] ||
     fechas[k]== zip_files[17] ||
     fechas[k]== zip_files[18] ||
     fechas[k]== zip_files[19] ||
     fechas[k]== zip_files[20] ||
     fechas[k]== zip_files[21] ||
     fechas[k]== zip_files[22] ||
     fechas[k]== zip_files[23] ||
     fechas[k]== zip_files[24] ||
     fechas[k]== zip_files[25] ||
     fechas[k]== zip_files[26] ||
     fechas[k]== zip_files[27] ||
     fechas[k]== zip_files[28] ||
     fechas[k]== zip_files[29] ||
     fechas[k]== zip_files[30] ||
     fechas[k]== zip_files[31] ||
     fechas[k]== zip_files[32] ||
     fechas[k]== zip_files[33] ||
     fechas[k]== zip_files[34] ||
     fechas[k]== zip_files[35] ||
     fechas[k]== zip_files[36] ||
     fechas[k]== zip_files[37] ||
     fechas[k]== zip_files[38] ||
     fechas[k]== zip_files[39] ||
     fechas[k]== zip_files[40] ||
     fechas[k]== zip_files[41] ||
     fechas[k]== zip_files[42] ||
     fechas[k]== zip_files[43] ||
     fechas[k]== zip_files[44] ||
     fechas[k]== zip_files[45] ||
     fechas[k]== zip_files[46]   
  )
  {format[k] = paste0(fechas[k], ".zip")}
  else
  {format[k] = paste0(fechas[k], ".rar")}
}

#El bucle para descagar archivos es el siguiente:
destfile <- vector()
URL <- vector()
for (j in 1:N)
{
  URL[[j]] <- paste0("https://www.ins.gov.co/BoletinesCasosCOVID19Colombia/",format[j])
  destfile [[j]] =paste0("D:\\Desktop\\Subset_5\\",format[j])
  #MacOS:
  #download.file(URL[j],destfile[j])
  #Windows, Linux:
  #download.file(URL[j],destfile[j], method="wininet", mode="wb")
}

###############################
#### TRATAMIENTO PARA LA ######
#### BASE DE DATOS MADRE ######
###############################

#Definicion de la funcion
#Definicion de las funciones principales
funcion_w_n = function(a){
  dataset_0 = data.frame()
  colnames(a) <- c("Fecha_reporte", "Caso", "Not", "Cod_Dep", "Nom_Dep", "Cod_Mun",  
                   "Nom_Mun", "Edad", "Unidad", "Sexo", "Fuente", "Ubic", "Estado", 
                   "Pais_cod", "Pais_nom", "Dic_Recup",
                   "Inicio", "Muerte", "Diagnos", "Recup", "Tipo", "Pert_Etn", "Nom_Etn")
  a <- a[,-1]
  a$Caso = as.character(a$Caso)
  a$Caso = as.numeric(gsub(",", ".", a$Caso))
  a$Sexo <- as.factor(a$Sexo)
  #a$Not <- as.Date(as.POSIXct(a$Not, "UTC"))
  a$Cod_Mun <- as.numeric(a$Cod_Mun)
  a$Fuente <- as.factor(a$Fuente)
  
  a$Ubic <- as.factor(a$Ubic)
  a$Ubic <- as.character(a$Ubic)
  
  a$Pais_nom <- as.character(a$Pais_nom)
  #a$Muerte <- as.Date(as.POSIXct(a$Muerte, "UTC"))
  #a$Diagnos <- as.Date(as.POSIXct(a$Diagnos, "UTC"))
  #a$Recup <- as.Date(as.POSIXct(a$Recup, "UTC"))
  #a$Cargue <- as.Date(as.POSIXct(a$Cargue, "UTC"))
  a$Estado <- as.factor(a$Estado)
  
  a$Fuente <- revalue(a$Fuente, c("EN ESTUDIO" = "En estudio",
                                  "Relacionado" = "Relacionado"))
  
  a$Estado <- revalue(a$Estado, c("Asintomático = Asintomático", "Fallecido" = "Fallecido",
                                  "Grave" = "Grave", "Leve" = "Leve", "Moderado" = "Moderado",
                                  "N/A" = NA))
  a <- a[order(a$Caso),]
  a <- a[a$Cod_Mun == 76001, ]
  dataset_0 = a
  return(dataset_0)
}

#extraccion y lectura de master_dataset
within_archive = vector()
for (m in 1:length(fechas)){
  if(fechas[m]== zip_files[1] ||
     fechas[m]== zip_files[2] 
  )
  {within_archive[m] = paste0(fechas[m], ".csv")}
  else
  {within_archive[m] = "Salida_Datos_Abiertos.csv"}
}

within_archive[which(fechas=="2021-01-05")] = "Salida_Datos_Abiertos.CSV"
within_archive[which(fechas=="2020-11-30")] = "2020-11-30.CSV"

temp_dir_0 = setwd(tempdir())
large_dataset = funcion_w_n(data.frame(fread(archive_extract(archive = destfile[N], 
                                                             dir = temp_dir_0, 
                                                             files = within_archive[N]))))

funcion_x = function(a){
  dataset_0 = data.frame()
  colnames(a) <- c("Fecha_reporte", "Caso", "Not", "Cod_Dep", "Nom_Dep", "Cod_Mun",  
                   "Nom_Mun", "Edad", "Unidad", "Sexo", "Fuente", "Ubic", "Estado", 
                   "Pais_cod", "Pais_nom", "Dic_Recup",
                   "Inicio", "Muerte", "Diagnos", "Recup", "Tipo", "Pert_Etn", "Nom_Etn")
  a <- a[,-1]
  a$Caso = as.character(a$Caso)
  a$Caso = as.numeric(gsub(",", ".", a$Caso))
  a$Sexo <- as.factor(a$Sexo)
  #a$Not <- as.Date(as.POSIXct(a$Not, "UTC"))
  a$Cod_Mun <- as.numeric(a$Cod_Mun)
  a$Fuente <- as.factor(a$Fuente)
  
  a$Ubic <- as.character(a$Ubic)
  
  a$Pais_nom <- as.character(a$Pais_nom)
  #a$Muerte <- as.Date(as.POSIXct(a$Muerte, "UTC"))
  #a$Diagnos <- as.Date(as.POSIXct(a$Diagnos, "UTC"))
  #a$Recup <- as.Date(as.POSIXct(a$Recup, "UTC"))
  #a$Cargue <- as.Date(as.POSIXct(a$Cargue, "UTC"))
  a$Estado <- as.factor(a$Estado)
  
  a$Fuente <- revalue(a$Fuente, c("EN ESTUDIO" = "En estudio",
                                  "Relacionado" = "Relacionado"))
  
  a$Estado <- revalue(a$Estado, c("Asintomático = Asintomático", "Fallecido" = "Fallecido",
                                  "Grave" = "Grave", "Leve" = "Leve", "Moderado" = "Moderado",
                                  "N/A" = NA))
  
  a <- a[order(a$Caso),]
  n_f = length(a$Caso)
  filter_vec = large_dataset$Caso[large_dataset$Caso <= a$Caso[n_f]]
  a = a[a$Caso %in% filter_vec, ]
  dataset_0 = a
  return(dataset_0)
}

funcion_y = function(a){
  dataset_x = data.frame()
  a$Ubic <- as.character(a$Ubic)
  
  a$Ubic <- gsub(pattern = "\\s*(c|C)+(a|A|)+(s|S|)+(a|A)+\\s*", 
                 replacement = "Casa", a$Ubic)
  a$Ubic <- gsub(pattern = "\\s*(f|F)+(a|A|)+(l|L|)+(e|E|)+(c|C|)+(i|I|)+(d|D|)+(o|O)+\\s*", 
                 replacement = "Fallecido", a$Ubic)
  a$Ubic <- gsub(pattern = "\\s*(h|H)+(o|O|)+(s|S|)+(p|P|)+(i|I|)+(t|T|)+(a|A|)+(l|L)+\\s*", 
                 replacement = "Hospital", a$Ubic)
  a$Ubic <- gsub(pattern = "\\s*(h|H)+(o|O|)+(s|S|)+(p|P|)+(i|I|)+(t|T|)+(a|A|)+(l|L|).(u|U)+(c|C|)+(i|I|)+\\s*", 
                 replacement = "Hospital_UCI", a$Ubic)
  a$Ubic <- gsub(pattern = "\\s*(r|R)+(e|E|)+(c|C|)+(u|U|)+(p|P|)+(e|E|)+(r|R|)+(a|A|)+(d|D|)+(o|O)+\\s*", 
                 replacement = "Recuperado", a$Ubic)
  a["Ubic"][is.na(a["Ubic"])] <- "OMITIR"
  a$Ubic <- gsub(pattern = "\\s*(N|n)+(.*)+(A|a)+\\s*", 
                 replacement = "NO_COVID", a$Ubic)
  
  a$Dic_Recup <- as.character(a$Dic_Recup)
  a$Dic_Recup <- gsub(pattern = "\\s*(f|F)+(a|A|)+(l|L|)+(e|E|)+(c|C|)+(i|I|)+(d|D|)+(o|O)+\\s*", 
                      replacement = "Fallecido", a$Dic_Recup)
  a$Dic_Recup <- gsub(pattern = "\\s*(r|R)+(e|E|)+(c|C|)+(u|U|)+(p|P|)+(e|E|)+(r|R|)+(a|A|)+(d|D|)+(o|O)+\\s*", 
                      replacement = "Recuperado", a$Dic_Recup)
  dataset_x = a
  return(dataset_x)
}

first_dataset <- funcion_x(data.frame(fread(archive_extract(archive = destfile[1], 
                                                            dir = temp_dir_0, files = within_archive[1]))))
first_dataset <- funcion_y(first_dataset)


#Construir dataset vacio
Caso <- large_dataset$Caso
Casa <- rep(NA, length(large_dataset$Caso))
Casa <- as.Date(as.POSIXct(Casa, "UTC"))
Hospital <- rep(NA, length(large_dataset$Caso))
Hospital <- as.Date(as.POSIXct(Hospital, "UTC"))
Fallecido <- rep(NA, length(large_dataset$Caso))
Fallecido <- as.Date(as.POSIXct(Fallecido, "UTC"))
Hospital_UCI <- rep(NA, length(large_dataset$Caso))
Hospital_UCI <- as.Date(as.POSIXct(Hospital_UCI, "UTC"))
Recuperado <- rep(NA, length(large_dataset$Caso))
Recuperado <- as.Date(as.POSIXct(Recuperado, "UTC"))
NO_COVID <- rep(NA, length(large_dataset$Caso))
NO_COVID <- as.Date(as.POSIXct(NO_COVID, "UTC"))
dataframe_test <- data.frame(Caso, Casa, Hospital, Fallecido, Hospital_UCI, 
                             Recuperado, NO_COVID)
rm(Caso, Casa, Hospital, Fallecido, Hospital_UCI, 
   Recuperado, NO_COVID)


#Primeras observaciones
for(j in 1:length(first_dataset$Caso)){
  if(first_dataset[j, "Caso"]==large_dataset[j, "Caso"] & 
     first_dataset[j, "Ubic"]=="Casa" & 
     first_dataset[j, "Dic_Recup"] != "Recuperado")
  {dataframe_test[j, "Casa"] = fechas[1]}
  
  if(first_dataset[j, "Caso"]==large_dataset[j, "Caso"] & 
     first_dataset[j, "Ubic"]=="Casa" & 
     first_dataset[j, "Dic_Recup"] == "Recuperado")
  {dataframe_test[j, "Recuperado"] = fechas[1]}
  
  if(first_dataset[j, "Caso"]==large_dataset[j, "Caso"] & 
     first_dataset[j, "Ubic"]=="Fallecido")
  {dataframe_test[j, "Fallecido"] = fechas[1]}
  
  if(first_dataset[j, "Caso"]==large_dataset[j, "Caso"] & 
     first_dataset[j, "Ubic"]=="Hospital" & 
     first_dataset[j, "Dic_Recup"] != "Recuperado")
  {dataframe_test[j, "Hospital"] = fechas[1]}
  
  if(first_dataset[j, "Caso"]==large_dataset[j, "Caso"] & 
     first_dataset[j, "Ubic"]=="Hospital" & 
     first_dataset[j, "Dic_Recup"] == "Recuperado")
  {dataframe_test[j, "Recuperado"] = fechas[1]}
  
  if(first_dataset[j, "Caso"]==large_dataset[j, "Caso"] & 
     first_dataset[j, "Ubic"]=="Hospital_UCI" & 
     first_dataset[j, "Dic_Recup"] != "Recuperado")
  {dataframe_test[j, "Hospital_UCI"] = fechas[1]}
  
  if(first_dataset[j, "Caso"]==large_dataset[j, "Caso"] & 
     first_dataset[j, "Ubic"]=="Hospital_UCI" & 
     first_dataset[j, "Dic_Recup"] == "Recuperado")
  {dataframe_test[j, "Recuperado"] = fechas[1]}
  
  if(first_dataset[j, "Caso"]==large_dataset[j, "Caso"] & 
     first_dataset[j, "Ubic"]=="Recuperado")
  {dataframe_test[j, "Recuperado"] = fechas[1]} 
  
  if(first_dataset[j, "Caso"]==large_dataset[j, "Caso"] & 
     first_dataset[j, "Ubic"]=="NO_COVID" & 
     first_dataset[j, "Dic_Recup"] != "Recuperado")
  {dataframe_test[j, "NO_COVID"] = fechas[1]}
  
  if(first_dataset[j, "Caso"]==large_dataset[j, "Caso"] & 
     first_dataset[j, "Ubic"]=="NO_COVID" & 
     first_dataset[j, "Dic_Recup"] == "Recuperado")
  {dataframe_test[j, "Recuperado"] = fechas[1]}
}  

#Bucle general
funcion_z = function(a){
  dataset_1 = data.frame()
  for (h in 1:length(a$Caso)) {
    if (a[h, "Dic_Recup"] == "Recuperado") {
      a[h, "Ubic"] = "Recuperado"
    }
    if (a[h, "Dic_Recup"] == "Fallecido") {
      a[h, "Ubic"] = "Fallecido"
    }
  }
  dataset_1 = a
  return(dataset_1)
}

n <- length(fechas) - 1
for (k in {1:n}) {
  dataset_y0 <- data.frame()
  dataset_y1 <- data.frame()
  
  dataset_y0 <- funcion_x(data.frame(fread(archive_extract(archive = destfile[k], 
                                                           dir = temp_dir_0, files = within_archive[k]))))
  dataset_y1 <- funcion_x(data.frame(fread(archive_extract(archive = destfile[k+1], 
                                                           dir = temp_dir_0, files = within_archive[k+1]))))
  dataset_y0 <- funcion_y(dataset_y0)
  dataset_y1 <- funcion_y(dataset_y1)
  dataset_y0 <- funcion_z(dataset_y0)
  dataset_y1 <- funcion_z(dataset_y1)
  
  
  names_0 <- vector()
  names_1 <- vector()
  
  names_0[1] <- "Caso"
  names_0[2] <- paste0("Not_", fechas[k])
  names_0[3] <- paste0("Cod_Dep_", fechas[k])
  names_0[4] <- paste0("Nom_Dep_", fechas[k])
  names_0[5] <- paste0("Cod_Mun_", fechas[k])
  names_0[6] <- paste0("Nom_Mun_", fechas[k])
  names_0[7] <- paste0("Edad_", fechas[k])
  names_0[8] <- paste0("Unidad_", fechas[k])
  names_0[9] <- paste0("Sexo_", fechas[k])
  names_0[10] <- paste0("Fuente_", fechas[k])
  names_0[11] <- paste0("Ubic_", fechas[k])
  names_0[12] <- paste0("Estado_", fechas[k])
  names_0[13] <- paste0("Pais_cod_", fechas[k])
  names_0[14] <- paste0("Pais_nom_", fechas[k])
  names_0[15] <- paste0("Dic_Recup_", fechas[k])
  names_0[16] <- paste0("Inicio_", fechas[k])
  names_0[17] <- paste0("Muerte_", fechas[k])
  names_0[18] <- paste0("Diagnos_", fechas[k])
  names_0[19] <- paste0("Recup_", fechas[k])
  names_0[20] <- paste0("Tipo_", fechas[k])
  names_0[21] <- paste0("Pert_Etn_", fechas[k])
  names_0[22] <- paste0("Nom_Etn_", fechas[k])
  
  names_1[1] <- "Caso"
  names_1[2] <- paste0("Not_", fechas[k+1])
  names_1[3] <- paste0("Cod_Dep_", fechas[k+1])
  names_1[4] <- paste0("Nom_Dep_", fechas[k+1])
  names_1[5] <- paste0("Cod_Mun_", fechas[k+1])
  names_1[6] <- paste0("Nom_Mun_", fechas[k+1])
  names_1[7] <- paste0("Edad_", fechas[k+1])
  names_1[8] <- paste0("Unidad_", fechas[k+1])
  names_1[9] <- paste0("Sexo_", fechas[k+1])
  names_1[10] <- paste0("Fuente_", fechas[k+1])
  names_1[11] <- paste0("Ubic_", fechas[k+1])
  names_1[12] <- paste0("Estado_", fechas[k+1])
  names_1[13] <- paste0("Pais_cod_", fechas[k+1])
  names_1[14] <- paste0("Pais_nom_", fechas[k+1])
  names_1[15] <- paste0("Dic_Recup_", fechas[k+1])
  names_1[16] <- paste0("Inicio_", fechas[k+1])
  names_1[17] <- paste0("Muerte_", fechas[k+1])
  names_1[18] <- paste0("Diagnos_", fechas[k+1])
  names_1[19] <- paste0("Recup_", fechas[k+1])
  names_1[20] <- paste0("Tipo_", fechas[k+1])
  names_1[21] <- paste0("Pert_Etn_", fechas[k+1])
  names_1[22] <- paste0("Nom_Etn_", fechas[k+1])
  
  colnames(dataset_y0) <- names_0 
  colnames(dataset_y1) <- names_1
  dataset_y1 <- merge(dataset_y1, dataset_y0, by= "Caso", all.x = TRUE) 
  rm(dataset_y0)
  
  dataset_y1$shft <- NA
  x <- paste0("Ubic_",fechas[k])
  y <- paste0("Ubic_",fechas[k+1])
  dataset_y1[x][is.na(dataset_y1[x])] <- "N_A"
  dataset_y1[y][is.na(dataset_y1[y])] <- "N_A"
  dataset_y1$shft <- dataset_y1[[x]] != dataset_y1[[y]] 
  rm(x,y)
  names(dataset_y1)[names(dataset_y1)=="shft"] <- paste0("shft_",fechas[k],"&",fechas[k+1])
  
  for(p in 1:length(dataset_y1$Caso)){
    w <- paste0("shft_",fechas[k],"&",fechas[k+1])
    z <- paste0("Ubic_",fechas[k+1])
    if(dataset_y1[p, w]==TRUE 
       & dataset_y1[p, z]=="Casa")
    {dataframe_test[p, "Casa"] = fechas[k+1]}
    rm(w, z)
    
    w <- paste0("shft_",fechas[k],"&",fechas[k+1])
    z <- paste0("Ubic_",fechas[k+1])
    if(dataset_y1[p, w]==TRUE & 
       dataset_y1[p, z]=="Fallecido")
    {dataframe_test[p, "Fallecido"] = fechas[k+1]}
    rm(w, z)
    
    w <- paste0("shft_",fechas[k],"&",fechas[k+1])
    z <- paste0("Ubic_",fechas[k+1])
    if(dataset_y1[p, w]==TRUE & 
       dataset_y1[p, z]=="Hospital")
    {dataframe_test[p, "Hospital"] = fechas[k+1]}
    rm(w, z)
    
    w <- paste0("shft_",fechas[k],"&",fechas[k+1])
    z <- paste0("Ubic_",fechas[k+1])
    if(dataset_y1[p, w]==TRUE & 
       dataset_y1[p, z]=="Hospital_UCI")
    {dataframe_test[p, "Hospital_UCI"] = fechas[k+1]}
    rm(w, z)
    
    w <- paste0("shft_",fechas[k],"&",fechas[k+1])
    z <- paste0("Ubic_",fechas[k+1])
    if(dataset_y1[p, w]==TRUE & 
       dataset_y1[p, z]=="Recuperado")
    {dataframe_test[p, "Recuperado"] = fechas[k+1]}
    rm(w, z)
    
    w <- paste0("shft_",fechas[k],"&",fechas[k+1])
    z <- paste0("Ubic_",fechas[k+1])
    if(dataset_y1[p, w]==TRUE & 
       dataset_y1[p, z]=="NO_COVID")
    {dataframe_test[p, "NO_COVID"] = fechas[k+1]}
    rm(w, z)
  }
  rm(dataset_y1)
  print(fechas[k+1])
}

dataframe_subset_1 <- dataframe_test

write.csv(dataframe_subset_1, 
          "/Volumes/SERGIOB/Proyecto_S.22.08.22/dataframe_subset_1", 
          row.names = FALSE)

