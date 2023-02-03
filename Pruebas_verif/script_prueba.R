
#################################################
# Verificación de la recodificación del sexo   ##
#################################################

library(here)

source(here::here("Script/Script_subsets", "40_covariables.R"))

# vector prueba 
sexo = c("F", " ffffffff ", " f", "mmmm ", "mM", " Mm ", " Mm")

# código de recodificación

sexo <- gsub(pattern = "\\s*(f|F)+\\s*", 
replacement = "1", sexo)

sexo <- gsub(pattern = "\\s*(m|M)+\\s*", 
                        replacement = "0", sexo)

sexo = as.factor(as.character(sexo))

############################################
## Verificación de la base de datos final ##
############################################

View(input)
View(dataset)

# base de datos de pacientes de UCI
cas_x = c(cas, "c_11", "c_12", "c_13", "c_14", "c_15")
fal_x = c(fal, "f_11", "f_12", "f_13", "f_14", "f_15")
hos_x = c(hos, "h_11", "h_12", "h_13", "h_14", "h_15")
noc_x =  c(noc, "n_11", "n_12", "n_13", "n_14", "n_15")
rec_x = c(rec, "r_11", "r_12", "r_13", "r_14", "r_15")
uci_x = c(uci, "u_11", "u_12", "u_13", "u_14", "u_15")


order = c(uci_x, rec_x, cas_x, hos_x, noc_x, fal_x)
verif_uci = dataset_na[,c("Caso",order)]

# observaciones sospechosas
# mayores de 30 días (el estudio de Tafur reporta un RIC de 6-15)

sus_final = final_df_z %>% filter(t > 30) # n = 806

sus_input = input %>% filter(Caso %in% sus_final$Caso)

sus_verif = verif_uci %>% filter(Caso %in% sus_final$Caso)


#######################################
## Verificación de casos sospechosos ##
#######################################

# Caso de interés
caso = 24532

# Descargar fechas
t_0 <- as.Date("2020-11-30")    
t_T <- as.Date("2021-03-31")   
fechas <- seq(from=t_0, to=t_T, by=1)

fechas <- c("2020-06-03", "2020-06-04", "2020-08-28", "2020-08-29", "2020-08-30")
N <- length(fechas)
#El bucle para descagar archivos es el siguiente:
destfile <- vector()
URL <- vector()
for (j in 1:N){
  URL[[j]] <- paste0("https://www.ins.gov.co/BoletinesCasosCOVID19Colombia/",fechas[j],".xlsx")
  destfile [[j]] =paste0("Pruebas_verif/Datasets/",fechas[j],".xlsx")
  #MacOS:
  #download.file(URL[j],destfile[j])
  #Windows, Linux:
  #download.file(URL[j],destfile[j], method="wininet", mode="wb")
}



# Verificar
caso_verif_x =  read_excel(destfile[1])
caso_verif_x = caso_verif_x[,c(1,9,10)]
colnames(caso_verif_x) = c("Caso", "x_1", "x_2")
caso_verif_x = caso_verif_x %>% filter(Caso %in% caso)

for (j in 2:N){
  print(destfile[j])
  df_verif = read_excel(destfile[j])
  caso_verif = df_verif
  caso_verif = df_verif[,c(1,9, 10)]
  colnames(caso_verif) = c("Caso", "x_1", "x_2")
  caso_verif = caso_verif %>% filter(Caso %in% caso)
  caso_verif_x = rbind(caso_verif_x, caso_verif)
}

# recuperar fechas
caso_verif_x$id = 1:nrow(caso_verif_x)
fechas_df = data.frame(id = caso_verif_x$id, date = fechas)
caso_verif_x = merge(caso_verif_x, fechas_df, by = "id")







