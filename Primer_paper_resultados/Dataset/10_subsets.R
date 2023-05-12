###################
## 100_subsets.R ##
###################

source(here::here("Script/Script_subsets", "00_librerias.R"))

# Rutas para los subconjuntos de datos
destfile = vector()
n = 6   #cardinalidad 

for (j in 1:n){
  destfile [[j]] =paste0("Paper_resultados/Dataset/Subsets/dataframe_subset_",
                         j,".RDS")
}

# Cargar los subconjuntos de datos
aux_list = list()
length(aux_list) = n

for (i in 1:n) {
  aux_list[[i]] = readRDS(destfile[i])
  names(aux_list)[i] = paste0("df_subset_",i)
}

# Recodificar los nombres de las variables para los subconjuntos 2-6
for (k in 2:length(aux_list)) {
  colnames(aux_list[[k]]) = c("Caso", 
                           paste0("c_",9+k),
                           paste0("h_",9+k),
                           paste0("f_",9+k),
                           paste0("u_",9+k),
                           paste0("r_",9+k),
                           paste0("n_",9+k))
  
}

# Recodificar base de datos extensa (subset 1)
cas = vector(length = 10) 
hos =vector(length = 10) 
fal =vector(length = 10) 
uci =vector(length = 10) 
rec =vector(length = 10) 
noc =vector(length = 10) 

for (j in 1:10) {
  cas[j] = paste0("c_",j)
  hos[j] = paste0("h_",j)
  fal[j] = paste0("f_",j)
  uci[j] = paste0("u_",j)
  rec[j] = paste0("r_",j)
  noc[j] = paste0("n_",j)
}

colnames(aux_list[[1]]) = c("Caso", cas,
                         hos, fal, uci, rec, noc)


# merge completo entre los subsets
dataset = merge(aux_list[[6]], aux_list[[5]], by = "Caso", all.x = TRUE)
dataset = merge(dataset, aux_list[[4]], by = "Caso", all.x = TRUE)
dataset = merge(dataset, aux_list[[3]], by = "Caso", all.x = TRUE)
dataset = merge(dataset, aux_list[[2]], by = "Caso", all.x = TRUE)
dataset = merge(dataset, aux_list[[1]], by = "Caso", all.x = TRUE)

# Extraer pacientes en UCI
# procedimiento: se mantienen todas las filas en las cuales existe 
# al menos un U_i no NA

subset_uci = dataset[c("Caso", uci, "u_11", 
                       "u_12", "u_13", 
                       "u_14", "u_15")]

# Lista de pacientes en UCI
uci_no_na = subset_uci[rowSums(is.na(subset_uci[,2:ncol(subset_uci)])) != ncol(subset_uci[,2:ncol(subset_uci)]), ]

# Ad hoc: eliminar pacientes con dos o más admisiones en UCI
uci_no_na$row_na = rowSums(is.na(uci_no_na))
uci_no_na$adm = NA

for (k in 1:nrow(uci_no_na)) {
  if (uci_no_na$row_na[k] == 14) {
    uci_no_na$adm[k] = 1
  } else {
    uci_no_na$adm[k] = 0
  }
}

# Conservar pacientes en UCI con admisión = 1
casos_uci_no_na = uci_no_na %>% filter(adm == 1)
dataset_na = dataset %>% filter(Caso %in% casos_uci_no_na$Caso)

# remover
rm(subset_uci, aux_list, j, k, n, destfile,
   dataset, uci_no_na, fal, hos, noc, rec, i, casos_uci_no_na)

