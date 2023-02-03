###################
## 100_subsets.R ##
###################

source(here::here("Script/Script_subsets", "00_librerias.R"))


# Definición de las rutas para los subconjuntos de datos
destfile = vector()
n = 6   #número de subconjuntos

for (j in 1:n)
{
  destfile [[j]] =paste0(
    "Subsets\\dataframe_subset_",
    j,".csv")
}

# Cargar los subconjuntos de datos
df_subset_1 = read.csv(destfile[1])
df_subset_2 = read.csv(destfile[2])
df_subset_3 = read.csv(destfile[3])
df_subset_4 = read.csv(destfile[4])
df_subset_5 = read.csv(destfile[5])
df_subset_6 = read.csv(destfile[length(destfile)])

# Definir una familia de subconjuntos de datos
aux_0 = list(df_subset_1, df_subset_2, df_subset_3, df_subset_4, df_subset_5,
             df_subset_6)

# Recodificar los nombres de las variables para los subconjuntos 2-6
for (k in 2:length(aux_0)) {
  colnames(aux_0[[k]]) = c("Caso", 
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

colnames(aux_0[[1]]) = c("Caso", cas,
                         hos, fal, uci, rec, noc)


# merge completo entre los subsets
dataset = merge(aux_0[[6]], aux_0[[5]], by = "Caso", all.x = TRUE)
dataset = merge(dataset, aux_0[[4]], by = "Caso", all.x = TRUE)
dataset = merge(dataset, aux_0[[3]], by = "Caso", all.x = TRUE)
dataset = merge(dataset, aux_0[[2]], by = "Caso", all.x = TRUE)
dataset = merge(dataset, aux_0[[1]], by = "Caso", all.x = TRUE)

# restricción para extraer pacientes en UCI
# procedimiento: se mantienen todas las filas en las cuales existe al menos un U_i no vacío
subset_uci = dataset[c("Caso", uci, "u_11", 
                       "u_12", "u_13", 
                       "u_14", "u_15")]

subset_uci_na = subset_uci[rowSums(is.na(subset_uci[,2:ncol(subset_uci)])) != ncol(subset_uci[,2:ncol(subset_uci)]), ]

dataset_na = dataset %>% filter(Caso %in% subset_uci_na$Caso)

# remover
rm(subset_uci, subset_uci_na, aux_0, 
   df_subset_6, df_subset_5, df_subset_4, 
   df_subset_3, df_subset_2, df_subset_1, j, k, n, destfile)








