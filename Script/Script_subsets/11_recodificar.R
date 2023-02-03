 

########################
## 110_recodificar.R  ##
########################

source(here::here("Script/Script_subsets", "10_subsets.R"))

# convertir las variables en chr

for (k in 2:ncol(dataset_na)) {
  dataset_na[,k] = as.character(dataset_na[,k])
}

