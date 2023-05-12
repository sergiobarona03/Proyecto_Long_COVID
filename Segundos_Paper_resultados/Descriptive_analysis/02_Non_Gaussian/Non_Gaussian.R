
##########################################
##       Análisis estadístico [2]       ##
##  (Summary: non-Gaussian variables)   ##
##########################################
library(here)
source(here::here("Paper_resultados/Descriptive_analysis/01_Shapiro_Wilk/", "Shapiro_Wilk.R"))

###############################################
## Preparar resumen descriptivo (continuas)  ##
###############################################

# Creación de variable CCI
dataset$CCI = NA
for (k in 1:nrow(dataset)) {
  if (dataset$t_UCI_desc[k] >= 21) {
    dataset$CCI[k] = "CCI"
  } else {dataset$CCI[k] = "non-CCI"}
}

# Tres bases de datos: total, CCI & non-CCI
list_input = list(Total = dataset, 
                  CCI = dataset %>% filter(CCI == "CCI"),
                  Non_CCI = dataset %>% filter(CCI == "non-CCI"))

######################################
## Resumen descriptivo (continuas)  ##
######################################
non_gaussian = c("age", "t_UCI_desc", "t_so")
list_output = list(total = data.frame(Variable = non_gaussian, Q2 = NA, 
                                      Q1 = NA, Q3 = NA),
                   CCI = data.frame(Variable = non_gaussian, Q2 = NA, 
                                    Q1 = NA, Q3 = NA),
                   Non_CCI = data.frame(Variable = non_gaussian, Q2 = NA, 
                                        Q1 = NA, Q3 = NA))

for (i in 1:length(list_input)) {
  df_1 = list_input[[i]]
  for (j in 1:length(non_gaussian)) {
  df_2 = list_input[[i]][c("Caso",non_gaussian[[j]])]
  colnames(df_2) = c("Caso", "non_gaussian")
  n_row = which(list_output[[i]]$Variable == non_gaussian[[j]])
  
  list_output[[i]]$Q2[j] = median(df_2$non_gaussian)
  list_output[[i]]$Q1[j] = quantile(df_2$non_gaussian, c(0.25), type = 6)
  list_output[[i]]$Q3[j] = quantile(df_2$non_gaussian, c(0.75), type = 6)
  }
}

for (k in 1:length(list_output)) {
  write_xlsx(list_output[[k]],
             paste0("Paper_resultados/Descriptive_analysis/02_Non_Gaussian/xlsx/",
                    "summary_",names(list_output)[k],".xlsx"))
}

#############################################
## Latex: Resumen descriptivo (continuas)  ##
#############################################
gaussian_summary = data.frame(Variable = non_gaussian)

# Preparar y guardar en formato Latex
for (i in 1:length(list_output)) {
  df_1 = list_output[[i]]
  df_1$summary = NA
  
  for (k in 1:nrow(df_1)) {
    df_1$summary[k] = paste0(df_1$Q2[k], " (",df_1$Q1[k],", ",df_1$Q3[k],")")
  }
  
  df_1 = df_1[c("Variable", "summary")]
  colnames(df_1) = c("Variable", names(list_output)[i])
  gaussian_summary = merge(gaussian_summary, df_1, by = "Variable")
  rm(df_1)
}

gaussian_summary_latex = kable(gaussian_summary,
                                        caption = "Non-Gaussian Continuous Variables, Median (IQR)", 
                               format = "latex")

writeLines(gaussian_summary_latex,
           "Paper_resultados/Descriptive_analysis/02_Non_Gaussian/tex/Non_Gaussian.tex")

rm(list_output, df_2)
