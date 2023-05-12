
##################################
##  Análisis estadístico [3]    ##
##  (Categorical variables)     ##
##################################
library(here)
source(here::here("Paper_resultados/Descriptive_analysis/02_Non_Gaussian/", "Non_Gaussian.R"))

#######################################
## [I] Resumen descriptivo (Sexo)    ##
#######################################
list_output_sex = list(Total = data.frame(),
                             CCI = data.frame(), Non_CCI = data.frame())
for (i in 1:length(list_input)) {
 df_1 = plyr::count(list_input[[i]]$sex)
 df_1$share = (df_1$freq/sum(df_1$freq))*100
 colnames(df_1) = c("sex", "freq", "share")
 
 write_xlsx(df_1, 
            paste0("Paper_resultados/Descriptive_analysis/03_Categorical/xlsx/Sex/",
                   names(list_input)[i],".xlsx"))
 list_output_sex[[i]] = df_1
}

# Preparar y guardar en formato .tex
summary_sex = data.frame(sex = c(0,1))
for (j in 1:length(list_output_sex)) {
  df_1 = list_output_sex[[j]]
  df_1$summary = paste0(df_1$freq, " (",
                        round(df_1$share,2),"%)")
  df_1 = df_1[c("sex", "summary")]
  colnames(df_1) = c("sex", names(list_output_sex)[j])
  summary_sex = merge(summary_sex, df_1, by ="sex")
}

summary_sex_latex = kable(summary_sex,
                               caption = "Sex, n (%)", 
                               format = "latex")
writeLines(summary_sex_latex,
           "Paper_resultados/Descriptive_analysis/03_Categorical/tex/Sex.tex")
rm(list_output_sex, df_1)
##########################################
## [II] Resumen descriptivo (Outcome)   ##
##########################################
list_output_outcome = list(Total = data.frame(),
                       CCI = data.frame(), Non_CCI = data.frame())

for (i in 1:length(list_input)) {
  df_1 = plyr::count(list_input[[i]]$outcome)
  df_1$share = (df_1$freq/sum(df_1$freq))*100
  colnames(df_1) = c("outcome", "freq", "share")
  
  write_xlsx(df_1, 
             paste0("Paper_resultados/Descriptive_analysis/03_Categorical/xlsx/Outcome/",
                    names(list_input)[i],".xlsx"))
  list_output_outcome[[i]] = df_1
}

# Preparar y guardar en formato .tex
summary_outcome = data.frame(outcome = c("Alive", "Censored", "Dead"))
for (j in 1:length(list_output_outcome)) {
  df_1 = list_output_outcome[[j]]
  df_1$summary = paste0(df_1$freq, " (",
                        round(df_1$share,2),"%)")
  df_1 = df_1[c("outcome", "summary")]
  colnames(df_1) = c("outcome", names(list_output_outcome)[j])
  summary_outcome = merge(summary_outcome, df_1, by ="outcome")
}

summary_outcome_latex = kable(summary_outcome,
                          caption = "Outcome, n (%)", 
                          format = "latex")
writeLines(summary_outcome_latex,
           "Paper_resultados/Descriptive_analysis/03_Categorical/tex/Outcome.tex")
rm(df_1, list_output_outcome, n_row, non_gaussian, i,j,k)
