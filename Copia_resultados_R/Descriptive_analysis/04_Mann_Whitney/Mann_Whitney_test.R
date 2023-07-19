
#################################
##  Análisis estadístico [4]   ##
##     (Mann-Whitney test)     ##
#################################
library(here)
source(here::here("Paper_resultados/Descriptive_analysis/03_Categorical/", "Categorical_variables.R"))

# base de datos de recepcion
mann_whitney = data.frame(Variable = c("age", "t_UCI", "t_so"),
                          Sex_W = NA, Sex_p = NA,
                          CCI_W = NA, CCI_p = NA)

##############
## [I] Sex  ##
##############
# age
wilcox_sex_age = wilcox.test(as.vector((dataset %>% filter(sex == 0))$age), 
                             as.vector((dataset %>% filter(sex == 1))$age))
mann_whitney$Sex_W[1] = wilcox_sex_age$statistic
mann_whitney$Sex_p[1] = wilcox_sex_age$p.value

# LoS ICU
wilcox_sex_LoS = wilcox.test(as.vector((dataset %>% filter(sex == 0))$t_UCI), 
                              as.vector((dataset %>% filter(sex == 1))$t_UCI))
mann_whitney$Sex_W[2] = wilcox_sex_LoS$statistic
mann_whitney$Sex_p[2] = wilcox_sex_LoS$p.value

# Time from SO to ICU admission
wilcox_sex_SO = wilcox.test(as.vector((dataset %>% filter(sex == 0))$t_so),
                             as.vector((dataset %>% filter(sex == 1))$t_so))
mann_whitney$Sex_W[3] = wilcox_sex_SO$statistic
mann_whitney$Sex_p[3] = wilcox_sex_SO$p.value

###########################
## [II] CCI vs non-CCI   ##
###########################

# age
wilcox_CCI_age = wilcox.test(as.vector((dataset %>% filter(CCI == "CCI"))$age),
                             as.vector((dataset %>% filter(CCI == "non-CCI"))$age))
mann_whitney$CCI_W[1] = wilcox_CCI_age$statistic
mann_whitney$CCI_p[1] = wilcox_CCI_age$p.value

# LoS ICU
wilcox_CCI_LoS = wilcox.test(as.vector((dataset %>% filter(CCI == "CCI"))$t_UCI),
                            as.vector((dataset %>% filter(CCI == "non-CCI"))$t_UCI))
mann_whitney$CCI_W[2] = wilcox_CCI_LoS$statistic
mann_whitney$CCI_p[2] = wilcox_CCI_LoS$p.value

# Time from SO to ICU admission
wilcox_CCI_so =  wilcox.test(as.vector((dataset %>% filter(CCI == "CCI"))$t_so),
                            as.vector((dataset %>% filter(CCI == "non-CCI"))$t_so))
mann_whitney$CCI_W[3] = wilcox_CCI_so$statistic
mann_whitney$CCI_p[3] = wilcox_CCI_so$p.value


# Guardar en .xlsx
write_xlsx(mann_whitney, 
           "Paper_resultados/Descriptive_analysis/04_Mann_Whitney/xlsx/Mann_Whitney.xlsx")

# Guardar en .tex
for (j in 2:(ncol(mann_whitney)-1)) {
  mann_whitney[,j] = round(mann_whitney[,j], 2)
}

mann_whitney_latex  = kable(mann_whitney, 
                            caption = "Mann-Whitney U test",
                            format = "latex")
writeLines(mann_whitney_latex,
           "Paper_resultados/Descriptive_analysis/04_Mann_Whitney/tex/Mann_Whitney.tex")

rm(wilcox_CCI_age, wilcox_CCI_LoS, wilcox_CCI_so,
   wilcox_sex_age, wilcox_sex_LoS, wilcox_sex_SO, j)
