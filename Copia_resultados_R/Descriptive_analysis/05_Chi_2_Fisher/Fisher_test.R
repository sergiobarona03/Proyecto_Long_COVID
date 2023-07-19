
################################
##  Análisis estadístico [5]  ##
##        (Fisher test)       ##
################################
library(here)
source(here::here("Paper_resultados/Descriptive_analysis/04_Mann_Whitney/", "Mann_Whitney_test.R"))

#####################
##   Chi-2 test    ##
#####################
chi_2 = data.frame(V1 = c("Sex", "Sex", "CCI"), 
                   V2 = c("CCI", "Outcome", "Outcome"),
                   st = c(NA, NA, NA),
                   df = c(NA, NA, NA),
                   p = c(NA, NA, NA))

# Sex - CCI
chi_sex_CCI = chisq.test(dataset$sex, dataset$CCI)
chi_2$st[1] = chi_sex_CCI$statistic
chi_2$df[1] = chi_sex_CCI$parameter
chi_2$p[1] = chi_sex_CCI$p.value

# Sex - Outcome 
chi_sex_outcome = chisq.test(dataset$sex, dataset$outcome)
chi_2$st[2] = chi_sex_outcome$statistic
chi_2$df[2] = chi_sex_outcome$parameter
chi_2$p[2] = chi_sex_outcome$p.value

# CCI - Outcome
chi_CCI_outcome = chisq.test(dataset$CCI, dataset$outcome)
chi_2$st[3] = chi_CCI_outcome$statistic
chi_2$df[3] = chi_CCI_outcome$parameter
chi_2$p[3] = chi_CCI_outcome$p.value

# guardar en .xlsx
write_xlsx(chi_2, 
           "Paper_resultados/Descriptive_analysis/05_Chi_2_Fisher/xlsx/Chi_2.xlsx")

# guardar en .tex
for (j in 3:(ncol(chi_2))) {
  chi_2[,j] = round(chi_2[,j], 2)
}

chi_2_latex  = kable(chi_2, 
                     caption = "Chi-2 test of independence",
                     format = "latex")
writeLines(chi_2_latex,
           "Paper_resultados/Descriptive_analysis/05_Chi_2_Fisher/tex/Chi_2.tex")

############################
##  Fisher's exact test   ##
############################
fisher = data.frame(V1 = c("Sex", "Sex", "CCI"), 
                   V2 = c("CCI", "Outcome", "Outcome"),
                  p = c(NA, NA, NA),
                   odds_ratio = c(NA, NA, NA),
                   ci_1 = c(NA, NA, NA),
                  ci_2 = c(NA, NA, NA))

# Sex - CCI
fisher_sex_CCI = fisher.test(dataset$sex, dataset$CCI)
fisher$p[1] = fisher_sex_CCI$p.value
fisher$odds_ratio[1] = fisher_sex_CCI$estimate
fisher$ci_1[1] = fisher_sex_CCI$conf.int[1]
fisher$ci_2[1] = fisher_sex_CCI$conf.int[2]

# Sex - Outcome 
fisher_sex_outcome = fisher.test(dataset$sex, dataset$outcome)
fisher$p[2] = fisher_sex_outcome$p.value
fisher$odds_ratio[2] = NA
fisher$ci_1[2] = NA
fisher$ci_2[2] = NA

# CCI - Outcome
fisher_CCI_outcome = fisher.test(dataset$CCI, dataset$outcome)
fisher$p[3] = fisher_CCI_outcome$p.value
fisher$odds_ratio[3] = NA
fisher$ci_1[3] = NA
fisher$ci_2[3] = NA

# guardar en .xlsx
write_xlsx(fisher, 
           "Paper_resultados/Descriptive_analysis/05_Chi_2_Fisher/xlsx/Fisher.xlsx")

# guardar en .tex
for (j in 3:(ncol(fisher))) {
  fisher[,j] = round(fisher[,j], 2)
}

fisher_latex  = kable(fisher, 
                     caption = "Fisher's exact test",
                     format = "latex")
writeLines(fisher_latex,
           "Paper_resultados/Descriptive_analysis/05_Chi_2_Fisher/tex/Fisher.tex")

rm(fisher_CCI_outcome, fisher_sex_CCI, fisher_sex_outcome,
   chi_CCI_outcome, chi_sex_CCI, chi_sex_outcome, j)
