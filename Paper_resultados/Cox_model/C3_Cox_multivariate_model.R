
################################
## C3: Cox_multivariate_model ##
################################

source(here::here("Paper_resultados/Cox_model/",
                  "C2_Cox_univariate_model.R"))


#############################
#############################
## Full multivariate model ##
#############################
#############################
cox_multivariate = coxph(formula = Surv(t_UCI, d) ~ Group_age + CCI, data = dataset_semiparametric)

coef_mv = as.data.frame(summary(cox_multivariate)[["coefficients"]])
colnames(coef_mv) = c("coef", "HR", "sd_coef", "z", "p_value")
coef_mv = coef_mv[c("HR", "p_value")]

inference_mv = as.data.frame(summary(cox_multivariate)[["conf.int"]])
colnames(inference_mv) = c("HR", "exp_(-)coef", "Lower_IC", "Upper_IC")
inference_mv = inference_mv[c("HR", "Lower_IC", "Upper_IC")]

summary_mv = cbind(coef_mv, inference_mv)
summary_mv$variables = row.names(summary_mv)
summary_mv = summary_mv[c("variables", "HR", "Lower_IC", "Upper_IC", "p_value")]

# Guardar en .xlsx
writexl::write_xlsx(summary_mv, 
                    "Paper_resultados/Cox_model/Output/Multivariate_model/Multivariate_models.xlsx")


# Guardar en .tex
multivariate_latex = summary_mv
multivariate_latex$est = NA

multivariate_latex$p_value = round(multivariate_latex$p_value, 4)

for (k in 1:length(var_sign)) {
  multivariate_latex$est[k] = paste0(round(multivariate_latex$HR[k], 4), 
                                   " (", round(multivariate_latex$Lower_IC[k], 4),
                                   " - ", round(multivariate_latex$Upper_IC[k], 4),")")
}

multivariate_latex = multivariate_latex[c("variables", "est", "p_value")]

multivariate_latex_tex = kable(multivariate_latex, , 
                             caption = "Cox multivariate analysis", 
                             format = "latex")

writeLines(multivariate_latex_tex,
           "Paper_resultados/Cox_model/Output/Multivariate_model/Multivariate_models.tex")



#############################
#############################
## CCI multivariate model ##
#############################
#############################
CCI_semiparametric = dataset_semiparametric %>% filter(CCI == "CCI")
cox_CCI = coxph(formula = Surv(t_UCI, d) ~ sex + Group_age, data = CCI_semiparametric)

coef_CCI = as.data.frame(summary(cox_CCI)[["coefficients"]])
colnames(coef_CCI) = c("coef", "HR", "sd_coef", "z", "p_value")
coef_CCI = coef_CCI[c("HR", "p_value")]

inference_CCI = as.data.frame(summary(cox_CCI)[["conf.int"]])
colnames(inference_CCI) = c("HR", "exp_(-)coef", "Lower_IC", "Upper_IC")
inference_mv = inference_CCI[c("HR", "Lower_IC", "Upper_IC")]

summary_CCI = cbind(coef_CCI, inference_CCI)
summary_CCI$variables = row.names(summary_CCI)
summary_CCI = summary_CCI[c("variables", "HR", "Lower_IC", "Upper_IC", "p_value")]

# Guardar en .xlsx
writexl::write_xlsx(summary_CCI, 
                    "Paper_resultados/Cox_model/Output/Multivariate_model/CCI_Multivariate.xlsx")


# Guardar en .tex
CCI_latex = summary_CCI
CCI_latex$est = NA

CCI_latex$p_value = round(CCI_latex$p_value, 4)

for (k in 1:2) {
  CCI_latex$est[k] = paste0(round(CCI_latex$HR[k], 4), 
                                     " (", round(CCI_latex$Lower_IC[k], 4),
                                     " - ", round(CCI_latex$Upper_IC[k], 4),")")
}

CCI_latex = CCI_latex[c("variables", "est", "p_value")]

CCI_latex_tex = kable(CCI_latex, , 
                               caption = "CCI: Cox multivariate analysis", 
                               format = "latex")

writeLines(CCI_latex_tex,
           "Paper_resultados/Cox_model/Output/Multivariate_model/CCI_Multivariate.tex")


#################################
#################################
## Non-CCI multivariate model  ##
#################################
#################################

non_CCI_semiparametric = dataset_semiparametric %>% filter(CCI == "non-CCI")
cox_non_CCI = coxph(formula = Surv(t_UCI, d) ~ sex + Group_age, data = non_CCI_semiparametric)

coef_non_CCI = as.data.frame(summary(cox_non_CCI)[["coefficients"]])
colnames(coef_non_CCI) = c("coef", "HR", "sd_coef", "z", "p_value")
coef_non_CCI = coef_non_CCI[c("HR", "p_value")]

inference_non_CCI = as.data.frame(summary(cox_non_CCI)[["conf.int"]])
colnames(inference_non_CCI) = c("HR", "exp_(-)coef", "Lower_IC", "Upper_IC")
inference_mv = inference_non_CCI[c("HR", "Lower_IC", "Upper_IC")]

summary_non_CCI = cbind(coef_non_CCI, inference_non_CCI)
summary_non_CCI$variables = row.names(summary_non_CCI)
summary_non_CCI = summary_non_CCI[c("variables", "HR", "Lower_IC", "Upper_IC", "p_value")]

# Guardar en .xlsx
writexl::write_xlsx(summary_non_CCI, 
                    "Paper_resultados/Cox_model/Output/Multivariate_model/non_CCI_Multivariate.xlsx")


# Guardar en .tex
non_CCI_latex = summary_non_CCI
non_CCI_latex$est = NA

non_CCI_latex$p_value = round(non_CCI_latex$p_value, 4)

for (k in 1:2) {
  non_CCI_latex$est[k] = paste0(round(non_CCI_latex$HR[k], 4), 
                            " (", round(non_CCI_latex$Lower_IC[k], 4),
                            " - ", round(non_CCI_latex$Upper_IC[k], 4),")")
}

non_CCI_latex = non_CCI_latex[c("variables", "est", "p_value")]

non_CCI_latex_tex = kable(non_CCI_latex, , 
                      caption = "non_CCI: Cox multivariate analysis", 
                      format = "latex")

writeLines(non_CCI_latex_tex,
           "Paper_resultados/Cox_model/Output/Multivariate_model/non_CCI_Multivariate.tex")

