
#######################################
## Appendix: Multivariate model      ##
#######################################



source(here::here("Paper_resultados/Cox_model/Appendix/",
                  "Appendix_univariate_model.R"))

################
################
## Full model ##
################
################

# Multivariate model
cox_multivariate = coxph(formula = Surv(t_UCI, d) ~ age + sex, data = dataset_semiparametric)

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
                    "Paper_resultados/Cox_model/Appendix/Output/Multivariate_model/Multivariate_models.xlsx")


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
           "Paper_resultados/Cox_model/Appendix/Output/Multivariate_model/Multivariate_models.tex")

rm(cox_multivariate, coef_mv, inference_mv, summary_mv, multivariate_latex, multivariate_latex_tex)
################
################
## CCI model  ##
################
################
dataset_CCI = dataset_semiparametric %>% filter(CCI == "CCI")
cox_multivariate = coxph(formula = Surv(t_UCI, d) ~ sex + age, data = dataset_CCI)

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
                    "Paper_resultados/Cox_model/Appendix/Output/Multivariate_model/CCI/CCI_model.xlsx")


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
           "Paper_resultados/Cox_model/Appendix/Output/Multivariate_model/CCI/CCI_model.tex")


rm(cox_multivariate, coef_mv, inference_mv, summary_mv,
   multivariate_latex, multivariate_latex_tex, dataset_CCI)

###################
###################
## Non-CCI model ##
###################
###################

dataset_non_CCI = dataset_semiparametric %>% filter(CCI == "non-CCI")
cox_multivariate = coxph(formula = Surv(t_UCI, d) ~ sex + age, data = dataset_non_CCI)

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
                    "Paper_resultados/Cox_model/Appendix/Output/Multivariate_model/Non-CCI/Non_CCI_model.xlsx")


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
           "Paper_resultados/Cox_model/Appendix/Output/Multivariate_model/Non-CCI/Non_CCI_model.tex")







