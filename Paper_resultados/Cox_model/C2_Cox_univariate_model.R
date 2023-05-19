

################################
## C2: Cox univariate model   ##
################################

source(here::here("Paper_resultados/Cox_model/",
                  "C1_Semiparametric_data.R"))

# Vector de covariables fijas
covariate = c("sex", "Group_age", "CCI")

# Estimar modelos univariados
output_univariate = list()
length(output_univariate) = length(covariate)

for (k in 1:length(covariate)) {
  var_aux = c("t_UCI", "d", covariate[k])
  df_aux = dataset_semiparametric[var_aux]
  colnames(df_aux) = c("t", "d", "covariate")
  cox_model <- coxph(Surv(t, d) ~ covariate, data = df_aux)
  output_univariate[[k]] = summary(cox_model)
  names(output_univariate)[k] = covariate[k]
}

# Base de datos de recepcion
univariate = data.frame(variable = covariate,
  HR = rep(NA, length(covariate)),
                        Lower_IC = rep(NA, length(covariate)),
  Upper_IC = rep(NA, length(covariate)),
                        p_value = rep(NA, length(covariate)))

for (k in 1:length(covariate)) {
  n_row = which(univariate$variable == covariate[k])
  
  univariate$HR[n_row] = output_univariate[[k]][["coefficients"]][2]
  univariate$Lower_IC[n_row] = output_univariate[[k]][["conf.int"]][3]
  univariate$Upper_IC[n_row] = output_univariate[[k]][["conf.int"]][4]
  univariate$p_value[n_row] = output_univariate[[k]][["waldtest"]][3]
}


# Variables significativas:
var_sign = NA
for (k in 1:nrow(univariate)) {
  if (univariate$p_value[k] < 0.05) {
    var_sign[k] = univariate$variable[k]
  }
}

var_sign = var_sign[!is.na(var_sign)]


# Guardar en .xlsx
writexl::write_xlsx(univariate, 
                    "Paper_resultados/Cox_model/Output/Univariate_model/Univariate_models.xlsx")

# Guardar en .tex
univariate_latex = univariate
univariate_latex$est = NA

univariate_latex$p_value = round(univariate_latex$p_value, 4)

for (k in 1:nrow(univariate_latex)) {
  univariate_latex$est[k] = paste0(round(univariate_latex$HR[k], 4), 
                                   " (", round(univariate_latex$Lower_IC[k], 4),
                                   " - ", round(univariate_latex$Upper_IC[k], 4),")")
}

univariate_latex = univariate_latex[c("variable", "est", "p_value")]

univariate_latex_tex = kable(univariate_latex, , 
                   caption = "Cox univariate analysis", 
                   format = "latex")

writeLines(univariate_latex_tex,
           "Paper_resultados/Cox_model/Output/Univariate_model/Univariate_models.tex")




