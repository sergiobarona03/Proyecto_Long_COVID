
#####################################
## Assumptions: multivariate model ##
#####################################

source(here::here("Paper_resultados/Cox_model/",
                  "C3_Cox_multivariate_model.R"))

##########################################
##     A1: riesgos proporcionales       ##  
## (residuos parciales de Schoenfeld)   ##
##########################################

# Full model
schoenfeld_full <- cox.zph(cox_multivariate)
print(schoenfeld_full)
ggcoxzph(schoenfeld_full)

# CCI model
schoenfeld_CCI <- cox.zph(cox_CCI)
schoenfeld_CCI_output = as.data.frame(print(schoenfeld_CCI))

writexl::write_xlsx(schoenfeld_CCI_output,
                    "Paper_resultados/Cox_model/Output/Assumptions/CCI/Schoenfeld_CCI.xlsx")

setEPS()
postscript("Paper_resultados/Cox_model/Output/Assumptions/CCI/Schoenfeld_CCI_sex.eps")
ggcoxzph(schoenfeld_CCI, se = TRUE,
         var = c("sex"), point.col = "black") + labs(x = "Time", y = "B(t)", title = "", 
                                                     subtitle = "") 
dev.off()


setEPS()
postscript("Paper_resultados/Cox_model/Output/Assumptions/CCI/Schoenfeld_CCI_age.eps")
ggcoxzph(schoenfeld_CCI, se = TRUE,
         var = c("Group_age"), point.col = "black") + labs(x = "Time", y = "B(t)", title = "", 
                                subtitle = "")
dev.off()



############################# Par?ntesis #############################################################
############## Estimaci?n de la curva de supervivencia ###############################################
CCI_semiparametric$sex = as.factor(CCI_semiparametric$sex)
cox_model_inter = coxph(Surv(t_UCI, d) ~ sex, data = CCI_semiparametric)

datosnuevos <- data.frame(sex = c("Female", "Male"))
datosnuevos$sex <- as.factor(datosnuevos$sex)

curva_sup_inter <- survfit(cox_model_inter, newdata = datosnuevos, conf.type="log-log")

ggsurvplot(fit = curva_sup_inter, data = datosnuevos, conf.int = T, title = "Curva de Supervivencia",
           xlab = "Tiempo", ylab = "Probabilidad de supervivencia")
############################# Par?ntesis #############################################################
######################################################################################################

# Non-CCI model
schoenfeld_non_CCI <- cox.zph(cox_non_CCI)
print(schoenfeld_non_CCI)



schoenfeld_non_CCI_output = as.data.frame(print(schoenfeld_non_CCI))

writexl::write_xlsx(schoenfeld_non_CCI_output,
                    "Paper_resultados/Cox_model/Output/Assumptions/Non_CCI/Schoenfeld_Non_CCI.xlsx")


setEPS()
postscript("Paper_resultados/Cox_model/Output/Assumptions/Non_CCI/Schoenfeld_Non_CCI_sex.eps")
ggcoxzph(schoenfeld_non_CCI, se = TRUE,
         var = c("sex"), point.col = "black") + labs(x = "Time", y = "B(t)", title = "", 
                                                     subtitle = "") 

dev.off()


setEPS()
postscript("Paper_resultados/Cox_model/Output/Assumptions/Non_CCI/Schoenfeld_Non_CCI_age.eps")
ggcoxzph(schoenfeld_non_CCI, se = TRUE,
         var = c("Group_age"), point.col = "black") + labs(x = "Time", y = "B(t)", title = "", 
                                                           subtitle = "")
dev.off()
