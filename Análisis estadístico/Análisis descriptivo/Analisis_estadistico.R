
###########################
## An?lisis estad?stico  ##
## (Garc?a et al., 2023) ##
###########################

library(readxl)
library(writexl)
library(dplyr)
library(ggplot2)
library(moments)

# Carga de datos
dataset_cov = read.csv("Resultados\\04.01.23\\final_cov_040123.csv")
dataset_cov = dataset_cov[c("Caso", "Edad", "Sexo")]
dataset_corr = read.csv("Resultados\\04.01.23\\final_dataset_corr.csv")

dataset = merge(dataset_corr, dataset_cov, by = "Caso")   


# Adultos
dataset = dataset %>% filter(Edad >= 18)
dataset$Sexo = as.factor(dataset$Sexo)
dataset$d = as.factor(dataset$d)

# Outcome
dataset$outcome = dataset$death_binaria
dataset$outcome[dataset$outcome == 1] = "Fallecido"

for (i in 1:nrow(dataset)) {
  if (dataset$d[i] == 0) {
    dataset$outcome[i] = "Censurado"
  }
}

dataset$outcome[dataset$outcome == 0] = "Alta"

# Pacientes criticos cronicos
cronicos = dataset %>%  filter(t >= 21)

# Pacientes criticos
criticos = dataset %>%  filter(t < 21)
  


# Calcular t para casos censurados



# Variable binaria (cr?ticos y cr?ticos cr?nicos)
dataset$Cronico = NA

for (i in 1:nrow(dataset)) {
  
  if (dataset$t[i] >= 21) {
    dataset$Cronico[i] = "Crónico"
  }else{
    dataset$Cronico[i] = "Crítico"
  }
  
}

############################
############################
## 1. An?lisis preliminar ##
############################
############################

######################
## 1.1. Normalidad  ##
######################

normal = data.frame(Variable = c("Edad", "ICU_LoS", "Onset_Adm"), 
                    Skewness = c(NA,NA,NA), Kurtosis=c(NA,NA,NA),
                    W= c(NA,NA,NA), p_value= c(NA, NA,NA))

normal$Skewness[1] = skewness(dataset$Edad)
normal$Kurtosis[1] = kurtosis(dataset$Edad)
normal$W[1] = as.numeric(shapiro.test(dataset$Edad)$statistic)
normal$p_value[1] = as.numeric(shapiro.test(dataset$Edad)$p.value)
  
normal$Skewness[2] = skewness(dataset$t)
normal$Kurtosis[2] = kurtosis(dataset$t)
normal$W[2] = as.numeric(shapiro.test(dataset$t)$statistic)
normal$p_value[2] = as.numeric(shapiro.test(dataset$t)$p.value)

normal$Skewness[3] = skewness(dataset$t_OS)
normal$Kurtosis[3] = kurtosis(dataset$t_OS)
normal$W[3] = as.numeric(shapiro.test(dataset$t_OS)$statistic)
normal$p_value[3] = as.numeric(shapiro.test(dataset$t_OS)$p.value)

###########################################
## 1.2. Resumen descriptivo (continuas)  ##
###########################################
continuas = data.frame(Variable = c("Edad", "ICU_LoS", "Onset_Adm"),
                       Mediana = c(NA, NA, NA), 
                       Q1 = c(NA, NA, NA),
                       Q3 = c(NA, NA, NA))

continuas$Mediana[1] = median(dataset$Edad)
continuas$Q1[1] = quantile(dataset$Edad, c(0.25), type = 6)
continuas$Q3[1] = quantile(dataset$Edad, c(0.75), type = 6)

continuas$Mediana[2] = median(dataset$t)
continuas$Q1[2] = quantile(dataset$t, c(0.25), type = 6)
continuas$Q3[2] = quantile(dataset$t, c(0.75), type = 6)

continuas$Mediana[3] = median(dataset$t_OS)
continuas$Q1[3] = quantile(dataset$t_OS, c(0.25), type = 6)
continuas$Q3[3] = quantile(dataset$t_OS, c(0.75), type = 6)

continuas_cronicos = data.frame(Variable = c("Edad", "ICU_LoS", "Onset_Adm"),
                                Mediana = c(NA, NA, NA), 
                                Q1 = c(NA, NA, NA),
                                Q3 = c(NA, NA, NA))

continuas_cronicos$Mediana[1] = median(cronicos$Edad)
continuas_cronicos$Q1[1] = quantile(cronicos$Edad, c(0.25), type = 6)
continuas_cronicos$Q3[1] = quantile(cronicos$Edad, c(0.75), type = 6)

continuas_cronicos$Mediana[2] = median(cronicos$t)
continuas_cronicos$Q1[2] = quantile(cronicos$t, c(0.25), type = 6)
continuas_cronicos$Q3[2] = quantile(cronicos$t, c(0.75), type = 6)

continuas_cronicos$Mediana[3] = median(cronicos$t_OS)
continuas_cronicos$Q1[3] = quantile(cronicos$t_OS, c(0.25), type = 6)
continuas_cronicos$Q3[3] = quantile(cronicos$t_OS, c(0.75), type = 6)

continuas_criticos = data.frame(Variable = c("Edad", "ICU_LoS", "Onset_Adm"),
                                Mediana = c(NA, NA, NA), 
                                Q1 = c(NA, NA, NA),
                                Q3 = c(NA, NA, NA))

continuas_criticos$Mediana[1] = median(criticos$Edad)
continuas_criticos$Q1[1] = quantile(criticos$Edad, c(0.25), type = 6)
continuas_criticos$Q3[1] = quantile(criticos$Edad, c(0.75), type = 6)

continuas_criticos$Mediana[2] = median(criticos$t)
continuas_criticos$Q1[2] = quantile(criticos$t, c(0.25), type = 6)
continuas_criticos$Q3[2] = quantile(criticos$t, c(0.75), type = 6)

continuas_criticos$Mediana[3] = median(criticos$t_OS)
continuas_criticos$Q1[3] = quantile(criticos$t_OS, c(0.25), type = 6)
continuas_criticos$Q3[3] = quantile(criticos$t_OS, c(0.75), type = 6)

descriptivo_continuas = list(continuas, continuas_cronicos, continuas_criticos)
names(descriptivo_continuas) = c("Continuas", "Cr?nicos", "Cr?ticos")
rm(continuas, continuas_cronicos, continuas_criticos)

###########################################
## 1.3. Resumen descriptivo (Sexo)       ##
###########################################
sexo_total = plyr::count(dataset$Sexo)
colnames(sexo_total) = c("Sexo", "Freq")
sexo_total$Share = (sexo_total$Freq/sum(sexo_total$Freq))*100

sexo_cronicos = plyr::count(cronicos$Sexo)
colnames(sexo_cronicos) = c("Sexo", "Freq")
sexo_cronicos$Share = (sexo_cronicos$Freq/sum(sexo_cronicos$Freq))*100

sexo_criticos = plyr::count(criticos$Sexo)
colnames(sexo_criticos) = c("Sexo", "Freq")
sexo_criticos$Share = (sexo_criticos$Freq/sum(sexo_criticos$Freq))*100

descriptivo_sexo = list(sexo_total, sexo_cronicos, sexo_criticos)
names(descriptivo_sexo) = c("Total", "Cr?nicos", "Cr?ticos")
rm(sexo_total, sexo_criticos, sexo_cronicos)


##########################################
## 1.4. Resumen descriptivo (outcome)   ##
##########################################

outcome_total = plyr::count(dataset$outcome)
colnames(outcome_total) = c("Outcome", "Freq")
outcome_total$Share = (outcome_total$Freq/sum(outcome_total$Freq))*100

outcome_cronicos = plyr::count(cronicos$outcome)
colnames(outcome_cronicos) = c("Outcome", "Freq")
outcome_cronicos$Share = (outcome_cronicos$Freq/sum(outcome_cronicos$Freq))*100

outcome_criticos = plyr::count(criticos$outcome)
colnames(outcome_criticos) = c("Outcome", "Freq")
outcome_criticos$Share = (outcome_criticos$Freq/sum(outcome_criticos$Freq))*100

descriptivo_outcome = list(outcome_total, outcome_cronicos, outcome_criticos)
names(descriptivo_outcome) = c("Total", "Cr?nicos", "Cr?ticos")
rm(outcome_total, outcome_cronicos, outcome_criticos)

############################################
## 1.5. Mann-Whitney U test (Sexo)       ##
############################################

mann_whitney = data.frame(Variable = c("Age", "LoS ICU", "SO_ICU"),
                          Sexo_W = c(NA, NA, NA), Sexo_p = c(NA, NA, NA),
                          Cronico_W = c(NA, NA, NA), Cronico_p = c(NA, NA, NA))

# Edad
sexo_edad_m = as.vector((dataset %>% filter(Sexo == 0))$Edad)
sexo_edad_f = as.vector((dataset %>% filter(Sexo == 1))$Edad)

wilcox_sexo_edad = wilcox.test(sexo_data_m, sexo_data_f)
mann_whitney$Sexo_W[1] = wilcox_sexo_edad$statistic
mann_whitney$Sexo_p[1] = wilcox_sexo_edad$p.value

# LoS ICU
sexo_LoS_m = as.vector((dataset %>% filter(Sexo == 0))$t)
sexo_LoS_f = as.vector((dataset %>% filter(Sexo == 1))$t)

wilcox_sexo_LoS = wilcox.test(sexo_LoS_m, sexo_LoS_f)
mann_whitney$Sexo_W[2] = wilcox_sexo_LoS$statistic
mann_whitney$Sexo_p[2] = wilcox_sexo_LoS$p.value

# Time from SO to ICU admission
sexo_OS_m = as.vector((dataset %>% filter(Sexo == 0))$t_OS)
sexo_OS_f = as.vector((dataset %>% filter(Sexo == 1))$t_OS)

wilcox_sexo_OS = wilcox.test(sexo_OS_m, sexo_OS_f)
mann_whitney$Sexo_W[3] = wilcox_sexo_OS$statistic
mann_whitney$Sexo_p[3] = wilcox_sexo_OS$p.value

############################################
## 1.6. Mann-Whitney U test (Cr?nicos)    ##
############################################

# Edad
cronicos_edad = as.vector(cronicos$Edad)
criticos_edad = as.vector(criticos$Edad)

wilcox_cc_edad = wilcox.test(cronicos_edad, criticos_edad)
mann_whitney$Cronico_W[1] = wilcox_cc_edad$statistic
mann_whitney$Cronico_p[1] = wilcox_cc_edad$p.value

# LoS ICU
cronicos_LoS = as.vector(cronicos$t)
criticos_LoS = as.vector(criticos$t)

wilcox_cc_LoS = wilcox.test(cronicos_LoS, criticos_LoS)
mann_whitney$Cronico_W[2] = wilcox_cc_LoS$statistic
mann_whitney$Cronico_p[2] = wilcox_cc_LoS$p.value

# Time from SO to ICU admission
cronicos_SO = as.vector(cronicos$t_OS)
criticos_SO = as.vector(criticos$t_OS)

wilcox_cc_SO = wilcox.test(cronicos_SO, criticos_SO)
mann_whitney$Cronico_W[3] = wilcox_cc_SO$statistic
mann_whitney$Cronico_p[3] = wilcox_cc_SO$p.value

##########################
## 1.7. Chi-2 test      ##
##########################
chi_2 = data.frame(V1 = c("Sexo", "Sexo", "Cronico"), 
                   V2 = c("Cronico", "Outcome", "Outcome"),
                   st = c(NA, NA, NA),
                   df = c(NA, NA, NA),
                   p = c(NA, NA, NA))

# Sexo - cronico
chi_sexo_cronico = chisq.test(dataset$Sexo, dataset$Cronico)
chi_2$st[1] = chi_sexo_cronico$statistic
chi_2$df[1] = chi_sexo_cronico$parameter
chi_2$p[1] = chi_sexo_cronico$p.value

# Sexo - outcome 
chi_sexo_outcome = chisq.test(dataset$Sexo, dataset$outcome)
chi_2$st[2] = chi_sexo_outcome$statistic
chi_2$df[2] = chi_sexo_outcome$parameter
chi_2$p[2] = chi_sexo_outcome$p.value

# Cronico-outcome
chi_cronico_outcome = chisq.test(dataset$Cronico, dataset$outcome)
chi_2$st[3] = chi_cronico_outcome$statistic
chi_2$df[3] = chi_cronico_outcome$parameter
chi_2$p[3] = chi_cronico_outcome$p.value

################################
## 1.8. Fisher's exact test   ##
################################
fisher = data.frame(V1 = c("Sexo", "Sexo", "Cronico"), 
                   V2 = c("Cronico", "Outcome", "Outcome"),
                  p = c(NA, NA, NA),
                   odds_ratio = c(NA, NA, NA),
                   ci_1 = c(NA, NA, NA),
                  ci_2 = c(NA, NA, NA))

# Sexo - cronico
fisher_sexo_cronico = fisher.test(dataset$Sexo, dataset$Cronico)
fisher$p[1] = fisher_sexo_cronico$p.value
fisher$odds_ratio[1] = fisher_sexo_cronico$estimate
fisher$ci_1[1] = fisher_sexo_cronico$conf.int[1]
fisher$ci_2[1] = fisher_sexo_cronico$conf.int[2]

# Sexo - outcome 
fisher_sexo_outcome = fisher.test(dataset$Sexo, dataset$outcome)
fisher$p[2] = fisher_sexo_outcome$p.value
fisher$odds_ratio[2] = NA
fisher$ci_1[2] = NA
fisher$ci_2[2] = NA

# Cronico-outcome
fisher_cronico_outcome = fisher.test(dataset$Cronico, dataset$outcome)
fisher$p[3] = fisher_cronico_outcome$p.value
fisher$odds_ratio[3] = NA
fisher$ci_1[3] = NA
fisher$ci_2[3] = NA

