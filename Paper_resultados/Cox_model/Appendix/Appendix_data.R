
#######################################
## Appendix: Semiparametric dataset  ##
#######################################

source(here::here("Paper_resultados/Descriptive_analysis/05_Chi_2_Fisher/",
                  "Fisher_test.R"))

library(survival) 
library(KMsurv) 
library(survMisc) 
library(survminer) 
library(ggfortify) 
library(flexsurv) 
library(actuar) 
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)
library(xts)
library(plyr)
library(dplyr)
library(writexl)
library(knitr)
library(moments)
library(fitdistrplus)
library(logspline)

# construcción de edad:
dataset_semiparametric = dataset %>% filter(age > 18)

# Sexo: el hazard ratio es 2 vs. 1, i.e., male vs. female
dataset_semiparametric$sex = as.character(dataset_semiparametric$sex)

for (i in 1:nrow(dataset_semiparametric)) {
  if (dataset_semiparametric$sex[i] == "0") {
    dataset_semiparametric$sex[i] = "2"
  }
  
  if (dataset_semiparametric$sex[i] == "1") {
    dataset_semiparametric$sex[i] = "1"
  }
}

dataset_semiparametric <- dataset_semiparametric %>% mutate(sex = factor(sex,
                                                                         levels = c("1", "2"),
                                                                         labels = c("Female", "Male")))




# Sexo: el hazard ratio es 2 vs. 1, i.e., CCI vs. non-CCI
dataset_semiparametric$CCI = as.character(dataset_semiparametric$CCI)

for (i in 1:nrow(dataset_semiparametric)) {
  if (dataset_semiparametric$CCI[i] == "CCI") {
    dataset_semiparametric$CCI[i] = "1"
  }
  
  if (dataset_semiparametric$CCI[i] == "non-CCI") {
    dataset_semiparametric$CCI[i] = "2"
  }
}

dataset_semiparametric <- dataset_semiparametric %>% mutate(CCI = factor(CCI,
                                                                         levels = c("1", "2"),
                                                                         labels = c("CCI", "non-CCI")))



