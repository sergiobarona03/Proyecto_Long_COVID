
############################
## ICU LoS: análisis      ##
##     de distribución    ##
############################

source(here::here("Paper_resultados/Descriptive_analysis/05_Chi_2_Fisher/",
                  "Fisher_test.R"))

# Cargar datos
library(here)
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

source(here::here("Paper_resultados/Descriptive_analysis/05_Chi_2_Fisher", "Fisher_test.R"))

# En lo sucesivo, el análisis opera con datos no-censurados
dataset$censura_aux = NA

for (i in 1: nrow(dataset)) {
  if (dataset$outcome[i] == "Censored") {
    dataset$censura_aux[i] = 0
  } else {dataset$censura_aux[i] = 1}
}

dataset = dataset %>% filter(censura_aux == 1)

# Considérese tres distrbuciones: Weibull, 
# exponencial, gamma generalizada, log-normal, gamma

# Función de densidad
ggplot(dataset, aes(x=t_UCI_desc)) + geom_histogram(aes(y=..density..),
                 colour = "black",
                 fill = "white", bins = 35) + geom_density(alpha=0.4, fill = "#FF6666") 

# Cullen-Fray graph
descdist(dataset$t_UCI_desc, discrete = FALSE)

#############################################
## Distribución Gamma, Weibull, Log-Normal ##
#############################################
plot_legend <- c("Generalized Gamma", "Weibull", "Log-Normal", 
                 "Log-logis", "Gompertz")

# Hitorgram vs theoretical densities
png(file = "Paper_resultados/Parametric_model/LoS/Output/Model_selection/Theor_densities.png",
    width = 558, height = 407)
denscomp(list(fitdist(dataset$t_UCI_desc, "gengamma",
                      start = list(mu = 1, sigma = 1, Q = 1)), 
              fitdist(dataset$t_UCI_desc, "weibull"),
              fitdist(dataset$t_UCI_desc, "lnorm"),
              fitdist(dataset$t_UCI_desc, "llogis"), 
              fitdist(dataset$t_UCI_desc, distr = "gompertz",
                      start = list(shape =1, rate = 1), 
                      lower = c(0,0))), 
         legendtext = c("Generalized Gamma", "Weibull", 
                        "Log-Normal", "Log-logis", "Gompertz"))
dev.off()

# QQ plot
png(file = "Paper_resultados/Parametric_model/LoS/Output/Model_selection/QQ_plot.png",
    width = 558, height = 407)
qqcomp(list(fitdist(dataset$t_UCI_desc, "gengamma",
                    start = list(mu = 1, sigma = 1, Q = 1)), 
            fitdist(dataset$t_UCI_desc, "weibull"), 
            fitdist(dataset$t_UCI_desc, "lnorm"), 
            fitdist(dataset$t_UCI_desc, "llogis"), 
            fitdist(dataset$t_UCI_desc, distr = "gompertz",
                    start = list(shape =1, rate = 1), 
                    lower = c(0,0))), legendtext = c("Generalized Gamma", "Weibull", 
                      "Log-Normal", "Log-logis", "Gompertz"))

dev.off()

# Empirical vs theoretical CDFs
png(file = "Paper_resultados/Parametric_model/LoS/Output/Model_selection/Theor_CDF.png",
    width = 558, height = 407)
cdfcomp(list(fitdist(dataset$t_UCI_desc, "gengamma",
                     start = list(mu = 1, sigma = 1, Q = 1)), 
             fitdist(dataset$t_UCI_desc, "weibull"),
             fitdist(dataset$t_UCI_desc, "lnorm"),
             fitdist(dataset$t_UCI_desc, "llogis"), 
             fitdist(dataset$t_UCI_desc, distr = "gompertz",
                     start = list(shape =1, rate = 1), 
                     lower = c(0,0))),
        legendtext = c("Generalized Gamma", "Weibull", 
                       "Log-Normal", "Log-logis", "Gompertz"))

dev.off()

# PP plot
png(file = "Paper_resultados/Parametric_model/LoS/Output/Model_selection/PP_plot.png",
    width = 558, height = 407)
ppcomp(list(fitdist(dataset$t_UCI_desc, "gengamma",
                    start = list(mu = 1, sigma = 1, Q = 1)),
            fitdist(dataset$t_UCI_desc, "weibull"), 
            fitdist(dataset$t_UCI_desc, "lnorm"),
            fitdist(dataset$t_UCI_desc, "llogis"), 
            fitdist(dataset$t_UCI_desc, distr = "gompertz",
                    start = list(shape =1, rate = 1), 
                    lower = c(0,0))), 
       legendtext = c("Generalized Gamma", "Weibull", 
                      "Log-Normal", "Log-logis", "Gompertz"))

dev.off()
