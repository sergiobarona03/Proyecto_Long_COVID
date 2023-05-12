
############################
## Selección de la        ##
##   distribución teórica ##
############################
source(here::here("Paper_resultados/Parametric_model/LoS/",
                  "B1_Fitting_LoS.R"))

###############################
##    Alternativa 1:         ##
## criterios de información  ##
###############################

weibull.fit = fitdist(dataset$t_UCI_desc, "weibull")
gamma.fit = fitdist(dataset$t_UCI_desc, "gengamma",
                    start = list(mu = 1, sigma = 1, Q = 1))
lnorm.fit = fitdist(dataset$t_UCI_desc, "lnorm")
llogis.fit = fitdist(dataset$t_UCI_desc, "llogis")
gompertz.fit = fitdist(dataset$t_UCI_desc, distr = "gompertz",
                       start = list(shape =1, rate = 1), 
                       lower = c(0,0))

lista_dist = list(weibull.fit, gamma.fit, lnorm.fit,
                  llogis.fit, gompertz.fit)

fitting = data.frame(Distribution = rep(NA,5),
                     AIC = rep(NA,5), BIC = rep(NA,5),
                     LL = rep(NA,5))

for (k in 1:length(lista_dist)) {
  df_aux = lista_dist[[k]]
  fitting$Distribution[k] = df_aux$distname
  fitting$AIC[k] = df_aux$aic
  fitting$BIC[k] = df_aux$bic
  fitting$LL[k] = df_aux$loglik
  rm(df_aux)
}

writexl::write_xlsx(fitting, 
                    "Paper_resultados/Parametric_model/LoS/Output/Model_selection/AIC.xlsx")

#####################################
##    Alternativa 2: prueba        ##
## simulada de Kolmogorov-Smirnov  ##
#####################################

n.sims <- 5e4

stats <- replicate(n.sims, {      
  r <- rweibull(n = length(dataset$t_UCI)
                , shape= weibull.fit$estimate["shape"]
                , scale = weibull.fit$estimate["scale"]
  )
  estfit.weibull <- fitdist(r, "weibull") # added to account for the estimated parameters
  as.numeric(ks.test(r
                     , "pweibull"
                     , shape= estfit.weibull$estimate["shape"]
                     , scale = estfit.weibull$estimate["scale"])$statistic
  )      
})

#calculando el valor p:
fit <- logspline(stats)

1 - plogspline(ks.test(x
                       , "pweibull"
                       , shape= fit.weibull$estimate["shape"]
                       , scale = fit.weibull$estimate["scale"])$statistic
               , fit
)

