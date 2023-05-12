

######################################
## Procedimiento de Lawless (1980). ##
##    Inference in the Generalized  ##
##         Gamma Distribution       ##
######################################

# Datos
log_lawless = c(2.884, 3.365,3.497,3.726, 3.741,3.820,
            3.881, 3.948, 3.950, 3.991, 4.017, 4.217,
            4.229, 4.229, 4.232, 4.432, 4.534, 4.591, 4.655,
            4.662, 4.851, 4.852, 5.156)

lawless=log_lawless
for (k in 1:23) {
  lawless[k] = exp(log_lawless[k])
}

k = 10
mu = 4.232
sigma = 0.510
p = 0.5

# para Qp, i.e. p = 0.5, se calcula Q(10,0.5)
Q_k = sqrt(k)*log((1/(2*k))*qchisq(p,df = 2*k, lower.tail = FALSE))

# Valor en Qp (yp) para yp = mu + sigma(Q_k)
y_p = mu + sigma*Q_k

# Ancillaries (a1, ..., a23)
a = vector(length = 23)
for (i in 1:23) {
  a[i] = (log_lawless[i] - mu)/sigma
}

# Constant C(a,23,10): numerical integration
install.packages("rmutil")
library(rmutil)
i = 1:23
num_1 = (x^{n-2})
num_2 = (exp(sqrt(k)*(x-1)*sum(a)))
den = ((1/n)*(sum(exp((a[i]*x)/sqrt(k)))))^n
n = 23
integrand = function(x){((x^{n-2})*(exp(sqrt(k)*(x-1)*sum(a))))/(((1/n)*(sum(exp((a[i]*x)/sqrt(k)))))^n)}
int(integrand,a = 0, b = Inf, eps = 0.0001)


############################
## Procedimiento directo  ##
############################



