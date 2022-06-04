# Problema 1 ------------------------------------------------------------------------

## a) Carga de datos y gráfico de dispersión
base <- read.csv('Datasets/poblacion.csv')
x    <- base$x
y    <- base$y

plot(x, y, pch = 19,
     bty = "n",
     las = 1,
     xlab = "Horas",
     ylab = "Número de organismos")

## b) Definición de funciones

f1 <- function(x, lambda, beta, k){
 return(lambda - log(1 + beta*exp(-k*x)))
}

## c) Valores iniciales

f1(x, 20, 17, -1/4)
curve(f1(x, 20, 1, 1), add = T, col = "red")

## d) Ajuste modelo

mod <- nls(y ~ lambda - log(1 + beta*exp(-k*x)),
           start = list(lambda = 20, beta=1, k=1))
summary(mod)

## e) Gráfico curva ajustada

coef(mod)
lambda <- coef(mod)[1]
beta   <- coef(mod)[2]
k      <- coef(mod)[3]
curve(f1(x, lambda, beta, k), add = T, col = "purple")

sigma2   <- summary(mod)$sigma^2
B        <- summary(mod)$cov.unscaled[2,2]
sd.beta  <- sqrt(sigma2*B)
t        <- abs(beta)/sd.beta
t
n        <- length(x)
p        <- length(coef(mod))

valor.p <- 2*(1 - pt(t, n - p))
valor.p
summary(mod)

# Problema 2 ------------------------------------------------------------------

## a)

x <- c(9, 14, 21, 28, 42, 57, 63, 70, 79)
y <- c(8.93, 10.8, 18.59, 22.33, 39.35, 56.11, 61.73, 64.62, 67.08)
datos <- as.data.frame(cbind(x,y))

plot(x,y, xlab = "",
     ylab = "",
     las = 1,
     bty = "n",
     pch = 19,
     main = "Tiempo vs. Rendimiento")

# Modelo lineal clásico

modelo_lineal <- lm(y ~ x)
abline(modelo_lineal)
summary(modelo_lineal)

y.hat    <- modelo_lineal$fitted.values
residuos <- modelo_lineal$residuals
plot(modelo_lineal, which = 1)
plot(y.hat, residuos, type = "l")

# No es adecuado ajustar un modelo lineal.

# Parte b) 

# Modelo de crecimiento logístico

f.cl <- function(x,alpha,beta,k){
 return(alpha/(1+beta*exp(-k*x)))
}

curve(f.cl(x,90,9,0.04), add = T) # pueden servir de partida

# beta = depende del tamaño inicial de la población.
# x = 0 -> f = alpha/(1+beta)
# jugar con k.

modelo_cl  <- nls(y ~ alpha/(1+beta*exp(-k*x)),
                 start=list(alpha = 90,beta = 9, k = 0.04))
coef(modelo_cl)
summary(modelo_cl)

alpha <- coef(modelo_cl)[1]
beta  <- coef(modelo_cl)[2]
k     <- coef(modelo_cl)[3]

curve(f.cl(x,alpha,beta,k), add = T, col = "red")

# Parte c)

# Modelo Weibull

f.w    <- function(x,theta1,theta2,theta3,theta4){
 return(theta1- theta2*exp(-exp(theta3+theta4*log(x))))
}

# Parte d)

modelo_w  <- nls2(y ~ theta1- theta2*exp(-exp(theta3+theta4*log(x))),start=list(theta1=90,theta2=65,theta3=-9, theta4 = 2))

coef(modelo_w)

theta1 <- coef(modelo_w)[1]
theta2 <- coef(modelo_w)[2]
theta3 <- coef(modelo_w)[3]
theta4 <- coef(modelo_w)[4]

curve(f.w(x,theta1,theta2,theta3,theta4), add = T, col = "blue")
summary(modelo_w)

# Parte e)

# Análisis de supuestos:

# Función de media bien especificada:

library(nlstools)
residuos_cl <- nlsResiduals(modelo_cl)
residuos_w  <- nlsResiduals(modelo_w)
plot(residuos_cl, which = 1)
plot(residuos_w, which = 1)

# Homocedasticidad:

plot(residuos_cl, which = 2)
plot(residuos_w, which = 2)

# Distr. Normal

r_cl <- residuos_cl$resi2[,2]
r_w  <- residuos_w$resi2[,2]

r_cl[abs(r_cl) >= qnorm(0.975)]
r_w[abs(r_w) >= qnorm(0.975)]

plot(residuos_cl, which = 6)
plot(residuos_w, which = 6)

AIC(modelo_cl,modelo_w)
