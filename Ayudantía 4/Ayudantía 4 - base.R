
# Ejercicio 1 ---------------------------------------------------------------------------------
setwd("~/Desktop")
library(rio)
data = rio::import("poverty.txt")

#### Parte a ####

plot(data$PovPct, data$Brth15to17, 
     main = "Gráfico de Dispersión",
     col = "purple",
     las = 1,
     xlab = "Tasa de Pobreza",
     ylab = "Tasa de Natalidad (15-17) años",
     pch = 20,
     bty = "n")

x = data$PovPct
y = data$Brth15to17

#### Parte b ####

# Se ve una asociación. A mayor pobreza, mayor tasa de natalidad.

?cor()
cor(x, y) # Correlación positiva.

#### Parte c ####

# y_i = \beta_0 + \beta1*x_i + e_i, con e_i ~ Normal(0,sigma^2) iid.
# y_i ~ Normal(\beta_0 + \beta1*x_i, sigma^2) con y_i independientes.

#### Parte d #### 

hat_beta1 = sum( (x-mean(x))*(y-mean(y)) ) / sum( (x-mean(x))^2 )
hat_beta1

hat_beta0 = mean(y) - hat_beta1*mean(x)
hat_beta0

hat_betas = c(hat_beta0, hat_beta1)

# hat(y_i) = hat_beta0 + hat_beta1*x_i

hat.y = hat_beta0 + hat_beta1*x

#### parte e ####

modelo = lm(y ~ x)
coef(modelo)

#### parte f ####

residuos = resid(modelo)
hat.y_lm = modelo$fitted.values
n = dim(data)[1] ; n
hat.sigma2 = 1/(n-2)*sum((residuos)^2) ; hat.sigma2
Sxx = sum((x-mean(x))^2)
alpha = 0.05
t0 = abs(coef(modelo)[2])/(sqrt(hat.sigma2)/sqrt(Sxx)) ; t0
t0 >= qt(1-alpha/2, n-2) 

#### parte g ####

valor.p = 2*(1-pt(t0, df = (n-2)))
valor.p 

#### parte h ####

summary(modelo)

#### parte i ####

x.C = x[data$Location == "California"]
hat.y_C = coef(modelo)[1] + coef(modelo)[2]*x.C

plot(data$PovPct, data$Brth15to17, 
     main = "Gráfico de Dispersión",
     col = "purple",
     las = 1,
     xlab = "Tasa de Pobreza",
     ylab = "Tasa de Natalidad (15-17) años",
     pch = 20,
     bty = "n")

abline(modelo, col ="pink", lwd = 2.5)

# Ejercicio 2 ---------------------------------------------------------------------------------

#### Parte a #####

library(corrplot)

data2 = rio::import("grasas.csv")
str(data2)
C = cor(data2[,-1])
corrplot(C, method = "number")

# Obesidad vs Desnutrición:
# plot(data2$Obesity, data2$Undernourished)
# summary(lm(data2$Undernourished ~ data2$Obesity))
# abline(lm(data2$Undernourished ~ data2$Obesity))

plot(data2$`Animal Products`, data2$Meat)

#### Parte b ####

x = data2$`Animal Products`
y = data2$Meat

# y = beta0 + beta1*x + e, con e_i ~ Normal(0, sigma2)
# E(y | x) = beta0 + beta1*x

#### Parte c ####

modelo = lm(y ~ x)
summary(modelo)
beta.hat = coef(modelo) ; beta.hat 

# E(y|x) = 1.519 + 0.36*x

# En promedio, cuando la tasa de ingesta de grasa por productos aniamales es 0, la tasa de consumo de carne es 1.519.
# Cuando la tasa de ingesta de grasa por productos animales aumenta en una unidad (un porcentaje), en promedio, la tasa de consumo de carne aumenta en 1.519 unidades (porcentaje)

plot(data2$`Animal Products`, data2$Meat, 
     main = "Gráfico de Dispersión",
     col = "blue",
     las = 1,
     xlab = "Tasa de ingesta de Grasa de productos animales",
     ylab = "Tasa de consumo de Carne",
     pch = 20,
     bty = "n")

abline(modelo, col = 'red')

#### Parte d ####

SCE = sum((resid(modelo)^2))
SCR = sum((modelo$fitted.values-mean(y))^2) 
SCT = sum((y-mean(y))^2)
n = dim(data2)[1]
F0 = (n-2)*SCR/SCE ; F0
alpha = 0.05
valor.p = 1-pf(F0,1,n-2)
valor.p

#### Parte e ####

R2 = SCR/SCT ; R2 
summary(modelo)
rxy = cor(x,y)^2 

# el modelo explica el 52% de la variabilidad de los datos.
