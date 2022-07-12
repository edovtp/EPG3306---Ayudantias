library(tidyverse)
library(rio)
library(corrplot)


# Ejercicio 2 -----------------------------------------------------------------------

## a) INICIO DEL ANÁLISIS

### Carga de datos
datos_pobreza <- rio::import('Datasets/poverty.txt')
head(datos_pobreza)

### Identificación de variables
x <- datos_pobreza$PovPct
y <- datos_pobreza$Brth15to17

### Gráfico de dispersión
ggplot(data = datos_pobreza) +
  geom_point(mapping = aes(x = PovPct, y = Brth15to17),
             size = 2, col = 'royalblue4') +
  labs(x = 'Tasa de pobreza', y = 'Tasa de Natalidad (15-17) años',
       title = 'Asociación entre Tasa de Natalidad (15 a 17 años) y Tasa de Pobreza') +
  theme_minimal()

## b) CORRELACIÓN

# A partir del gráfico anterior, se ve una asociación positiva, esto es,
# a mayor pobreza, mayor tasa de natalidad.

?cor
cor(x, y) # Correlación positiva.

## c) MODELO

# y_i = \beta_0 + \beta1*x_i + e_i, con e_i ~ Normal(0,sigma^2) iid.
# y_i ~ Normal(\beta_0 + \beta1*x_i, sigma^2) con y_i independientes.

## d) ESTIMACIONES DE MÍNIMOS CUADRADOS

hat_beta1 <- sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2)
hat_beta1

hat_beta0 <- mean(y) - hat_beta1 * mean(x)
hat_beta0

(hat_betas <- c(hat_beta0, hat_beta1))

# hat(y_i) = hat_beta0 + hat_beta1*x_i

hat.y = hat_beta0 + hat_beta1 * x

## e) AJUSTE DEL MODELO EN R

modelo_p2 <- lm(y ~ x)
coef(modelo_p2)

## f) SIGNIFICANCIA DE LA PENDIENTE (BETA1)

residuos <- resid(modelo_p2)
hat.y_lm <- modelo_p2$fitted.values

(n <- nrow(datos_pobreza))
(hat.sigma2 <- sum(residuos^2) / (n - 2))

Sxx <- sum((x - mean(x))^2)

alpha <- 0.05
(t0 <- abs(coef(modelo_p2)[2]) / (sqrt(hat.sigma2) / sqrt(Sxx)))
t0 >= qt(1 - alpha/2, df = n - 2)

## g) VALOR P

valor.p <- 2 * (1 - pt(t0, df = n - 2))
valor.p 

## h) USANDO SUMMARY

summary(modelo_p2)

## i) AJUSTE

x_california <- x[datos_pobreza$Location == "California"]
hat.y_california = coef(modelo_p2)[1] + coef(modelo_p2)[2] * x_california

ggplot(data = datos_pobreza) +
  geom_point(mapping = aes(x = PovPct, y = Brth15to17),
             size = 2, col = 'royalblue4') +
  labs(x = 'Tasa de pobreza', y = 'Tasa de Natalidad (15-17) años',
       title = 'Asociación entre Tasa de Natalidad (15 a 17 años) y Tasa de Pobreza') +
  theme_minimal() +
  geom_abline(intercept = coef(modelo_p2)[1], slope = coef(modelo_p2)[2],
              col = 'tomato', lwd = 1.3)

# Ejercicio 2 -----------------------------------------------------------------------

## a) ANÁLISIS INICIAL

datos_grasas <- rio::import('Datasets/grasas.csv')
head(datos_grasas)

### Revisamos la estructura de los datos
str(datos_grasas)

### Gráfico de correlación
matriz_corr_grasas = cor(datos_grasas[, -1])

corrplot::corrplot(matriz_corr_grasas, method = "number")
corrplot::corrplot(matriz_corr_grasas, method = "circle")
corrplot::corrplot(matriz_corr_grasas, method = "color")

# Obesidad vs Desnutrición:
# plot(data2$Obesity, data2$Undernourished)
# summary(lm(data2$Undernourished ~ data2$Obesity))
# abline(lm(data2$Undernourished ~ data2$Obesity))

ggplot(data = datos_grasas) +
  geom_point(mapping = aes(x = `Animal Products`, y = Meat),
             size = 2, col = 'royalblue4') +
  labs(x = 'Tasa de ingesta de grasa de productos de origen animal',
       y = 'Tasa de consumo de carne',
       title = 'Consumo de carne y grasa de productos de origen animal') +
  theme_minimal()

## b) ESCOGER UN MODELO

x <- datos_grasas$`Animal Products`
y <- datos_grasas$Meat

# y = beta0 + beta1*x + e, con e_i ~ Normal(0, sigma2)
# E(y | x) = beta0 + beta1*x

## c) AJUSTE DEL MODELO

modelo_p3 = lm(y ~ x)
summary(modelo_p3)

beta.hat <- coef(modelo_p3)
beta.hat 

# E(y|x) = 1.519 + 0.36*x

# En promedio, cuando la tasa de ingesta de grasa por productos animales es 0,
# la tasa de consumo de carne es 1.519.

# Cuando la tasa de ingesta de grasa por productos animales aumenta en una unidad
# (un porcentaje), en promedio, la tasa de consumo de carne aumenta en 1.519
# unidades (porcentaje).

ggplot(data = datos_grasas) +
  geom_point(mapping = aes(x = `Animal Products`, y = Meat),
             size = 2, col = 'royalblue4') +
  labs(x = 'Tasa de ingesta de grasa de productos de origen animal',
       y = 'Tasa de consumo de carne',
       title = 'Consumo de carne y grasa de productos de origen animal') +
  theme_minimal() +
  geom_abline(intercept = coef(modelo_p3)[1],
              slope = coef(modelo_p3)[2],
              col = 'salmon', lwd = 1.3)

## d) TEST F

SCE <- sum((resid(modelo_p3)^2))
SCR <- sum((modelo_p3$fitted.values - mean(y))^2) 
SCT <- sum((y - mean(y))^2)
n <- nrow(datos_grasas)

F0 <- (n - 2) * SCR / SCE
F0

alpha <- 0.05
valor.p <- 1 - pf(F0, df1 = 1, df2 = n - 2)
valor.p

anova(modelo_p3)

## e) COEFICIENTE DE DETERMINACIÓN

R2 <- SCR / SCT
R2

summary(modelo_p3)

rxy <- cor(x, y)^2 
rxy

# El modelo explica el 52% de la variabilidad de los datos.
