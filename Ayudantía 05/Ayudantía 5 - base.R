library(ggfortify)
library(lmtest)


# Pregunta 1 ------------------------------------------------------------------------

## a)
# Carga de datos
datos_kung <- read.csv(file = 'Datasets/!Kung.csv',
                       sep = ';')

# Análisis exploratorio
dim(datos_kung)
str(datos_kung)
summary(datos_kung)

plot(x = datos_kung$weight,
     y = datos_kung$height,
     pch = 16, main = 'Relación peso y altura pueblo !Kung San',
     xlab = 'Peso', ylab = 'Altura')

## b) Ajuste del modelo
modelo_kung1 <- lm(formula = height ~ weight,
                   data = datos_kung)
summary(modelo_kung1)

plot(x = datos_kung$weight,
     y = datos_kung$height,
     pch = 16, main = 'Relación peso y altura pueblo !Kung San',
     xlab = 'Peso', ylab = 'Altura')
abline(modelo_kung1, lwd = 3, col = 'red', lty = 'dashed')

## c) Supuestos del modelo

## Necesitamos residuos, valores ajustados, residuos estandarizados
## y residuos studentizados

## A mano
ajustados_kung1 <- fitted(modelo_kung1)
resid_kung1 <- datos_kung$height - ajustados_kung1

hii <- hatvalues(modelo_kung1)
sigma <- summary(modelo_kung1)$sig
std_resid_kung1 <- resid_kung1 / (sigma * sqrt(1 - hii))

n <- nrow(datos_kung)
sigma_hat_i <- sqrt(sigma^2 * (n - 2 - std_resid_kung1^2) / (n - 3))
stu_resid_kung1 <- resid_kung1 / (sigma_hat_i * sqrt(1 - hii))

## Directo de R
resid_kung2 <- residuals(modelo_kung1)
std_resid_kung2 <- rstandard(modelo_kung1)
stu_resid_kung2 <- rstudent(modelo_kung1) 

## Comparación
head(resid_kung1) 
head(resid_kung2) 

head(std_resid_kung1)
head(std_resid_kung2)

head(stu_resid_kung1)
head(stu_resid_kung2)

datos_aux <- data.frame(peso = datos_kung$weight,
                    ajustados = ajustados_kung1,
                    residuos = resid_kung1,
                    residuos_est = std_resid_kung1,
                    residuos_stu = stu_resid_kung1)

# 1. Outliers
plot(x = datos_aux$peso,
     y = datos_aux$residuos_stu,
     pch = 16, main = 'Presencia de outliers',
     xlab = 'Peso', ylab = 'Residuos studentizados')
abline(h = qt(0.025, df = n - 3), lty = 'dashed', col = 'red', lwd = 2)
abline(h = qt(0.975, df = n - 3), lty = 'dashed', col = 'red', lwd = 2)

# 2. Media lineal
plot(x = datos_aux$peso,
     y = datos_aux$residuos,
     pch = 16, main = 'Presencia de outliers',
     xlab = 'Peso', ylab = 'Residuos')
abline(h = 0, lty = 'dashed', col = 'red', lwd = 2)

plot(x = datos_aux$ajustados,
     y = datos_aux$residuos,
     pch = 16, main = 'Presencia de outliers',
     xlab = 'Peso', ylab = 'Residuos')
abline(h = 0, lty = 'dashed', col = 'red', lwd = 2)

# 3. Homocedasticidad
plot(x = datos_aux$peso,
     y = datos_aux$residuos_est,
     pch = 16, main = 'Presencia de outliers',
     xlab = 'Peso', ylab = 'Residuos estandarizados')

## Test de Breusch-Pagan
## H0: Varianza constante
lmtest::bptest(modelo_kung1)

# 4. Normalidad
hist(x = datos_aux$residuos_est,
     col = 'turquoise', main = 'Supuesto de normalidad',
     xlab = 'Residuos estandarizados', ylab = 'Densidad',
     freq = FALSE)
lines(density(datos_aux$residuos_est))
polygon(density(datos_aux$residuos_est),
        col = adjustcolor('salmon', alpha.f = 0.4))

qqnorm(datos_aux$residuos_est,
       pch = 16,
       main = 'Supuesto de normalidad',
       xlab = 'Cuantiles teóricos', ylab = 'Cuantiles muestrales')
qqline(datos_aux$residuos_est, lwd = 2, col = 'red', lty = 'dashed')

## Test de Shapiro-Wilk
## H0: La muestra es normal estándar
shapiro.test(std_resid_kung1)

## Gráficos que entrega R
plot(modelo_kung1)


## d.1) Usando solo adultos
datos_kung_adultos <- datos_kung[datos_kung$age >= 18, ]

dim(datos_kung_adultos)
str(datos_kung_adultos)
summary(datos_kung_adultos)

plot(x = datos_kung_adultos$weight,
     y = datos_kung_adultos$height,
     pch = 16, main = 'Relación peso y altura pueblo !Kung San (adultos)',
     xlab = 'Peso', ylab = 'Altura')

## Ajuste del modelo
modelo_kung2 <- lm(formula = height ~ weight,
                   data = datos_kung_adultos)
summary(modelo_kung2)

plot(x = datos_kung_adultos$weight,
     y = datos_kung_adultos$height,
     pch = 16, main = 'Relación peso y altura pueblo !Kung San (adultos)',
     xlab = 'Peso', ylab = 'Altura')
abline(modelo_kung2, lwd = 3, col = 'red', lty = 'dashed')

## Supuestos del modelo

## Necesitamos residuos, valores ajustados, residuos estandarizados
## y residuos studentizados

## Directo de R
ajustados_kung1 <- fitted(modelo_kung2)
resid_kung1 <- residuals(modelo_kung2)
std_resid_kung1 <- rstandard(modelo_kung2)
stu_resid_kung1 <- rstudent(modelo_kung2)

datos_aux <- tibble(peso = datos_kung_adultos$weight,
                    ajustados = ajustados_kung1,
                    residuos = resid_kung1,
                    residuos_est = std_resid_kung1,
                    residuos_stu = stu_resid_kung1)

# 1. Outliers
plot(x = datos_aux$peso,
     y = datos_aux$residuos_stu,
     pch = 16, main = 'Presencia de outliers',
     xlab = 'Peso', ylab = 'Residuos studentizados')
abline(h = qt(0.025, df = n - 3), lty = 'dashed', col = 'red', lwd = 2)
abline(h = qt(0.975, df = n - 3), lty = 'dashed', col = 'red', lwd = 2)

# 2. Media lineal
plot(x = datos_aux$peso,
     y = datos_aux$residuos,
     pch = 16, main = 'Presencia de outliers',
     xlab = 'Peso', ylab = 'Residuos')
abline(h = 0, lty = 'dashed', col = 'red', lwd = 2)

plot(x = datos_aux$ajustados,
     y = datos_aux$residuos,
     pch = 16, main = 'Presencia de outliers',
     xlab = 'Peso', ylab = 'Residuos')
abline(h = 0, lty = 'dashed', col = 'red', lwd = 2)

# 3. Homocedasticidad
plot(x = datos_aux$peso,
     y = datos_aux$residuos_est,
     pch = 16, main = 'Presencia de outliers',
     xlab = 'Peso', ylab = 'Residuos estandarizados')

## Test de Breusch-Pagan
## H0: Varianza constante
lmtest::bptest(modelo_kung2)

# 4. Normalidad
hist(x = datos_aux$residuos_est,
     col = 'turquoise', main = 'Supuesto de normalidad',
     xlab = 'Residuos estandarizados', ylab = 'Densidad',
     freq = FALSE)
lines(density(datos_aux$residuos_est))
polygon(density(datos_aux$residuos_est),
        col = adjustcolor('salmon', alpha.f = 0.4))

qqnorm(datos_aux$residuos_est,
       pch = 16,
       main = 'Supuesto de normalidad',
       xlab = 'Cuantiles teóricos', ylab = 'Cuantiles muestrales')
qqline(datos_aux$residuos_est, lwd = 2, col = 'red', lty = 'dashed')

## Test de Shapiro-Wilk
## H0: La muestra es normal estándar
shapiro.test(std_resid_kung1)

## Gráficos que entrega R
plot(modelo_kung2)

## d.2) Usando el logaritmo del peso
datos_kung_log <- datos_kung %>% 
  dplyr::mutate(logweight = log(weight))

dim(datos_kung_log)
str(datos_kung_log)
summary(datos_kung_log)

ggplot(data = datos_kung_log,
       mapping = aes(x = logweight, y = height)) +
  geom_point() +
  labs(title = 'Relación log-peso y altura pueblo !Kung San',
       x = 'Log-peso', y = 'Altura')

## Ajuste del modelo
modelo_kung3 <- lm(formula = height ~ logweight,
                   data = datos_kung_log)
summary(modelo_kung3)

ggplot(data = datos_kung_log,
       mapping = aes(x = logweight, y = height)) +
  geom_point() +
  geom_abline(slope = modelo_kung3$coefficients[2],
              intercept = modelo_kung3$coefficients[1],
              col = 'red', lwd = 1.2, lty = 'dashed') + 
  labs(title = 'Relación log-peso y altura pueblo !Kung San',
       x = 'Peso', y = 'Altura')


## Supuestos del modelo

## Necesitamos residuos, valores ajustados, residuos estandarizados
## y residuos studentizados

## Directo de R
ajustados_kung1 <- fitted(modelo_kung3)
resid_kung1 <- residuals(modelo_kung3)
std_resid_kung1 <- rstandard(modelo_kung3)
stu_resid_kung1 <- rstudent(modelo_kung3)

datos_aux <- tibble(logpeso = datos_kung_log$logweight,
                    ajustados = ajustados_kung1,
                    residuos = resid_kung1,
                    residuos_est = std_resid_kung1,
                    residuos_stu = stu_resid_kung1)

# 1. Outliers
plot(x = datos_aux$logpeso,
     y = datos_aux$residuos_stu,
     pch = 16, main = 'Presencia de outliers',
     xlab = 'log peso', ylab = 'Residuos studentizados')
abline(h = qt(0.025, df = n - 3), lty = 'dashed', col = 'red', lwd = 2)
abline(h = qt(0.975, df = n - 3), lty = 'dashed', col = 'red', lwd = 2)

# 2. Media lineal
plot(x = datos_aux$logpeso,
     y = datos_aux$residuos,
     pch = 16, main = 'Presencia de outliers',
     xlab = 'log peso', ylab = 'Residuos')
abline(h = 0, lty = 'dashed', col = 'red', lwd = 2)

plot(x = datos_aux$ajustados,
     y = datos_aux$residuos,
     pch = 16, main = 'Presencia de outliers',
     xlab = 'log peso', ylab = 'Residuos')
abline(h = 0, lty = 'dashed', col = 'red', lwd = 2)

# 2. Homocedasticidad
plot(x = datos_aux$logpeso,
     y = datos_aux$residuos_est,
     pch = 16, main = 'Presencia de outliers',
     xlab = 'Peso', ylab = 'Residuos estandarizados')

## Test de Breusch-Pagan
## H0: Varianza constante
lmtest::bptest(modelo_kung3)

# 3. Normalidad
hist(x = datos_aux$residuos_est,
     col = 'turquoise', main = 'Supuesto de normalidad',
     xlab = 'Residuos estandarizados', ylab = 'Densidad',
     freq = FALSE)
lines(density(datos_aux$residuos_est))
polygon(density(datos_aux$residuos_est),
        col = adjustcolor('salmon', alpha.f = 0.4))

qqnorm(datos_aux$residuos_est,
       pch = 16,
       main = 'Supuesto de normalidad',
       xlab = 'Cuantiles teóricos', ylab = 'Cuantiles muestrales')
qqline(datos_aux$residuos_est, lwd = 2, col = 'red', lty = 'dashed')

## Test de Shapiro-Wilk
## H0: La muestra es normal estándar
shapiro.test(std_resid_kung1)

# Gráficos que entrega R
plot(modelo_kung3)
ggplot2::autoplot(modelo_kung3)
