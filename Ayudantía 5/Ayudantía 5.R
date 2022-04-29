library(tidyverse)
library(ggfortify)
library(lmtest)


# Pregunta 1 ------------------------------------------------------------------------

## a)
# Carga de datos
datos_kung <- readr::read_delim(
  file = "Datasets/!Kung.csv",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE)

# Análisis exploratorio
dim(datos_kung)
str(datos_kung)
summary(datos_kung)

ggplot(data = datos_kung,
       mapping = aes(x = weight, y = height)) +
  geom_point() +
  labs(title = 'Relación peso y altura pueblo !Kung San',
       x = 'Peso', y = 'Altura')

## b) Ajuste del modelo
modelo_kung1 <- lm(formula = height ~ weight,
                   data = datos_kung)
summary(modelo_kung1)

ggplot(data = datos_kung,
       mapping = aes(x = weight, y = height)) +
  geom_point() +
  geom_abline(slope = modelo_kung1$coefficients[2],
              intercept = modelo_kung1$coefficients[1],
              col = 'red', lwd = 1.2, lty = 'dashed') + 
  labs(title = 'Relación peso y altura pueblo !Kung San',
       x = 'Peso', y = 'Altura')

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

datos_aux <- tibble(peso = datos_kung$weight,
                    ajustados = ajustados_kung1,
                    residuos = resid_kung1,
                    residuos_est = std_resid_kung1,
                    residuos_stu = stu_resid_kung1)

# 1. Outliers
ggplot(data = datos_aux, mapping = aes(x = peso, y = residuos_stu)) +
  geom_point() +
  geom_hline(yintercept = qt(0.975, df = n - 3)) +
  geom_hline(yintercept = qt(0.025, df = n - 3))

# 2. Media lineal
ggplot(data = datos_aux) +
  geom_point(mapping = aes(x = peso, y = residuos)) +
  geom_hline(yintercept = 0, col = 'red',
             lty = 'dashed', lwd = 1.3) +
  labs(title = 'Supuesto de media lineal',
       x = 'Peso', y = 'Residuo')

ggplot(data = datos_aux) +
  geom_point(mapping = aes(x = ajustados, y = residuos)) +
  geom_hline(yintercept = 0, col = 'red',
             lty = 'dashed', lwd = 1.3) +
  labs(title = 'Supuesto de media lineal',
       x = 'Valor ajustado', y = 'Residuo')

# 3. Homocedasticidad
ggplot(data = datos_aux) +
  geom_point(mapping = aes(x = ajustados, y = residuos_est)) +
  labs(title = 'Supuesto de homocedasticidad',
       x = 'Valor ajustado', y = 'Residuos estandarizados')

## Test de Breusch-Pagan
## H0: Varianza constante
lmtest::bptest(modelo_kung1)

# 4. Normalidad
ggplot(data = datos_aux,
       mapping = aes(x = residuos_est, y = ..density..)) +
  geom_histogram(fill = 'turquoise', col = 'black') +
  geom_density(fill = 'salmon', alpha = 0.4) +
  labs(title = 'Supuesto de normalidad',
       x = 'Residuos estandarizados', y = 'Densidad')

ggplot(data = datos_aux,
       mapping = aes(sample = residuos_est)) +
  geom_qq() +
  geom_qq_line(col = 'red', lty = 'dashed', lwd = 1.5) +
  labs(title = 'Supuesto de normalidad',
       x = 'Residuos estandarizados', y = 'Densidad')

## Test de Shapiro-Wilk
## H0: La muestra es normal estándar
shapiro.test(std_resid_kung1)

## Gráficos que entrega R
plot(modelo_kung1)
ggplot2::autoplot(modelo_kung1)


## d.1) Usando solo adultos
datos_kung_adultos <- datos_kung %>% 
  dplyr::filter(age >= 18)

dim(datos_kung_adultos)
str(datos_kung_adultos)
summary(datos_kung_adultos)

ggplot(data = datos_kung_adultos,
       mapping = aes(x = weight, y = height)) +
  geom_point() +
  labs(title = 'Relación peso y altura pueblo !Kung San',
       subtitle = 'Sólo adultos',
       x = 'Peso', y = 'Altura')

## Ajuste del modelo
modelo_kung2 <- lm(formula = height ~ weight,
                   data = datos_kung_adultos)
summary(modelo_kung2)

ggplot(data = datos_kung_adultos,
       mapping = aes(x = weight, y = height)) +
  geom_point() +
  geom_abline(slope = modelo_kung2$coefficients[2],
              intercept = modelo_kung2$coefficients[1],
              col = 'red', lwd = 1.2, lty = 'dashed') + 
  labs(title = 'Relación peso y altura pueblo !Kung San',
       subtitle = 'Sólo adultos',
       x = 'Peso', y = 'Altura')


## Supuestos del modelo

## Necesitamos residuos, valores ajustados, residuos estandarizados
## y residuos studentizados

## Directo de R
ajustados_kung1 <- fitted(modelo_kung2)
resid_kung1 <- residuals(modelo_kung2)
std_resid_kung1 <- rstandard(modelo_kung2)
stu_resid_kung1 <- rstudent(modelo_kung2)

# 1. Media lineal
datos_aux <- tibble(peso = datos_kung_adultos$weight,
                    ajustados = ajustados_kung1,
                    residuos = resid_kung1,
                    residuos_est = std_resid_kung1,
                    residuos_stu = stu_resid_kung1)

ggplot(data = datos_aux) +
  geom_point(mapping = aes(x = peso, y = residuos)) +
  geom_hline(yintercept = 0, col = 'red',
             lty = 'dashed', lwd = 1.3) +
  labs(title = 'Supuesto de media lineal',
       x = 'Peso', y = 'Residuo')

ggplot(data = datos_aux) +
  geom_point(mapping = aes(x = ajustados, y = residuos)) +
  geom_hline(yintercept = 0, col = 'red',
             lty = 'dashed', lwd = 1.3) +
  labs(title = 'Supuesto de media lineal',
       x = 'Valor ajustado', y = 'Residuo')

# 2. Homocedasticidad
ggplot(data = datos_aux) +
  geom_point(mapping = aes(x = ajustados, y = residuos_est)) +
  labs(title = 'Supuesto de homocedasticidad',
       x = 'Valor ajustado', y = 'Residuos estandarizados')

## Test de Breusch-Pagan
## H0: Varianza constante
lmtest::bptest(modelo_kung2)

# 3. Normalidad
ggplot(data = datos_aux,
       mapping = aes(x = residuos_est, y = ..density..)) +
  geom_histogram(fill = 'turquoise', col = 'black') +
  geom_density(fill = 'salmon', alpha = 0.4) +
  labs(title = 'Supuesto de normalidad',
       x = 'Residuos estandarizados', y = 'Densidad')

ggplot(data = datos_aux,
       mapping = aes(sample = residuos_est)) +
  geom_qq() +
  geom_qq_line(col = 'red', lty = 'dashed', lwd = 1.5) +
  labs(title = 'Supuesto de normalidad',
       x = 'Residuos estandarizados', y = 'Densidad')

## Test de Shapiro-Wilk
## H0: La muestra es normal estándar
shapiro.test(std_resid_kung1)

# Gráficos que entrega R
plot(modelo_kung2)
ggplot2::autoplot(modelo_kung2)

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

# 1. Media lineal
datos_aux <- tibble(logpeso = datos_kung_log$logweight,
                    ajustados = ajustados_kung1,
                    residuos = resid_kung1,
                    residuos_est = std_resid_kung1,
                    residuos_stu = stu_resid_kung1)

ggplot(data = datos_aux) +
  geom_point(mapping = aes(x = logpeso, y = residuos)) +
  geom_hline(yintercept = 0, col = 'red',
             lty = 'dashed', lwd = 1.3) +
  labs(title = 'Supuesto de media lineal',
       x = 'Peso', y = 'Residuo')

ggplot(data = datos_aux) +
  geom_point(mapping = aes(x = ajustados, y = residuos)) +
  geom_hline(yintercept = 0, col = 'red',
             lty = 'dashed', lwd = 1.3) +
  labs(title = 'Supuesto de media lineal',
       x = 'Valor ajustado', y = 'Residuo')

# 2. Homocedasticidad
ggplot(data = datos_aux) +
  geom_point(mapping = aes(x = ajustados, y = residuos_est)) +
  labs(title = 'Supuesto de homocedasticidad',
       x = 'Valor ajustado', y = 'Residuos estandarizados')

## Test de Breusch-Pagan
## H0: Varianza constante
lmtest::bptest(modelo_kung3)

# 3. Normalidad
ggplot(data = datos_aux,
       mapping = aes(x = residuos_est, y = ..density..)) +
  geom_histogram(fill = 'turquoise', col = 'black') +
  geom_density(fill = 'salmon', alpha = 0.4) +
  labs(title = 'Supuesto de normalidad',
       x = 'Residuos estandarizados', y = 'Densidad')

ggplot(data = datos_aux,
       mapping = aes(sample = residuos_est)) +
  geom_qq() +
  geom_qq_line(col = 'red', lty = 'dashed', lwd = 1.5) +
  labs(title = 'Supuesto de normalidad',
       x = 'Residuos estandarizados', y = 'Densidad')

## Test de Shapiro-Wilk
## H0: La muestra es normal estándar
shapiro.test(std_resid_kung1)

# Gráficos que entrega R
plot(modelo_kung3)
ggplot2::autoplot(modelo_kung3)
