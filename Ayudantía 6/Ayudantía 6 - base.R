library(GGally)
library(ggfortify)
library(lmtest)


# Pregunta 1 ------------------------------------------------------------------------

## a) Análisis exploratorio
### Cargamos los datos
leche_primates <- read.csv(file = 'Datasets/milk.csv', sep = ';')
colnames(leche_primates)
View(leche_primates)
dim(leche_primates)

### Notamos que hay algunos datos faltantes (lm los elimina automáticamente)
leche_primates <- leche_primates[!is.na(leche_primates$neocortex.perc), ]
View(leche_primates)
dim(leche_primates)

### Vemos algunos estadísticos de los datos
leche_primates_num <- leche_primates[, -c(1, 2)]
summary(leche_primates_num)

### Correlación
round(cor(leche_primates_num), 3)

### Gráficos de correlación
GGally::ggpairs(leche_primates_num)
GGally::ggcorr(leche_primates_num)

## b) Ajuste de modelo completo
lm_completo <- lm(kcal.per.g ~ ., data = leche_primates_num)
summary(lm_completo)

### Problema importante: las composiciones suman 100, por lo que una variable es una
### comb. lineal de las otras dos, así que debemos eliminarla para el modelo
leche_primates_num <- subset(leche_primates_num, select = -perc.protein)
View(leche_primates_num)
colnames(leche_primates_num)

lm_completo <- lm(kcal.per.g ~ ., data = leche_primates_num)
summary(lm_completo)

### Modelo significativo (i.e. se rechaza H0: todos los coef son 0), pero ningún
### predictor es significativo

## c) Selección forward
### A mano
n <- nrow(leche_primates_num)

#### Primer paso
lm_nulo <- lm(kcal.per.g ~ 1, data = leche_primates_num)
SCE0 <- sum(lm_nulo$residuals^2)
p0 <- length(coef(lm_nulo))

lm_forward_fat <- lm(kcal.per.g ~ perc.fat, data = leche_primates_num)
SCE <- sum(lm_forward_fat$residuals^2)
p <- p0 + 1
MCE <- summary(lm_forward_fat)$sigma^2
F0_fat <- (SCE0 - SCE)/(p - p0)/MCE
valor.p <- pf(F0_fat, df1 = p - p0, df2 = n - p, lower.tail = FALSE)
F0_fat ; valor.p
summary(lm_forward_fat)

lm_forward_lactose <- lm(kcal.per.g ~ perc.lactose, data = leche_primates_num)
SCE <- sum(lm_forward_lactose$residuals^2)
p <- p0 + 1
MCE <- summary(lm_forward_lactose)$sigma^2
F0_lactose <- (SCE0 - SCE)/(p - p0)/MCE
valor.p <- pf(F0_lactose, df1 = p - p0, df2 = n - p, lower.tail = FALSE)
F0_lactose ; valor.p

lm_forward_mass <- lm(kcal.per.g ~ mass, data = leche_primates_num)
SCE <- sum(lm_forward_mass$residuals^2)
p <- p0 + 1
MCE <- summary(lm_forward_mass)$sigma^2
F0_mass <- (SCE0 - SCE)/(p - p0)/MCE
valor.p <- pf(F0_mass, df1 = p - p0, df2 = n - p, lower.tail = FALSE)
F0_mass ; valor.p

lm_forward_neo <- lm(kcal.per.g ~ neocortex.perc, data = leche_primates_num)
SCE <- sum(lm_forward_neo$residuals^2)
p <- p0 + 1
MCE <- summary(lm_forward_neo)$sigma^2
F0_neo <- (SCE0 - SCE)/(p - p0)/MCE
valor.p <- pf(F0_neo, df1 = p - p0, df2 = n - p, lower.tail = FALSE)
F0_neo ; valor.p

#### Así, agregamos perc.lactose
#### Segundo paso
lm_forward_fat <- lm(kcal.per.g ~ perc.lactose + perc.fat, data = leche_primates_num)
anova(lm_forward_lactose, lm_forward_fat)

lm_forward_mass <- lm(kcal.per.g ~ perc.lactose + mass, data = leche_primates_num)
anova(lm_forward_lactose, lm_forward_mass)

lm_forward_neo <- lm(kcal.per.g ~ perc.lactose + neocortex.perc,
                     data = leche_primates_num)
anova(lm_forward_lactose, lm_forward_neo)

#### No agregamos más variables: nos quedamos solo con lactosa

### Usando add1
lm_forward <- lm(kcal.per.g ~ 1, data = leche_primates_num)
add1(lm_forward, ~ perc.fat + perc.lactose + mass + neocortex.perc,
     data = leche_primates_num, test = 'F')
lm_forward <- update(lm_forward, formula. = ~ perc.lactose)

add1(lm_forward, ~ perc.fat + perc.lactose + mass + neocortex.perc,
     data = leche_primates_num, test = 'F')

#### Notamos que efectivamente la selección de modelos backward nos entrega perc.lactose

## d) Selección backward
### A mano
n <- nrow(leche_primates_num)

#### Primer paso
lm_completo <- lm(kcal.per.g ~ ., data = leche_primates_num)
SCE <- sum(lm_completo$residuals^2)
MCE <- summary(lm_completo)$sigma^2
p <- length(coef(lm_completo))

lm_backward_fat <- lm(kcal.per.g ~ . -perc.fat, data = leche_primates_num)
SCE0 <- sum(lm_backward_fat$residuals^2)
p0 <- p - 1
F0_fat <- (SCE0 - SCE)/(p - p0)/MCE
valor.p <- pf(F0_fat, df1 = p - p0, df2 = n - p, lower.tail = FALSE)
F0_fat ; valor.p

lm_backward_lactose <- lm(kcal.per.g ~ . -perc.lactose, data = leche_primates_num)
SCE0 <- sum(lm_backward_lactose$residuals^2)
p0 <- p - 1
F0_fat <- (SCE0 - SCE)/(p - p0)/MCE
valor.p <- pf(F0_fat, df1 = p - p0, df2 = n - p, lower.tail = FALSE)
F0_fat ; valor.p

lm_backward_mass <- lm(kcal.per.g ~ . -mass, data = leche_primates_num)
SCE0 <- sum(lm_backward_mass$residuals^2)
p0 <- p - 1
F0_fat <- (SCE0 - SCE)/(p - p0)/MCE
valor.p <- pf(F0_fat, df1 = p - p0, df2 = n - p, lower.tail = FALSE)
F0_fat ; valor.p

lm_backward_neo <- lm(kcal.per.g ~ . -neocortex.perc, data = leche_primates_num)
SCE0 <- sum(lm_backward_neo$residuals^2)
p0 <- p - 1
F0_fat <- (SCE0 - SCE)/(p - p0)/MCE
valor.p <- pf(F0_fat, df1 = p - p0, df2 = n - p, lower.tail = FALSE)
F0_fat ; valor.p

#### Así, eliminamos neocortex.perc
#### Segundo paso
lm_backward_fat <- lm(kcal.per.g ~ . -perc.fat -neocortex.perc,
                      data = leche_primates_num)
anova(lm_backward_neo, lm_backward_fat)

lm_backward_lactose <- lm(kcal.per.g ~ . -perc.lactose -neocortex.perc,
                          data = leche_primates_num)
anova(lm_backward_neo, lm_backward_lactose)

lm_backward_mass <- lm(kcal.per.g ~ . -mass -neocortex.perc,
                       data = leche_primates_num)
anova(lm_backward_neo, lm_backward_mass)

#### Así, eliminamos perc.fat
#### Tercer paso
lm_backward_lactose <- lm(kcal.per.g ~ . -perc.fat -neocortex.perc -perc.lactose,
                          data = leche_primates_num)
anova(lm_backward_fat, lm_backward_lactose)

lm_backward_mass <- lm(kcal.per.g ~ . -perc.fat -mass -neocortex.perc,
                       data = leche_primates_num)
anova(lm_backward_fat, lm_backward_mass)

#### Así, eliminamos masa y nos quedamos solo con perc.lactose

### Usando drop1
lm_backward <- lm(kcal.per.g ~ ., data = leche_primates_num)
drop1(lm_backward, ~ perc.fat + perc.lactose + mass + neocortex.perc,
      data = leche_primates_num, test = 'F')
lm_backward <- update(lm_backward, ~ . - neocortex.perc)

drop1(lm_backward, ~ perc.fat + perc.lactose + mass,
      data = leche_primates_num, test = 'F')
lm_backward <- update(lm_backward, ~ . - neocortex.perc - perc.fat)

drop1(lm_backward, ~ perc.lactose + mass, data = leche_primates_num, test = 'F')
lm_backward <- update(lm_backward, ~ perc.lactose)

#### Notamos que efectivamente

### e) Revisión de supuestos

## Necesitamos residuos, valores ajustados, residuos estandarizados
## y residuos studentizados

ajustados_leche <- fitted(lm_backward)
resid_leche <- residuals(lm_backward)
std_resid_leche <- rstandard(lm_backward)
stu_resid_leche <- rstudent(lm_backward) 

datos_aux <- data.frame(perc.lactose = leche_primates_num$perc.lactose,
                        ajustados = ajustados_leche,
                        residuos = resid_leche,
                        residuos_est = std_resid_leche,
                        residuos_stu = stu_resid_leche)

# 1. Outliers
plot(x = datos_aux$perc.lactose,
     y = datos_aux$residuos_stu,
     pch = 16, main = 'Presencia de outliers',
     xlab = 'Porcentaje lactosa', ylab = 'Residuos studentizados',
     ylim = c(-2.5, 2.5))
abline(h = qt(0.025, df = n - 3), lty = 'dashed', col = 'red', lwd = 2)
abline(h = qt(0.975, df = n - 3), lty = 'dashed', col = 'red', lwd = 2)

# 2. Media lineal
plot(x = datos_aux$perc.lactose,
     y = datos_aux$residuos,
     pch = 16, main = 'Presencia de outliers',
     xlab = 'Porcentaje lactosa', ylab = 'Residuos')
abline(h = 0, lty = 'dashed', col = 'red', lwd = 2)

plot(x = datos_aux$ajustados,
     y = datos_aux$residuos,
     pch = 16, main = 'Presencia de outliers',
     xlab = 'Valores ajustados', ylab = 'Residuos')
abline(h = 0, lty = 'dashed', col = 'red', lwd = 2)

# 2. Homocedasticidad
plot(x = datos_aux$perc.lactose,
     y = datos_aux$residuos_est,
     pch = 16, main = 'Presencia de outliers',
     xlab = 'Porcentaje lactosa', ylab = 'Residuos estandarizados')

## Test de Breusch-Pagan
## H0: Varianza constante
lmtest::bptest(lm_backward)

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
shapiro.test(std_resid_leche)

# Gráficos que entrega R
plot(lm_backward)
ggplot2::autoplot(lm_backward) # requiere ggfortify

summary(lm_backward)
