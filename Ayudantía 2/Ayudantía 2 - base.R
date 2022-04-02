# Ejercicio 1 - Test de una cola ----------------------------------------------------

## Cargamos los datos
delitos_mcs_2021 <- read.csv(file = 'Datasets/delitos_mcs_2021.csv')

## Análisis exploratorio
par(mar = c(4, 11, 4, 4))
barplot(tasa ~ region, data = delitos_mcs_2021,
        horiz = TRUE, las = 1, col="#69b3a2",
        main = 'Delitos de mayor connotación social por región', ylab = '')

## b)
tasa_media <- mean(delitos_mcs_2021$tasa[delitos_mcs_2021$region != 'Metropolitana'])
tasa_media

## c)
muestra_1 <- c(1773.7, 1602.2, 1866.8, 1987.8,
               1765.6, 1511.5, 1476, 1467.8)

qqnorm(y = muestra_1, main = "QQ-plot muestra de tasas de delitos",
       pch = 16, xlab = "Cuantiles teóricos", ylab = "Cuantiles muestrales")
qqline(y = muestra_1, col = "salmon", lwd = 2)

shapiro.test(muestra_1)

### H0: media_rm >= tasa_media vs H1: media_rm < tasa_media
t.test(x = muestra_1,
       mu = tasa_media,
       alternative = 'less')

### A "mano"
n <- length(muestra_1)
t0 <- (mean(muestra_1) - tasa_media)/(sd(muestra_1)/sqrt(n))
valor_p <- pt(t0, df = n - 1)

n
t0
valor_p

## d)
muestra_2 <- c(955.4, 1121.1, 1533.1, 1662.3,
               1017.7, 808.8, 1029, 1495)

shapiro.test(muestra_2)

### H0: media_rm >= tasa_media vs H1: media_rm < tasa_media
t.test(x = muestra_2,
       mu = tasa_media,
       alternative = 'less')

### A "mano"
n <- length(muestra_2)
t0 <- (mean(muestra_2) - tasa_media)/(sd(muestra_2)/sqrt(n))
valor_p <- pt(t0, df = n - 1)

n
t0
valor_p

# Ejercicio 2 - Comparación de medias -----------------------------------------------

## a) Cargamos los datos
pesos <- read.csv("Datasets/Pesos.txt", sep="")
pesos

pesos_mujeres <- pesos[pesos$Sexo == 'F', ]
pesos_hombres <- pesos[pesos$Sexo == 'M', ]

## b) Vemos los supuestos
### Histograma
hist(pesos_mujeres$Peso, col = 'salmon', breaks = 20,
     freq = FALSE, ylim = c(0, 0.045), main = 'Pesos de mujeres y hombres',
     xlab = 'Peso', ylab = 'Densidad')
hist(pesos_hombres$Peso, col = rgb(64, 224, 208, alpha = 150, maxColorValue = 255),
     freq = FALSE, add = TRUE)
legend('topright', legend = c('Mujeres', 'Hombres'), title = 'Sexo',
       fill = c('salmon', 'turquoise'))

### Densidades
plot(density(pesos_mujeres$Peso),
     main = 'Distribución de los pesos de mujeres y hombres',
     xlab = 'Peso', ylab = 'Densidad', ylim = c(0, 0.045))
lines(density(pesos_hombres$Peso))
polygon(density(pesos_mujeres$Peso),
        col = rgb(250, 128, 114, alpha = 200, maxColorValue = 255))
polygon(density(pesos_hombres$Peso),
        col = rgb(64, 224, 208, alpha = 200, maxColorValue = 255))
legend('topright', legend = c('Mujeres', 'Hombres'), title = 'Sexo',
       fill = c('salmon', 'turquoise'))

### QQ-plot
qqnorm(y = pesos_mujeres$Peso,
       main = "QQ-plot pesos de mujeres",
       pch = 16, xlab = "Cuantiles teóricos", ylab = "Cuantiles muestrales")
qqline(y = pesos_mujeres$Peso, col = "salmon", lwd = 2)

qqnorm(y = pesos_hombres$Peso,
       main = "Q-Q plot pesos de nacimiento, madres fumadoras",
       pch = 16, xlab = "Cuantiles teóricos", ylab = "Cuantiles muestrales")
qqline(y = pesos_hombres$Peso, col = "salmon", lwd = 2)

### Test de Shapiro-Wilk
shapiro.test(pesos_mujeres$Peso)
shapiro.test(pesos_hombres$Peso)

### Test de varianzas
var(pesos_mujeres$Peso)
var(pesos_hombres$Peso)

# H0: var_F/var_M = 1 vs H1: var_F/var_M != 1
var.test(pesos_mujeres$Peso, pesos_hombres$Peso)

## c) Realizamos el test
media_mujeres <- mean(pesos_mujeres$Peso)
media_hombres <- mean(pesos_hombres$Peso)

media_mujeres
media_hombres

# H0: mu_F >= mu_M vs H1: mu_F < mu_M considerando varianzas diferentes y desconocidas
alpha <- 0.05

### A mano
n_mujeres <- length(pesos_mujeres$Peso)
n_hombres <- length(pesos_hombres$Peso)
var_mujeres <- var(pesos_mujeres$Peso)
var_hombres <- var(pesos_hombres$Peso)

num <- (var_hombres/n_hombres + var_mujeres/n_mujeres)^2
den1 <- (var_hombres/n_hombres)^2 / (n_hombres - 1)
den2 <- (var_mujeres/n_mujeres)^2 / (n_mujeres - 1)
df <- round(num/(den1 + den2))
t0 <- (media_mujeres - media_hombres)/sqrt(var_hombres/n_hombres + var_mujeres/n_mujeres)

t0
df
t0 <= qt(alpha, df) # Se rechaza H0

valor_p <- pt(t0, df)
valor_p

### Con R
t.test(x = pesos_mujeres$Peso,
       y = pesos_hombres$Peso,
       alternative = 'less',
       var.equal = FALSE, 
       conf.level = 1 - alpha)
