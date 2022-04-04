library(tidyverse)


# Ejercicio 1 - Test de una cola ----------------------------------------------------

## Cargamos los datos
delitos_mcs_2021 <- readr::read_csv(file = "Datasets/delitos_mcs_2021.csv") %>% 
  dplyr::mutate(region = factor(region, levels = region))

## Análisis exploratorio
ggplot(delitos_mcs_2021,
       mapping = aes(x = reorder(region, desc(region)), y = tasa, fill = region)) +
  geom_col() +
  coord_flip() +
  labs(title = "Delitos de mayor connotación social por región",
       subtitle = "Tasa cada 100.000 habitantes",
       y = "tasa", x = "Región") +
  theme(legend.position = 'none')

## b)
tasa_media <- delitos_mcs_2021 %>% 
  dplyr::filter(region != "Metropolitana") %>% 
  dplyr::summarise(tasa_media = mean(tasa)) %>% 
  as.numeric()

tasa_media

## c)
muestra_1 <- c(1773.7, 1602.2, 1866.8, 1987.8,
               1765.6, 1511.5, 1476, 1467.8)

ggplot(tibble(tasa = muestra_1), mapping = aes(sample = scale(tasa))) +
  geom_qq(size = 3) +
  geom_qq_line(col = "salmon", lwd = 1.5) +
  labs(title = paste0("Tasa de delitos de mayor connotación social", 
                      ", comunas Región Metropolitana"),
       subtitle = "Comparado con una distribución normal",
       x = "Cuantiles teóricos", y = "Cuantiles muestrales")

shapiro.test(muestra_1)

### H0: media_rm >= tasa_media vs H1: media_rm < tasa_media
t.test(x = muestra_1,
       mu = tasa_media,
       alternative = 'less')

### A "mano"
alpha <- 0.05
n <- length(muestra_1)

t0 <- (mean(muestra_1) - tasa_media)/(sd(muestra_1)/sqrt(n))
t <- qt(alpha, df = n - 1)
t0 <= t 

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
t <- qt(alpha, df = n - 1)
t0 <= t

valor_p <- pt(t0, df = n - 1)

n
t0
valor_p


# Ejercicio 2 - Comparación de medias -----------------------------------------------

## a) Cargamos los datos
pesos <- readr::read_delim("Datasets/Pesos.txt", quote = "\"")
pesos

pesos_mujeres <- pesos %>% filter(Sexo == 'F')
pesos_hombres <- pesos %>% filter(Sexo == 'M')

## b) Vemos los supuestos
### Histograma
ggplot(pesos, mapping = aes(x = Peso)) +
  geom_histogram(data = subset(pesos, Sexo == 'F'), aes(y = ..density..),
                 fill = 'salmon', color = 'black', bins = 26) +
  geom_histogram(data = subset(pesos, Sexo == 'M'), aes(y = ..density..),
                 fill = 'turquoise', color = 'black', bins = 26, alpha = 0.7) +
  labs(x = 'Peso', y = 'Frecuecia', title = 'Pesos de hombres y mujeres')

### Densidades
ggplot(pesos) +
  geom_density(mapping = aes(x = Peso, fill = Sexo), alpha = 0.8) +
  labs(x = 'Peso', y = 'Densidad', title = 'Pesos de hombres y mujeres')

### QQ-plot
ggplot(pesos_mujeres, mapping = aes(sample = Peso)) +
  geom_qq(size = 2) +
  geom_qq_line(col = "salmon", lwd = 1.5) +
  labs(title = "QQ-plot pesos mujeres",
       subtitle = "Comparados con una distribución normal",
       x = "Cuantiles teóricos", y = "Cuantiles muestrales")

ggplot(pesos_hombres, mapping = aes(sample = Peso)) +
  geom_qq(size = 2) +
  geom_qq_line(col = "salmon", lwd = 1.5) +
  labs(title = "QQ-plot pesos hombres",
       subtitle = "Comparados con una distribución normal",
       x = "Cuantiles teóricos", y = "Cuantiles muestrales")

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

# Ejercicio 3 - Comparación de proporciones -----------------------------------------

hospitales <- readr::read_table(file = 'Datasets/hospitales.txt')
head(hospitales)

hosp_tipo0 <- hospitales %>% dplyr::filter(tipo == 0)
hosp_tipo1 <- hospitales %>% dplyr::filter(tipo == 1)

n_tipo0 <- nrow(hosp_tipo0)
n_tipo1 <- nrow(hosp_tipo1)

p0_hat <- mean(hosp_tipo0$rodilla)
p1_hat <- mean(hosp_tipo1$rodilla)

# p0_hat distribuye aprox. Normal(p0, p0 * (1 - p0)/n0)
# p1_hat distribuye aprox. Normal(p1, p1 * (1 - p1)/n1)

# H0: p1 = p0 vs p1 != p0

alpha <- 0.05

p_hat <- mean(hospitales$rodilla)
sp2 <- p_hat * (1 - p_hat)
z0 <- abs(p0_hat - p1_hat) / sqrt(sp2 * (1/n_tipo0 + 1/n_tipo1))

z0 >= qnorm(1 - alpha/2)

valor_p <- 1 - pnorm(z0)
valor_p

## No se rechaza H0
