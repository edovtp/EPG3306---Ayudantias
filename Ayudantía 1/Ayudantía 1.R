library(tidyverse)
library(nortest)


# Ejercicio 1 - Evaluación de supuestos ---------------------------------------------

## Cargamos los datos
arcilla <- readRDS(file = "Datasets/arcilla.rds")
arcilla

## a) Supuesto de normalidad

### Histograma
ggplot(arcilla, mapping = aes(x = prop)) +
  geom_histogram(bins = 15, fill = "salmon") +
  labs(title = "Distribución de las proporciones de tablas de arcilla",
       x = "Proporción", y = "Frecuencia")

### Ver que pasa si cambio el número de bins
ggplot(arcilla, mapping = aes(x = prop)) +
  geom_histogram(bins = 20, fill = "salmon") +
  labs(title = "Distribución de las proporciones de tablas de arcilla",
       x = "Proporción", y = "Frecuencia")

### Usar densidad
ggplot(arcilla, mapping = aes(x = prop)) +
  geom_density(fill = "salmon") +
  labs(title = "Distribución de las proporciones de tablas de arcilla",
       x = "Proporción", y = "Frecuencia")

### Box-plot
ggplot(arcilla, mapping = aes(y = prop)) +
  geom_boxplot(fill = 'salmon') +
  labs(title = "Distribución de las proporciones",
       y = "Proporción")

### QQ-plot
ggplot(arcilla, mapping = aes(sample = prop)) +
  geom_qq(size = 3) +
  geom_qq_line(col = "salmon", lwd = 1.5) +
  labs(title = "Q-Q plot proporciones tablas de arcilla",
       subtitle = "Comparados con una distribución normal",
       x = "Cuantiles teóricos", y = "Cuantiles muestrales")

qqnorm(y = arcilla$prop, main = "Q-Q plot proporciones tablas de arcilla",
       pch = 16, xlab = "Cuantiles teóricos", ylab = "Cuantiles muestrales")
qqline(y = arcilla$prop, col = "salmon", lwd = 2)

## b) Tests de normalidad

### Estandarizar datos
arcilla$prop <- (arcilla$prop - mean(arcilla$prop))/sd(arcilla$prop)
# arcilla$prop <- scale(arcilla$prop) # Equivalente a lo de arriba

### Kolmogorov-Smirnov
ks.test(arcilla$prop, "pnorm")

### Shapiro-Wilk
shapiro.test(arcilla$prop)

### Anderson-Darling
nortest::ad.test(arcilla$prop)


# Ejercicio 2 - Test de dos colas ---------------------------------------------------

## Cargamos los datos
datos_nacimiento <- readr::read_csv(file = "Datasets/birthweight.csv")

## Nos enfocamos sólo en los pesos de nacimiento
pesos_nacimiento <- datos_nacimiento %>% 
  dplyr::select(Birthweight, smoker) %>% 
  dplyr::mutate(smoker = factor(smoker, levels = c(0, 1),
                                labels = c("No", "Sí")))

## Análisis exploratorio
summary(pesos_nacimiento)

## Boxplot
ggplot(pesos_nacimiento, mapping = aes(x = smoker, y = Birthweight, fill = smoker)) +
  geom_boxplot() +
  labs(
    title = "Diferencias de pesos de nacimiento entre madres fumadoras y no fumadoras",
    y = "Peso al nacer") +
  guides(fill = guide_legend(title = "Fuma"))

## a) Filtramos por madres no fumadoras
madres_no_fumadoras <- pesos_nacimiento %>% 
  dplyr::filter(smoker == "No")
madres_no_fumadoras

ggplot(madres_no_fumadoras, mapping = aes(x = Birthweight)) +
  geom_density(fill = "salmon") +
  labs(title = "Distribución de los pesos de nacimiento",
       subtitle = "Madres no fumadoras", x = "Peso nacimiento", y = "Frecuencia") +
  xlim(1.8, 5.5)

media_pesos <- mean(madres_no_fumadoras$Birthweight)
media_pesos 

## b) Supuesto de normalidad

madres_fumadoras <- pesos_nacimiento %>% 
  dplyr::filter(smoker == "Sí")
madres_fumadoras

ggplot(madres_fumadoras, mapping = aes(x = Birthweight)) +
  geom_density(fill = "salmon") +
  labs(title = "Distribución de los pesos de nacimiento",
       subtitle = "Madres fumadoras", x = "Peso nacimiento", y = "Frecuencia") +
  xlim(1.2, 5.5)

ggplot(pesos_nacimiento, mapping = aes(x = Birthweight, group = smoker, fill = smoker)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribución de los pesos de nacimiento",
       subtitle = "Madres fumadoras y no fumadoras", x = "Peso nacimiento",
       y = "Frecuencia") +
  xlim(1.2, 5.5)

ggplot(madres_fumadoras, mapping = aes(sample = scale(Birthweight))) +
  geom_qq(size = 3) +
  geom_qq_line(col = "salmon", lwd = 1.5) +
  labs(title = "Q-Q plot pesos de nacimiento, madres fumadoras",
       subtitle = "Comparado con una distribución normal",
       x = "Cuantiles teóricos", y = "Cuantiles muestrales")

### Test de normalidad
shapiro.test(madres_fumadoras$Birthweight)

## c) Test de hipótesis

### A mano

media_muestral <- mean(madres_fumadoras$Birthweight)
de <- sd(madres_fumadoras$Birthweight)
n_muestra <- length(madres_fumadoras$Birthweight)

estadistico_t <- abs(media_muestral - media_pesos)*sqrt(n_muestra)/de
estadistico_t

valor_p <- pt(estadistico_t, df = n_muestra - 1, lower.tail = FALSE) * 2
valor_p

### Usando t.test
t.test(madres_fumadoras$Birthweight,
       mu = media_pesos,
       alternative = "two.sided")

estadistico_t
n_muestra - 1
valor_p
media_muestral

# Ejercicio 3 - Test de una cola ----------------------------------------------------

## Cargamos los datos
delitos_mcs_2021 <- readr::read_csv(file = "Datasets/delitos_mcs_2021.csv") %>% 
  dplyr::mutate(region = factor(region, levels = region))

## Análisis exploratorio
summary(delitos_mcs_2021$tasa)

ggplot(delitos_mcs_2021,
       mapping = aes(x = reorder(region, desc(region)), y = tasa, fill = region)) +
  geom_col() +
  coord_flip() +
  labs(title = "Delitos de mayor connotación social por región",
       subtitle = "Tasa cada 100.000 habitantes",
       y = "tasa", x = "Región")

## b)
tasa_media <- delitos_mcs_2021 %>% 
  dplyr::filter(region != "Metropolitana") %>% 
  dplyr::summarise(tasa_media = mean(tasa)) %>% 
  as.numeric()

## c)
muestra_1 <- c(1773.7, 1602.2, 1569.9, 1702.2,
               1685.6, 2050.3, 1765.6, 1495)

ggplot(tibble(tasa = muestra_1), mapping = aes(sample = scale(tasa))) +
  geom_qq(size = 3) +
  geom_qq_line(col = "salmon", lwd = 1.5) +
  labs(title = paste0("Tasa de delitos de mayor connotación social", 
                      ", comunas Región Metropolitana"),
       subtitle = "Comparado con una distribución normal",
       x = "Cuantiles teóricos", y = "Cuantiles muestrales")

shapiro.test(muestra_1)

t.test(x = muestra_1,
       mu = tasa_media,
       alternative = 'less')

## d)
muestra_2 <- c(955.4, 1121.1, 1533.1, 1662.3,
               1017.7, 808.8, 1029, 1495)

shapiro.test(muestra_2)

t.test(x = muestra_2,
       mu = tasa_media,
       alternative = 'less')
