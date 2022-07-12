# Ejercicio 1 - Evaluación de supuestos ---------------------------------------------

## Cargamos los datos
arcilla <- readRDS(file = "Datasets/arcilla.rds")
arcilla

## a) Supuesto de normalidad

### Histograma
hist(arcilla$prop, breaks = 10, col = 'salmon',
     main = 'Distribución de las proporciones de tablas de arcilla',
     xlab = 'Proporción', ylab = 'Frecuencia')

### Ver que pasa si cambio el número de bins
hist(arcilla$prop, breaks = 20, col = 'salmon',
     main = 'Distribución de las proporciones de tablas de arcilla',
     xlab = 'Proporción', ylab = 'Frecuencia')

### Usar densidad
plot(density(arcilla$prop),
     main = 'Distribución de las proporciones de tablas de arcilla',
     xlab = 'Proporción', ylab = 'Densidad')
polygon(density(arcilla$prop), col = 'salmon')

### Box-plot
boxplot(arcilla$prop, col = 'salmon',
        main = "Distribución de las proporciones de tablas de arcilla",
        ylab = 'Proporción')

### QQ-plot
qqnorm(y = arcilla$prop, main = "Q-Q plot proporciones tablas de arcilla",
       pch = 16, xlab = "Cuantiles teóricos", ylab = "Cuantiles muestrales")
qqline(y = arcilla$prop, col = "salmon", lwd = 2)

## b) Tests de normalidad

### Estandarizar datos para usar Kolmogorov-Smirnov (esto no es siempre necesario)
arcilla$prop <- (arcilla$prop - mean(arcilla$prop))/sd(arcilla$prop)
# arcilla$prop <- scale(arcilla$prop) # Equivalente a lo de arriba

### Kolmogorov-Smirnov
ks.test(arcilla$prop, "pnorm")

### Shapiro-Wilk
shapiro.test(arcilla$prop)

### Anderson-Darling (este lo saqué ya que no es parte de R base)

# Ejercicio 2 - Test de dos colas ---------------------------------------------------

## Cargamos los datos
datos_nacimiento <- readr::read_csv(file = 'Datasets/birthweight.csv')
datos_nacimiento <- read.csv(file = 'Datasets/birthweight.csv')

## Nos enfocamos sólo en los pesos de nacimiento
pesos_nacimiento <- datos_nacimiento[, c('Birthweight', 'smoker')]
pesos_nacimiento$smoker <- factor(pesos_nacimiento$smoker, labels = c('No', 'Sí'))

## Análisis exploratorio
summary(pesos_nacimiento)

## Boxplot
boxplot(formula = Birthweight ~ smoker, data = pesos_nacimiento,
        main = "Diferencias de pesos de nacimiento entre madres fumadoras y no fumadoras",
        ylab = 'Peso al nacer', xlab = 'Fuma', col = c('salmon', 'turquoise'))

## a) Filtramos por madres no fumadoras
madres_no_fumadoras <- pesos_nacimiento[pesos_nacimiento$smoker == 'No', ]
madres_no_fumadoras

ggplot(madres_no_fumadoras, mapping = aes(x = Birthweight)) +
  geom_density(fill = "salmon") +
  labs(title = "Distribución de los pesos de nacimiento",
       subtitle = "Madres no fumadoras", x = "Peso nacimiento", y = "Frecuencia") +
  xlim(1.8, 5.5)

plot(density(madres_no_fumadoras$Birthweight),
     main = 'Distribución de los pesos de nacimiento, madres no fumadoras',
     xlab = 'Peso de nacimiento', ylab = 'Densidad')
polygon(density(madres_no_fumadoras$Birthweight), col = 'salmon')

media_pesos_nf <- mean(madres_no_fumadoras$Birthweight)
media_pesos_nf

## b) Supuesto de normalidad

madres_fumadoras <- pesos_nacimiento[pesos_nacimiento$smoker == 'Sí', ]
madres_fumadoras

## Densidad de madres fumadoras
plot(density(madres_fumadoras$Birthweight),
     main = 'Distribución de los pesos de nacimiento, madres fumadoras',
     xlab = 'Peso de nacimiento', ylab = 'Densidad')
polygon(density(madres_fumadoras$Birthweight), col = 'salmon')

### Vemos ambas densidades en conjunto
plot(density(madres_no_fumadoras$Birthweight),
     main = 'Distribución de los pesos de nacimiento',
     xlab = 'Peso de nacimiento', ylab = 'Densidad')
lines(density(madres_fumadoras$Birthweight))
polygon(density(madres_no_fumadoras$Birthweight),
        col = rgb(250, 128, 114, alpha = 200, maxColorValue = 255))
polygon(density(madres_fumadoras$Birthweight),
        col = rgb(64, 224, 208, alpha = 200, maxColorValue = 255))
legend(x = 4.7, y = 0.65, legend = c('No', 'Sí'), title = 'Fuma',
       fill = c('salmon', 'turquoise'))

## Q-Q plot
qqnorm(y = madres_fumadoras$Birthweight,
       main = "Q-Q plot pesos de nacimiento, madres fumadoras",
       pch = 16, xlab = "Cuantiles teóricos", ylab = "Cuantiles muestrales")
qqline(y = madres_fumadoras$Birthweight, col = "salmon", lwd = 2)

### Test de normalidad: Shapiro-Wilk
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

### Vemos que obtenemos los mismos resultados
estadistico_t
n_muestra - 1
valor_p
media_muestral
