library(tidyverse)
library(ggfortify)


# Ejercicio 1 -----------------------------------------------------------------------

# Para ver todas las bases que vienen en 
# R y paquetes (que hayan sido llamados)
data()

View(EuStockMarkets)
eu_index_91 <- window(EuStockMarkets,
                      start = 1995.000,
                      end = 1995.052)
nrow(eu_index_91)

# GRÁFICOS
## Precios
autoplot(eu_index_91, columns = c('DAX'), col = 'tomato') +
  labs(title = 'Precios de cierre por día - índice DAX',
       subtitle = 'Año 1995',
       x = 'fecha', y = 'precio (USD)')

## Retornos (netos)
retornos <- diff(eu_index_91)/eu_index_91[-nrow(eu_index_91), ]
autoplot(retornos, columns = c('DAX'), col = 'tomato') +
  labs(title = 'Retornos por día - índice DAX',
       subtitle = 'Año 1995',
       x = 'fecha', y = 'precio (USD)')

## Log-retornos
log_retornos <- log(retornos + 1)
autoplot(log_retornos, columns = c('DAX'), col = 'tomato') +
  labs(title = 'Log-retornos por día - índice DAX',
       subtitle = 'Año 1991',
       x = 'fecha', y = 'precio (USD)') +
  geom_hline(yintercept = 0, lty = 'dashed', col = 'blue', lwd = 0.8)

# Hipótesis: M_DAX = 0 vs M_DAX != 0

log_retornos_dax <- tibble::as_tibble(log_retornos) %>% 
  dplyr::select(DAX)
log_retornos_dax

# Supuesto de simetría
ggplot(data = log_retornos_dax) +
  geom_density(mapping = aes(x = DAX), fill = 'salmon') +
  xlim(-0.03, 0.03) +
  labs(title = 'Distribución de los log-retornos - índice DAX',
       subtitle = 'Año 1991',
       x = 'log-retorno', y = 'densidad')

# Obtenemos el estadístico W
M_hipotesis <- 0

Z <- as.numeric(log_retornos_dax - M_hipotesis > 0)
head(Z)

dif_abs <- abs(log_retornos_dax - M_hipotesis)
head(dif_abs)

orden_relativo <- as.vector(rank(dif_abs))
head(orden_relativo)

W <- sum(Z * orden_relativo)
W

# Valor de "equilibrio" para W bajo H0
n <- nrow(log_retornos_dax)
n
n * (n + 1)/4 ; W

# Región de rechazo
alpha <- 0.05

k1 <- qsignrank(alpha/2, n) - 1
k2 <- qsignrank(1 - alpha/2, n) + 1
k1 ; W ; k2

valor_p <- psignrank(W, n) * 2
valor_p

# Verificamos que no nos pasamos del alpha
psignrank(k1, n) + psignrank(k2, n, lower.tail = FALSE)

# Usando wilcox.test
wilcox.test(x = dplyr::pull(log_retornos_dax),
            mu = 0,
            alternative = 'two.sided', 
            conf.level = 1 - alpha)

W ; valor_p

# Ejercicio 2 -----------------------------------------------------------------------

# Cargamos los datos
aurora <- readr::read_table(file = 'Datasets/aurora.txt')

## a) Diferencias con la competencia

# Vemos los datos
ggplot(data = aurora) +
  geom_density(mapping = aes(x = web), fill = 'salmon') +
  labs(title = 'Tiempos medios de lectura Aurora de Chile',
       subtitle = 'Página web',
       x = 'Tiempo (minutos)', y = 'densidad') +
  xlim(13.8, 20.5) +
  geom_vline(xintercept = 17, col = 'blue', lwd = 2, lty = 'dashed')

### I.- Test exacto
n <- nrow(aurora)
alpha <- 0.05
k <- qbinom(p = 1 - alpha, size = 20, prob = 1/2)

# Comprobamos que no nos pasamos del alpha
pbinom(q = k, size = n, prob = 1/2, lower.tail = FALSE)
pbinom(q = k - 1, size = n, prob = 1/2, lower.tail = FALSE)

# Vemos si rechazamos H0
sum(aurora$web >= 17)
sum(aurora$web >= 17) >= k # Se rechaza H0

valor.p = pbinom(sum(aurora$web >= 17), n, 1/2, lower.tail = FALSE) 
valor.p

### II.- Aproximación normal (H1: p < 1/2)
z0 <- (mean(aurora$web >= 17) - 1/2)/(sqrt(1/(4*n)))
pnorm(z0, lower.tail = FALSE)

### III.- DESAFÍO: ver el test de Wilcoxon para la hipótesis en este caso

# b) Diferencia entre productos

## Comparación de densidades
ggplot(data = aurora) +
  geom_density(mapping = aes(x = web), fill = 'salmon') +
  geom_density(mapping = aes(x = app), fill = 'turquoise', alpha = 0.6) +
  labs(title = 'Tiempos medios de lectura Aurora de Chile',
       subtitle = 'Página web (rojo) y aplicación móvil (azul)',
       x = 'Tiempo (minutos)', y = 'densidad') +
  xlim(13, 30)

## Estadístico U
alpha <- 0.05

n <- length(aurora$web)
m <- length(aurora$app)

datos_conjuntos <- c(aurora$web, aurora$app)
datos_conjuntos

rangos <- rank(datos_conjuntos)
rangos

R1 <- sum(rangos[1 : n])
R2 <- sum(rangos[(n+1) : (n + m)])

U1 <- sum(R1) - n * (n + 1) / 2
U2 <- sum(R2) - m * (m + 1) / 2

U <- min(U1, U2)
valor_p <- 2 * pwilcox(q = U, m = m, n = n)

## Usando wilcox.test
wilcox.test(x = aurora$web,
            y = aurora$app,
            alternative = 'two.sided',
            conf.level = 1 - alpha)

2 * pwilcox(q = U1 - 1, m = m, n = n, lower.tail = FALSE)
valor_p

## Aproximación normal

mu_aprox <- n * m / 2
var_aprox <- n * m * (n + m + 1) / 12

z0 <- abs(U - mu_aprox) / sqrt(var_aprox)
z0

z0 >= qnorm(1 - alpha/2)

valor_p_aprox <- 2 * (1 - pnorm(z0))
valor_p_aprox ; valor_p
