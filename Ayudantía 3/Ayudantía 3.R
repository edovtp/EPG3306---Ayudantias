library(tidyverse)
library(ggfortify)


# Ejercicio 1 -----------------------------------------------------------------------

eu_index_91 <- window(EuStockMarkets, end = 1992)

# GRÁFICOS
## Precios
autoplot(eu_index_91, columns = c('DAX'), col = 'tomato') +
  labs(title = 'Precios de cierre por día - índice DAX',
       subtitle = 'Año 1991',
       x = 'fecha', y = 'precio (USD)')

## Retornos (netos)
retornos <- diff(eu_index_91)/eu_index_91[-nrow(eu_index_91), ]
autoplot(retornos, columns = c('DAX'), col = 'tomato') +
  labs(title = 'Retornos por día - índice DAX',
       subtitle = 'Año 1991',
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

# Supuesto de simetría
ggplot(data = log_retornos_dax) +
  geom_density(mapping = aes(x = DAX), fill = 'salmon') +
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
n*(n + 1)/4 ; W

# Región de rechazo
alpha <- 0.05

k1 <- qsignrank(alpha/2, n) - 1
k2 <- qsignrank(1 - alpha/2, n) + 1
k1 ; W ; k2

psignrank(k1, n) + psignrank(k2, n, lower.tail = FALSE)

# Usando wilcox.test
wilcox.test(log_retornos,
            mu = 0,
            alternative = 'two.sided')


# Ejercicio 2 -----------------------------------------------------------------------

# Cargamos los datos
set.seed(2211)
web <- rt(20, df = 2) + 17.5
app <- rt(20, df = 2) + 16.5

aurora <- tibble::tibble(web = web, app = app)

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
k_m1 <- qbinom(alpha, 20, 1/2)
k_m1

# Restamos uno por la naturaleza discreta de la variable aleatoria
k <- k_m1 - 1
k

# Comprobamos que no nos pasamos del alpha
pbinom(k, n, 1/2)
pbinom(k_m1, n, 1/2)

# Vemos si rechazamos H0
sum(web < 17) <= k

### II.- Aproximación normal (H1: p < 1/2)
z0 <- (mean(web < 17) - 1/2)/(sqrt(1/(4*n)))
pnorm(z0)
