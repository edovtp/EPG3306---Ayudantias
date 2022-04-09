# Ejercicio 1 -----------------------------------------------------------------------

# Para ver todas las bases que vienen en R y paquetes (que hayan sido llamados)
data()

View(EuStockMarkets)
eu_index_91 <- window(EuStockMarkets,
                      start = 1995.000,
                      end = 1995.052)
nrow(eu_index_91)

# GRÁFICOS
## Precios
plot.ts(eu_index_91[, 'DAX'], col = 'salmon',
        main = 'Precios de cierre por día - índice DAX (1995)',
        xlab = 'fecha', ylab = 'precio (USD)', lwd = 2)

## Retornos (netos)
retornos <- diff(eu_index_91)/eu_index_91[-nrow(eu_index_91), ]
plot.ts(retornos[, 'DAX'], col = 'salmon',
        main = 'Retornos por día - índice DAX (1995)',
        xlab = 'fecha', ylab = 'precio (USD)', lwd = 2)

## Log-retornos
log_retornos <- log(retornos + 1)
plot.ts(log_retornos[, 'DAX'], col = 'salmon',
        main = 'Log-retornos por día - índice DAX (1995)',
        xlab = 'fecha', ylab = 'precio (USD)', lwd = 2)
abline(h = 0, col = 'blue', lty = 'dashed', lwd = 2)

# Hipótesis: M_DAX = 0 vs M_DAX != 0

log_retornos_dax <- as.vector(log_retornos[, 'DAX'])
log_retornos_dax

# Supuesto de simetría
plot(density(log_retornos_dax), main = 'Distribución de los log-retornos DAX (1991)',
     xlab = 'log-retorno', ylab = 'densidad')
polygon(density(log_retornos_dax),
        col = rgb(250, 128, 114, alpha = 200, maxColorValue = 255))

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
n <- length(log_retornos_dax)
n
n * (n + 1)/4 ; W

# Región de rechazo
alpha <- 0.05

k1 <- qsignrank(alpha/2, n) - 1
k2 <- qsignrank(1 - alpha/2, n) + 1
k1 ; W ; k2

valor_p <- psignrank(W, n) * 2
valor_p

psignrank(k1, n) + psignrank(k2, n, lower.tail = FALSE)

# Usando wilcox.test
wilcox.test(x = log_retornos_dax,
            mu = 0,
            alternative = 'two.sided', 
            conf.level = 1 - alpha)

W ; valor_p

# Ejercicio 2 -----------------------------------------------------------------------

# Cargamos los datos
aurora <- read.table(file = 'Datasets/aurora.txt', header = TRUE)

## a) Diferencias con la competencia

# Vemos los datos
plot(density(aurora$web), main = 'Tiempos medios de lectura Aurora de Chile (web)',
     xlab = 'Tiempo (minutos)', ylab = 'densidad')
polygon(density(aurora$web),
        col = rgb(250, 128, 114, alpha = 200, maxColorValue = 255))
abline(v = 17, col = 'blue', lwd = 2, lty = 'dashed')

### I.- Test exacto
n <- nrow(aurora)
alpha <- 0.05
k <- qbinom(p = 1 - alpha, size = 20, prob = 1/2)

# Comprobamos que no nos pasamos del alpha
pbinom(q = k, size = n, prob = 1/2, lower.tail = FALSE)
pbinom(q = k - 1, size = n, prob = 1/2, lower.tail = FALSE)

# Vemos si rechazamos H0
sum(aurora$web >= 17)
sum(aurora$web >= 17) <= k # Se rechaza H0

pbinom(sum(aurora$web >= 17), n, 1/2)

### II.- Aproximación normal (H1: p < 1/2)
z0 <- (mean(aurora$web >= 17) - 1/2)/(sqrt(1/(4*n)))
pnorm(z0)

### III.- DESAFÍO: ver el test de Wilcoxon para la hipótesis en este caso

# b) Diferencia entre productos

## Comparación de densidades
plot(density(aurora$web),
     main = 'Tiempos medios de lectura Aurora de Chile',
     xlab = 'Tiempo (minutos)', ylab = 'densidad', xlim = c(13, 30))
lines(density(aurora$app))
polygon(density(aurora$web),
        col = rgb(250, 128, 114, alpha = 255, maxColorValue = 255))
polygon(density(aurora$app),
        col = rgb(64, 224, 208, alpha = 180, maxColorValue = 255))
legend('topright', legend = c('Página web', 'Aplicación'), title = 'Dispositivo',
       fill = c('salmon', 'turquoise'))

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

## Aproximación normal

mu_aprox <- n * m / 2
var_aprox <- n * m * (n + m + 1) / 12

z0 <- abs(U - mu_aprox) / sqrt(var_aprox)
z0

z0 >= qnorm(1 - alpha/2)

valor_p_aprox <- 2 * (1 - pnorm(z0))
valor_p_aprox ; valor_p
