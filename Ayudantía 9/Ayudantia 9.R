library(PLRModels)
library(MASS)


# Pregunta 1 ------------------------------------------------------------------------
## a) Suavizamiento polinomios locales
censo_1971 <- read.csv('Datasets/cps71.txt', sep = ' ')
censo_1971 <- na.omit(censo_1971)
head(censo_1971)

edad       <- censo_1971$age
log_sueldo <- censo_1971$logwage

plot(edad, log_sueldo, 
     pch = 19, xlab = "Edad", ylab = "log(Sueldo)",
     main = "Relación edad-sueldo (Censo 1971 Canadá)")

kernel_gaussiano <- function(x){return(dnorm(x, 0, 1))}

r2.edad     <- NULL
edad2       <- edad^2
n           <- length(edad)

### Probando ancho de banda
h        <- 1
for(i in 1:n){
 w       <- kernel_gaussiano((edad[i] - edad)/h)
 modelo  <- lm(log_sueldo ~ edad + edad2, weights=w)	
 r2.edad[i] <- fitted(modelo)[i]
}
lines(edad, r2.edad, col="green", lwd=2)

h        <- 2 
for(i in 1:n){
 w       <- kernel_gaussiano((edad[i] - edad)/h)
 modelo  <- lm(log_sueldo ~ edad + edad2, weights=w)	
 r2.edad[i] <- fitted(modelo)[i]
}
lines(edad, r2.edad, col="blue", lwd=2)

h        <- 5 
for(i in 1:n){
 w       <- kernel_gaussiano((edad[i] - edad)/h)
 modelo  <- lm(log_sueldo ~ edad + edad2, weights=w)	
 r2.edad[i] <- fitted(modelo)[i]
}
lines(edad, r2.edad, col="red", lwd=2)

## b) Validación cruzada
### CV a mano

h  <- seq(1, 10, by=0.1)
m  <- length(h) ; m
CV <- NULL

for(k in 1:m){
 y.gorro <- NULL
 for(i in 1:n){
   r.ks <- ksmooth(edad[-i],
                   log_sueldo[-i],
                   kernel="normal",
                   bandwidth = h[k],
                   x.points = edad[i])
 y.gorro[i] <- r.ks$y 
}
 CV[k] <- mean((log_sueldo - y.gorro)^2)
}

indices <- seq(1, m, 1)
h0      <- h[indices[CV==min(CV)]]
h0

h        <- h0
for(i in 1:n){
 w       <- kernel_gaussiano((edad[i] - edad)/h)
 modelo  <- lm(log_sueldo ~ edad + edad2, weights=w)	
 r2.edad[i] <- fitted(modelo)[i]
}
lines(edad, r2.edad, col="purple", lwd=2)

### Usando np.cv de PLRModels

aux    <- cbind(log_sueldo, edad) # np.cv pide datos en este formato
resultados_cv <- PLRModels::np.cv(
  data = aux,
  h.seq = NULL,
  num.h = m,
  w = NULL,
  num.ln = 1, 
  ln.0 = 0,
  step.ln = 2,
  estimator = "NW",
  kernel = "quadratic"
)

names(resultados_cv)
resultados_cv$h.opt ; h0

# Pregunta 2 ------------------------------------------------------------------------
## a) Gráfico de dispersión
datos_oro <- read.csv2('Datasets/oro20172018.csv', sep=";")
names(datos_oro)

datos_oro$Oro   <- as.numeric(datos_oro$Oro)
datos_oro$Fecha <- as.Date(datos_oro$Fecha, "%d-%m-%Y")
plot(datos_oro$Fecha, datos_oro$Oro,
     xlab = "Fecha",
     ylab = "Valor del Oro (USD)",
     main = 'Evolución del precio del oro a través del tiempo',
     pch = 19)

## b) Polinomios locales grado 0 

h  <- 40 # ir probando de 30 en adelante.
lines(ksmooth(datos_oro$Fecha, datos_oro$Oro, kernel="normal", bandwidth=h),
      col="green", lwd=2)
lines(ksmooth(datos_oro$Fecha, datos_oro$Oro, kernel="box", bandwidth=h),
      col="orange", lwd=2)

### No se ve mucha diferencia, pero el gaussiano tiene una curva más suave.

## c) Polinomios locales

h                <- 50 # jugar con h.
fecha            <- as.numeric(datos_oro$Fecha)
precio_oro       <- datos_oro$Oro
fecha2           <- fecha^2
kernel_gaussiano <- function(x){return(dnorm(x, 0, 1))}

n          <- length(fecha)
r0.fecha   <- NULL
r1.fecha   <- NULL
r2.fecha   <- NULL

for(i in 1:n){
 w       <- kernel_gaussiano((fecha[i] - fecha)/h)
 modelo0 <- lm(precio_oro ~ 1, weights=w)	
 modelo1 <- lm(precio_oro ~ fecha, weights=w)	
 modelo2 <- lm(precio_oro ~ fecha + fecha2, weights=w)	
 
 r0.fecha[i]<-fitted(modelo0)[i]
 r1.fecha[i]<-fitted(modelo1)[i]
 r2.fecha[i]<-fitted(modelo2)[i]
}

plot(datos_oro$Fecha, datos_oro$Oro,
     xlab = "Fecha",
     ylab = "Valor del Oro (USD)",
     main = 'Evolución del precio del oro a través del tiempo',
     pch = 19)

lines(sort(fecha), r0.fecha[order(fecha)], col="red", lwd = 3)
lines(sort(fecha), r1.fecha[order(fecha)], col="cyan", lwd = 3)
lines(sort(fecha), r2.fecha[order(fecha)], col="pink", lwd = 3)

legend("topright", title = 'Grado polinomio',
       col= c("red", "cyan", "pink"),
       legend = c("grado 0", "grado 1", "grado 2"),
       lty=1, lwd=3)

### Los tres se comportan bastante bien, hay más efecto en los extremos.

## d) LOESS

plot(datos_oro$Fecha, datos_oro$Oro,
     xlab = "Fecha",
     ylab = "Valor del Oro (USD)",
     main = 'Evolución del precio del oro a traveś del tiempo',
     pch = 19)

### span: parámetro de suavizamiento de la curva.

modelo1 <- loess(precio_oro ~ fecha, span = 0.1, degree = 1)
lines(fecha, modelo1$fitted, lwd = 3, col = "red")

modelo2 <- loess(precio_oro ~ fecha, span = 0.1, degree=2)
lines(fecha, modelo2$fitted, lwd = 3, col = "blue")
