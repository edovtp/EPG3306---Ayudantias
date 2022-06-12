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
## a)

datos <- read.csv2("oro20172018.csv",sep=";")
names(datos)

datos$Oro   <- as.numeric(datos$Oro)
datos$Fecha <- as.Date(datos$Fecha,"%d-%m-%Y")
plot(datos$Fecha,datos$Oro,
     xlab = "Fecha",
     ylab = "Valor del Oro",
     pch = 19)

# b 

h  <- 40 # ir probando de 30 en adelante.
lines(ksmooth(datos$Fecha, datos$Oro, kernel="normal", bandwidth = h), col="green", lwd=2)
lines(ksmooth(datos$Fecha, datos$Oro, kernel="box", bandwidth = h), col="orange", lwd=2)

# No se mucha diferencia, pero el gaussiano tiene una curva más suave.

# c

h      <- 50 # jugar con h.
x      <- as.numeric(datos$Fecha)
y      <- datos$Oro
x2     <- x^2
Kernel <- function(x){return(dnorm(x,0,1))
}
n      <-length(x)
r0.x   <-NULL
r1.x   <-NULL
r2.x   <-NULL

for(i in 1:n){
 w       <- Kernel((x[i] - x)/h)
 modelo0 <- lm(y ~ 1, weights=w)	
 modelo1 <- lm(y ~ x, weights=w)	
 modelo2 <- lm(y ~x + x2, weights=w)	
 
 r0.x[i]<-fitted(modelo0)[i]
 r1.x[i]<-fitted(modelo1)[i]
 r2.x[i]<-fitted(modelo2)[i]
}

plot(datos$Fecha,datos$Oro,
     xlab = "Fecha",
     ylab = "Valor del Oro",
     pch = 19)

lines(sort(x),r0.x[order(x)], col="red", lwd = 2)
lines(sort(x),r1.x[order(x)], col="cyan", lwd = 2)
lines(sort(x),r2.x[order(x)], col="pink", lwd = 2)

legend("bottomleft",col=c("red","cyan","pink"),c("p = 0","p = 1","p = 2"),lty=1,lwd=2)

# Los tres se comportan bastante bien, hay más efecto en los extremos.

# d)

plot(datos$Fecha,datos$Oro,
     xlab = "Fecha",
     ylab = "Valor del Oro",
     pch = 19)

# span: parámetro de suavizamiento de la curva.

modelo1 <- loess(y ~ x,span = 0.1,degree = 1)
lines(x, modelo1$fitted, lwd = 2, col = "red")

modelo2 <- loess(y ~ x, span = 0.1,degree=2)
lines(x, modelo2$fitted, lwd = 2, col = "blue")
