##### AYUDANTÍA 2 ####
######################

# Problema 2 ----------------------------------------------------------------------------------

library(rio)
setwd("~/Dropbox/AyudantiaMEI")

pesos = rio::import("Pesos.txt")
View(pesos)

X = pesos$Peso[pesos$Sexo == "F"] ; mean(X)
Y = pesos$Peso[pesos$Sexo == "M"] ; mean(Y)

nX = length(X)
nY = length(Y)

# H0 : mu.X - mu.Y >= 0 vs H1: mu.X - mu.Y < 0 

# Chequeo de supuestos  ------------------------------------------------------------------------

par(mfrow=c(1,2))

hist(X, col = "violet", las = 1, main = "Pesos mujeres")
hist(Y, col = "blue", las = 1, main = "Pesos hombres")

plot(density(X), main = "Densidad pesos mujeres", 
     col = "violet", las = 1, lwd = "3",
     xlab = "X")
plot(density(X), main = "Densidad pesos hombres", 
     col = "blue", las = 1, lwd = "3",
     xlab = "Y")

# las dos densidades sobrepuesta por el otro y los histogramas.

par(mfrow=c(1,2))

qqnorm(X, pch = 20,
       main = "QQplot peso mujeres")
qqline(X, col = "violet")

qqnorm(Y, pch = 20,
       main = "QQplot peso de hombres")
qqline(Y, col = "blue")

shapiro.test(X)
shapiro.test(Y)

boxplot(X,Y, col = c("violet", "blue"), las = 1, main = "Boxplot X e Y")
medias = c(mean(X),mean(Y))
points(medias, pch = 20, col = "green")

# Chequeo de varianzas ------------------------------------------------------------------------

var(X) ; var(Y)
var.test(X,Y) # test de comparación de varianzas

# H0 sigma2X/sigma2Y = 1 vs H1 sigma2X/sigma2Y != 1

# Cálculo de df y t0 --------------------------------------------------------------------------

alpha = 0.05
df = round((((var(X)/nX) + (var(Y))/nY)^2)/((var(X)/nX)^2/(nX-1) + (var(Y)/nY)^2/(nY-1)))
df
t0 = (mean(X)-mean(Y))/sqrt(var(X)/nX + var(Y)/nY) ; t0
t0 <= qt(alpha, df) # esto es a "mano" 

t.test(X,Y, alternative = "less", var.equal = FALSE, conf.level = 1-alpha) # test.

valor.p = pt(t0, df) ; valor.p

# Se rechaza H0

# Problema 3 ----------------------------------------------------------------------------------

dev.off()

datos = rio::import("hospitales.txt")
head(datos, 5)
dim(datos)

# X0 v.a de hospital de tipo 0, X1 v.a de hospital de tipo 1

X0 = datos[datos$tipo == 0,"rodilla"]
X1 = datos[datos$tipo == 1, "rodilla"]
n0 = length(X0)
n1 = length(X1)

p0.hat = mean(X0) ; p0.hat # proporción de operación de rodillas hosp. tipo 0
p1.hat = mean(X1) ; p1.hat # proporción de operación de rodillas hosp. tipo 1
p.hat = mean(datos$rodilla) 

# p0.hat distribuye aprox. Normal(p0,p0*(1-p0)/n0)
# p1.hat distribuye aprox. Normal(p1,p1*(1-p1)/n1)

# H0 : p1 - p0 <= 0 v/s H1: p1 - p0 > 0 
# Bajo H0 se tiene que p0 - p1 = 0 y (1-p0)*p0 = (1-p1)*p1 = (1-p)*p

alpha = 0.05
sp2 = p.hat*(1-p.hat) # estimador insesgado de p*(1-p)
z0 = (p1.hat-p0.hat)/sqrt(sp2*(1/n0+1/n1)) ; z0
z0 >= qnorm(1-alpha)

valor.p = 1-pnorm(z0) ; valor.p

# no se rechaza H0.