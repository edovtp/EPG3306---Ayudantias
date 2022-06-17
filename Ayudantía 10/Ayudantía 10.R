library(tidyverse)
library(ROCR)
library(pROC)
library(ggplotify)


# Ayudantía 10 y final :( -----------------------------------------------------------

# a) Cargamos los datos
datos_cm <- readr::read_csv(file = 'Datasets/breast-cancer-wisconsin.csv')
View(datos_cm)

## Notamos que los valores faltantes están indicados por ?
datos_cm <- readr::read_csv(file = 'Datasets/breast-cancer-wisconsin.csv',
                            na = '?')
summary(datos_cm)
dim(datos_cm)

## Contamos el número de NAs
sapply(datos_cm, function(x) sum(is.na(x)))

## Trabajaremos sin los NAs, pero es necesario estudiar si esto es correcto o no
datos_cm <- na.omit(datos_cm)
dim(datos_cm)

## No nos interesa el id
datos_cm <- dplyr::select(datos_cm, -id)
summary(datos_cm)

## Pasamos class a factor con valores 0 y 1
datos_cm$class_num <- recode(datos_cm$class, `2` = 0, `4` = 1)
datos_cm$class <- factor(datos_cm$class, levels = c(2, 4),
                         labels = c('benign', 'malignant'))

## Vemos cada gráfico por separado
ggplot(data = datos_cm, mapping = aes_string(x = 'ct', y = 'class')) +
  geom_point()

### Tenemos que mover un poco los puntos para que se note la cantidad
for (col in names(datos_cm)){
  if (col == 'class' | col == 'class_num')
    break
  print(ggplot(data = datos_cm, mapping = aes_string(x = col, y = 'class')) +
    geom_jitter(width = 0.3, height = 0.3))
}


# b) Modelo de regresión logit
modelo_rl_1b <- glm(formula = class ~ . - class_num, data = datos_cm,
                   family = binomial(link = 'logit'))
summary(modelo_rl_1b)

## Test de hipótesis a mano
### Versus modelo saturado
dev_1b <- modelo_rl_1b$deviance
n <- nrow(datos_cm)
p <- length(coef(modelo_rl_1b))
valor_p_sat <- pchisq(dev_1b, df = n - p, lower.tail = FALSE)
valor_p_sat # No rechazamos H0 con respecto al modelo saturado

### Versus modelo nulo
dev_nulo <- modelo_rl_1b$null.deviance
G <- dev_nulo - dev_1b
valor_p_nulo <- pchisq(G, df = p - 1, lower.tail = FALSE)
valor_p_nulo # Rechazamos con respecto al modelo nulo

summary(modelo_rl_1b)

# c) Modelo de regresión probit
modelo_rl_1c <- glm(formula = class ~ . - class_num, data = datos_cm,
                    family = binomial(link = 'probit'))
summary(modelo_rl_1c)
summary(modelo_rl_1b)

## Test de hipótesis a mano
### Versus modelo saturado
dev_1c <- modelo_rl_1c$deviance
n <- nrow(datos_cm)
p <- length(coef(modelo_rl_1c))
valor_p_sat <- pchisq(dev_1c, df = n - p, lower.tail = FALSE)
valor_p_sat # No rechazamos H0 con respecto al modelo saturado

### Versus modelo nulo
dev_nulo <- modelo_rl_1c$null.deviance
G <- dev_nulo - dev_1c
valor_p_nulo <- pchisq(G, df = p - 1, lower.tail = FALSE)
valor_p_nulo # Rechazamos con respecto al modelo nulo

# d) Selección de variables forward - modelo logit
## Partimos con el modelo nulo
datos_cm_aux <- datos_cm %>% dplyr::select(-class_num)

modelo_nulo <- glm(formula = class ~ 1, data = datos_cm_aux,
                   family = binomial(link = 'logit'))
add1(modelo_nulo, ~ . + ct + ucsize + ucshape + ma + sepcs + bn + bc + nn + mitoses,
     test = 'Chisq')

## Agregamos ucsize (vemos el AIC)
modelo_1 <- glm(formula = class ~ ucsize, data = datos_cm_aux,
                family = binomial(link = 'logit'))
add1(modelo_1, ~ . + ucsize + ct + ucshape + ma + sepcs + bn + bc + nn + mitoses,
     test = 'Chisq')

## Agregamos bn
modelo_2 <- glm(formula = class ~ ucsize + bn, data = datos_cm_aux,
                family = binomial(link = 'logit'))
add1(modelo_2, ~ . + ucsize + bn + ct + ucshape + ma + sepcs + bc + nn + mitoses,
     test = 'Chisq')

## Agregamos ct
modelo_3 <- glm(formula = class ~ ucsize + bn + ct, data = datos_cm_aux,
                family = binomial(link = 'logit'))
summary(modelo_3)
add1(modelo_3, ~ . + ucsize + bn + ct + ucshape + ma + sepcs + bc + nn + mitoses,
     test = 'Chisq')

## Agregamos bc
modelo_4 <- glm(formula = class ~ ucsize + bn + ct + bc, data = datos_cm_aux,
                family = binomial(link = 'logit'))
add1(modelo_4, ~ . + ucsize + bn + ct + ucshape + ma + sepcs + bc + nn + mitoses,
     test = 'Chisq')

## Agregamos ma
modelo_5 <- glm(formula = class ~ ucsize + bn + ct + bc + ma, data = datos_cm_aux,
                family = binomial(link = 'logit'))
add1(modelo_5, ~ . + ucsize + bn + ct + ucshape + ma + sepcs + bc + nn + mitoses,
     test = 'Chisq')

## Agregamos nn
modelo_6 <- glm(formula = class ~ ucsize + bn + ct + bc + ma + nn, data = datos_cm_aux,
                family = binomial(link = 'logit'))
add1(modelo_6, ~ . + ucsize + bn + ct + ucshape + ma + sepcs + bc + nn + mitoses,
     test = 'Chisq')

## Nos quedamos entonces con las anteriores, dejando afuera ucshape, sepcs y mitoses
modelo_logit <- modelo_6

## Viendo los gráficos en (a), ¿por qué eliminamos ucshape?
cor(datos_cm$ucshape, datos_cm$ucsize)
ggplot(data = datos_cm, aes(x = ucshape, y = ucsize)) +
  geom_jitter() +
  labs(title = 'Relación ucshape y ucsize')

## Test vs el modelo saturado
### Versus modelo saturado
dev_forward <- modelo_6$deviance
n <- nrow(datos_cm)
p <- length(coef(modelo_6))
valor_p_sat <- pchisq(dev_forward, df = n - p, lower.tail = FALSE)
valor_p_sat # No rechazamos H0 con respecto al modelo saturado

# d) Selección de variables forward - modelo probit
## Partimos con el modelo nulo
modelo_nulo <- glm(formula = class ~ 1, data = datos_cm_aux,
                   family = binomial(link = 'probit'))
add1(modelo_nulo, ~ . + ct + ucsize + ucshape + ma + sepcs + bn + bc + nn + mitoses,
     test = 'Chisq')

## Agregamos ucsize (vemos el AIC)
modelo_1 <- glm(formula = class ~ ucsize, data = datos_cm_aux,
                family = binomial(link = 'probit'))
add1(modelo_1, ~ . + ucsize + ct + ucshape + ma + sepcs + bn + bc + nn + mitoses,
     test = 'Chisq')

## Agregamos bn
modelo_2 <- glm(formula = class ~ ucsize + bn, data = datos_cm_aux,
                family = binomial(link = 'probit'))
add1(modelo_2, ~ . + ucsize + bn + ct + ucshape + ma + sepcs + bc + nn + mitoses,
     test = 'Chisq')

## Agregamos ct
modelo_3 <- glm(formula = class ~ ucsize + bn + ct, data = datos_cm_aux,
                family = binomial(link = 'probit'))
summary(modelo_3)
add1(modelo_3, ~ . + ucsize + bn + ct + ucshape + ma + sepcs + bc + nn + mitoses,
     test = 'Chisq')

## Agregamos bc
modelo_4 <- glm(formula = class ~ ucsize + bn + ct + bc, data = datos_cm_aux,
                family = binomial(link = 'probit'))
add1(modelo_4, ~ . + ucsize + bn + ct + ucshape + ma + sepcs + bc + nn + mitoses,
     test = 'Chisq')

## Agregamos ma
modelo_5 <- glm(formula = class ~ ucsize + bn + ct + bc + ma, data = datos_cm_aux,
                family = binomial(link = 'probit'))
add1(modelo_5, ~ . + ucsize + bn + ct + ucshape + ma + sepcs + bc + nn + mitoses,
     test = 'Chisq')

## Agregamos nn
modelo_6 <- glm(formula = class ~ ucsize + bn + ct + bc + ma + nn, data = datos_cm_aux,
                family = binomial(link = 'probit'))
add1(modelo_6, ~ . + ucsize + bn + ct + ucshape + ma + sepcs + bc + nn + mitoses,
     test = 'Chisq')

## Nos quedamos entonces con las anteriores, dejando afuera ucshape, sepcs y mitoses
## Notamos que obtenemos exactamente el mismo modelo que con logit
modelo_probit <- modelo_6

# f) Selección entre modelo logit y probit
summary(modelo_logit)
summary(modelo_probit)

## Nos quedamos con el modelo de menor AIC. De todas maneras, los valores son
## esencialmente iguales, y nos quedamos entonces con el logit al ser el más trabajado
## en clases

# g) Tablas de clasificación
## Podemos ver primero la cantidad de instancias por clase
table(datos_cm$class)

## Sensibilidad y especificidad p = 0.5
cutoff <- 0.5
prob_ajustadas <- fitted(modelo_logit)
prob_ajustadas

clases_pred <- ifelse(prob_ajustadas >= cutoff, yes = 1, no = 0)
clases_pred

addmargins(table(clases_pred, datos_cm$class))
accuracy <- (434 + 228)/683
sensibilidad <- 228/239
especificidad <- 434/444
  
accuracy
sensibilidad
especificidad

## Función para obtener sensibilidad y especificidad
sen_esp <- function(cutoff){
  clases_pred <- ifelse(prob_ajustadas >= cutoff, yes = 1, no = 0)
  tabla_pred <- addmargins(table(clases_pred, datos_cm$class))
  
  sens <- tabla_pred[2, 2]/tabla_pred[2, 3]
  esp <- tabla_pred[1, 1]/tabla_pred[1, 3]
  
  return(c(sensibilidad = sens, especificidad = esp))
}

## Aplicamos la función en una grilla de valores de corte
grilla_cutoff <- seq(0.01, 0.99, by = 0.001)
aux <- sapply(grilla_cutoff, FUN = sen_esp, simplify = TRUE)
data_sens_esp <- cbind(grilla_cutoff, t(aux)) %>% 
  as.data.frame() %>% 
  tidyr::pivot_longer(cols = c(sensibilidad, especificidad),
                      names_to = 'medida', values_to = 'valor')
head(data_sens_esp)

ggplot(data = data_sens_esp, aes(x = grilla_cutoff, y = valor, col = medida)) +
  geom_line(lwd = 2) +
  labs(x = 'Punto de corte', y = '',
       title = 'Sensibilidad y especificidad para diferentes puntos de corte')

## ¿Cómo podemos tener tan buenos resultados incluso con un muy bajo punto de corte?
hist(prob_ajustadas, main = '', col = 'salmon', lwd = 4, breaks = 100)

# h) Curva ROC y área bajo la curva
predictions <- predict(modelo_logit,
                      data = datos_cm)
head(predictions)

pred <- ROCR::prediction(predictions, labels = datos_cm$class)
perf <- ROCR::performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, xlab = '1 - especificidad', ylab = 'Sensibilidad', col = 'salmon', lwd = 2)

## Área bajo la curva
roc_obj <- pROC::roc(datos_cm$class, fitted(modelo_logit))
auc(roc_obj)
text(0.8, 0.3,
     labels = paste("AUC = ", round(auc(roc_obj), 3)),
     cex = 1.2)
