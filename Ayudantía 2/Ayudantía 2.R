library(tidyverse)


# Ejercicio 1 - Test de una cola ----------------------------------------------------

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

tasa_media

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
