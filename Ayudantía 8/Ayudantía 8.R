library(tidyverse)
library(patchwork)
library(nls2)
library(nlstools)
library(nlsMicrobio)
library(PLRModels)


# Ayudantía 1 -----------------------------------------------------------------------
## Cargamos los datos
competition1
competition1$flora <- as.factor(competition1$flora)

## a) Análisis exploratorio
ggplot(data=competition1, mapping = aes(x=t, y=LOG10N, col=flora)) +
  geom_point(size = 8) +
  labs(title = 'Evolución simultánea de la densidad bacterial',
       x = 'Tiempo (horas)', y = 'Densidad bacterial (log10)') +
  theme_minimal()

ggplot(data=competition1, mapping = aes(x=t, y=LOG10N, col = flora)) +
  geom_point(size = 8) +
  facet_grid(rows = vars(flora)) +
  labs(title = 'Evolución simultánea de la densidad bacterial',
       x = 'Tiempo (horas)', y = 'Densidad bacterial (log10)')

## b) Funciones de los modelos
jameson_wl <- function(t, y0, mu_max, t_max){
  if (t < t_max){
    aux <- mu_max/log(10)*t
  } else {
    aux <- mu_max/log(10)*t_max
  }
  
  y_t <- y0 + aux
  return(y_t)}
jameson_wl <- Vectorize(jameson_wl)

jameson_bu <- function(t, y0, mu_max, t_max, lambda){
  if (t < lambda){
    aux <- 0
  } else if (t < t_max){
    aux <- mu_max/log(10)*(t - lambda)
  } else {
    aux <- mu_max/log(10)*(t_max - lambda)
  }
  
  y_t <- y0 + aux
  return(y_t)
}
jameson_bu <- Vectorize(jameson_bu)

## c) Ajuste de modelos
### Flora 1
model_jw_f1 <- nls(
  formula = LOG10N ~ jameson_wl(t, y0, mu_max, t_max),
  data    = filter(competition1, flora == 1),
  start   = list(mu_max = 1, y0 = 1, t_max = 12)
)

model_jb_f1 <- nls(
  formula = LOG10N ~ jameson_bu(t, y0, mu_max, t_max, lambda),
  data    = filter(competition1, flora == 1),
  start   = list(mu_max = 1, y0 = 1, t_max = 12, lambda = 2)
)

summary(model_jw_f1)
nlstools::overview(model_jw_f1)
cov2cor(summary(model_jw_f1)$cov.unscaled)

summary(model_jb_f1)
nlstools::overview(model_jb_f1)
cov2cor(summary(model_jb_f1)$cov.unscaled)

#### Gráfico de los ajustes
competition1 %>% 
  filter(flora == 1) %>% 
  ggplot(mapping = aes(x = t, y = LOG10N)) +
  geom_point(size = 8) +
  stat_function(fun = jameson_wl, args = list(mu_max = 1.3, y0 = 0.4, t_max = 13),
                col = 'red', lwd = 1.7) +
  stat_function(fun = jameson_bu, args = list(mu_max = 1.44, y0 = 0.9,
                                              lambda = 1.56, t_max = 12.59),
                col = 'blue', lwd = 1.7, lty = 'dashed') +
  labs(title = 'Modelo de regresión no lineal ajustado',
       subtitle = 'Flora 1 - Sin lag',
       x = 'Tiempo (horas)', y = 'Densidad bacterial (log10)') +
  theme_minimal()

nlstools::plotfit(model_jw_f1, smooth = TRUE)
nlstools::plotfit(model_jb_f1, smooth = TRUE)

#### Test de hipótesis
anova(model_jw_f1, model_jb_f1) # No rechazamos H0

### Flora 2
model_jw_f2 <- nls(
  formula = LOG10N ~ jameson_wl(t, y0, mu_max, t_max),
  data    = filter(competition1, flora == 2),
  start   = list(mu_max = 1, y0 = 4, t_max = 12)
)

model_jb_f2 <- nls(
  formula = LOG10N ~ jameson_bu(t, y0, mu_max, t_max, lambda),
  data    = filter(competition1, flora == 2),
  start   = list(mu_max = 1, y0 = 4, t_max = 12, lambda = 2)
)

summary(model_jw_f2)
nlstools::overview(model_jw_f2)
cov2cor(summary(model_jw_f2)$cov.unscaled)

summary(model_jb_f2)
nlstools::overview(model_jb_f2)
cov2cor(summary(model_jb_f2)$cov.unscaled)

#### Gráfico de los ajustes
competition1 %>% 
  filter(flora == 2) %>% 
  ggplot(mapping = aes(x = t, y = LOG10N)) +
  geom_point(size = 8) +
  stat_function(fun = jameson_wl, args = list(mu_max = 1, y0 = 2.95, t_max = 13),
                col = 'red', lwd = 1.7) +
  stat_function(fun = jameson_bu, args = list(mu_max = 1.25, y0 = 3.63,
                                              lambda = 3.02, t_max = 12.19),
                col = 'blue', lwd = 1.7, lty = 'dashed') +
  labs(title = 'Modelo de regresión no lineal ajustado',
     subtitle = 'Flora 2 - Sin lag',
     x = 'Tiempo (horas)', y = 'Densidad bacterial (log10)') +
  theme_minimal()

nlstools::plotfit(model_jw_f1, smooth = TRUE)
nlstools::plotfit(model_jb_f1, smooth = TRUE)

#### Test de hipótesis
anova(model_jw_f2, model_jb_f2) # Rechazamos H0

## d) Formulas de los modelos
jameson_without_lag
jameson_buchanan

### Ajuste
model_jw <- nls(
  formula = jameson_without_lag,
  data    = competition1,
  start   = list(mumax_1 = 1, LOG10N0_1 = 1, tmax = 12,
                 mumax_2 = 1, LOG10N0_2 = 4)
)

model_jb <- nls(
  formula = jameson_buchanan,
  data    = competition1,
  start   = list(lag_1 = 2, mumax_1 = 1, LOG10N0_1 = 1, tmax = 12,
                 lag_2 = 2, mumax_2 = 1, LOG10N0_2 = 4)
)

nlstools::overview(model_jw)
nlstools::overview(model_jb)

#### Gráfico de los ajustes
nlstools::plotfit(model_jw, pch.obs = 1, cex = 1.5)
nlstools::plotfit(model_jb, pch.obs = 1, cex = 1.5)

plot_without_lag <- competition1 %>%
  ggplot(mapping = aes(x = t, y = LOG10N, col = flora)) +
  geom_point(size = 8) +
  stat_function(fun = jameson_wl, args = list(mu_max = 1.32, y0 = 0.38, t_max = 12.97),
                col = 'red', lwd = 1.7) +
  stat_function(fun = jameson_wl, args = list(mu_max = 1, y0 = 2.94, t_max = 12.97),
                col = 'blue', lwd = 1.7, lty = 'dashed') +
  labs(title = 'Modelo de regresión no lineal ajustado',
       subtitle = 'Sin lag',
       x = 'Tiempo (horas)', y = 'Densidad bacterial (log10)') +
  theme_minimal()

plot_jameson_buchanan <- competition1 %>%
  ggplot(mapping = aes(x = t, y = LOG10N, col = flora)) +
  geom_point(size = 8) +
  stat_function(fun = jameson_bu, args = list(mu_max = 1.47, y0 = 0.9,
                                              t_max = 12.44, lambda =1.61),
                col = 'red', lwd = 1.7) +
  stat_function(fun = jameson_bu, args = list(mu_max = 1.21, y0 = 3.63,
                                              t_max = 12.44, lambda = 2.94),
                col = 'blue', lwd = 1.7, lty = 'dashed') +
  labs(title = 'Modelo de regresión no lineal ajustado',
       subtitle = 'Con lag',
       x = 'Tiempo (horas)', y = 'Densidad bacterial (log10)') +
  theme_minimal()

plot_without_lag / plot_jameson_buchanan

#### Test de hipótesis
anova(model_jw, model_jb)

# Ayudantía 2 -----------------------------------------------------------------------

