library(chilemapas)
library(ggplot2)
library(dplyr)


nombres_1 <- c('Huechuraba', 'Las Condes', 'Lo Prado', 'Lo Espejo', 'La Florida',
               'Talagante', 'Lampa', 'Buin')
nombres_2 <- c('Alhue', 'Paine', 'San Pedro', 'Melipilla',
               'Maria Pinto', 'Isla de Maipo', 'El Monte', 'Curacavi')

comunas_rm <- mapa_comunas %>% 
  filter(codigo_region == 13) %>% 
  left_join(
    codigos_territoriales %>% 
      select(matches('comuna'))
  ) %>% 
  mutate(muestra_1 = nombre_comuna %in% nombres_1) %>% 
  mutate(muestra_2 = nombre_comuna %in% nombres_2)

# Muestra 1
ggplot(comunas_rm, show.legend = FALSE) +
  geom_sf(aes(geometry = geometry, fill = muestra_1)) +
  theme_minimal() +
  labs(title = 'Muestra de comunas de la Región Metropolitana',
       subtitle = 'Primera muestra') +
  theme(legend.position = 'none')

# Muestra 2
ggplot(comunas_rm) +
  geom_sf(aes(geometry = geometry, fill = muestra_2)) +
  theme_minimal() +
  labs(title = 'Muestra de comunas de la Región Metropolitana',
       subtitle = 'Segunda muestra') +
  theme(legend.position = 'none')
