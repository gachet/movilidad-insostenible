library(tidyverse)
library(lubridate)

setwd('~/Documents/_projects/2018/movilidad-insostenible/residencia_trabajo/r/')


# ============================== READ DATA ============================== #
routes <- read_csv('../data/routes.csv', col_types = 'ccccccciiiici') %>% 
  mutate(
    mode_traffic = ifelse(mode != 'driving', mode,  paste(mode, traffic, sep = '_')), 
    mode_traffic = factor(mode_traffic, levels = c('walking' , 'bicycling', 'transit', 'driving_best_guess','driving_optimistic', 'driving_pessimistic')),
    distance = ifelse(origin_code != destination_code, distance, 0),
    duration = ifelse(origin_code != destination_code, duration, 0),
    duration_in_traffic = ifelse(origin_code != destination_code, duration_in_traffic, 0)
  ) 
  
routes_f <- routes %>%
  filter(origin != destination & !is.na(duration_in_traffic)) %>% 
  filter(mode != 'driving' | traffic == 'best_guess')
  

routes_top <- routes_f %>% 
  group_by(route) %>% 
  summarise(workers_count = sum(workers_count, na.rm = T)) %>% 
  top_n(40) %>% 
  .$route

locations <- read_csv('../data/locations.csv', col_types = 'ciiiddicccc')
locations_sorted <- locations %>% 
  arrange(-workers_count) %>% 
  .$name



# en total, cuantas personas pueden ir en cada medio de transporte: más rápido y más sostenible
# lo mismo por origen

# hay mucha diferencia de tiempo entre modos de transporte

# para cada rango de 1 minuto, para menos de 30 minutos ¿cual es la distribución de los medios de transporte?
# identificar las rutas en bici, para hablar de ellas por encima (de leganés a móstoles, de chamartín a ciudad lineal... )

# ============================== QUICKEST & 'SUSTAINABLEST' ============================== #
quickest <- routes_f %>% 
  group_by(route) %>% 
  slice(which.min(duration)) %>% 
  mutate(option = 'quickest') %>% 
  select(route, origin_code, destination_code, origin, destination, option, mode, duration_in_traffic, workers_count) %>% 
  mutate(mode = factor(mode, levels = c('walking' , 'bicycling', 'transit', 'driving')))
ggplot(quickest, aes(x = duration_in_traffic, weight = workers_count, fill = mode, colour = mode)) + 
  geom_density(position = position_stack(reverse = TRUE), alpha = 0.5)


sustainablest_b <- routes_f %>%
  distinct(route, mode, duration_in_traffic) %>% 
  spread(mode, duration_in_traffic) %>% 
  mutate(mode = case_when(
    walking < 1831 ~ 'walking',
    bicycling < 1831 ~ 'bicycling',
    transit < 1831 ~ 'transit',
    TRUE ~ 'driving'
  )) %>% 
  distinct(route, mode) %>% 
  left_join(routes_f) %>% 
  mutate(option = 'sustainablest',
         mode = ifelse(duration_in_traffic < 1831, mode, 'no mode')) %>% 
  select(route, origin_code, destination_code, origin, destination, option, mode, duration_in_traffic, workers_count) %>% 
  mutate(mode = factor(mode, levels = c('walking' , 'bicycling', 'transit', 'driving', 'no mode')))

## number of workers by mode and by time
modes <- sustainablest_b %>% 
  mutate(minutes = round(duration_in_traffic/60)) %>% 
  group_by(mode, minutes) %>% 
  summarise(workers = sum(workers_count, na.rm = T)) %>% 
  spread(mode, workers)

write_csv(modes, '../data/workers_by_mode.csv')

densPlot <- ggplot(sustainablest, aes(x = duration_in_traffic, weight = workers_count/sum(workers_count, na.rm = T), fill = mode, colour = mode)) + 
  # geom_density(position = position_stack(reverse = TRUE), alpha = 0.5) +
  geom_density(alpha = 0.5) +
  facet_grid(mode ~ .) +
  xlim(0, 4000)

sustainablest %>% 
  group_by(mode) %>% 
  summarise(
    w_count = sum(workers_count),
    w_per = sum(workers_count)/sum(.$workers_count)
  ) %>% 
  ggplot(aes(mode, w_count, fill = mode)) +   
    geom_col() +
    geom_text(aes(label = w_count), vjust = -1)




### Density routes
# target
densPlot
ggplot(sustainablest, aes(mode, duration_in_traffic, weight = workers_count/ sum(sustainablest$workers_count), fill = mode, colour = mode)) +
  geom_violin() +
  coord_flip()


dens <- density(sustainablest$duration_in_traffic, weights = sustainablest$workers_count/ sum(sustainablest$workers_count))

transit = sustainablest %>% 
  filter(mode == 'bicycling') 

density(transit$duration_in_traffic, weights = transit$workers_count/ sum(transit$workers_count)) %>% 
  plot()



# percentage of people
timelapse <- 1801
sustainablest_30 <- sustainablest %>% 
  filter(duration_in_traffic < timelapse) %>% 
  mutate(period = cut(duration_in_traffic, breaks=seq(0, 1800, 60), labels = paste0(seq(1, 30, 1), ' min'))) %>% 
  group_by(period, mode) %>% 
  summarise(
    routes_count = n(),
    workers_count = sum(workers_count),
    workers_per = workers_count/sum(.$workers_count)
  )

ggplot(sustainablest_30, aes(period, workers_count, fill = mode)) +   
  geom_bar( stat = 'identity')

# 400.000/ 1.924.542 personas pueden llegar al trabajo en bici en media hora
# 444 170/ 1.924.542 no pueden llegar en 30 min de ninguna de las maneras.

sustainablest_30 %>% 
  group_by(mode) %>% 
  summarise(workers_count = sum(workers_count)) %>% 
  ggplot(aes(mode, workers_count, fill = mode)) +   
    geom_bar( stat = 'identity')+
    geom_text(aes(label = workers_count), vjust = -1)

### ¿en qué municipios hay un porcentaje mayor de gente que puede ir en bici/andando...?
tmp <- sustainablest %>% 
  mutate(mode = ifelse(duration_in_traffic < 1801, as.character(mode), 'no mode')) %>% 
  group_by(origin) %>%
  mutate(muni_workers = sum(workers_count)) %>% 
  ungroup() %>% 
  group_by(origin, mode) %>%
  mutate(routes_count = n(),
          workers_count = sum(workers_count),
          workers_per = sum(workers_count)) %>% 
  ungroup() %>% 
  distinct(origin_code, origin, mode, routes_count, workers_count, muni_workers) %>% 
  arrange(muni_workers) %>% 
  mutate(mode = factor(mode, levels = c('walking', 'bicycling', 'transit', 'driving', 'no mode')),
         origin = factor(origin, levels = unique(.$origin)),
         workers_per = workers_count/muni_workers)



# grouped bars
ggplot(tmp, aes(x = origin, y = workers_count, fill = mode)) + 
  geom_bar(stat = 'identity', position = "stack") + 
  coord_flip()

# table bars
ggplot(tmp, aes(x = origin, y = workers_per, fill = mode)) + 
  geom_bar(stat = 'identity') + 
  facet_grid(. ~ mode) +
  coord_flip()

### !!!!!!!!!!!!!! Si toda esa gente fuera en bici, cuantos coches menos? Cuantas emisiones menos de CO2

### !!!!!!!!!!!!!! ¿qué rutas aglutinan más gente en bici?
bicycling <-  sustainablest %>% 
  filter(mode == 'bicycling') %>% 
  arrange(workers_count) %>% 
  mutate(label = paste(origin, destination, sep = ' - ')) %>% 
  # mutate(label = factor(label, levesl = unique(.$label))) %>% 
  top_n(100, workers_count) %>% 
  ggplot(aes(reorder(label, workers_count), workers_count)) +
  geom_bar(stat = 'identity') +
  coord_flip()


### !!!!!!!!!!!!!! ¿en qué municipios sólo se puede ir en coche?
# En general municipios lejanos
tmp %>% 
  group_by(origin) %>% 
  filter(all(!c('walking', 'bicycling', 'transit') %in% mode)) %>% 
  ggplot(aes(x = origin, y = workers_per, fill = mode)) + 
    geom_bar(stat = 'identity') + 
    facet_grid(. ~ mode) +
    coord_flip()

# si hay alguno cercano, sería 'punto negro'.

### !!!!!!!!!!!!!! ¿en qué municipios sólo se podría ir sólo en transporte sostenible?
# No hay municipios en los que se pueda ir sólo en transporte sostenible
# No hay municipios en los que todos los trabajadores puedan llegar en 30 min.


### !!!!!!!!!!!!!! Análissis transit:
# da rodeo?
# en qué situaciones es útil?

  
### !!!!!!!!!!!!!! Machine leraning: donde deberían estar los puestos de trabajo para que la gente no tuviera que demorar más de 30 min.
# Basándonos en esto, ¿en qué municipios hay más déficit de puestos de trabajo?







### !!!!!!!!!!!!!! beeswarm, mueves la linea del tiempo, y te dice: mode transport mix (% de gente en cada modo, rutas sostenibles que aglutinan más gente )

### !!!!!!!!!!!!!! No me importa ir más tiempo en bici/andando





# el 77% de las personas pueden tardar menos de 30 min en llegar al trabajo
sum(sustainablest_30$workers_count)/sum(sustainablest$workers_count)



