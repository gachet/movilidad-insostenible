library(openxlsx)
library(tidyverse)
library(lubridate)
library(magrittr)
library(scales)
library(Hmisc)
library(rgdal)
library(rgeos)
library(plotly)
setwd('~/Documents/_projects/2018/movilidad-insostenible/residencia_trabajo/r/')

######################## READ THE DATA #########################
## Routes
distances <- read_csv('outputs/distances/all_distances.csv', col_types = 'ciicccciccicci')
excelFile <-  '../raw_data/residencia_trabajo_distritos.xlsx'
routes <- read.xlsx(excelFile, 'Lista origen-destino') %>% 
  set_colnames(c('route', 
                 'origin_ine_code', 
                 'destination_ine_code', 
                 'workers_count'))

routes_complete <- routes %>% 
  left_join(distances)

nodos20top <- nodos %>% 
  ungroup() %>% 
  top_n(20, total_workers)
  

############################################################################
######################## DISTANCE MEDIAN / AVERAGE #########################
############################################################################

# Which is the min dist
min_dist <- routes_complete %>% 
  filter(mode != 'driving' | mode_traffic == 'driving_best_guess') %>% 
  mutate(mode = factor(mode, levels = c('walking', 'bicycling', 'transit', 'driving'))) %>% 
  group_by(route) %>% 
  slice(which.min(distance))

table(min_dist$mode)

ggplot(min_dist, aes(mode)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust = 2, colour = 'white') +
  labs(title = "Modo de transporte más corto (metros de distancia)",
       subtitle = "Base total: 19.941 rutas",
       y = 'Número de rutas',
       x = 'Modo de transporte')

min_dist %>% 
  filter(origin_ine_code %in% nodos20top$ine_code) %>% 
  ggplot(aes(mode)) +
    geom_bar() +
    geom_text(stat='count', aes(label=..count..), vjust = 2, colour = 'white') +
    labs(title = "Modo de transporte más corto (metros de distancia)",
        subtitle = "Base total: 19.941 rutas",
         y = 'Número de rutas',
        x = 'Modo de transporte') +
    facet_grid(origin ~ .,  switch = 'both') +
    theme(strip.text.y = element_text(angle = 180))


# cuáles son las rutas más cortas en transporte
min_dist_f_t <- min_dist %>% 
  filter(origin_ine_code != dest_ine_code & mode == 'transit') 

transit <- routes_complete %>% 
  filter(mode != 'driving' | mode_traffic == 'driving_best_guess') %>%
  filter(route %in% min_dist_f_t$route) %>% 
  distinct(route, origin, destination, mode_traffic, distance) %>% 
  spread(mode_traffic, distance) %>% 
  mutate(dif = driving_best_guess - transit)

summary(transit)

# mean and median distance walking
walking <- routes_complete %>% 
  filter(mode == 'walking')

ggplot(walking, aes(mode, distance)) + 
  geom_boxplot() +
  labs(title = "Distribución de las rutas",
       subtitle = "Base total: 19.941 rutas",
       y = 'Distancia (metros)') 

# Ponderada por personas
ggplot(walking, aes(mode, distance, weight = workers_count)) + 
  geom_boxplot() +
  labs(title = "Distribución de las personas",
       subtitle = "Base total: 2.309.226 personas",
       y = 'Distancia (metros)') 

total_mean <- wtd.mean(walking$distance, weights=walking$workers_count)
total_median <- as.numeric(wtd.quantile(walking$distance, weights=walking$workers_count)[3])


# Ponderada por personas top 20
tmp <- walking %>% 
  filter(origin_ine_code %in% nodos20top$ine_code) %>% 
  group_by(origin_ine_code) %>% 
  mutate(median = as.numeric(wtd.quantile(distance, weights=workers_count)[3])) %>% 
  ungroup() %>% 
  arrange(median) %>% 
  mutate(origin = factor(origin, levels = c("Salamanca, Madrid, Spain", "Chamberí, Madrid, Spain", "Chamartín, Madrid, Spain",  "Centro, Madrid, Spain", "Tetuán, Madrid, Spain", "Ciudad Lineal, Madrid, Spain",  "Arganzuela, Madrid, Spain", "Puente de Vallecas, Madrid, Spain", "San Blas, Madrid, Spain",  "Hortaleza, Madrid, Spain", "Latina, Madrid, Spain", "Carabanchel, Madrid, Spain",  "Villaverde, Madrid, Spain" , "Leganés, Madrid, Spain", "Getafe, Madrid, Spain",  "Alcala de Henares, Madrid, Spain", "Alcorcón, Madrid, Spain", "Fuenlabrada, Madrid, Spain",  "Fuencarral-El Pardo, 28048 Madrid, Spain", "Móstoles, Madrid, Spain"))) 

ggplot(tmp, aes(origin, distance, weight = workers_count)) + 
  geom_boxplot() +
  labs(title = "Distribución de las personas",
       y = 'Distancia (metros)') +
  geom_hline(yintercept = total_median, color = 'red') +
  coord_flip()



#all origin plotted in a map
walking_g <- walking %>% 
  group_by(origin_ine_code) %>% 
  mutate(mean = wtd.mean(distance, weights=workers_count),
            mean_dif = mean - total_mean,
            median = as.numeric(wtd.quantile(distance, weights=workers_count)[3]),
            median_dif = median - total_median
  ) %>% 
  distinct(origin_ine_code, origin, mean, mean_dif, median, median_dif)

distritos <- readOGR('../shapefiles/distritos/200001563.shp')

distritosLongLat <- spTransform(distritos, CRS("+proj=longlat"))  
distritosLongLat@data$id <- rownames(distritosLongLat@data)

dataDistritos <- fortify(distritosLongLat, region="id") 

tmp <- dataDistritos %>% 
  left_join(distritosLongLat@data) %>% 
  rowwise() %>% 
  mutate(
    code_muni = paste0('28', str_split(DESBDT, '-')[[1]][1]),
    code_distrito = str_split(DESBDT, '-')[[1]][2],
    code = ifelse(code_muni != '28079', code_muni, paste0('28', DESBDT))
  ) 

tmp <- tmp %>% 
  left_join(walking_g, by = c('code' = 'origin_ine_code'))


map <- ggplot() + 
  geom_polygon(aes(text=code, x=long, y=lat, group = group, fill = median_dif), data = tmp) + 
  scale_fill_distiller(palette = "Spectral")+ 
  labs(title='Diferencia distancia (m) a la mediana total',
       subtitle = "Base total: 2.309.226 personas",
       fill='Diferencia (m)') +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()
  )
map

ggplotly(map)



########################################################################
######################## TIME MEDIAN / AVERAGE #########################
########################################################################

# Which is the min time
min_time <- routes_complete %>% 
  filter(mode != 'driving' | mode_traffic == 'driving_best_guess') %>% 
  mutate(mode = factor(mode, levels = c('walking', 'bicycling', 'transit', 'driving'))) %>% 
  group_by(route) %>% 
  slice(which.min(real_duration))

table(min_time$mode)

ggplot(min_time, aes(mode)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust = -1, colour = 'grey20') +
  labs(title = "Modo de transporte más rápido",
       subtitle = "Base total: 19.941 rutas",
       y = 'Número de rutas',
       x = 'Modo de transporte')

min_time %>% 
  filter(origin_ine_code %in% nodos20top$ine_code) %>% 
  ggplot(aes(mode)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust = 2, colour = 'white') +
  labs(title = "Modo de transporte más corto (metros de distancia)",
       subtitle = "Base total: 19.941 rutas",
       y = 'Número de rutas',
       x = 'Modo de transporte') +
  facet_grid(origin ~ .,  switch = 'both') +
  theme(strip.text.y = element_text(angle = 180))


# cuáles son las rutas más cortas en bici
min_time_f_b <- min_time %>% 
  filter(origin_ine_code != dest_ine_code & mode == 'bicycling') 

bici <- routes_complete %>% 
  filter(mode != 'driving' | mode_traffic == 'driving_best_guess') %>%
  filter(route %in% min_time_f$route) %>% 
  filter(mode == 'bicycling' | mode == 'driving') %>% 
  distinct(route, origin, destination, mode_traffic, real_duration) %>% 
  spread(mode_traffic, real_duration) %>% 
  filter(!is.na(driving_best_guess)) %>% 
  mutate(dif = (driving_best_guess - bicycling)/60)

summary(bici)

# cuáles son las rutas más cortas en transporte
min_time_f_t <- min_time %>% 
  filter(origin_ine_code != dest_ine_code & mode == 'transit') 

transit <- routes_complete %>% 
  filter(mode != 'driving' | mode_traffic == 'driving_best_guess') %>%
  filter(route %in% min_time_f_t$route) %>% 
  filter(mode == 'transit' | mode == 'driving') %>% 
  distinct(route, origin, destination, mode_traffic, real_duration) %>% 
  spread(mode_traffic, real_duration) %>% 
  mutate(dif = (driving_best_guess - transit)/60)

summary(transit)



### mean and median time driving
driving <- routes_complete %>% 
  filter(mode_traffic == 'driving_best_guess') %>% 
  mutate(real_duration_min = seconds_to_period(real_duration))

ggplot(driving, aes(mode, real_duration)) + 
  geom_boxplot() +
  labs(title = "Distribución de las rutas",
       subtitle = "Base total: 19.941 rutas",
       y = 'Tiempo (segundos)') 

summary(driving)

# Ponderada por personas
ggplot(driving, aes(mode, real_duration, weight = workers_count)) + 
  geom_boxplot() +
  labs(title = "Distribución de las personas",
       subtitle = "Base total: 2.309.226 personas",
       y = 'Tiempo (segundos)') 

total_mean <- wtd.mean(driving$real_duration, weights=driving$workers_count)
total_median <- as.numeric(wtd.quantile(driving$real_duration, weights=driving$workers_count)[3])


# Ponderada por personas top 20
tmp <- driving %>% 
  select(-real_duration_min) %>% 
  filter(origin_ine_code %in% nodos20top$ine_code) %>% 
  group_by(origin_ine_code) %>% 
  mutate(median = as.numeric(wtd.quantile(real_duration, weights=workers_count)[3])) %>% 
  ungroup() %>% 
  arrange(median) %>% 
  mutate(origin = factor(origin, levels = rev(unique(tmp$origin))))

ggplot(tmp, aes(origin, real_duration, weight = workers_count)) + 
  geom_boxplot() +
  labs(title = "Distribución de las personas",
       y = 'Tiempo (segundos)') +
  geom_hline(yintercept = total_median, color = 'red') +
  coord_flip()




#all origin plotted in a map
driving_g <- driving %>% 
  group_by(origin_ine_code) %>% 
  mutate(mean = wtd.mean(real_duration, weights=workers_count),
            mean_dif = mean - total_mean,
            median = as.numeric(wtd.quantile(real_duration, weights=workers_count)[3]),
            median_dif = median - total_median
  ) %>% 
  distinct(origin_ine_code, origin, mean, mean_dif, median, median_dif)


distritos <- readOGR('../shapefiles/distritos/200001563.shp')

distritosLongLat <- spTransform(distritos, CRS("+proj=longlat"))  
distritosLongLat@data$id <- rownames(distritosLongLat@data)

dataDistritos <- fortify(distritosLongLat, region="id") 

tmp <- dataDistritos %>% 
  left_join(distritosLongLat@data) %>% 
  rowwise() %>% 
  mutate(
    code_muni = paste0('28', str_split(DESBDT, '-')[[1]][1]),
    code_distrito = str_split(DESBDT, '-')[[1]][2],
    code = ifelse(code_muni != '28079', code_muni, paste0('28', DESBDT))
  )  %>% 
  left_join(driving_g, by = c('code' = 'origin_ine_code'))


map <- ggplot() + 
  geom_polygon(aes(text = origin,x=long, y=lat, group = group, fill = median_dif), data = tmp) + 
  scale_fill_distiller(palette = "Spectral", limits = c(-500, 1000), na.value = "grey90")+ 
  labs(title='Diferencia tiempo (s) a la mediana total',
       subtitle = "Base total: 2.309.226 personas",
       fill='Diferencia (s)') +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()
  )
map

aux <- tmp %>% distinct(code, origin, mean_dif)
ggplotly(map, tooltip = 'text')



###################################################################
######################## DISTANCE ~ TIME  #########################
###################################################################

dist_time <- walking %>% 
  distinct(route, origin, destination, distance, workers_count) %>% 
  rename('distance_walking' = distance) %>% 
  left_join(driving) %>% 
  distinct(route, origin, destination, distance_walking, real_duration, workers_count) %>% 
  filter(distance_walking < 70000 & distance_walking > 0)

m1 = lm(real_duration ~ distance_walking, data=dist_time, na.action = 'na.exclude')

dist_time <- dist_time %>% 
  mutate(resid=abs(resid(m1)),
         fitted=fitted(m1))

ggplot(dist_time %>% mutate(resid=cut(abs(resid(m1)), 4),
                         fitted=fitted(m1))) +
  geom_line(aes(distance_walking, fitted)) + 
  geom_point(aes(distance_walking, real_duration, colour=resid)) +
  scale_colour_manual(values=hcl(0,100,seq(70,20,len=4))) +
  theme_classic() +
  labs(x="Weight", y="MPG", colour="Residuals")

plot <- ggplot(dist_time, aes(text = paste(route, origin, destination))) +
  geom_point(aes(distance_walking, real_duration, colour=resid, size = workers_count), alpha = 0.5) +
  scale_colour_gradient(low="blue", high="red") +
  labs(title='Relación distancia tiempo',
       subtitle = "Base total: 19.941 rutas",
       size='Número de personas',
       fill='Distancia residual',
       y = 'Tiempo en coche (s)',
       x = 'Distancia caminando (m)') 

plot
ggplotly(plot, tooltip = 'text')



##############################################################################
######################## INSIDE -  OUTSIDE BY ORIGIN #########################
##############################################################################
excelFile <-  '../raw_data/residencia_trabajo_distritos.xlsx'
routes <- read.xlsx(excelFile, 'Lista origen-destino') %>% 
  set_colnames(c('route', 
                 'origin_ine_code', 
                 'destination_ine_code', 
                 'workers_count'))
municipios <- read.xlsx(excelFile, 'Municipios y Distritos') %>% 
  set_colnames(c('ine_code', 'statistical_zone', 'name', 'province')) %>% 
  select(-statistical_zone)


nplaces <- routes %>% 
  group_by(origin_ine_code) %>% 
  summarise( places = n()) %>% 
  rename('ine_code' = origin_ine_code)

nodos <- routes %>% 
  mutate(destination = ifelse((origin_ine_code == destination_ine_code), 'inside', 'outside')) %>%  
  group_by(origin_ine_code, destination) %>% 
  summarise(count = sum(workers_count)) %>% 
  spread(destination, count) %>% 
  mutate(
    total_workers = sum(inside, outside, na.rm = TRUE),
    inside_per = inside / total_workers,
    outside_per = outside / total_workers
  ) %>% 
  rename('ine_code' = origin_ine_code) %>% 
  left_join(nplaces) %>% 
  left_join(municipios)
  
write_csv(nodos, 'outputs/nodos.csv')

###### MEKKO PLOT #####
mekko <- nodos %>% 
  mutate(label = ifelse(province != 'Madrid', paste0(name, ' (', province, ')'), name)) %>% 
  rename('code' = origin_ine_code) %>% 
  mutate(total_per = (total_workers / sum(nodos$total_workers, na.rm = T))) %>% 
  arrange(total_per) %>% 
  mutate(temp = 1) %>% 
  group_by(temp) %>% 
  mutate(
    xmax = cumsum(total_per),
    xmin = xmax - total_per
  ) %>% 
  ungroup() %>% 
  select(code, label, inside_per, outside_per, total_per, xmax, xmin) %>% 
  gather(key = destination, value = percentage, inside_per, outside_per) %>% 
  transform(percentage = ifelse(is.na(percentage), 0, percentage)) %>% 
  group_by(code) %>% 
  arrange(destination) %>% 
  mutate(
    ymax = cumsum(percentage),
    ymin = ymax - percentage
  ) %>% 
  ungroup() %>% 
  mutate(xtext = xmin + (xmax - xmin) / 2) 

mekko_plot <- ggplot(mekko, aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax, fill = destination)) +
  geom_rect(colour = "white") +
  geom_text(aes(x = xtext, y = 0.98, label = label), size = 4, hjust = "inward") +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  theme(
    panel.background = element_blank(),
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  )

ggsave('outputs/imgs/inside_outside_mekko.png', mekko_plot, width = 15.2, height = 30)

###### MAP #####


distritos <- readOGR('../shapefiles/distritos/200001563.shp')
    

head(distritos@data) 

distritosLongLat <- spTransform(distritos, CRS("+proj=longlat"))  
distritosLongLat@data$id <- rownames(distritosLongLat@data)
head(distritosLongLat@data)

dataDistritos <- fortify(distritosLongLat, region="id") 

tmp <- dataDistritos %>% 
  left_join(distritosLongLat@data) %>% 
  rowwise() %>% 
  mutate(
    code_muni = paste0('28', str_split(DESBDT, '-')[[1]][1]),
    code_distrito = str_split(DESBDT, '-')[[1]][2],
    code = ifelse(code_muni != '28079', code_muni, paste0('28', DESBDT))
  ) %>% 
  left_join(inOut, by = c('code' = 'origin_ine_code'))


map <- ggplot() + 
  geom_polygon(aes(x=long, y=lat, group = group, fill = outside_per), data = tmp) + 
  # scale_fill_gradient(low="#fff0eb", high="#f4561e", limits=c(min(tmp$outside_per), max(tmp$outside_per))) +
  scale_fill_continuous(high = "#ed340c", low = "#ffffeb") +
  labs(fill='% trabajadores que se desplazan') +
  theme(legend.position = 'bottom',
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()
      )
map

ggsave('outputs/imgs/map.png', map)


##########################################################################
######################## PERCENT INSIDE MARCHETTI ########################
##########################################################################

distances <- read_csv('outputs/distances/all_distances.csv', col_types = 'ciicccciccicci')

topNodos <- nodos %>% 
  arrange(-total_workers) %>% 
  filter(total_workers > 29999)

distances_filt <- distances %>% 
  filter(origin_ine_code %in% topNodos$ine_code) %>%
  filter(origin_ine_code !=  '28079-08') %>%
  mutate(mode_traffic = ifelse(is.na(traffic), mode, paste(mode, traffic, sep = '_')),
         real_duration = ifelse(is.na(duration_in_traffic), duration, duration_in_traffic)) %>% 
  filter(!is.na(real_duration))

#  Time distribution
time_distribution <- ggplot(distances_filt, aes(real_duration, colour = mode_traffic)) +
  geom_density() +
  geom_vline(xintercept= 1800, color = "grey") +
  xlim(0, 10000) +
  facet_grid(origin ~ .)

ggsave('outputs/imgs/time_distribution.png', plot, width = 10, height = 40)



##########################################################################
############################# TRANSPORT MODE #############################
##########################################################################

distances <- read_csv('outputs/distances/all_distances.csv', col_types = 'ciicccciccicci')
nodos <- read_csv('outputs/nodos.csv', col_types = 'ciiiddicc')

#  Modes percentages
modes30min <- distances %>% 
  filter(mode != 'driving' | traffic == 'best_guess') %>% 
  distinct(route, mode_traffic, real_duration) %>% 
  spread(mode_traffic, real_duration) %>% 
  mutate(best = case_when(
    walking < 1801 ~ 'walking',
    bicycling < 1801 ~ 'bicycling',
    transit < 1801 ~ 'transit',
    driving_best_guess < 1801 ~ 'driving_best_guess',
    TRUE ~ 'más de 30 min'
  )) %>% 
  left_join(routes)

write_csv(modes30min, 'outputs/modes30min.csv')

total_modes30min <- modes30min %>% 
  group_by(best) %>% 
  summarise(routes_count = n(),
            routes_per = round(n() * 100/ nrow(routes), 2),
            workers_count = sum(workers_count, na.rm = T),
            workers_per = round(sum(workers_count, na.rm = T) * 100/ sum(routes$workers_count, na.rm = T), 2)
            )

modes <- data_frame()
for (i in 1:nrow(nodos)) {
  code <- nodos$ine_code[i]
  
  sbst <- modes30min %>% 
    filter(grepl(code, route)) %>% 
    group_by(best) %>% 
    summarise(routes_count = n(),
              workers_count = sum(workers_count, na.rm = T)) %>% 
    mutate(code = code)
  
  modes <- rbind(modes, sbst)
}


##########################################################################
############### PEOPLE INSIDE MARCHETTI by TRANSPORT MODE ################
##########################################################################

excelFile <-  '../raw_data/residencia_trabajo_distritos.xlsx'
routes <- read.xlsx(excelFile, 'Lista origen-destino') %>% 
  set_colnames(c('route', 
                 'origin_ine_code', 
                 'destination_ine_code', 
                 'workers_count'))
nodos <- read_csv('outputs/nodos.csv', col_types = 'ciiiddicc')
modes30min <-  read_csv('outputs/modes30min.csv', col_types = 'ciiiiccci')


# Para cada tipo de transporte
# cuántas personas hay en cada segundo

# Ejemplo Arganzuela

code <- '28079-02' # Arganzuela

arg <- modes30min %>%
  filter(grepl(code, route)) %>% 
  gather(key = "mode", value = "duration", bicycling, driving_best_guess, walking, transit) %>% 
  # mutate(mode = factor(mode, levels = c('walking', 'bicycling', 'transit', 'driving_best_guess')))
  mutate(mode = factor(mode, levels = c('driving_best_guess', 'transit', 'bicycling', 'walking')))
  


ggplot(arg, aes(duration,  fill = mode, colour = mode, weight = workers_count/sum(workers_count))) +
  geom_density(position = "stack", alpha = 0.3) +
  xlim(c(0, 40000))


data_f <- distances %>% 
  left_join(routes) %>% 
  filter(mode != 'driving' | mode_traffic == 'driving_best_guess') %>% 
  filter(origin_ine_code != dest_ine_code) %>% 
  filter(origin_ine_code == '19024' | origin_ine_code == '28079-03') %>% 
  filter(workers_count > 10) %>% 
  # filter(distance < 10000) %>% 
  arrange(-workers_count) %>% 
  mutate(route = as.factor(route))

ggplot(data_f, aes(real_duration, 0)) + 
  # geom_hline(yintercept= 0, color = "grey80") +
  geom_line(aes(group = route), alpha = 0.1, color = "grey80") +
  geom_point(aes(size = workers_count, colour = factor(mode_traffic)), alpha = 0.5) +
  facet_grid(origin + route ~ ., switch = 'both') +
  geom_vline(xintercept= 1800, color = "grey50")
  # theme(strip.text.y = element_text(angle = 180),
  #       panel.background = element_blank(),
  #       panel.grid = element_blank(),
  #       axis.ticks = element_blank(),
  #       axis.text = element_blank(),
  #       axis.title = element_blank())
  


ggsave('outputs/imgs/routes_distance_time.png')
###########################################################################
######################### ROUTES DISTANCE VS TIME #########################
###########################################################################

# para cada trayecto, tiempo e bus tiempo en coche
# para cada origen, desviación de los trayectos respecto al coche: distancia coche vs bus y desviación tiempo coche bus. Cuadrantes, menos tiempo, menos distancia, mucho más tiempo, mucha más distancia...


