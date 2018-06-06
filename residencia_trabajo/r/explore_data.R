library(tidyverse)
setwd('~/Documents/_projects/2018/movilidad-insostenible/residencia_trabajo/r/')

lista_origen_trabajo <- read_csv('outputs/lista_origen_trabajo.csv')
municipios <- read.xlsx(excelFile, 'Municipios y Distritos') %>% 
  set_colnames(c('ine_code', 'statistical_zone', 'name', 'province')) %>% 
  select(-statistical_zone)

nplaces <- lista_origen_trabajo %>% 
  group_by(origin_ine_code) %>% 
  summarise( places = n()) 

inOut <- lista_origen_trabajo %>% 
  mutate(destination = ifelse((origin_ine_code == destination_ine_code), 'inside', 'outside')) %>%  
  group_by(origin_ine_code, destination) %>% 
  summarise(count = sum(workers_count)) %>% 
  spread(destination, count) %>% 
  mutate(
    total_workers = sum(inside, outside, na.rm = TRUE),
    inside_per = inside / total_workers,
    outside_per = outside / total_workers
  ) %>% 
  left_join(nplaces) %>% 
  left_join(municipios, by = c('origin_ine_code' = 'ine_code'))
  

###### MEKKO PLOT #####
mekko <- inOut %>% 
  mutate(label = ifelse(province != 'Madrid', paste0(name, ' (', province, ')'), name)) %>% 
  rename('code' = origin_ine_code) %>% 
  mutate(total_per = (total_workers / sum(inOut$total_workers, na.rm = T))) %>% 
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
library(rgdal)
library(rgeos)

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
