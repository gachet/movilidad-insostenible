library(openxlsx)
library(tidyverse)
library(magrittr)
library(jsonlite)

setwd('~/Documents/_projects/2018/movilidad-insostenible/residencia_trabajo/r/')

#######################################
########## SET THE ARGUMENTS ##########
#######################################

#done!
# driving & best_guess
# driving & pessimistic
# driving & optimistic
# transit
# bicycling
# walking

# mode: "transit", "bicycling", "walking", "driving" 
mode <- 'driving'

# traffic_model: 'best_guess',  'pessimistic', 'optimistic'
traffic <- 'optimistic'

# departure_time: in seconds from 1970
dept_time <- '1530000000' # to seconds de una fecha legible, elegir por el empleo del tiempo, a qué hora se va a trabajar, a qué hora se vuelve...





############################################
########## ORIGINS & DESTINATIONS ##########
############################################
# read the 'municipios' sheet from the excel
excelFile <-  '../raw_data/residencia_trabajo_distritos.xlsx'

municipios <- read.xlsx(excelFile, 'Municipios y Distritos') %>% 
  set_colnames(c('ine_code', 'statistical_zone', 'name', 'province')) %>% 
  mutate(loc_string = paste(gsub(' ', '+', name), province, 'spain', sep = '+')) %>% 
  transform(loc_string = ifelse(!grepl('Chamartín', name), loc_string, 'Chamartín+Madrid+Comunidad+de+Madrid+spain'))

lista_origen_trabajo <- read.xlsx(excelFile, 'Lista origen-destino') %>% 
  set_colnames(c('route', 
                 'origin_ine_code', 
                 'destination_ine_code', 
                 'workers_count'))

# write.csv(lista_origen_trabajo, 'outputs/lista_origen_trabajo.csv')
codes <- unique(lista_origen_trabajo$origin_ine_code)


results <- data_frame()
for (c in 1:length(codes)) {
  code <- codes[c]
  ########### ORIGIN & DESTINATIONS ###########
  origin <- municipios %>% 
    filter(ine_code == code) %>% 
    .$loc_string
  
  # api limit: 25 origins or 25 destinations
  # cut the  sbst_dest 
  sbst_dest <- lista_origen_trabajo %>% 
    filter(origin_ine_code == code) %>% 
    mutate(group = ceiling(row_number()/23))
  
  for (g in unique(sbst_dest$group)) {
    sbst_group <- filter(sbst_dest, group == g)
   
    destination <- c()
    for (dest_code in unique(sbst_group$destination_ine_code)) {
      tmp <- municipios %>% 
        filter(ine_code == dest_code) %>% 
        .$loc_string
      
      destination <- paste(tmp, destination, sep = '|')
    }
    
    ########### BUILD & CALL URL ###########
    url <- paste0('https://maps.googleapis.com/maps/api/distancematrix/json?origins=',
                  origin,
                  '&destinations=',
                  destination,
                  '&mode=',
                  mode,
                  '&departure_time=',
                  dept_time,
                  '&traffic_model=',
                  traffic,
                  '&key=',
                  APIkey)
    
    raw <- fromJSON(url)
    
    ########### RESHAPE THE DATA ###########
    for (i in 1:(length(raw$destination_addresses))) {
      # los datos
      j <- length(raw$destination_addresses) - i + 1;
      tmp <- tmp <- jsonlite::flatten(raw$rows[1, ][[1]])
      if (tmp[j, ] != 'ZERO_RESULTS') {
        sbst <- jsonlite::flatten(raw$rows[1, ][[1]][j, ]) %>%
          select(-contains('text')) 
          
      } else {
        sbst <- data_frame(status = 'ZERO_RESULTS',
                           distance.value = NA, 
                           duration.value = NA
                           )
      }
      
      sbst <- sbst %>% 
        mutate('origin' = raw$origin_addresses,
               'origin_ine_code' = code,
               'dest_ine_code' = sbst_group$destination_ine_code[i],
               'destination' = raw$destination_addresses[j])
      
      results <- bind_rows(results, sbst)
    }
  }
}




write_csv(results, 'outputs/temp_results.csv') # bicycling

########### PRETIFY THE OUTPUT ###########
results <- results %>% 
  mutate('mode' = mode,
         'traffic' = traffic,
         'departure_time' = dept_time,
         'duration_in_traffic.value' = ifelse(mode != 'driving', NA, duration_in_traffic.value))

names(results) <- gsub('.value', '', colnames(results), fixed = TRUE)

unique(results$status) # OK

results <- results %>% 
  distinct(status, distance, duration, duration_in_traffic, origin, origin_ine_code, destination, dest_ine_code, mode, traffic, departure_time)

########### WRITE THE OUTPUT ###########
filename <- paste(mode, traffic, '.csv', sep = '_')
filename <- paste(mode, '_chamartin.csv', sep = '')
write_csv(results, paste0('outputs/distances/', filename))

########### REMOVE OBJECTS ###########
rm(APIkey, c, code, codes, dept_time, destination, g, lista_origen_trabajo, mode, municipios, origin, raw, sbst, sbst_dest, sbst_group, tmp, traffic, url, dest_code, i)


########### MERGE FILES ###########

filePath <- 'outputs/distances'
files <-  list.files(filePath)

distances <- data_frame()
for (file in files) {
  name <- unlist(str_split(file, '\\.'))[1]
  df <- read_csv(paste(filePath, file, sep = '/'), col_types = 'ciicccccccc')
  assign(name, df)
  distances <- rbind(distances, df)
}

distances <- distances %>% 
  arrange(origin, destination)

write_csv(distances, 'outputs/distances/all_distances.csv')

