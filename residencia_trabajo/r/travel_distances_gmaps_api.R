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
mode <- 'transit'

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

routes <- read.xlsx(excelFile, 'Lista origen-destino') %>% 
  set_colnames(c('route', 
                 'origin_code', 
                 'destination_code', 
                 'workers_count'))

codes <- unique(routes$origin_code)

results <- data_frame()
for (c in 251:length(codes)) {
  code <- codes[c]
  ########### ORIGIN & DESTINATIONS ###########
  origin <- municipios %>% 
    filter(ine_code == code) %>% 
    .$loc_string
  
  # api limit: 25 origins or 25 destinations
  # cut the  sbst_dest 
  sbst_dest <- routes %>% 
    filter(origin_code == code) %>% 
    mutate(group = ceiling(row_number()/23))
  
  for (g in unique(sbst_dest$group)) {
    sbst_group <- filter(sbst_dest, group == g)
   
    destination <- c()
    for (dest_code in unique(sbst_group$destination_code)) {
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
                  # '&traffic_model=',
                  # traffic,
                  '&key=',
                  APIkey)
    
    raw <- fromJSON(url)
    
    ########### RESHAPE THE DATA ###########
    for (i in 1:(length(raw$destination_addresses))) {
      # los datos
      j <- length(raw$destination_addresses) - i + 1;
      tmp <- jsonlite::flatten(raw$rows[1, ][[1]])
      
    
      # if (tmp[j, ] != 'ZERO_RESULTS') {
      #   sbst <- jsonlite::flatten(raw$rows[1, ][[1]][j, ]) %>%
      #     select(-contains('text'))
      # 
      # } else {
      #   sbst <- data_frame(status = 'ZERO_RESULTS',
      #                      distance.value = NA,
      #                      duration.value = NA
      #                      )
      # }
      sbst <- jsonlite::flatten(raw$rows[1, ][[1]][j, ]) %>%
        select(-contains('text')) 
      
      sbst <- sbst %>% 
        mutate('origin' = municipios %>% 
                 filter(ine_code == code) %>% 
                 .$name,
               'origin_code' = code,
               'destination_code' = sbst_group$destination_code[i],
               'destination' = municipios %>% 
                 filter(ine_code == destination_code) %>% 
                 .$name)
      
      results <- bind_rows(results, sbst)
    }
  }
}




write_csv(results, 'outputs/temp_results_best_guess_cercedilla.csv') 

########### PRETIFY THE OUTPUT ###########
results <- results %>% 
  mutate('mode' = mode,
         'traffic' = traffic,
         'departure_time' = dept_time,
         'duration_in_traffic.value' = ifelse(mode != 'driving', duration.value, duration_in_traffic.value))

names(results) <- gsub('.value', '', colnames(results), fixed = TRUE)

unique(results$status) # OK

results <- results %>% 
  distinct(status, origin_code, origin, destination_code, destination,  mode, traffic, distance, duration, duration_in_traffic, departure_time) %>% 
  select(status, origin, origin_code, destination, destination_code,  mode, traffic, distance, duration, duration_in_traffic, departure_time)

write_csv(results, 'outputs/temp_results_best_guess_cercedilla.csv') 



########### WRITE THE OUTPUT ###########
filename <- paste0(mode, '_' ,traffic, '.csv', sep = '')
write_csv(results, paste0('outputs/distances/', filename))

########### REMOVE OBJECTS ###########
rm(APIkey, c, code, codes, dept_time, destination, g, routes, mode, municipios, origin, raw, sbst, sbst_dest, sbst_group, tmp, traffic, url, dest_code, i)



########### MERGE FILES ###########

filePath <- 'outputs/distances'
files <-  list.files(filePath)

distances <- data_frame()
for (file in files) {
  name <- unlist(str_split(file, '\\.'))[1]
  print(name)
  df <- read_csv(paste(filePath, file, sep = '/'), col_types = 'ccccccciiic')
  
  assign(name, df)
  distances <- rbind(distances, df)
  

  name_na <- paste0(name, '_na')
  assign(name_na, filter(df, is.na(duration)))
}
rm(df, file, filePath, files, name, walking, bicycling, transit, driving_best_guess, driving_optimistic, driving_pessimistic)


# Hay NA's en 10027 casos del transit (más del 50%)

