library(openxlsx)
library(tidyverse)
library(magrittr)
library(jsonlite)

setwd('~/Documents/_projects/2018/movilidad-insostenible/residencia_trabajo/r/')

#######################################
########## SET THE ARGUMENTS ##########
#######################################

APIkey <-  'AIzaSyAiEWRKSQfdgtDDDW3nhGL6K2atuPtwDlk'
APIkey <-  'AIzaSyCnC87kEtNLXFAKtpA4GWLwbrm6QROnteU'
# APIkey <-  'AIzaSyATzdnKH-qC4E_ZaI7ZfR6OcO57EGO4qOI'
# APIkey <-  'AIzaSyDQaIHzsAKx4_fZUInaCyw3ETTirvhRZ1I'
# APIkey <-  'AIzaSyDUgEyIXzsZVzvwxS9_MigaFG46mevDiqM'
# APIkey <-  'AIzaSyChK54c4Yh5Ue0LZL2e_l1HBA1jQsIxgAk'
# APIkey <-  'AIzaSyC7L2CfpwftaDK6XYoTDRN3ER6EAbc1tEM'
# APIkey <-  'AIzaSyDUe5aWAN_P2UFHVgcMefP1dBrcqWKjRj0'
# APIkey <-  'AIzaSyD4cYwV1h-fcdnmGH_Ewg1uV7cWYjxromg'
# APIkey <-  'AIzaSyDQFPzJ0Sn4AmGzGuqpVduDvuNRmzjsUyg'
# APIkey <-  'AIzaSyCyu6bC0uPOtC-JTQllUV6MCUoErWDHW6Q'


#done!
# driving & best_guess
# driving & pessimistic

# mode: "driving",  "walking",  "bicycling",  "transit"
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
  mutate(loc_string = paste(gsub(' ', '+', name), province, 'spain', sep = '+'))

lista_origen_trabajo <- read.xlsx(excelFile, 'Lista origen-destino') %>% 
  set_colnames(c('route', 
                 'origin_ine_code', 
                 'destination_ine_code', 
                 'workers_count'))

# write.csv(lista_origen_trabajo, 'outputs/lista_origen_trabajo.csv')
codes <- unique(lista_origen_trabajo$origin_ine_code)

# results <- data_frame()
# results <- read_csv('outputs/temp_results.csv') 

for (c in 69:length(codes)) {
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
      sbst <- jsonlite::flatten(raw$rows[1, ][[1]][j, ]) %>%
        select(-contains('text')) %>% 
        mutate('origin' = raw$origin_addresses,
               'origin_ine_code' = code,
               'dest_ine_code' = sbst_group$destination_ine_code[i],
               'destination' = raw$destination_addresses[j])
      
      results <- bind_rows(results, sbst)
    }
  }
}

write_csv(results, 'outputs/temp_results.csv') # drivin optimistic 69

########### PRETIFY THE OUTPUT ###########
results <- results %>% 
  mutate('mode' = mode,
         'traffic' = traffic,
         'departure_time' = dept_time)

names(results) <- gsub('.value', '', colnames(results), fixed = TRUE)

unique(results$status) # OK

results <- results %>% 
  distinct(status, distance, duration, duration_in_traffic, origin, destination, mode, traffic, departure_time)

########### WRITE THE OUTPUT ###########
filename <- paste(mode, traffic, 'distances.csv', sep = '_')
write_csv(results, paste0('outputs/', filename))

########### REMOVE OBJECTS ###########
rm(APIkey, c, code, codes, dept_time, destination, g, lista_origen_trabajo, mode, municipios, origin, raw, sbst, sbst_dest, sbst_group, tmp, traffic, url, dest_code, i)

