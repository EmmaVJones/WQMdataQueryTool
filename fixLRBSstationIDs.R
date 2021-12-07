#fix incorrect stationID's in LRBS dataset

library(tidyverse)
library(fuzzyjoin)
library(pins)



# Server connection things
conn <- config::get("connectionSettings") # get configuration settings


board_register_rsconnect(key = conn$CONNECT_API_KEY,  #Sys.getenv("CONNECT_API_KEY"),
                         server = conn$CONNECT_SERVER)#Sys.getenv("CONNECT_SERVER"))


LRBS <- pin_get('ejones/LRBS', board = 'rsconnect')
stations <- pin_get('ejones/WQM-Stations-Spatial', board = 'rsconnect')

LRBSstationsMissing <- left_join(LRBS, stations, by = 'StationID') %>% 
  filter(is.na(Latitude) | is.na(Longitude)) 


potentialOptions <- stringdist_join(
  dplyr::select(LRBSstationsMissing, SampleID, StationID, Date),
  dplyr::select(stations, StationID:Sta_Desc),
  by = "StationID",
  mode = "left",
  ignore_case = TRUE, 
  #method = "jw", 
  max_dist = 2, 
  distance_col = "dist") 
