library(tidyverse)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(inlmisc)
library(DT)
library(pool)
library(config)
library(sf)
library(plotly)
library(lubridate)
library(pool)
library(geojsonsf)
library(pins)
library(sqldf)
library(config)



# Server connection things
conn <- config::get("connectionSettings") # get configuration settings

## For testing: connect to ODS production
#pool <- dbPool(
#  drv = odbc::odbc(),
#  Driver = "SQL Server Native Client 11.0", 
#  Server= "DEQ-SQLODS-PROD,50000",
#  dbname = "ODS",
#  trusted_connection = "yes"
#)

# For deployment on the R server: Set up pool connection to production environment
pool <- dbPool(
  drv = odbc::odbc(),
  Driver = "SQLServer",   # note the LACK OF space between SQL and Server ( how RStudio named driver)
  # Production Environment
  Server= "DEQ-SQLODS-PROD,50000",
  dbname = "ODS",
  UID = conn$UID_prod, 
  PWD = conn$PWD_prod,
  #UID = Sys.getenv("userid_production"), # need to change in Connect {vars}
  #PWD = Sys.getenv("pwd_production")   # need to change in Connect {vars}
  # Test environment
  #Server= "WSQ04151,50000",
  #dbname = "ODS_test",
  #UID = Sys.getenv("userid"),  # need to change in Connect {vars}
  #PWD = Sys.getenv("pwd"),  # need to change in Connect {vars}
  trusted_connection = "yes"
)



# Pull as many details about station from REST service (if available). Work around provided in case station isn't on REST server
## Pull station info from REST service
WQM_Station_Full_REST_request <- function(pool, station){
  WQM_Station_Full_REST <- suppressWarnings(
    geojson_sf(
      paste0("https://gis.deq.virginia.gov/arcgis/rest/services/staff/DEQInternalDataViewer/MapServer/104/query?&where=STATION_ID%3D%27",
             toupper(station),"%27&outFields=*&f=geojson")))
  
  if(nrow(WQM_Station_Full_REST ) > 0){
    WQM_Station_Full_REST <- mutate(WQM_Station_Full_REST, WQM_YRS_YEAR = ifelse(!is.na(WQM_YRS_YEAR), lubridate::year(as.Date(as.POSIXct(WQM_YRS_YEAR/1000, origin="1970-01-01"))), NA))
    WQM_Station_Full_REST <- bind_cols(WQM_Station_Full_REST, st_coordinates(WQM_Station_Full_REST) %>% as_tibble()) %>%
      mutate(Latitude = Y, Longitude = X) # add lat/lng in DD
  } else { # station doesn't yet exist in WQM full dataset
    # get what we can from CEDS
    stationGISInfo <- pool %>% tbl( "WQM_Sta_GIS_View") %>%
      filter(Station_Id %in% !! toupper(station)) %>%
      as_tibble() 
    # pull a known station to steal data structure
    WQM_Station_Full_REST <- suppressWarnings(
      geojson_sf(
        paste0("https://gis.deq.virginia.gov/arcgis/rest/services/staff/DEQInternalDataViewer/MapServer/104/query?&where=STATION_ID%3D%272-JKS023.61%27&outFields=*&f=geojson")))[1,] %>%
      mutate(WQM_YRS_YEAR = ifelse(!is.na(WQM_YRS_YEAR), lubridate::year(as.Date(as.POSIXct(WQM_YRS_YEAR/1000, origin="1970-01-01"))), NA)) %>%
      st_drop_geometry()
    WQM_Station_Full_REST <- bind_rows(WQM_Station_Full_REST[0,],
                                       tibble(STATION_ID = stationGISInfo$Station_Id, 
                                              Latitude = stationGISInfo$Latitude,
                                              Longitude = stationGISInfo$Longitude,
                                              BASINS_HUC_8_NAME = stationGISInfo$Huc6_Huc_8_Name, 
                                              BASINS_VAHU6 = stationGISInfo$Huc6_Vahu6) ) %>%
      st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
               remove = F, # don't remove these lat/lon cols from df
               crs = 4326)    }
  return(WQM_Station_Full_REST) }

## Pull CEDS Station Information 
stationInfoConsolidated <- function(pool, station, WQM_Station_Full_REST){
  left_join(pool %>% tbl("Wqm_Stations_View") %>%  
              # need to repull data instead of calling stationInfo bc app crashes
              filter(Sta_Id %in% !! toupper(station)) %>%
              as_tibble() %>%
              # add link to data and add link to internal GIS web app with WQS layer on there
              mutate(`CEDS Station View Link` = paste0("<b><a href='https://ceds.deq.virginia.gov/ui#wqmStations/",
                                                       Sta_Id,"'", 
                                                       " target= '_blank'> View Monitoring Station in CEDS</a></b>"),
                     `DEQ GIS Web App Link` =  paste0("<b><a href='https://gis.deq.virginia.gov/GISStaffApplication/?query=WQM%20Stations%20(All%20stations%20with%20full%20attributes),STATION_ID,",
                                                      Sta_Id, 
                                                      "&showLayers=DEQInternalDataViewer_1723;WATER%20LAYERS;WQM%20Stations%20(All%20stations%20with%20full%20attributes);", 
                                                      ";2020%20Draft%20ADB%20WQA%20Layers;2020%20Rivers%20(Any%20Use)&level=14' target='_blank'>View Monitoring Station in DEQ Staff App</a></b>" )) %>%
              dplyr::select(Sta_Id, Sta_Desc, `CEDS Station View Link`, `DEQ GIS Web App Link`, everything()), 
            dplyr::select(WQM_Station_Full_REST, #WQM_STATIONS_FINAL, 
                          STATION_ID, Latitude, Longitude, WQM_STA_STRAHER_ORDER, EPA_ECO_US_L3CODE,
                          EPA_ECO_US_L3NAME, BASINS_HUC_8_NAME, BASINS_VAHU6, WQS_WATER_NAME, WQS_SEC, WQS_CLASS, 
                          WQS_SPSTDS, WQS_PWS, WQS_TROUT, WQS_TIER_III),
            by = c('Sta_Id' = 'STATION_ID')) %>%
    dplyr::select(Sta_Id, Sta_Desc, `CEDS Station View Link`, `DEQ GIS Web App Link`, Latitude, Longitude, WQM_STA_STRAHER_ORDER, EPA_ECO_US_L3CODE,
                  EPA_ECO_US_L3NAME, BASINS_HUC_8_NAME, BASINS_VAHU6, WQS_WATER_NAME, WQS_SEC, WQS_CLASS, 
                  WQS_SPSTDS, WQS_PWS, WQS_TROUT, WQS_TIER_III, everything())
}


# Quick Station Sampling Summary Information
stationSummarySampingMetrics <- function(stationInfo_sf){
  stationInfo_sf %>%
    group_by(STATION_ID) %>%
    mutate(`Years Sampled` = WQM_YRS_YEAR) %>% #paste0(year(WQM_YRS_YEAR))) %>% # for when column coming in as date
    dplyr::select(STATION_ID, WQM_YRS_SPG_CODE,WQM_YRS_YEAR,`Years Sampled`,WQM_SPG_DESCRIPTION) %>%
    st_drop_geometry() %>%
    group_by(STATION_ID, `Years Sampled`) %>%
    summarise(`Sample Codes` = paste0(WQM_YRS_SPG_CODE, collapse = ' | '))
} 

# Organize field and analyte info into prettier table
stationFieldAnalyteDataPretty <- function(stationAnalyteDataRaw, stationFieldDataRaw, repFilter){
  inner_join(stationFieldDataRaw,
             stationAnalyteDataRaw %>%
               filter(Ana_Sam_Mrs_Container_Id_Desc %in% repFilter) %>%
               group_by(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc, Ana_Sam_Mrs_Lcc_Parm_Group_Cd) %>%
               dplyr::select(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc, Ana_Sam_Mrs_Lcc_Parm_Group_Cd, Ana_Parameter_Name, Ana_Value) %>%
               pivot_wider(names_from = c('Ana_Parameter_Name','Ana_Sam_Mrs_Lcc_Parm_Group_Cd'), names_sep = " | ", 
                           values_from = "Ana_Value"),
             by = c("Fdt_Id" = "Ana_Sam_Fdt_Id", 'Fdt_Sta_Id', 'Fdt_Date_Time')) 
}