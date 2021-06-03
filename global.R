httr::set_config(httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))

library(tidyverse)
library(shiny)
library(shinybusy)
library(leaflet)
library(leaflet.extras)
library(inlmisc)
library(DT)
library(config)
library(sf)
library(plotly)
library(lubridate)
library(pool)
library(geojsonsf)
library(pins)
library(sqldf)
library(dbplyr)

#Bring in VLOOKUP-like function written in R
source('vlookup.R')
source('cdfRiskTable.R')
source('dissolvedMetalsModule.R') #also contains dissolved metals functions that are more flexible than from assessment apps

# Server connection things
conn <- config::get("connectionSettings") # get configuration settings


board_register_rsconnect(key = conn$CONNECT_API_KEY,  #Sys.getenv("CONNECT_API_KEY"),
                         server = conn$CONNECT_SERVER)#Sys.getenv("CONNECT_SERVER"))

# Retrieve Pins
WQM_Station_Full <- pin_get("ejones/WQM-Station-Full", board = "rsconnect")
Wqm_Stations_View <- pin_get("ejones/WQM-Stations-View", board = "rsconnect")
LRBS <- pin_get("ejones/LRBS", board = 'rsconnect')

mCCUmetals <- c("HARDNESS, CA MG CALCULATED (MG/L AS CACO3) AS DISSOLVED", "ARSENIC, DISSOLVED  (UG/L AS AS)",
                "CHROMIUM, DISSOLVED (UG/L AS CR)", "COPPER, DISSOLVED (UG/L AS CU)",
                "LEAD, DISSOLVED (UG/L AS PB)", "NICKEL, DISSOLVED (UG/L AS NI)","ZINC, DISSOLVED (UG/L AS ZN)")

## For testing: connect to ODS production
pool <- dbPool(
 drv = odbc::odbc(),
 Driver = "ODBC Driver 11 for SQL Server",#"SQL Server Native Client 11.0",
 Server= "DEQ-SQLODS-PROD,50000",
 dbname = "ODS",
 trusted_connection = "yes"
)

# For deployment on the R server: Set up pool connection to production environment
# pool <- dbPool(
#   drv = odbc::odbc(),
#   Driver = "SQLServer",   # note the LACK OF space between SQL and Server ( how RStudio named driver)
#   # Production Environment
#   Server= "DEQ-SQLODS-PROD,50000",
#   dbname = "ODS",
#   UID = conn$UID_prod,
#   PWD = conn$PWD_prod,
#   #UID = Sys.getenv("userid_production"), # need to change in Connect {vars}
#   #PWD = Sys.getenv("pwd_production")   # need to change in Connect {vars}
#   # Test environment
#   #Server= "WSQ04151,50000",
#   #dbname = "ODS_test",
#   #UID = Sys.getenv("userid"),  # need to change in Connect {vars}
#   #PWD = Sys.getenv("pwd"),  # need to change in Connect {vars}
#   trusted_connection = "yes"
# )

onStop(function() {
  poolClose(pool)
})

stationOptions <- pin_get('ejones/WQM-Sta-GIS-View-Stations', board= 'rsconnect')
programCodes <- pool %>% tbl(in_schema("wqm", "Wqm_Survey_Pgm_Cds_Codes_Wqm_View")) %>% as_tibble()
labMediaCodes <- pool %>% tbl(in_schema("wqm", "Wqm_Lab_Catalogs_View")) %>% as_tibble()

unitData <- read_csv('data/probParameterUnits.csv')
# Temporary list that we can compare data to. Maybe we increase to benthic metrics, MCCU, + more in time
probIndicators <- filter(unitData, AltName %in% #names(basicData))$AltName
                           c("Dissolved Oxygen", "pH", "Specific Conductance", "Total Nitrogen", "Total Phosphorus", "Total Dissolved Solids",
                             "Ammonia", "Total Nitrate Nitrogen", "Ortho Phosphorus", "Turbidity", "Total Suspended Solids", "Sodium", 
                             "Potassium", "Chloride", "Sulfate", "Suspended Sediment Concentration Coarse", "Suspended Sediment Concentration Fine",
                             "Arsenic", "Barium", "Beryllium", "Cadmium", "Chromium", "Copper", "Iron", "Lead", "Manganese", "Thallium", "Nickel",                                 
                             "Silver", "Zinc", "Antimony", "Aluminum", "Selenium", "Hardness"))
probEst <- readRDS('data/IR2020probMonCDFestimates.RDS') %>%
  filter(Indicator %in% probIndicators$Parameter) %>%
  # work around until basin system standardized on Probmon estimate side
  mutate(Subpopulation = case_when(Subpopulation == 'Roanoke Basin' ~ 'Roanoke',
                                   Subpopulation == 'James Basin' ~ 'James',
                                   Subpopulation == 'Blue Ridge Mountains' ~ 'Blue Ridge',
                                   Subpopulation == 'Central Appalachian Ridges and Valleys' ~ 'Ridge and Valley',
                                   TRUE ~ as.character(Subpopulation)))
#unique(ecoregion$US_L3NAME)
#[1] Blue Ridge                    Central Appalachians          Middle Atlantic Coastal Plain Northern Piedmont             Piedmont                     
#[6] Ridge and Valley              Southeastern Plains 


# WQS information for functions
# From: 9VAC25-260-50. Numerical Criteria for Dissolved Oxygen, Ph, and Maximum Temperature
# https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section50/
WQSvalues <- tibble(CLASS_BASIN = c('I',"II","II_7","III","IV","V","VI","VII"),
                    CLASS = c('I',"II","II","III","IV","V","VI","VII"),
                    `Description Of Waters` = c('Open Ocean', 'Tidal Waters in the Chowan Basin and the Atlantic Ocean Basin',
                                                'Tidal Waters in the Chesapeake Bay and its tidal tributaries',
                                                'Nontidal Waters (Coastal and Piedmont Zone)','Mountainous Zone Waters',
                                                'Stockable Trout Waters','Natural Trout Waters','Swamp Waters'),
                    `Dissolved Oxygen Min (mg/L)` = c(5,4,NA,4,4,5,6,NA),
                    `Dissolved Oxygen Daily Avg (mg/L)` = c(NA,5,NA,5,5,6,7,NA),
                    `pH Min` = c(6,6,6.0,6.0,6.0,6.0,6.0,3.7),
                    `pH Max` = c(9.0,9.0,9.0,9.0,9.0,9.0,9.0,8.0),
                    `Max Temperature (C)` = c(NA, NA, NA, 32, 31, 21, 20, NA)) %>%
  mutate(CLASS_DESCRIPTION = paste0(CLASS, " | ", `Description Of Waters`))

# Water Column Metals with static criteria (helpful for metals module)
staticLimit <- c("Antimony PWS", "Antimony All Other Surface Waters", "Arsenic Acute Freshwater", "Arsenic Chronic Freshwater", "Arsenic PWS",
                 "Arsenic Acute Saltwater", "Arsenic Chronic Saltwater", "Barium PWS","Cadmium PWS","ChromiumIII PWS",
                 "ChromiumVI Acute Freshwater", "ChromiumVI Chronic Freshwater", "ChromiumVI Acute Saltwater", "ChromiumVI Chronic Saltwater", 
                 "Lead PWS", "Mercury Acute Freshwater", "Mercury Chronic Freshwater", "Mercury Acute Saltwater", "Mercury Chronic Saltwater",
                 "Nickel PWS",  "Nickel All Other Surface Waters", "Uranium PWS","Selenium Acute Freshwater", "Selenium Chronic Freshwater", 
                 "Selenium PWS", "Selenium All Other Surface Waters","Thallium PWS", "Thallium All Other Surface Waters","Zinc PWS", 
                 "Zinc All Other Surface Waters")




# Pull as many details about station from REST service (if available). Work around provided in case station isn't on REST server
## Pull station info from REST service
WQM_Station_Full_REST_request <- function(pool, station, subbasinVAHU6crosswalk, subbasins, ecoregion){
  WQM_Station_Full_REST <- suppressWarnings(
    geojson_sf(
      paste0("http://gis.deq.virginia.gov/arcgis/rest/services/staff/DEQInternalDataViewer/MapServer/104/query?&where=STATION_ID%3D%27",
             toupper(station),"%27&outFields=*&f=geojson"))) 

  if(nrow(WQM_Station_Full_REST ) > 0){
    WQM_Station_Full_REST <- mutate(WQM_Station_Full_REST, WQM_YRS_YEAR = ifelse(!is.na(WQM_YRS_YEAR), lubridate::year(as.Date(as.POSIXct(WQM_YRS_YEAR/1000, origin="1970-01-01"))), NA)) %>% 
      left_join(dplyr::select(subbasinVAHU6crosswalk, SubbasinVAHU6code, BASIN_NAME), by = c('BASINS_VAHUSB' = 'SubbasinVAHU6code'))
    WQM_Station_Full_REST <- bind_cols(WQM_Station_Full_REST, st_coordinates(WQM_Station_Full_REST) %>% as_tibble()) %>%
      mutate(Latitude = Y, Longitude = X) %>% # add lat/lng in DD
      # have to strip geometry and add it back in to get st_intersection to work for some reason
      st_drop_geometry() %>%
      st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
               remove = F, # don't remove these lat/lon cols from df
               crs = 4326)
    stationBasin <- suppressMessages(suppressWarnings(st_intersection(WQM_Station_Full_REST, subbasins)$ProbBasin))
    stationSuperBasin <- suppressMessages(suppressWarnings(st_intersection(WQM_Station_Full_REST, subbasins)$ProbSuperBasin))
    
    WQM_Station_Full_REST <- mutate(WQM_Station_Full_REST, ProbBasin = stationBasin, ProbSuperBasin = stationSuperBasin)
    
  } else { # station doesn't yet exist in WQM full dataset
    # get what we can from CEDS
    stationGISInfo <- pool %>% tbl(in_schema("wqm",  "WQM_Sta_GIS_View")) %>%
      filter(Station_Id %in% !! toupper(station)) %>%
      as_tibble() 
    # pull a known station to steal data structure
    WQM_Station_Full_REST <- suppressWarnings(
      geojson_sf(
        paste0("http://gis.deq.virginia.gov/arcgis/rest/services/staff/DEQInternalDataViewer/MapServer/104/query?&where=STATION_ID%3D%272-JKS023.61%27&outFields=*&f=geojson")))[1,] %>%
      mutate(WQM_YRS_YEAR = ifelse(!is.na(WQM_YRS_YEAR), lubridate::year(as.Date(as.POSIXct(WQM_YRS_YEAR/1000, origin="1970-01-01"))), NA)) %>% 
      left_join(dplyr::select(subbasinVAHU6crosswalk, SubbasinVAHU6code, BASIN_NAME), by = c('BASINS_VAHUSB' = 'SubbasinVAHU6code')) %>%
      st_drop_geometry()
    WQM_Station_Full_REST <- bind_rows(WQM_Station_Full_REST[0,],
                                       tibble(STATION_ID = stationGISInfo$Station_Id, 
                                              Latitude = stationGISInfo$Latitude,
                                              Longitude = stationGISInfo$Longitude,
                                              BASINS_HUC_8_NAME = stationGISInfo$Huc6_Huc_8_Name, 
                                              BASINS_VAHU6 = stationGISInfo$Huc6_Vahu6) ) %>%
      st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
               remove = F, # don't remove these lat/lon cols from df
               crs = 4326)   
    # add in basin and ecoregion information
    stationEcoregion <- suppressMessages(suppressWarnings(st_intersection(WQM_Station_Full_REST, ecoregion)$US_L3NAME))
    stationBasin <- suppressMessages(suppressWarnings(st_intersection(WQM_Station_Full_REST, subbasins)$ProbBasin))
    stationSuperBasin <- suppressMessages(suppressWarnings(st_intersection(WQM_Station_Full_REST, subbasins)$ProbSuperBasin))
    
    WQM_Station_Full_REST <- mutate(WQM_Station_Full_REST, EPA_ECO_US_L3NAME = stationEcoregion, ProbBasin = stationBasin, ProbSuperBasin = stationSuperBasin)
    }
  return(WQM_Station_Full_REST) }

## Pull CEDS Station Information 
stationInfoConsolidated <- function(pool, station, WQM_Station_Full_REST, WQM_Stations_Spatial){
  left_join(pool %>% tbl(in_schema("wqm", "Wqm_Stations_View")) %>%  
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
                          STATION_ID, Latitude, Longitude, WQM_STA_STRAHER_ORDER, EPA_ECO_US_L3CODE, EPA_ECO_US_L3NAME, 
                          EPA_ECO_US_L4CODE, EPA_ECO_US_L4NAME, BASINS_HUC_8_NAME, BASINS_VAHU6, WQS_WATER_NAME, WQS_SEC, WQS_CLASS, 
                          WQS_SPSTDS, WQS_PWS, WQS_TROUT, WQS_TIER_III),
            by = c('Sta_Id' = 'STATION_ID')) %>%
    left_join(dplyr::select(WQM_Stations_Spatial, StationID, ASSESS_REG, CountyCityName), by = c('Sta_Id' = 'StationID')) %>% 
    dplyr::select(Sta_Id, Sta_Desc, `CEDS Station View Link`, `DEQ GIS Web App Link`, Latitude, Longitude, WQM_STA_STRAHER_ORDER, 
                  ASSESS_REG, CountyCityName, EPA_ECO_US_L3CODE, EPA_ECO_US_L3NAME, EPA_ECO_US_L4CODE, EPA_ECO_US_L4NAME, 
                  BASINS_HUC_8_NAME, BASINS_VAHU6, WQS_WATER_NAME, WQS_SEC, WQS_CLASS, 
                  WQS_SPSTDS, WQS_PWS, WQS_TROUT, WQS_TIER_III, everything())
}


# Quick Station Sampling Summary Information
stationSummarySampingMetrics <- function(stationInfo_sf, singleOrMulti){
  stationInfo_sf %>%
    group_by(STATION_ID) %>%
    {if(singleOrMulti == 'single')
      mutate(., `Years Sampled` = WQM_YRS_YEAR)
      else mutate(., `Years Sampled` = year(WQM_YRS_YEAR)) } %>% 
    dplyr::select(STATION_ID, WQM_YRS_SPG_CODE,WQM_YRS_YEAR,`Years Sampled`,WQM_SPG_DESCRIPTION) %>%
    st_drop_geometry() %>%
    group_by(STATION_ID, `Years Sampled`) %>%
    distinct(WQM_YRS_SPG_CODE, .keep_all = T) %>% # drop repetitive codes for each year
    summarise(`Sample Codes` = paste0(WQM_YRS_SPG_CODE, collapse = ' | '))
} 

# Organize field and analyte info into prettier table
stationFieldAnalyteDataPretty <- function(stationAnalyteDataRaw, stationFieldDataRaw, averageResults){
  if(averageResults == TRUE){
    if(nrow(stationAnalyteDataRaw) > 0){
      y <- stationAnalyteDataRaw %>%
        group_by(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc) %>%
        dplyr::select(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc, Ana_Com_Code, #Ana_Sam_Mrs_Lcc_Parm_Group_Cd,
                      Pg_Parm_Name, Ana_Uncensored_Value) %>% 
        mutate(`Associated Analyte Records` = 1:n(),
               LabComments = paste0(Pg_Parm_Name,' RMK'))
      y1 <- y %>%
        dplyr::select(Pg_Parm_Name, Ana_Uncensored_Value) %>% 
        pivot_wider(names_from = Pg_Parm_Name, #names_sep = " | ", 
                    values_from = "Ana_Uncensored_Value",
                    values_fn = list(Ana_Uncensored_Value = mean) ) %>% 
        left_join(y %>% ungroup() %>% group_by(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc, LabComments) %>%  
                    mutate(Ana_Com_Code2 = paste(Ana_Com_Code, sep = " ")) %>%
                    dplyr::select(LabComments, Ana_Com_Code2) %>% 
                    distinct() %>% 
                    pivot_wider(names_from = LabComments, values_from = Ana_Com_Code2),
                  by = c("Ana_Sam_Fdt_Id", "Fdt_Sta_Id", "Fdt_Date_Time", "Ana_Sam_Mrs_Container_Id_Desc")) %>% 
        dplyr::select(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc, sort(names(.)))
    } else {
      y1 <- stationAnalyteDataRaw %>%
        dplyr::select(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc)
    }
    
    suppressWarnings(
      full_join(stationFieldDataRaw, y1, by = c("Fdt_Id" = "Ana_Sam_Fdt_Id", 'Fdt_Sta_Id', 'Fdt_Date_Time')) %>%
        arrange(Fdt_Sta_Id, Fdt_Date_Time))
    # original method
    # suppressWarnings(
    # full_join(stationFieldDataRaw,
    #            stationAnalyteDataRaw %>%
    #              filter(Ana_Sam_Mrs_Container_Id_Desc %in% repFilter) %>%
    #              group_by(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc) %>%
    #              dplyr::select(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc, #Ana_Sam_Mrs_Lcc_Parm_Group_Cd,
    #                            Ana_Parameter_Name, Ana_Uncensored_Value) %>%
    #              pivot_wider(names_from = c('Ana_Parameter_Name'), names_sep = " | ", 
    #                          values_from = "Ana_Uncensored_Value",
    #                          values_fn = list(Ana_Uncensored_Value = mean)),
    #            by = c("Fdt_Id" = "Ana_Sam_Fdt_Id", 'Fdt_Sta_Id', 'Fdt_Date_Time')) %>%
    #   arrange(Fdt_Sta_Id, Fdt_Date_Time))
  } else {
    if(nrow(stationAnalyteDataRaw) > 0){
      z <- stationAnalyteDataRaw %>%
        group_by(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc, Pg_Parm_Name, Ana_Lab_Seq_Num) %>%
        dplyr::select(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc, Pg_Parm_Name, Ana_Com_Code, Ana_Uncensored_Value) %>%
        mutate(`Associated Analyte Records` = 1:n(),
               LabComments = paste0(Pg_Parm_Name,' RMK'))
      z1 <-  z %>% 
        dplyr::select(`Associated Analyte Records`, Pg_Parm_Name, Ana_Uncensored_Value) %>% 
        pivot_wider(names_from = Pg_Parm_Name, values_from = Ana_Uncensored_Value) %>%
        left_join(z %>% ungroup() %>% group_by(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc,Ana_Lab_Seq_Num) %>%  
                    dplyr::select(`Associated Analyte Records`, LabComments, Ana_Com_Code) %>% 
                    pivot_wider(names_from = LabComments, values_from = Ana_Com_Code),
                  by = c("Ana_Sam_Fdt_Id", "Fdt_Sta_Id", "Fdt_Date_Time", "Ana_Sam_Mrs_Container_Id_Desc", "Ana_Lab_Seq_Num", 
                         "Associated Analyte Records")) %>% 
        dplyr::select(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc, Ana_Lab_Seq_Num, 
                      `Associated Analyte Records`, sort(names(.)))
    } else {
      z1 <- stationAnalyteDataRaw %>% 
        mutate(`Associated Analyte Records` = NA) %>% 
        dplyr::select( Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc, Ana_Lab_Seq_Num, 
                         `Associated Analyte Records`)   }
    suppressWarnings(
      full_join(stationFieldDataRaw, 
                z1,
                # stationAnalyteDataRaw %>%
                #   filter(Ana_Sam_Mrs_Container_Id_Desc %in% repFilter) %>%
                #   group_by(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc, Pg_Parm_Name) %>%
                #   dplyr::select(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc, Ana_Lab_Seq_Num, Pg_Parm_Name , Ana_Uncensored_Value) %>%
                #   mutate(`Associated Analyte Records` = 1:n()) %>% 
                #   pivot_wider(names_from = 'Pg_Parm_Name',values_from = "Ana_Uncensored_Value") ,
                by = c("Fdt_Id" = "Ana_Sam_Fdt_Id", 'Fdt_Sta_Id', 'Fdt_Date_Time')) %>% 
        dplyr::select(`Associated Analyte Records`, everything()) %>%
        arrange(Fdt_Sta_Id, Fdt_Date_Time) ) }
    # full_join(stationFieldDataRaw,
    #            stationAnalyteDataRaw %>%
    #              filter(Ana_Sam_Mrs_Container_Id_Desc %in% repFilter) %>%
    #              group_by(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc, Ana_Sam_Mrs_Lcc_Parm_Group_Cd) %>%
    #              dplyr::select(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc, Ana_Sam_Mrs_Lcc_Parm_Group_Cd, Ana_Parameter_Name, Ana_Uncensored_Value) %>%
    #              pivot_wider(names_from = c('Ana_Parameter_Name','Ana_Sam_Mrs_Lcc_Parm_Group_Cd'), names_sep = " | ", 
    #                          values_from = "Ana_Uncensored_Value"),
    #            by = c("Fdt_Id" = "Ana_Sam_Fdt_Id", 'Fdt_Sta_Id', 'Fdt_Date_Time')) )}
}


# Field collector info
uniqueCollector <- function(stationFieldAnalyte){
  stationFieldAnalyte %>%
    group_by(Fdt_Sta_Id, Fdt_Collector_Id) %>%
    summarise(`n Samples` = n()) %>%
    arrange(Fdt_Sta_Id, desc(`n Samples`))
}


# Sample Codes Summary
uniqueSampleCodes <- function(stationFieldAnalyte){
  stationFieldAnalyte %>%
    mutate(`Years Sampled` = year(Fdt_Date_Time)) %>%
    group_by(Fdt_Sta_Id, `Years Sampled`, Fdt_Spg_Code) %>%#, Spg_Description) %>%
    summarise(`n Samples` = n(),
              `Sample Codes` = paste0(unique(Fdt_Spg_Code), collapse = ' | ')) %>%
    dplyr::select(-Fdt_Spg_Code)
}

# Sample Comment Summary
uniqueComments <- function(stationFieldAnalyte){
  stationFieldAnalyte %>%
    group_by(Fdt_Sta_Id, Fdt_Date_Time) %>%
    distinct(Fdt_Comment, .keep_all = FALSE) %>%
    filter(!is.na(Fdt_Comment))
}

# Concatenate multiple columns to one
concatenateCols <- function(df, containString){
  x <- dplyr::select(df, contains(containString))  %>% 
    dplyr::select(-contains('RMK'))
  if(length(x) > 0){
    mutate_if(x, is.numeric, as.character) %>%
    na_if('NA') %>%
    unite(newCol, contains(containString), na.rm = TRUE) %>%
    mutate(newCol = as.numeric(newCol)) %>%
    pull()} else {as.numeric(rep(NA, nrow(x)))}
}

concatenateCols2 <- function(df, containString){
  x <- dplyr::select(df, contains(containString))
  if(length(x) == 0){as.numeric(rep(NA, nrow(x)))}
  if(length(x) == 1){as.numeric(x %>% pull()) }
  if(length(x) > 1){
    mutate_if(x, is.numeric, as.character) %>%
      na_if('NA') %>%
      unite(newCol, contains(containString), na.rm = TRUE) %>%
      mutate(newCol = as.numeric(newCol)) %>%
      pull()} 
}

#concatenateCols(stationFieldAnalyte,  'CHLOROPHYLL-A UG/L SPECTROPHOTOMETRIC ACID. METH') #'SUSP. SED. CONC. TOTAL, MG/L,(Method B)')#, 
#df <- stationFieldAnalyte; containString <- 'E.COLI'

# Generic Info People Want
#stationFieldAnalyte <- stationFieldAnalyte1
#stationFieldAnalyte$Fdt_Do_Optical[5] <- NA
#stationFieldAnalyte$Fdt_Do_Winkler[5] <- 7.7
#stationFieldAnalyte$Fdt_Do_Probe[7] <- 8.6
#stationFieldAnalyte$Fdt_Do_Optical[7] <- NA


basicSummary <- function(stationFieldAnalyte){
  suppressWarnings(
  mutate(stationFieldAnalyte, 
         blankColForSelect = NA, # placeholder to enable selection below
         StationID = Fdt_Sta_Id,
         `Collection Date` = Fdt_Date_Time,
         Comments = Fdt_Comment, 
         `Collector ID` = Fdt_Collector_Id,
         `Run ID` = Fdt_Run_Id,
         `SPG Code` = Fdt_Spg_Code,
         `SPG Description` = Spg_Description,
         Depth = Fdt_Depth,
         `Weather Code` = Fdt_Weather_Code,
         `Tide Code` = Fdt_Tide_Code,
         Temperature = Fdt_Temp_Celcius,
         pH = Fdt_Field_Ph,
         `Dissolved Oxygen` = case_when(!is.na(Fdt_Do_Probe) ~ Fdt_Do_Probe,
                        !is.na(Fdt_Do_Optical) ~ Fdt_Do_Optical,
                        !is.na(Fdt_Do_Winkler) ~ Fdt_Do_Winkler,
                        TRUE ~ as.numeric(NA)),
         `DO Percent Saturation` = Fdt_Do_Satr_Per,
         `Specific Conductance` = Fdt_Specific_Conductance,
         Salinity = Fdt_Salinity,
         Turbidity = Fdt_Turbidity,
         `Secchi Depth` = Fdt_Secchi_Depth, 
         Hardness = concatenateCols(stationFieldAnalyte, 'HARDNESS, TOTAL (MG/L AS CACO3)'),
         Ecoli = concatenateCols(stationFieldAnalyte, 'E.COLI BY COLILERT SM 9223-B'),
         Enterococci = concatenateCols(stationFieldAnalyte, 'ENTEROCOCCI- ME-MF N0/100ML'),
         `Fecal Coliform` = concatenateCols(stationFieldAnalyte, 'FECAL COLIFORM,MEMBR FILTER,M-FC BROTH,44.5 C'),
         `Total Nitrogen` = concatenateCols(stationFieldAnalyte, 'NITROGEN, TOTAL (MG/L AS N)'),
         `Total Nitrate Nitrogen` = concatenateCols(stationFieldAnalyte, 'NITRITE NITROGEN, TOTAL (MG/L AS N)'),
         `Total Kjeldahl Nitrogen` = concatenateCols(stationFieldAnalyte, 'NITROGEN, KJELDAHL, TOTAL, (MG/L AS N)'),
         `Ammonia` = concatenateCols(stationFieldAnalyte, 'NITROGEN, AMMONIA, TOTAL (MG/L AS N)'),
         `Total Phosphorus` = concatenateCols(stationFieldAnalyte, 'PHOSPHORUS, TOTAL (MG/L AS P)'),
         `Ortho Phosphorus` = concatenateCols(stationFieldAnalyte, 'PHOSPHORUS, DISSOLVED ORTHOPHOSPHATE (MG/L AS P)'),
         `Chlorophyll a` = concatenateCols(stationFieldAnalyte, 'CHLOROPHYLL-A UG/L SPECTROPHOTOMETRIC ACID. METH'),
         Turbidity = concatenateCols(stationFieldAnalyte, 'TURBIDITY,LAB NEPHELOMETRIC TURBIDITY UNITS, NTU'),
         `Total Dissolved Solids` = concatenateCols(stationFieldAnalyte, 'TDS RESIDUE,TOTAL FILTRABLE (DRIED AT 180C),MG/L'),
         `Total Suspended Solids` = concatenateCols(stationFieldAnalyte, 'SUSP. SED. CONC. TOTAL, MG/L,(Method B)'),
         `Suspended Sediment Concentration Coarse` = concatenateCols(stationFieldAnalyte, 'SUSP. SED. CONC. - >62 um,MG/L, (Method C)'),
         `Suspended Sediment Concentration Fine` = concatenateCols(stationFieldAnalyte, 'SUSP. SED. CONC. - <62 um,MG/L, (Method C)'),
         `Calcium` = concatenateCols(stationFieldAnalyte, 'CALCIUM, DISSOLVED (MG/L AS CA)'),
         `Magnesium` = concatenateCols(stationFieldAnalyte, 'MAGNESIUM, DISSOLVED (MG/L AS MG)'),
         `Sodium` = concatenateCols(stationFieldAnalyte, 'SODIUM, DISSOLVED (MG/L AS NA)'),
         `Potassium` = concatenateCols(stationFieldAnalyte, 'POTASSIUM, DISSOLVED (MG/L AS K)'),
         `Chloride` = concatenateCols(stationFieldAnalyte, 'CHLORIDE, DISSOLVED IN WATER MG/L'),
         `Sulfate` = concatenateCols(stationFieldAnalyte, 'SULFATE, DISSOLVED (MG/L AS SO4)'),
         `Arsenic` = concatenateCols(stationFieldAnalyte, "ARSENIC, DISSOLVED  (UG/L AS AS)"),
         `Barium` = concatenateCols(stationFieldAnalyte, "BARIUM, DISSOLVED (UG/L AS BA)"),
         `Beryllium` = concatenateCols(stationFieldAnalyte, "BERYLLIUM, DISSOLVED (UG/L AS BE)"),
         `Cadmium` = concatenateCols(stationFieldAnalyte,  "CADMIUM, DISSOLVED (UG/L AS CD)"),
         `Chromium` = concatenateCols(stationFieldAnalyte, "CHROMIUM, DISSOLVED (UG/L AS CR)"),
         `Copper` = concatenateCols(stationFieldAnalyte, "COPPER, DISSOLVED (UG/L AS CU)"),
         `Iron` = concatenateCols(stationFieldAnalyte, "IRON, DISSOLVED (UG/L AS FE)"),
         `Lead` = concatenateCols(stationFieldAnalyte, "LEAD, DISSOLVED (UG/L AS PB)"), 
         `Manganese` = concatenateCols(stationFieldAnalyte, "MANGANESE, DISSOLVED (UG/L AS MN)"),
         `Thallium` = concatenateCols(stationFieldAnalyte, "THALLIUM, DISSOLVED (UG/L AS TL)"),
         `Nickel` = concatenateCols(stationFieldAnalyte, "NICKEL, DISSOLVED (UG/L AS NI)"),
         `Silver` = concatenateCols(stationFieldAnalyte, "SILVER, DISSOLVED (UG/L AS AG)"),
         `Strontium` = concatenateCols(stationFieldAnalyte, "STRONTIUM, DISSOLVED (UG/L AS SR)"),
         `Zinc` = concatenateCols(stationFieldAnalyte, "ZINC, DISSOLVED (UG/L AS ZN)"),
         `Antimony` = concatenateCols(stationFieldAnalyte, "ANTIMONY, DISSOLVED (UG/L AS SB)"),
         `Aluminum` = concatenateCols(stationFieldAnalyte, "ALUMINUM, DISSOLVED (UG/L AS AL)"),
         `Selenium` = concatenateCols(stationFieldAnalyte, "SELENIUM, DISSOLVED (UG/L AS SE)"),
         `Fecal Coliform` = concatenateCols(stationFieldAnalyte, "FECAL COLIFORM,MEMBR FILTER,M-FC BROTH,44.5 C"),                       
         `Total Organic Carbon` = concatenateCols(stationFieldAnalyte, 'CARBON, TOTAL ORGANIC (MG/L AS C)'),
         `Dissolved Organic Carbon` = concatenateCols(stationFieldAnalyte, 'CARBON, DISSOLVED ORGANIC (MG/L AS C)'),
         `Benthic Ash Free Dry Mass` = concatenateCols(stationFieldAnalyte, 'BENTHIC ASH FREE DRY MASS, GM/M2'),
         `Benthic Chlorophyll a` = concatenateCols(stationFieldAnalyte, 'BENTHIC CHLOROPHYLL A, MG/M2'), # billy addition
         `Benthic Chlorophyll b` = concatenateCols(stationFieldAnalyte, 'BENTHIC, CHLOROPHYLL B, MG/M2'),# billy addition
         `Benthic Pheophytin a` = concatenateCols(stationFieldAnalyte, 'BENTHIC, PHEOPHYTIN A, MG/M2')) %>% # billy addition
    dplyr::select(StationID, `Collection Date`, Comments, `Collector ID`, `Run ID`, `SPG Code`, `SPG Description`,
                  Depth, `Weather Code`, `Tide Code`, Temperature, pH, `Dissolved Oxygen`, `DO Percent Saturation`,
                  `Specific Conductance`, Salinity, Turbidity, `Secchi Depth`, Hardness, Ecoli, Enterococci, `Fecal Coliform`,
                  `Total Nitrogen`, `Total Nitrate Nitrogen`, `Total Kjeldahl Nitrogen`, `Ammonia`, `Total Phosphorus`, `Ortho Phosphorus`, 
                  `Chlorophyll a`, Turbidity, `Total Dissolved Solids`, `Total Suspended Solids`, `Suspended Sediment Concentration Coarse`, 
                  `Suspended Sediment Concentration Fine`, `Calcium`, `Magnesium`, `Sodium`, `Potassium`, `Chloride`, `Sulfate`, `Arsenic`, 
                  `Barium`, `Beryllium`, `Cadmium`, `Chromium`, `Copper`, `Iron`, `Lead`, `Manganese`, `Thallium`, `Nickel`, `Silver`, 
                  `Strontium`, `Zinc`, `Antimony`, `Aluminum`, `Selenium`, `Fecal Coliform`, `Total Organic Carbon`, `Dissolved Organic Carbon`, 
                  `Benthic Ash Free Dry Mass`,`Benthic Chlorophyll a`, `Benthic Chlorophyll b`, `Benthic Pheophytin a` )   )
}


# Actual scatterplot function
parameterScatterPlotly <- function(dat, parameter){
  plot_ly(data=dat) %>%
    {if(parameter %in% c('Temperature', 'Dissolved Oxygen'))
      add_lines(., data = dat, x=~`Collection Date`,y=~Standard, mode='line', line = list(color = 'black'),
                hoverinfo = "text", text= paste(parameter, "Standard"), name= paste(parameter, "Standard")) 
      else . } %>%
    {if(parameter %in% c('pH'))
      add_lines(., data = dat, x=~`Collection Date`,y=~Standard1, mode='line', line = list(color = 'black'),
                hoverinfo = "text", text= paste(parameter, "Standard"), name= paste(parameter, "Standard")) %>%
        add_lines(data = dat, x=~`Collection Date`,y=~Standard2, mode='line', line = list(color = 'black'),
                  hoverinfo = "text", text= paste(parameter, "Standard"), name= paste(parameter, "Standard")) 
      else . } %>%
    {if(length(unique(dat$StationID)) > 1)
      add_markers(., data=dat, x= ~`Collection Date`, y= ~Measure,mode = 'scatter', name= ~StationID, 
                  color=~StationID,  symbol = ~StationID, #symbols = c('circle','x','o'), #marker = list(color= '#535559'), 
                  hoverinfo="text",
                  text=~paste(sep="<br>",
                              paste("StationID: ",StationID),
                              paste("Sample Date: ",`Collection Date`),
                              paste("Depth: ",Depth, "m"),
                              paste(parameter, ": ",Measure," (mg/L)")))
      else add_markers(., data=dat, x= ~`Collection Date`, y= ~Measure,mode = 'scatter', name= ~StationID, 
                       marker = list(color= '#535559'), hoverinfo="text",
                       text=~paste(sep="<br>",
                                   paste("StationID: ",StationID),
                                   paste("Sample Date: ",`Collection Date`),
                                   paste("Depth: ",Depth, "m"),
                                   paste(parameter, ": ",Measure," (mg/L)"))) } %>%
    layout(showlegend=TRUE,
           yaxis=list(title=paste(parameter, unique(dat$units))),
           xaxis=list(title="Sample Date",tickfont = list(size = 10))) 
}


# Scatterplot with BSA colors
parameterScatterPlotlyBSA <- function(dat, parameter){
  if(nrow(dat) == 1){
    dat <- bind_rows(dat,
                     tibble(`Collection Date` = c(dat$`Collection Date`- days(5), dat$`Collection Date` + days(5))))
  }
  if(parameter == 'Chloride'){
    maxheight <- ifelse(max(dat$Measure, na.rm=T) < 50, 55, max(dat$Measure, na.rm=T)* 1.2)
    box1 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(50, maxheight, maxheight, 50))
    box2 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(25, 50, 50, 25))
    box3 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(10, 25, 25, 10))
    box4 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(0, 10, 10, 0))  }
  if(parameter == "Dissolved Oxygen"){
    maxheight <- ifelse(max(dat$Measure, na.rm=T) < 10, 12, max(dat$Measure, na.rm=T)* 1.2)
    box1 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(10, maxheight, maxheight, 10))
    box2 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(8, 10, 10, 8))
    box3 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(7, 8, 8, 7))
    box4 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(0, 7, 7, 0))}
  if(parameter == 'Total Nitrate Nitrogen'){
    maxheight <- ifelse(max(dat$Measure, na.rm=T) < 50, 55, max(dat$Measure, na.rm=T)* 1.2)
    box1 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(50, maxheight, maxheight, 50))
    box2 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(25, 50, 50, 25))
    box3 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(10, 25, 25, 10))
    box4 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(0, 10, 10, 0))}
  if(parameter == 'pH'){
    box1 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(9, 14, 14, 9))
    box2 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(6, 9, 9, 6))
    box3 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(0, 6, 6, 0))}
  if(parameter == 'Specific Conductance'){
    maxheight <- ifelse(max(dat$Measure, na.rm=T) < 500, 600, max(dat$Measure, na.rm=T)* 1.2)
    box1 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(500, maxheight, maxheight, 500))
    box2 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(350, 500, 500, 350))
    box3 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(250, 350, 350, 250))
    box4 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(0, 250, 250, 0)) }
  if(parameter == 'Sulfate'){
    maxheight <- ifelse(max(dat$Measure, na.rm=T) < 75, 100, max(dat$Measure, na.rm=T)* 1.2)
    box1 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(75, maxheight, maxheight, 75))
    box2 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(25, 75, 75, 25))
    box3 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(10, 25, 25, 10))
    box4 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(0, 10, 10, 0)) }
  if(parameter == "Total Nitrogen"){
    maxheight <- ifelse(max(dat$Measure, na.rm=T) < 2, 2.5, max(dat$Measure, na.rm=T)* 1.2)
    box1 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(2, maxheight, maxheight, 2))
    box2 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(1, 2, 2, 1))
    box3 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(0.5, 1, 1, 0.5))
    box4 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(0, 0.5, 0.5, 0)) }
  if(parameter == "Total Phosphorus"){
    maxheight <- ifelse(max(dat$Measure, na.rm=T) < 0.1, 0.12, max(dat$Measure, na.rm=T)* 1.2)
    box1 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(0.1, maxheight, maxheight, 0.1))
    box2 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(0.05, 0.1, 0.1, 0.05))
    box3 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(0.02, 0.05, 0.05, 0.02))
    box4 <- data.frame(x = c(min(dat$`Collection Date`), min(dat$`Collection Date`), max(dat$`Collection Date`),max(dat$`Collection Date`)), y = c(0, 0.02, 0.02, 0)) }
  
  
  plot_ly(data=dat) %>%
    # pH special case
    {if(parameter == 'pH')
      add_polygons(., data = box1, x = ~x, y = ~y, fillcolor = "#F0E442",opacity=0.6, line = list(width = 0),
                   hoverinfo="text", name =paste('Medium Probability of Stress to Aquatic Life')) %>%
        add_polygons(data = box2, x = ~x, y = ~y, fillcolor = "#009E73",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('Low Probability of Stress to Aquatic Life')) %>%
        add_polygons(data = box3, x = ~x, y = ~y, fillcolor = "#F0E442",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('Medium Probability of Stress to Aquatic Life'))
      else . } %>%
    #boxes go low to high stress
    {if(parameter %in% c("Dissolved Oxygen"))
      add_polygons(., data = box1, x = ~x, y = ~y, fillcolor = "#0072B2",opacity=0.6, line = list(width = 0),
                   hoverinfo="text", name =paste('No Probability of Stress to Aquatic Life')) %>%
      add_polygons(data = box2, x = ~x, y = ~y, fillcolor = "#009E73",opacity=0.6, line = list(width = 0),
                   hoverinfo="text", name =paste('Low Probability of Stress to Aquatic Life')) %>%
      add_polygons(data = box3, x = ~x, y = ~y, fillcolor = "#F0E442",opacity=0.6, line = list(width = 0),
                   hoverinfo="text", name =paste('Medium Probability of Stress to Aquatic Life')) %>%
      add_polygons(data = box4, x = ~x, y = ~y, fillcolor = "firebrick",opacity=0.6, line = list(width = 0),
                   hoverinfo="text", name =paste('High Probability of Stress to Aquatic Life')) 
      else . } %>% 
    # boxes go high to low stress
    {if(parameter %in% c('Chloride', 'Total Nitrate Nitrogen', 'Specific Conductance', 'Sulfate', "Total Nitrogen",
                         "Total Phosphorus"))
      add_polygons(., data = box1, x = ~x, y = ~y,  fillcolor = "firebrick",opacity=0.6, line = list(width = 0),
                   hoverinfo="text", name =paste('High Probability of Stress to Aquatic Life')) %>%
        add_polygons(data = box2, x = ~x, y = ~y, fillcolor = "#F0E442",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('Medium Probability of Stress to Aquatic Life')) %>%
        add_polygons(data = box3, x = ~x, y = ~y, fillcolor = "#009E73",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('Low Probability of Stress to Aquatic Life')) %>%
        add_polygons(data = box4, x = ~x, y = ~y, fillcolor = "#0072B2",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('No Probability of Stress to Aquatic Life'))
      else . } %>% 
    
    {if(parameter %in% c('Temperature', 'Dissolved Oxygen'))
      add_lines(., data = dat, x=~`Collection Date`,y=~Standard, mode='line', line = list(color = 'black'),
                hoverinfo = "text", text= paste(parameter, "Standard"), name= paste(parameter, "Standard")) 
      else . } %>%
    {if(parameter %in% c('pH'))
      add_lines(., data = dat, x=~`Collection Date`,y=~Standard1, mode='line', line = list(color = 'black'),
                hoverinfo = "text", text= paste(parameter, "Standard"), name= paste(parameter, "Standard")) %>%
        add_lines(data = dat, x=~`Collection Date`,y=~Standard2, mode='line', line = list(color = 'black'),
                  hoverinfo = "text", text= paste(parameter, "Standard"), name= paste(parameter, "Standard")) 
      else . } %>%
    {if(length(unique(filter(dat, !is.na(StationID))$StationID)) > 1)
      add_markers(., data=dat, x= ~`Collection Date`, y= ~Measure,mode = 'scatter', name= ~StationID, 
                  color=~StationID,  symbol = ~StationID,  #marker = list(color= '#535559'), 
                  hoverinfo="text",
                  text=~paste(sep="<br>",
                              paste("StationID: ",StationID),
                              paste("Sample Date: ",`Collection Date`),
                              paste("Depth: ",Depth, "m"),
                              paste(parameter, ": ",Measure," (mg/L)")))
      else add_markers(., data=dat, x= ~`Collection Date`, y= ~Measure,mode = 'scatter', name= ~StationID, 
                       marker = list(color= '#535559'), hoverinfo="text",
                       text=~paste(sep="<br>",
                                   paste("StationID: ",StationID),
                                   paste("Sample Date: ",`Collection Date`),
                                   paste("Depth: ",Depth, "m"),
                                   paste(parameter, ": ",Measure," (mg/L)"))) } %>%
    layout(showlegend=TRUE,
           yaxis=list(title=paste(parameter, unique(dat$units))),
           xaxis=list(title="Sample Date",tickfont = list(size = 10))) 
}



parameterPlotly <- function(basicData,
                            parameter,
                            unitData,
                            WQSlookup,
                            addBSAcolors){
  z <- dplyr::select(basicData, parameterPlot = !! parameter) %>% # rename clutch for nse
    filter(!is.na(parameterPlot)) 
  if(nrow(z) != 0){
    parameterUnits <- filter(unitData, AltName %in% !!parameter)$Units
    if(parameter %in% c('Temperature', 'Dissolved Oxygen', "pH")){
      if(parameter == 'Temperature'){parameterLimit <- 'Max Temperature (C)'; specialStandards <- NULL}
      if(parameter == 'Dissolved Oxygen'){parameterLimit <- 'Dissolved Oxygen Min (mg/L)'; specialStandards <- NULL}
      if(parameter == 'pH'){
        parameterLimit <- c('pH Min', 'pH Max')
        if(nrow(filter(WQSlookup, StationID %in% unique(basicData$StationID)) %>% 
                filter(str_detect(as.character(SPSTDS), '6.5-9.5'))) > 0){specialStandards <- c(6.5, 9.5)
        } else {specialStandards <- NULL}}
    } else { parameterLimit <- NULL 
    specialStandards <- NULL  }
    
    dat <- dplyr::select(basicData, StationID, `Collection Date`, Depth, Measure = !! parameter) %>%
      filter( !is.na(Measure))
    
    if(nrow(dat) > 0){
      dat <- left_join(dat, WQSlookup, by = c('StationID')) %>%
        mutate(CLASS_BASIN = paste(CLASS,substr(BASIN, 1,1), sep="_")) %>%
        mutate(CLASS_BASIN = ifelse(CLASS_BASIN == 'II_7', "II_7", as.character(CLASS))) %>%
        # Fix for Class II Tidal Waters in Chesapeake (bc complicated DO/temp/etc standard)
        left_join(WQSvalues, by = 'CLASS_BASIN') %>%
        dplyr::select(-c(CLASS.y,CLASS_BASIN)) %>%
        rename('CLASS' = 'CLASS.x') %>%
        # add standards info if available
        {if(!is.null(parameterLimit))
          dplyr::select(., StationID, `Collection Date`, Depth, Measure, Standard = !!parameterLimit) 
          else dplyr::select(., StationID, `Collection Date`, Depth, Measure) } %>%
        # pH special standards correction
        {if(!is.null(specialStandards))
          mutate(., Standard1 = specialStandards[1], Standard2 = specialStandards[2])
          else . } %>%
        mutate(units = parameterUnits) 
      #return(dat)
     
      if(addBSAcolors == FALSE){
        parameterScatterPlotly(dat, parameter)
      } else {
        parameterScatterPlotlyBSA(dat, parameter)     }
    } } 
}

##  Parameter graph with loess smoother
basicLoessPlotFunction <- function(basicData, parameter){
  ggplotly(
    basicData %>% 
      dplyr::select(StationID, `Collection Date`, Depth, Measure = !! parameter) %>%
      filter( !is.na(Measure)) %>% 
      ggplot()+
      geom_point(aes(x=`Collection Date`,y= Measure,  colour=StationID, shape = StationID) ) +
      geom_smooth(aes(x=`Collection Date`,y= Measure ),method='loess') +
      labs(y = parameter) +
      theme_minimal()) 
}
#basicLoessPlotFunction(basicData, 'pH')

# Find central tendency of each parameter based on filtered window
median_n <- list(
  median = ~signif(median(.x, na.rm = TRUE), digits = 2), 
  mean = ~signif(mean(.x, na.rm = TRUE), digits = 2), 
  n = ~sum(ifelse(!is.na(.x), 1,0))#, na.rm = TRUE)
)

centralTendenciesCalculation <- function(basicData){
  dplyr::select(basicData, StationID, probIndicators$AltName) %>%
    group_by(StationID) %>%
    pivot_longer(-StationID, names_to = 'parameter', values_to = 'measure') %>%
    group_by(StationID, parameter) %>%
    summarize(Median = signif(median(measure, na.rm = TRUE), digits = 2),
              Mean = signif(mean(measure, na.rm = TRUE), digits = 2),
              n = sum(ifelse(!is.na(measure), 1,0))) %>%
    pivot_longer(-c(StationID, parameter), names_to= 'Statistic', values_to = 'value') %>%
    pivot_wider(names_from = parameter, values_from = value) %>% ungroup() }





# Compare median of selected indicator to prob estimates
subFunction <- function(cdftable,parameter,userInput){
  return(filter(cdftable,Subpopulation%in%userInput & Indicator%in%parameter))
}
#View(subFunction(probEst, parameter = parameter, 'Virginia'))

subFunction2 <- function(cdftable,userValue){
  return(filter(cdftable,Estimate.P %in% userValue))
}
#subFunction2(subFunction(probEst, parameter = parameter, 'Virginia'), 48.14737)




# Return CDF percentile 
#cdfdata <- probEst
#statsTable <- probComparison 
#prettyParameterName <- 'Specific Conductance'
#parameter <- 'SpCond'
#userBasin <- 'James'
#userEcoregion <- "Blue Ridge" 
#userOrder <- NA
#rm(cdfdata); rm(statsTable); rm(parameter); rm(userBasin); rm(userEcoregion); rm(userOrder); rm(out); rm(va);rm(basin); rm(eco); rm(out_final); rm(prettyParameterName)
percentileTable <- function(cdfdata, statsTable,prettyParameterName, parameter, userSuperBasin, userBasin, userEcoregion,userOrder){
  out <- statsTable %>%
    dplyr::select(StationID, Statistic, starts_with(prettyParameterName))
  va <- bind_rows(data.frame(Value= 0, `Estimate.P` = 0),
                  data.frame(filter(cdfdata,Subpopulation=='Virginia',Indicator==parameter)%>%select(Value,Estimate.P)) )# needs to be df for vlookup to work
  if(!is.na(userSuperBasin)){
    superbasin <- bind_rows(data.frame(Value= 0, `Estimate.P` = 0),
                            data.frame(filter(cdfdata,Subpopulation==userSuperBasin,Indicator==parameter)%>%select(Value,Estimate.P)) ) } # needs to be df for vlookup to work
  basin <- bind_rows(data.frame(Value= 0, `Estimate.P` = 0),
                     data.frame(filter(cdfdata,Subpopulation==userBasin,Indicator==parameter)%>%select(Value,Estimate.P)) )# needs to be df for vlookup to work
  eco <- bind_rows(data.frame(Value= 0, `Estimate.P` = 0),
                   data.frame(filter(cdfdata,Subpopulation==userEcoregion,Indicator==parameter)%>%select(Value,Estimate.P))) # needs to be df for vlookup to work
  #order <- data.frame(filter(cdfdata,Subpopulation==userOrder,Indicator==parameter)%>%select(Value,Estimate.P)) # needs to be df for vlookup to work
  out_final <- list(statistics = out, 
                    percentiles = data.frame(Subpopulation = 'Virginia',
                                             Average = vlookup(filter(out, str_detect(Statistic, 'Mean')) %>% dplyr::select(starts_with(prettyParameterName)) %>% pull(), va, 2, range=TRUE),
                                             Median = vlookup(filter(out, str_detect(Statistic, 'Median')) %>% dplyr::select(starts_with(prettyParameterName)) %>% pull(), va, 2, range=TRUE)) %>%
                      {if(!is.na(userSuperBasin))
                        bind_rows(., data.frame(Subpopulation = userSuperBasin,
                                             Average = vlookup(filter(out, str_detect(Statistic, 'Mean')) %>% dplyr::select(starts_with(prettyParameterName)) %>% pull(), superbasin, 2, range=TRUE),
                                             Median = vlookup(filter(out, str_detect(Statistic, 'Median')) %>% dplyr::select(starts_with(prettyParameterName)) %>% pull(), superbasin, 2, range=TRUE)))
                        else bind_rows(., data.frame(Subpopulation = userSuperBasin,
                                                  Average = NA,
                                                  Median = NA))} %>%
                      bind_rows(data.frame(Subpopulation = userBasin,
                                           Average = vlookup(filter(out, str_detect(Statistic, 'Mean')) %>% dplyr::select(starts_with(prettyParameterName)) %>% pull(), basin, 2, range=TRUE),
                                           Median = vlookup(filter(out, str_detect(Statistic, 'Median')) %>% dplyr::select(starts_with(prettyParameterName)) %>% pull(), basin, 2, range=TRUE))) %>%
                      bind_rows(data.frame(Subpopulation = userEcoregion,
                                           Average = vlookup(filter(out, str_detect(Statistic, 'Mean')) %>% dplyr::select(starts_with(prettyParameterName)) %>% pull(), eco, 2, range=TRUE),
                                           Median = vlookup(filter(out, str_detect(Statistic, 'Median')) %>% dplyr::select(starts_with(prettyParameterName)) %>% pull(), eco, 2, range=TRUE))) )
  return(out_final)
}

# Giant list of percentile information
percentileList <- function(probEst, probComparison, probIndicators, userSuperBasin, userBasin, userEcoregion, userOrder){
  percentiles <- list()
  for(i in names(dplyr::select(probComparison, -c(StationID, Statistic)))){
    #print(i)
    parameterSwitch <- filter(probIndicators, AltName %in% i)$Parameter
    percentiles[[i]] <- percentileTable(probEst, probComparison, i, parameterSwitch, userSuperBasin, userBasin, userEcoregion, userOrder)
  }
  return(percentiles)
}

# CDF plot function
cdfplot <- function(cdfdata, prettyParameterName,parameter,subpopulation,dataset,CDFsettings){
  cdfsubset <- subFunction(cdfdata,parameter,subpopulation)
  avg1 <- filter(dataset[[prettyParameterName]]$percentiles, Subpopulation == subpopulation) %>% pull(Average)#filter(dataset$statistics, Statistic == "Mean")$DO
  avg <- subFunction2(cdfsubset,avg1)
  med1 <- filter(dataset[[prettyParameterName]]$percentiles, Subpopulation == subpopulation) %>% pull(Median)
  med <- subFunction2(cdfsubset,med1)
  m <- max(cdfsubset$NResp)
  if(is.null(CDFsettings)){
    p1 <- ggplot(cdfsubset, aes(x=Value,y=Estimate.P)) + 
      labs(x=paste(prettyParameterName,unique(cdfsubset$Units),sep=" "),y="Percentile") +
      ggtitle(paste(subpopulation,'\n', prettyParameterName,"\n Percentile Graph ( n = ",m,")",sep=" ")) +#ggtitle(paste(subpopulation,prettyParameterName,"Percentile Graph ( n = ",m,")",sep=" ")) + 
      theme(plot.title = element_text(hjust=0.5,face='bold',size=15)) +
      theme(axis.title = element_text(face='bold',size=12))+
      geom_point() +
      geom_point(data=avg,color='orange',size=4) + geom_text(data=avg,label='Average',hjust=1.2) +
      geom_point(data=med,color='gray',size=4)+ geom_text(data=med,label='Median',hjust=1.2) 
  } else {
    p1 <- ggplot(cdfsubset, aes(x=Value,y=Estimate.P)) + 
      labs(x=paste(prettyParameterName,unique(cdfsubset$Units),sep=" "),y="Percentile") +
      ggtitle(paste(subpopulation,'\n', prettyParameterName,"\n Percentile Graph ( n = ",m,")",sep=" ")) +#ggtitle(paste(subpopulation,prettyParameterName,"Percentile Graph ( n = ",m,")",sep=" ")) + 
      theme(plot.title = element_text(hjust=0.5,face='bold',size=15)) +
      theme(axis.title = element_text(face='bold',size=12))+
      
      CDFsettings  +
      
      geom_point() +
      geom_point(data=avg,color='orange',size=4) + geom_text(data=avg,label='Average',hjust=1.2) +
      geom_point(data=med,color='gray',size=4)+ geom_text(data=med,label='Median',hjust=1.2) 
  }
  
  return(p1)
}




### Multistation Specific functions

# # Pull WQM stations based on spatial and analyte info
WQM_Stations_Filter_function <- function(queryType, pool, WQM_Stations_Spatial, VAHU6Filter, subbasinFilter, assessmentRegionFilter,
                                         ecoregionFilter,  ecoregionLevel4Filter, countyFilter, dateRange_multistation, analyte_Filter, programCodeFilter, 
                                         labGroupCodeFilter, runIDfilter, manualSelection, wildcardSelection){
  # preliminary stations before daterange filter
  if(queryType == 'Spatial Filters' ){
    preliminaryStations <- WQM_Stations_Spatial %>%
      # go small to large spatial filters
      {if(!is.null(VAHU6Filter))
        filter(., VAHU6 %in% VAHU6Filter)
        #st_intersection(., filter(assessmentLayer, VAHU6 %in% VAHU6Filter))
        else .} %>%
      {if(is.null(VAHU6Filter) & !is.null(subbasinFilter))
        filter(., Basin_Name %in% subbasinFilter)
        #st_intersection(., filter(subbasins, SUBBASIN %in% subbasinFilter))
        else .} %>%
      {if(is.null(VAHU6Filter) & !is.null(assessmentRegionFilter)) # don't need assessment region filter if VAHU6 available
        filter(., ASSESS_REG %in% assessmentRegionFilter)
        #st_intersection(., filter(assessmentRegions, ASSESS_REG %in% assessmentRegionFilter))
        else .} %>%
      {if(!is.null(ecoregionFilter))
        filter(., US_L3NAME %in% ecoregionFilter)
        #st_intersection(., filter(ecoregion, US_L3NAME %in% ecoregionFilter))
        else .} %>% 
      {if(!is.null(ecoregionLevel4Filter))
        filter(., US_L4NAME %in% ecoregionLevel4Filter)
        else .} %>%
      {if(!is.null(countyFilter))
        filter(., CountyCityName %in% countyFilter)
        else .}}
  if(str_detect(queryType, 'Manually Specify')){
    preliminaryStations <-  filter(WQM_Stations_Spatial, StationID %in% as.character(manualSelection)) %>% 
      {if(!is.null(ecoregionFilter))
        filter(., US_L3NAME %in% ecoregionFilter)
        #st_intersection(., filter(ecoregion, US_L3NAME %in% ecoregionFilter))
        else .} %>% 
      {if(!is.null(ecoregionLevel4Filter))
        filter(., US_L4NAME %in% ecoregionLevel4Filter)
        else .} %>%
      {if(!is.null(countyFilter))
        filter(., CountyCityName %in% countyFilter)
        else .} }
  if(queryType == 'Wildcard Selection' ){
    preliminaryStations <- sqldf(paste0('SELECT * FROM WQM_Stations_Spatial WHERE StationID like "',
                                        wildcardSelection, '"')) %>% 
      {if(!is.null(ecoregionFilter))
        filter(., US_L3NAME %in% ecoregionFilter)
        #st_intersection(., filter(ecoregion, US_L3NAME %in% ecoregionFilter))
        else .} %>% 
      {if(!is.null(ecoregionLevel4Filter))
        filter(., US_L4NAME %in% ecoregionLevel4Filter)
        else .} %>%
      {if(!is.null(countyFilter))
        filter(., CountyCityName %in% countyFilter)
        else .} }
  

  # add daterange filter based on preliminary station results
  if(nrow(preliminaryStations) > 0){
    if(!is.null(dateRange_multistation)){
      stationField <- pool %>% tbl(in_schema("wqm", "Wqm_Field_Data_View")) %>%
        filter(Fdt_Sta_Id %in% !! preliminaryStations$StationID &
                 between(as.Date(Fdt_Date_Time), !! dateRange_multistation[1], !! dateRange_multistation[2]) ) %>% # & # x >= left & x <= right
        #Ssc_Description != "INVALID DATA SET QUALITY ASSURANCE FAILURE") %>%
        # dplyr::select(Fdt_Sta_Id, Fdt_Id) %>% # save time by only bringing back station names
        as_tibble() %>% 
        {if(!is.null(programCodeFilter))
          filter(., Fdt_Spg_Code %in% programCodeFilter)
          else .}
      # wildcard runIDfilter if needed
      if(!is.null(runIDfilter) & runIDfilter != ""){
        stationField <- sqldf(paste0('SELECT * FROM stationField WHERE Fdt_Run_Id like "',
                                     runIDfilter, '"'))      }
      
    # filter by lab group code before bringing in analyte data
    if(nrow(stationField) > 0 & !is.null(labGroupCodeFilter) ){
      sampleView <- pool %>% tbl(in_schema("wqm", "Wqm_Samples_View")) %>%
        filter(Sam_Fdt_Id %in% !! stationField$Fdt_Id &
                 Sam_Mrs_Lcc_Parm_Group_Cd %in% !! labGroupCodeFilter) %>% 
        as_tibble()   
      stationField <- filter(stationField, Fdt_Id %in% sampleView$Sam_Fdt_Id)  } # update stationField to just stations that have lab codes needed

    preliminaryStations <- filter(preliminaryStations, StationID %in% stationField$Fdt_Sta_Id)  }
  } else {
    return(preliminaryStations)
  }

  if(!is.null(analyte_Filter)){
    stationAnalyte <- pool %>% tbl(in_schema("wqm", "Wqm_Analytes_View")) %>%
      filter(Ana_Sam_Fdt_Id %in% !!  stationField$Fdt_Id &
               between(as.Date(Ana_Received_Date), !! dateRange_multistation[1], !! dateRange_multistation[2]) & # x >= left & x <= right
               Pg_Parm_Name %in% analyte_Filter) %>%
      dplyr::select(Ana_Sam_Fdt_Id) %>% # save time by only bringing back station names
      as_tibble() %>%
      # need to join back to field data to get station name
      left_join(stationField, by = c("Ana_Sam_Fdt_Id" = "Fdt_Id")) %>%
      distinct(Fdt_Sta_Id)
    preliminaryStations <- filter(preliminaryStations, StationID %in% stationAnalyte$Fdt_Sta_Id)  }

  return(preliminaryStations) }





# BSA tool data output

# Habitat Data pull for BSA
BSAhabitatQuery <- function(pool, station, dateRangeFilter){
  totalHabitatSample <- pool %>% tbl(in_schema("wqm", "Edas_Habitat_Sample_View")) %>%
    filter(STA_ID %in% !! toupper(station) &  between(as.Date(FDT_DATE_TIME), !!dateRangeFilter[1], !!dateRangeFilter[2])) %>%
    as_tibble() %>% 
    rename("StationID" = "STA_ID",
           "HabSampID" = "WHS_SAMP_ID",
           "Entered Date" = "WHS_INSERTED_DATE",
           "Entered By" = "WHS_INSERTED_BY",
           "Field Team" = "WHS_FIELD_TEAM",
           "HabSample Comment" = "WHS_COMMENT",
           "Gradient" = "WSGC_DESCRIPTION",
           "Collection Date" = "FDT_DATE_TIME") %>%
    # Add sample season 
    mutate(monthday = as.numeric(paste0(sprintf("%02d",month(`Collection Date`)),
                                        sprintf("%02d",day(`Collection Date`)))),
           Season = case_when(monthday >= 0215 & monthday <= 0615 ~ 'Spring',
                              monthday >= 0815 & monthday <= 1215 ~ 'Fall',
                              TRUE ~ as.character("Outside Sample Window"))) %>%
    dplyr::select(HabSampID, StationID, `Collection Date`, `Entered By`, `Entered Date`, `Field Team`, `HabSample Comment`, Gradient, Season)
  if(nrow(totalHabitatSample) > 0){
    totalHabitat <- pool %>% tbl(in_schema("wqm", "Edas_Habitat_Values_View")) %>%
      filter(WHS_SAMP_ID %in% !! totalHabitatSample$HabSampID) %>%
      as_tibble() %>%
      rename("HabSampID" = "WHS_SAMP_ID",
             "HabParameter" = "WHVP_CODE",
             "HabParameterDescription" = "WHVP_DESCRIPTION",
             "HabValue" = "WHV_HAB_VALUE",
             "HabValue Comment" = "WHV_COMMENT") %>%
      left_join(dplyr::select(totalHabitatSample, StationID, HabSampID, `Collection Date`), by = 'HabSampID') %>% 
      # what I really want after BSA update
      #dplyr::select(StationID, `Collection Date`, HabParameter, HabParameterDescription, HabValue, `HabValue Comment`, Gradient, Season)
      dplyr::select(StationID, CollDate = `Collection Date`, HabParameter, HabValue) %>% 
      group_by(StationID, CollDate) %>% 
      mutate(`Total Habitat Score` = sum(HabValue, na.rm = T)) %>%  ungroup()
  } else {
    totalHabitat <- tibble(StationID = station, CollDate = NA, HabParameter = NA, HabValue = NA, `Total Habitat Score` = NA) }
  return(totalHabitat)
}
#BSAhabitatQuery(pool, station, dateRangeFilter)


BSAtooloutputFunction <- function(pool, station, dateRangeFilter, LRBS, stationInfo_sf, stationAnalyteDataUserFilter, stationFieldDataUserFilter){
  # Get habitat Information
  totHab <- BSAhabitatQuery(pool, station, dateRangeFilter) %>% 
    distinct(CollDate, .keep_all = T) %>% 
    dplyr::select(-c(HabParameter, HabValue))
  
  # Get LRBS information
  lrbs <- filter(LRBS, StationID %in% station & between(Date, dateRangeFilter[1], dateRangeFilter[2])) %>% 
    dplyr::select(StationID, Date, LRBS = LRBS2)
  
  # Get Metals Information
  metalsCCU <- filter(stationAnalyteDataUserFilter, Pg_Parm_Name %in% mCCUmetals)
  if(nrow(metalsCCU) > 0){
    metalsCCU <- metalsCCU %>% 
      mutate(Parameter = recode(`Pg_Parm_Name`,  "CALCIUM, DISSOLVED (MG/L AS CA)" = "Calcium",
                                "ARSENIC, DISSOLVED  (UG/L AS AS)" = "Arsenic",
                                "CHROMIUM, DISSOLVED (UG/L AS CR)" = "Chromium",
                                "COPPER, DISSOLVED (UG/L AS CU)" = "Copper", 
                                "LEAD, DISSOLVED (UG/L AS PB)" = "Lead",
                                "NICKEL, DISSOLVED (UG/L AS NI)" = "Nickel",
                                "ZINC, DISSOLVED (UG/L AS ZN)" = "Zinc",
                                "HARDNESS, CA MG CALCULATED (MG/L AS CACO3) AS DISSOLVED" = "Hardness")) %>% 
      dplyr::select(StationID = Fdt_Sta_Id, `Collection Date` =  Fdt_Date_Time, Parameter, Ana_Uncensored_Value) %>% 
      group_by(StationID, `Collection Date`) %>% 
      pivot_wider(names_from = Parameter, values_from = Ana_Uncensored_Value) %>%
      mutate(criteriaHardness = ifelse(Hardness<25,25,ifelse(Hardness>400,400,Hardness)),
             ArsenicChronic = Arsenic/150,
             ChromiumChronic = Chromium/((exp(0.819*(log(criteriaHardness))+0.6848))*0.86),
             CopperChronic = Copper/((exp(0.8545*(log(criteriaHardness))-1.702))*0.96),
             LeadChronic = Lead/((exp(1.273*(log(criteriaHardness))-3.259))*(1.46203-(log(criteriaHardness)*(0.145712)))),
             NickelChronic = Nickel/((exp(0.846*(log(criteriaHardness))-0.884))*0.997),
             ZincChronic = Zinc/((exp(0.8473*(log(criteriaHardness))+0.884))*0.986)) %>% 
      summarise(MetalsCCU = sum(ArsenicChronic, ChromiumChronic, CopperChronic, LeadChronic, NickelChronic, ZincChronic, na.rm = T))
  } else {metalsCCU <- tibble(StationID = station,  `Collection Date`= NA, MetalsCCU = NA)}
  
  
  z <- bind_rows(tibble('Fdt_Sta_Id'= 'FakeRow', 'Fdt_Date_Time'= NA, 'Fdt_Depth' = NA, 'Fdt_Temp_Celcius'= NA, 'Fdt_Field_Ph'= NA, 'Fdt_Do_Probe'= NA, 'Fdt_Do_Optical'= NA, 'Fdt_Do_Winkler'= NA, 
                        'Fdt_Specific_Conductance'= NA,'NITROGEN, TOTAL (MG/L AS N)'= NA, 'PHOSPHORUS, TOTAL (MG/L AS P)'= NA, 'TDS RESIDUE,TOTAL FILTRABLE (DRIED AT 180C),MG/L'= NA,
                        'SULFATE, TOTAL (MG/L AS SO4)'= NA, 'CHLORIDE,TOTAL IN WATER MG/L'= NA,  'SODIUM, DISSOLVED (MG/L AS NA)'= NA, 'POTASSIUM, DISSOLVED (MG/L AS K)' = NA),
                 stationFieldAnalyteDataPretty(stationAnalyteDataUserFilter, stationFieldDataUserFilter, averageResults = FALSE) %>%
                   dplyr::select(one_of(c('Fdt_Sta_Id', 'Fdt_Date_Time', 'Fdt_Depth', 'Fdt_Temp_Celcius', 'Fdt_Field_Ph', 'Fdt_Do_Probe', 'Fdt_Do_Optical', 'Fdt_Do_Winkler', 
                                          'Fdt_Specific_Conductance','NITROGEN, TOTAL (MG/L AS N)', 'PHOSPHORUS, TOTAL (MG/L AS P)', 'SULFATE, TOTAL (MG/L AS SO4)',
                                          'TDS RESIDUE,TOTAL FILTRABLE (DRIED AT 180C),MG/L','CHLORIDE,TOTAL IN WATER MG/L', 'SODIUM, DISSOLVED (MG/L AS NA)',
                                          'POTASSIUM, DISSOLVED (MG/L AS K)')))) %>% 
    filter(Fdt_Sta_Id != 'FakeRow') %>% 
    mutate(`Dissolved Oxygen` = case_when(!is.na(Fdt_Do_Probe) ~ Fdt_Do_Probe,
                                          !is.na(Fdt_Do_Optical) ~ Fdt_Do_Optical,
                                          !is.na(Fdt_Do_Winkler) ~ Fdt_Do_Winkler,
                                          TRUE ~ as.numeric(NA))) %>%
    full_join(totHab, by = c('Fdt_Sta_Id'= 'StationID', 'Fdt_Date_Time' = 'CollDate')) %>% # full join in case benthic date/time doesn't match somewhere in CEDS
    full_join(lrbs, by = c('Fdt_Sta_Id'= 'StationID', 'Fdt_Date_Time' = 'Date')) %>% # full join in case LRBS date/time doesn't match somewhere in CEDS
    full_join(metalsCCU, by = c('Fdt_Sta_Id'= 'StationID', 'Fdt_Date_Time' = 'Collection Date')) %>% # full join in case LRBS date/time doesn't match somewhere in CEDS
    filter(!is.na(Fdt_Date_Time)) %>%  #drop any empty rows after joining other datasets
    left_join(dplyr::select(stationInfo_sf, STATION_ID, Latitude, Longitude) %>% 
                distinct(STATION_ID, .keep_all= T) %>% 
                st_drop_geometry(), by = c('Fdt_Sta_Id'='STATION_ID')) %>% 
    dplyr::select(StationID = Fdt_Sta_Id, CollectionDateTime = Fdt_Date_Time, Depth = Fdt_Depth, Longitude,  Latitude,
                  `pH (unitless)` = Fdt_Field_Ph, `DO (mg/L)` = `Dissolved Oxygen`, `TN (mg/L)` = `NITROGEN, TOTAL (MG/L AS N)`,
                  `TP (mg/L)` = `PHOSPHORUS, TOTAL (MG/L AS P)`, `Total Habitat (unitless)` = `Total Habitat Score`, 
                  `LRBS (unitless)` = LRBS, `MetalsCCU (unitless)` = MetalsCCU,
                  `SpCond (uS/cm)` = Fdt_Specific_Conductance, `TDS (mg/L)` = `TDS RESIDUE,TOTAL FILTRABLE (DRIED AT 180C),MG/L`,
                  `DSulfate (mg/L)` = `SULFATE, TOTAL (MG/L AS SO4)`, `DChloride (mg/L)` = `CHLORIDE,TOTAL IN WATER MG/L`, 
                  `DPotassium (mg/L)` = `POTASSIUM, DISSOLVED (MG/L AS K)`, `DSodium (mg/L)` = `SODIUM, DISSOLVED (MG/L AS NA)`, 
                  `Temperature (C)` = Fdt_Temp_Celcius) %>% 
    arrange(StationID, CollectionDateTime) %>% 
    mutate(row_number()) #number rows
  # drop empty rows
  z2 <- filter_at(z, vars(`pH (unitless)`:`Temperature (C)`), all_vars(is.na(.)))
  z <- filter(z, ! `row_number()` %in% z2$`row_number()`)  %>% 
    dplyr::select(-`row_number()`)
  return(z)
} 


BSAtoolMetalsFunction <- function(station, stationInfo_sf, stationAnalyteDataUserFilter){

    z <- stationAnalyteDataUserFilter %>%
      mutate(Parameter = recode(`Pg_Parm_Name`,
                                "SELENIUM, DISSOLVED (UG/L AS SE)" = "Selenium",
                                "CALCIUM, DISSOLVED (MG/L AS CA)" = "Calcium",
                                "MAGNESIUM, DISSOLVED (MG/L AS MG)" = "Magnesium",
                                "ARSENIC, DISSOLVED  (UG/L AS AS)" = "Arsenic",
                                "BARIUM, DISSOLVED (UG/L AS BA)" = "Barium",
                                "ALUMINUM, DISSOLVED (UG/L AS AL)" = "Aluminum",
                                "BERYLLIUM, DISSOLVED (UG/L AS BE)" = "Beryllium",
                                "CADMIUM, DISSOLVED (UG/L AS CD)" = "Cadmium",
                                "CHROMIUM, DISSOLVED (UG/L AS CR)" = "Chromium",
                                "COPPER, DISSOLVED (UG/L AS CU)" = "Copper", 
                                "IRON, DISSOLVED (UG/L AS FE)" = "Iron",
                                "LEAD, DISSOLVED (UG/L AS PB)" = "Lead",
                                "MANGANESE, DISSOLVED (UG/L AS MN)" = "Manganese",
                                "THALLIUM, DISSOLVED (UG/L AS TL)" = "Thallium",
                                "NICKEL, DISSOLVED (UG/L AS NI)" = "Nickel",
                                "SILVER, DISSOLVED (UG/L AS AG)" = "Silver",
                                "ZINC, DISSOLVED (UG/L AS ZN)" = "Zinc",
                                "ANTIMONY, DISSOLVED (UG/L AS SB)" = "Antimony", 
                                "HARDNESS, CA MG CALCULATED (MG/L AS CACO3) AS DISSOLVED" = "Hardness")) %>% 
      filter(Parameter %in% c('Calcium', 'Magnesium', 'Arsenic', 'Barium', 'Beryllium', 'Cadmium', 'Chromium', 'Copper', 
                              'Iron', 'Lead', 'Manganese', 'Thallium', 'Nickel', 'Silver', 'Zinc', 'Antimony', 'Aluminum', 'Selenium', 'Hardness'))
    if(nrow(z)> 0){
      z <- bind_rows(tibble(StationID = 'FakeRow',	CollectionDateTime = NA, Longitude = NA, Latitude = NA, Calcium = NA, Magnesium = NA, 
                       Arsenic = NA, Barium = NA, Beryllium = NA, Cadmium = NA, Chromium = NA, Copper = NA, Iron = NA, Lead = NA, Manganese = NA, Thallium = NA, 
                       Nickel = NA, Silver = NA, Zinc = NA, Antimony = NA, Aluminum = NA, Selenium = NA, Hardness = NA),
                z %>% 
                  dplyr::select(StationID = Fdt_Sta_Id, CollectionDateTime =  Fdt_Date_Time, Parameter, Ana_Uncensored_Value) %>% 
                  group_by(StationID, CollectionDateTime) %>% 
                  pivot_wider(names_from = Parameter, values_from = Ana_Uncensored_Value)  %>% 
                  left_join(dplyr::select(stationInfo_sf, StationID = STATION_ID, Longitude, Latitude) %>% 
                              distinct(StationID, .keep_all= T) %>% st_drop_geometry(), by = 'StationID') %>% 
                  dplyr::select(StationID, CollectionDateTime,  Longitude, Latitude, everything())) %>% 
        filter(StationID != 'FakeRow')
  } else  {
      z <- tibble(StationID = NA, CollectionDateTime = NA, Longitude = NA, Latitude = NA, Calcium = NA, Magnesium = NA, 
                  Arsenic = NA, Barium = NA, Beryllium = NA, Cadmium = NA, Chromium = NA, Copper = NA, Iron = NA, Lead = NA, Manganese = NA, Thallium = NA, 
                  Nickel = NA, Silver = NA, Zinc = NA, Antimony = NA, Aluminum = NA, Selenium = NA, Hardness = NA)    }
  
  return(z)
}   
#BSAtoolMetalsFunction(station, stationInfo_sf, stationAnalyteDataUserFilter)



## Individual parameter boxplot
parameterBoxplotFunction <- function(basicData, parameter, unitData, WQSlookup, addJitter){
  z <- dplyr::select(basicData, parameterPlot = !! parameter) %>% # rename clutch for nse
    filter(!is.na(parameterPlot)) 
  if(nrow(z) != 0){
    parameterUnits <- filter(unitData, AltName %in% !!parameter)$Units
    if(parameter %in% c('Temperature', 'Dissolved Oxygen', "pH")){
      if(parameter == 'Temperature'){parameterLimit <- 'Max Temperature (C)'; specialStandards <- NULL}
      if(parameter == 'Dissolved Oxygen'){parameterLimit <- 'Dissolved Oxygen Min (mg/L)'; specialStandards <- NULL}
      if(parameter == 'pH'){
        parameterLimit <- c('pH Min', 'pH Max')
        if(nrow(filter(WQSlookup, StationID %in% unique(basicData$StationID)) %>% 
                filter(str_detect(as.character(SPSTDS), '6.5-9.5'))) > 0){specialStandards <- c(6.5, 9.5)
        } else {specialStandards <- NULL}}
    } else { parameterLimit <- NULL 
    specialStandards <- NULL  }
    
    dat <- dplyr::select(basicData, StationID, `Collection Date`, Depth, Measure = !! parameter) %>%
      filter( !is.na(Measure))
    
    if(nrow(dat) > 0){
      dat <- left_join(dat, WQSlookup, by = c('StationID')) %>%
        mutate(CLASS_BASIN = paste(CLASS,substr(BASIN, 1,1), sep="_")) %>%
        mutate(CLASS_BASIN = ifelse(CLASS_BASIN == 'II_7', "II_7", as.character(CLASS))) %>%
        # Fix for Class II Tidal Waters in Chesapeake (bc complicated DO/temp/etc standard)
        left_join(WQSvalues, by = 'CLASS_BASIN') %>%
        dplyr::select(-c(CLASS.y,CLASS_BASIN)) %>%
        rename('CLASS' = 'CLASS.x') %>%
        # add standards info if available
        {if(!is.null(parameterLimit))
          dplyr::select(., StationID, `Collection Date`, Depth, Measure, Standard = !!parameterLimit) 
          else dplyr::select(., StationID, `Collection Date`, Depth, Measure) } %>%
        # pH special standards correction
        {if(!is.null(specialStandards))
          mutate(., Standard1 = specialStandards[1], Standard2 = specialStandards[2])
          else . } %>%
        mutate(units = parameterUnits) 
      #return(dat)
      if(addJitter == TRUE){
        plot_ly(data=dat) %>% 
          add_boxplot( y = ~Measure, color = ~StationID, type = "box", boxpoints = "all", 
                       jitter = 0.3, pointpos = -1.8) %>% 
          {if(parameter %in% c('Temperature', 'Dissolved Oxygen'))
            add_markers(., x = ~StationID, y=~Standard, mode='scatter', symbols = 'square', marker = list(color = 'black'),
                        hoverinfo = "text", text= paste(parameter, "Standard"), name= paste(parameter, "Standard"))
            else .} %>% 
          {if(parameter %in% c('pH'))
            add_markers(., x = ~StationID, y=~Standard1, mode='scatter', symbols = 'square', marker = list(color = 'black'),
                        hoverinfo = "text", text= paste(parameter, "Standard"), name= paste(parameter, "Standard")) %>% 
              add_markers(., x = ~StationID, y=~Standard2, mode='scatter', symbols = 'square', marker = list(color = 'black'),
                          hoverinfo = "text", text= paste(parameter, "Standard"), name= paste(parameter, "Standard"))
            else .} 
        
      } else {
        plot_ly(data=dat) %>% 
          add_boxplot(y = ~Measure, color = ~StationID, type = "box") %>% 
          {if(parameter %in% c('Temperature', 'Dissolved Oxygen'))
            add_markers(., x = ~StationID, y=~Standard, mode='scatter', symbols = 'square', marker = list(color = 'black'),
                        hoverinfo = "text", text= paste(parameter, "Standard"), name= paste(parameter, "Standard"))
            else .} %>% 
          {if(parameter %in% c('pH'))
            add_markers(., x = ~StationID, y=~Standard1, mode='scatter', symbols = 'square', marker = list(color = 'black'),
                        hoverinfo = "text", text= paste(parameter, "Standard"), name= paste(parameter, "Standard")) %>% 
              add_markers(., x = ~StationID, y=~Standard2, mode='scatter', symbols = 'square', marker = list(color = 'black'),
                          hoverinfo = "text", text= paste(parameter, "Standard"), name= paste(parameter, "Standard"))
            else .}  }
    }
  }
}
#parameterBoxplotFunction(basicData, 'Dissolved Oxygen', unitData, WQSlookup, addJitter = F)
