library(tidyverse)
library(shiny)
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

#Bring in VLOOKUP-like function written in R
source('vlookup.R')
source('cdfRiskTable.R')

# Server connection things
conn <- config::get("connectionSettings") # get configuration settings


board_register_rsconnect(key = conn$CONNECT_API_KEY,  #Sys.getenv("CONNECT_API_KEY"),
                         server = conn$CONNECT_SERVER)#Sys.getenv("CONNECT_SERVER"))



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
                                   Subpopulation == 'James Basin' ~ 'James',
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






# Pull as many details about station from REST service (if available). Work around provided in case station isn't on REST server
## Pull station info from REST service
WQM_Station_Full_REST_request <- function(pool, station, subbasinVAHU6crosswalk, subbasins, ecoregion){
  WQM_Station_Full_REST <- suppressWarnings(
    geojson_sf(
      paste0("https://gis.deq.virginia.gov/arcgis/rest/services/staff/DEQInternalDataViewer/MapServer/104/query?&where=STATION_ID%3D%27",
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
    stationGISInfo <- pool %>% tbl( "WQM_Sta_GIS_View") %>%
      filter(Station_Id %in% !! toupper(station)) %>%
      as_tibble() 
    # pull a known station to steal data structure
    WQM_Station_Full_REST <- suppressWarnings(
      geojson_sf(
        paste0("https://gis.deq.virginia.gov/arcgis/rest/services/staff/DEQInternalDataViewer/MapServer/104/query?&where=STATION_ID%3D%272-JKS023.61%27&outFields=*&f=geojson")))[1,] %>%
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
    y <- stationAnalyteDataRaw %>%
      group_by(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc) %>%
      dplyr::select(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc, Ana_Com_Code, #Ana_Sam_Mrs_Lcc_Parm_Group_Cd,
                    Pg_Parm_Name, Ana_Value) %>% 
      mutate(`Associated Analyte Records` = 1:n(),
             LabComments = paste0(Pg_Parm_Name,' RMK'))
    y1 <- y %>%
      dplyr::select(Pg_Parm_Name, Ana_Value) %>% 
      pivot_wider(names_from = Pg_Parm_Name, #names_sep = " | ", 
                  values_from = "Ana_Value",
                  values_fn = list(Ana_Value = mean) ) %>% 
      left_join(y %>% ungroup() %>% group_by(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc, LabComments) %>%  
                  mutate(Ana_Com_Code2 = paste(Ana_Com_Code, sep = " ")) %>%
                  dplyr::select(LabComments, Ana_Com_Code2) %>% 
                  distinct() %>% 
                  pivot_wider(names_from = LabComments, values_from = Ana_Com_Code2),
                by = c("Ana_Sam_Fdt_Id", "Fdt_Sta_Id", "Fdt_Date_Time", "Ana_Sam_Mrs_Container_Id_Desc")) %>% 
      dplyr::select(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc, sort(names(.)))
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
    #                            Ana_Parameter_Name, Ana_Value) %>%
    #              pivot_wider(names_from = c('Ana_Parameter_Name'), names_sep = " | ", 
    #                          values_from = "Ana_Value",
    #                          values_fn = list(Ana_Value = mean)),
    #            by = c("Fdt_Id" = "Ana_Sam_Fdt_Id", 'Fdt_Sta_Id', 'Fdt_Date_Time')) %>%
    #   arrange(Fdt_Sta_Id, Fdt_Date_Time))
  } else {
    z <- stationAnalyteDataRaw %>%
      group_by(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc, Pg_Parm_Name, Ana_Lab_Seq_Num) %>%
      dplyr::select(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc, Pg_Parm_Name, Ana_Com_Code, Ana_Value) %>%
      mutate(`Associated Analyte Records` = 1:n(),
             LabComments = paste0(Pg_Parm_Name,' RMK'))
    
    z1 <-  z %>% 
      dplyr::select(`Associated Analyte Records`, Pg_Parm_Name, Ana_Value) %>% 
      pivot_wider(names_from = Pg_Parm_Name, values_from = Ana_Value) %>%
      left_join(z %>% ungroup() %>% group_by(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc,Ana_Lab_Seq_Num) %>%  
                  dplyr::select(`Associated Analyte Records`, LabComments, Ana_Com_Code) %>% 
                  pivot_wider(names_from = LabComments, values_from = Ana_Com_Code),
                by = c("Ana_Sam_Fdt_Id", "Fdt_Sta_Id", "Fdt_Date_Time", "Ana_Sam_Mrs_Container_Id_Desc", "Ana_Lab_Seq_Num", 
                       "Associated Analyte Records")) %>% 
      dplyr::select(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc, Ana_Lab_Seq_Num, 
                    `Associated Analyte Records`, sort(names(.)))
    
    
    suppressWarnings(
      full_join(stationFieldDataRaw, 
                z1,
                # stationAnalyteDataRaw %>%
                #   filter(Ana_Sam_Mrs_Container_Id_Desc %in% repFilter) %>%
                #   group_by(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc, Pg_Parm_Name) %>%
                #   dplyr::select(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc, Ana_Lab_Seq_Num, Pg_Parm_Name , Ana_Value) %>%
                #   mutate(`Associated Analyte Records` = 1:n()) %>% 
                #   pivot_wider(names_from = 'Pg_Parm_Name',values_from = "Ana_Value") ,
                by = c("Fdt_Id" = "Ana_Sam_Fdt_Id", 'Fdt_Sta_Id', 'Fdt_Date_Time')) %>% 
        dplyr::select(`Associated Analyte Records`, everything()) %>%
        arrange(Fdt_Sta_Id, Fdt_Date_Time) ) }
    # full_join(stationFieldDataRaw,
    #            stationAnalyteDataRaw %>%
    #              filter(Ana_Sam_Mrs_Container_Id_Desc %in% repFilter) %>%
    #              group_by(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc, Ana_Sam_Mrs_Lcc_Parm_Group_Cd) %>%
    #              dplyr::select(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc, Ana_Sam_Mrs_Lcc_Parm_Group_Cd, Ana_Parameter_Name, Ana_Value) %>%
    #              pivot_wider(names_from = c('Ana_Parameter_Name','Ana_Sam_Mrs_Lcc_Parm_Group_Cd'), names_sep = " | ", 
    #                          values_from = "Ana_Value"),
    #            by = c("Fdt_Id" = "Ana_Sam_Fdt_Id", 'Fdt_Sta_Id', 'Fdt_Date_Time')) )}
}


# Field collector info
uniqueCollector <- function(stationFieldAnalyte){
  stationFieldAnalyte %>%
    group_by(Fdt_Sta_Id, Fdt_Collector_Id) %>%
    summarise(`n Samples` = n()) %>%
    arrange(desc(`n Samples`))
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
  x <- dplyr::select(df, contains(containString)) 
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


parameterPlotly <- function(basicData,
                            parameter,
                            unitData,
                            WQSlookup){
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
      plot_ly(data=dat) %>%
        {if(parameter %in% c('Temperature', 'Dissolved Oxygen'))
          add_lines(.,  x=~`Collection Date`,y=~Standard, mode='line', line = list(color = 'black'),
                    hoverinfo = "text", text= paste(parameter, "Standard"), name= paste(parameter, "Standard")) 
          else . } %>%
        {if(parameter %in% c('pH'))
          add_lines(.,  x=~`Collection Date`,y=~Standard1, mode='line', line = list(color = 'black'),
                    hoverinfo = "text", text= paste(parameter, "Standard"), name= paste(parameter, "Standard")) %>%
            add_lines(x=~`Collection Date`,y=~Standard2, mode='line', line = list(color = 'black'),
                      hoverinfo = "text", text= paste(parameter, "Standard"), name= paste(parameter, "Standard")) 
          else . } %>%
        add_markers(data=dat, x= ~`Collection Date`, y= ~Measure,mode = 'scatter', name= paste(parameter, unique(dat$units)), 
                    marker = list(color= '#535559'), hoverinfo="text",
                    text=~paste(sep="<br>",
                                paste("Sample Date: ",`Collection Date`),
                                paste("Depth: ",Depth, "m"),
                                paste(parameter, ": ",Measure," (mg/L)"))) %>%
        layout(showlegend=FALSE,
               yaxis=list(title=paste(parameter, unique(dat$units))),
               xaxis=list(title="Sample Date",tickfont = list(size = 10))) 
    } } 
  
  
}




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
                                         ecoregionFilter, dateRange_multistation, analyte_Filter, manualSelection, wildcardSelection){
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
        else .}  }
  if(str_detect(queryType, 'Manually Specify')){
    preliminaryStations <-  filter(WQM_Stations_Spatial, StationID %in% as.character(manualSelection))  }
  if(queryType == 'Wildcard Selection' ){
    preliminaryStations <- sqldf(paste0('SELECT * FROM WQM_Stations_Spatial WHERE StationID like "',
                                        wildcardSelection, '"'))  }
  

  # add daterange filter based on preliminary station results
  if(nrow(preliminaryStations) > 0){
    if(!is.null(dateRange_multistation)){
    stationField <- pool %>% tbl("Wqm_Field_Data_View") %>%
      filter(Fdt_Sta_Id %in% !! preliminaryStations$StationID &
               between(as.Date(Fdt_Date_Time), !! dateRange_multistation[1], !! dateRange_multistation[2]) ) %>% # & # x >= left & x <= right
               #Ssc_Description != "INVALID DATA SET QUALITY ASSURANCE FAILURE") %>%
     # dplyr::select(Fdt_Sta_Id, Fdt_Id) %>% # save time by only bringing back station names
      as_tibble()

    preliminaryStations <- filter(preliminaryStations, StationID %in% stationField$Fdt_Sta_Id)  }
  } else {
    return(preliminaryStations)
  }

  if(!is.null(analyte_Filter)){
    stationAnalyte <- pool %>% tbl("Wqm_Analytes_View") %>%
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
