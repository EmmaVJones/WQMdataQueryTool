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
  Driver = "SQL Server Native Client 11.0", 
  Server= "DEQ-SQLODS-PROD,50000",
  dbname = "ODS",
  trusted_connection = "yes"
)

# For deployment on the R server: Set up pool connection to production environment
#pool <- dbPool(
#  drv = odbc::odbc(),
#  Driver = "SQLServer",   # note the LACK OF space between SQL and Server ( how RStudio named driver)
  # Production Environment
#  Server= "DEQ-SQLODS-PROD,50000",
#  dbname = "ODS",
#  UID = conn$UID_prod, 
#  PWD = conn$PWD_prod,
  #UID = Sys.getenv("userid_production"), # need to change in Connect {vars}
  #PWD = Sys.getenv("pwd_production")   # need to change in Connect {vars}
  # Test environment
  #Server= "WSQ04151,50000",
  #dbname = "ODS_test",
  #UID = Sys.getenv("userid"),  # need to change in Connect {vars}
  #PWD = Sys.getenv("pwd"),  # need to change in Connect {vars}
#  trusted_connection = "yes"
#)

onStop(function() {
  poolClose(pool)
})


unitData <- read_csv('data/probParameterUnits.csv')
# Temporary list that we can compare data to. Maybe we increase to benthic metrics, MCCU, + more in time
probIndicators <- filter(unitData, AltName %in% #names(basicData))$AltName
                           c("DO", "pH", "Specific Conductance", "Total Nitrogen", "Total Phosphorus", "Total Dissolved Solids",
                             "Ammonia", "Total Nitrate Nitrogen", "Ortho Phosphorus", "Turbidity", "Total Suspended Solids", "Sodium", 
                             "Potassium", "Chloride", "Sulfate", "Suspended Sediment Concentration Coarse", "Suspended Sediment Concentration Fine",
                             "Arsenic", "Barium", "Beryllium", "Cadmium", "Chromium", "Copper", "Iron", "Lead", "Manganese", "Thallium", "Nickel",                                 
                             "Silver", "Zinc", "Antimony", "Aluminum", "Selenium", "Hardness", "Ecoli"))
  #c("DO", "pH", "SpCond", "TN", "TP", "TDS", "NH4", "NO3", "Turb", "TSS", "Na", "K", "Cl", "Sf", 
                  #  "SSCCOARSE", "SSCFINE", "ARSENIC", "BARIUM", "BERYLLIUM", "CADMIUM", "CHROMIUM", "COPPER", "IRON", 
                  #  "LEAD", "MANGANESE", "THALLIUM", "NICKEL", "SILVER", "ZINC", "ANTIMONY", "ALUMINUM" ,"SELENIUM", "HARDNESS" )
probEst <- readRDS('data/IR2020probMonCDFestimates.RDS') %>%
  filter(Indicator %in% probIndicators$Parameter)



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
stationFieldAnalyteDataPretty <- function(stationAnalyteDataRaw, stationFieldDataRaw, repFilter, averageResults){
  if(averageResults == TRUE){
    inner_join(stationFieldDataRaw,
               stationAnalyteDataRaw %>%
                 filter(Ana_Sam_Mrs_Container_Id_Desc %in% repFilter) %>%
                 group_by(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc) %>%
                 dplyr::select(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc, #Ana_Sam_Mrs_Lcc_Parm_Group_Cd,
                               Ana_Parameter_Name, Ana_Value) %>%
                 pivot_wider(names_from = c('Ana_Parameter_Name'), names_sep = " | ", 
                             values_from = "Ana_Value",
                             values_fn = list(Ana_Value = mean)),
               by = c("Fdt_Id" = "Ana_Sam_Fdt_Id", 'Fdt_Sta_Id', 'Fdt_Date_Time')) 
  } else {
    inner_join(stationFieldDataRaw,
               stationAnalyteDataRaw %>%
                 filter(Ana_Sam_Mrs_Container_Id_Desc %in% repFilter) %>%
                 group_by(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc, Ana_Sam_Mrs_Lcc_Parm_Group_Cd) %>%
                 dplyr::select(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc, Ana_Sam_Mrs_Lcc_Parm_Group_Cd, Ana_Parameter_Name, Ana_Value) %>%
                 pivot_wider(names_from = c('Ana_Parameter_Name','Ana_Sam_Mrs_Lcc_Parm_Group_Cd'), names_sep = " | ", 
                             values_from = "Ana_Value"),
               by = c("Fdt_Id" = "Ana_Sam_Fdt_Id", 'Fdt_Sta_Id', 'Fdt_Date_Time')) }
}


# Field collector info
uniqueCollector <- function(stationFieldAnalyte){
  stationFieldAnalyte %>%
    group_by(Fdt_Sta_Id, Fdt_Collector_Id) %>%
    summarise(`n Samples` = n()) 
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
}#
#concatenateCols(stationFieldAnalyte,  'CHLOROPHYLL-A UG/L SPECTROPHOTOMETRIC ACID. METH') #'SUSP. SED. CONC. TOTAL, MG/L,(Method B)')#, 
#df <- stationFieldAnalyte; containString <- 'E.COLI'

# Generic Info People Want
#stationFieldAnalyte <- stationFieldAnalyte1
#stationFieldAnalyte$Fdt_Do_Optical[5] <- NA
#stationFieldAnalyte$Fdt_Do_Winkler[5] <- 7.7
#stationFieldAnalyte$Fdt_Do_Probe[7] <- 8.6
#stationFieldAnalyte$Fdt_Do_Optical[7] <- NA


basicSummary <- function(stationFieldAnalyte){
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
         DO = case_when(!is.na(Fdt_Do_Probe) ~ Fdt_Do_Probe,
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
         `Benthic Ash Free Dry Mass` = concatenateCols(stationFieldAnalyte, 'BENTHIC ASH FREE DRY MASS, GM/M2')) %>%
    dplyr::select(-c(Fdt_Id:blankColForSelect) )   
}




parameterPlotly <- function(basicData,
                            parameter,
                            unitData,
                            WQSlookup){
  parameterUnits <- filter(unitData, AltName %in% !!parameter)$Units
  if(parameter %in% c('Temperature', 'DO', "pH")){
    if(parameter == 'Temperature'){parameterLimit <- 'Max Temperature (C)'; specialStandards <- NULL}
    if(parameter == 'DO'){parameterLimit <- 'Dissolved Oxygen Min (mg/L)'; specialStandards <- NULL}
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
      {if(parameter %in% c('Temperature', 'DO'))
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
  }
}




# Find central tendency of each parameter based on filtered window
median_n <- list(
  median = ~signif(median(.x, na.rm = TRUE), digits = 2), 
  mean = ~signif(mean(.x, na.rm = TRUE), digits = 2), 
  n = ~sum(ifelse(!is.na(.x), 1,0))#, na.rm = TRUE)
)

centralTendencies <- function(basicData){
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
percentileTable <- function(cdfdata, statsTable,parameter,userBasin,userEco,userOrder){
  out <- statsTable %>%
    dplyr::select(StationID, Statistic, starts_with(parameter))
  va <- data.frame(filter(cdfdata,Subpopulation=='Virginia',Indicator==parameter)%>%select(Value,Estimate.P)) # needs to be df for vlookup to work
  basin <- data.frame(filter(cdfdata,Subpopulation==userBasin,Indicator==parameter)%>%select(Value,Estimate.P)) # needs to be df for vlookup to work
  eco <- data.frame(filter(cdfdata,Subpopulation==userEco,Indicator==parameter)%>%select(Value,Estimate.P)) # needs to be df for vlookup to work
  #order <- data.frame(filter(cdfdata,Subpopulation==userOrder,Indicator==parameter)%>%select(Value,Estimate.P)) # needs to be df for vlookup to work
  out_final <- list(statistics = out, 
                    percentiles = data.frame(Subpopulation = 'Virginia',
                                             Average = vlookup(filter(out, str_detect(Statistic, 'Mean')) %>% dplyr::select(starts_with(parameter)) %>% pull(), va, 2, range=TRUE),
                                             Median = vlookup(filter(out, str_detect(Statistic, 'Median')) %>% dplyr::select(starts_with(parameter)) %>% pull(), va, 2, range=TRUE)) %>%
                      bind_rows(data.frame(Subpopulation = userBasin,
                                           Average = vlookup(filter(out, str_detect(Statistic, 'Mean')) %>% dplyr::select(starts_with(parameter)) %>% pull(), basin, 2, range=TRUE),
                                           Median = vlookup(filter(out, str_detect(Statistic, 'Median')) %>% dplyr::select(starts_with(parameter)) %>% pull(), basin, 2, range=TRUE))) %>%
                      bind_rows(data.frame(Subpopulation = userEco,
                                           Average = vlookup(filter(out, str_detect(Statistic, 'Mean')) %>% dplyr::select(starts_with(parameter)) %>% pull(), eco, 2, range=TRUE),
                                           Median = vlookup(filter(out, str_detect(Statistic, 'Median')) %>% dplyr::select(starts_with(parameter)) %>% pull(), eco, 2, range=TRUE))) )
  return(out_final)
}


# CDF plot function
cdfplot <- function(cdfdata, prettyParameterName,parameter,subpopulation,dataset,CDFsettings){
  cdfsubset <- subFunction(cdfdata,parameter,subpopulation)
  avg1 <- filter(dataset$percentiles, Subpopulation == subpopulation) %>% pull(Average)#filter(dataset$statistics, Statistic == "Mean")$DO
  avg <- subFunction2(cdfsubset,avg1)
  med1 <- filter(dataset$percentiles, Subpopulation == subpopulation) %>% pull(Median)
  med <- subFunction2(cdfsubset,med1)
  m <- max(cdfsubset$NResp)
  p1 <- ggplot(cdfsubset, aes(x=Value,y=Estimate.P)) + 
    labs(x=paste(prettyParameterName,unique(cdfsubset$Units),sep=" "),y="Percentile") +
    ggtitle(paste(subpopulation,prettyParameterName,"Percentile Graph ( n = ",m,")",sep=" ")) + 
    theme(plot.title = element_text(hjust=0.5,face='bold',size=15)) +
    theme(axis.title = element_text(face='bold',size=12))+
    
    CDFsettings  +
    
    geom_point() +
    geom_point(data=avg,color='orange',size=4) + geom_text(data=avg,label='Average',hjust=1.2) +
    geom_point(data=med,color='gray',size=4)+ geom_text(data=med,label='Median',hjust=1.2) 
  return(p1)
}