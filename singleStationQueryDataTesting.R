source('global.R')



# Critical Data
# goes in server

assessmentRegions <- st_read( 'data/GIS/AssessmentRegions_simple.shp')
ecoregion <- st_read('data/GIS/vaECOREGIONlevel3__proj84.shp')
assessmentLayer <- st_read('data/GIS/AssessmentRegions_VA84_basins.shp') %>%
  st_transform( st_crs(4326)) 
subbasins <- st_read('data/GIS/DEQ_VAHUSB_subbasins_EVJ.shp') %>%
  rename('SUBBASIN' = 'SUBBASIN_1') %>%
  mutate(SUBBASIN = ifelse(is.na(SUBBASIN), as.character(BASIN_NAME), as.character(SUBBASIN))) %>%
  mutate(ProbBasin = case_when(SUBBASIN == 'Big Sandy River' ~ 'Big Sandy',
                               SUBBASIN == 'Chowan River' ~ 'Chowan',
                               SUBBASIN %in% c('James River - Lower', "James River - Middle", "James River - Upper") ~ 'James',
                               SUBBASIN == 'New River' ~ 'New',
                               SUBBASIN == 'Potomac River' ~ 'Potomac',
                               SUBBASIN == 'Shenandoah River' ~ 'Shenandoah',
                               SUBBASIN == 'Rappahannock River' ~ 'Rappahannock',
                               SUBBASIN == 'Roanoke River' ~ 'Roanoke',
                               SUBBASIN == 'Clinch and Powell Rivers' ~ 'Clinch',
                               SUBBASIN == 'Holston River' ~ 'Holston',
                               SUBBASIN == 'York River' ~ 'York',
                               TRUE ~ as.character(NA)),
         ProbSuperBasin = case_when(SUBBASIN %in% c('Big Sandy River','Holston River','Clinch and Powell Rivers') ~ 'Tennessee',
                                    SUBBASIN %in% c('Potomac River', 'Shenandoah River') ~ 'Potomac-Shenandoah',
                                    SUBBASIN %in% c('Rappahannock River', 'York River') ~ 'Rappahannock-York',
                                    TRUE ~ as.character(NA))) 
subbasinVAHU6crosswalk <- read_csv('data/basinAssessmentReg_clb_EVJ.csv') %>%
  filter(!is.na(SubbasinVAHU6code)) %>%
  mutate(SUBBASIN = ifelse(is.na(SUBBASIN), BASIN_NAME, SUBBASIN)) #%>%
  #dplyr::select(SUBBASIN, SubbasinVAHU6code)

WQSlookup <- pin_get("WQSlookup-withStandards",  board = "rsconnect")
# while server is down
#readRDS('C:/HardDriveBackup/R/pins11182020/ejones/WQSlookup-withStandards.RDS')



## Pull one station

station <- '2-JKS023.61'#'4AROA217.38'# not in WQM_full on REST service '2-JKS023.61'#
dateRange <- c(as.Date('1970-01-01'), as.Date(Sys.Date()))
#dateRangeFilter <- c(as.Date(Sys.Date())-years(3), as.Date(Sys.Date()))
dateRangeFilter <-  c(as.Date('1970-01-01'), as.Date(Sys.Date()))#c(as.Date('2015-02-24'), as.Date(Sys.Date()))#


### Geospatial Information

# Basic station info and make sure station in CEDS
stationInfo <- pool %>% tbl( "Wqm_Stations_View") %>%
  filter(Sta_Id %in% !! toupper(station)) %>%
  as_tibble()

# Pull as many details about station from REST service (if available). Work around provided in case station isn't on REST server
## Pull station info from REST service
WQM_Station_Full_REST <- WQM_Station_Full_REST_request(pool, station, subbasinVAHU6crosswalk, subbasins, ecoregion)

## Pull CEDS Station Information 
stationInfoFin <- stationInfoConsolidated(pool, station, WQM_Station_Full_REST)

#extra step, kinda seems unnecessary
stationInfo_sf <- WQM_Station_Full_REST

# Quick Station Sampling Information
stationInfoSampleMetrics <- stationSummarySampingMetrics(stationInfo_sf)

userSuperBasin <- WQM_Station_Full_REST$ProbSuperBasin[1]
userBasin <- WQM_Station_Full_REST$ProbBasin[1]
userEcoregion <- WQM_Station_Full_REST$EPA_ECO_US_L3NAME[1]#, EPA_ECO_US_L3CODE
userOrder <- NA

### Field Data Information

stationFieldData <- pool %>% tbl("Wqm_Field_Data_View") %>%
  filter(Fdt_Sta_Id %in% !! station &
           between(as.Date(Fdt_Date_Time), !! dateRange[1], !! dateRange[2]) ) %>% # x >= left & x <= right
  as_tibble()


### Analyte information

stationAnalyteData <- pool %>% tbl("Wqm_Analytes_View") %>%
  filter(Ana_Sam_Fdt_Id %in% !! stationFieldData$Fdt_Id &
           between(as.Date(Ana_Received_Date), !! dateRange[1], !! dateRange[2]) ) %>% # x >= left & x <= right
  as_tibble() %>%
  left_join(dplyr::select(stationFieldData, Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time), by = c("Ana_Sam_Fdt_Id" = "Fdt_Id")) 

### Organize field and analyte info into prettier table

stationFieldAnalyte1 <- stationFieldAnalyteDataPretty(filter(stationAnalyteData, between(as.Date(Fdt_Date_Time), dateRangeFilter[1], dateRangeFilter[2]) ), 
                                                      filter(stationFieldData, between(as.Date(Fdt_Date_Time), dateRangeFilter[1], dateRangeFilter[2]) ), 
                                                      repFilter = c('R','S1'),
                                                      averageResults = FALSE)
#stationFieldAnalyte2 <- stationFieldAnalyteDataPretty(filter(stationAnalyteData, between(as.Date(Fdt_Date_Time), dateRangeFilter[1], dateRangeFilter[2]) ), 
#                                                      filter(stationFieldData, between(as.Date(Fdt_Date_Time), dateRangeFilter[1], dateRangeFilter[2]) ), 
#                                                      repFilter = c('R','S1'),
#                                                      averageResults = FALSE)
z <- stationFieldAnalyte1 %>% # drop all empty columns ( this method longer but handles dttm issues)
  map(~.x) %>%
  discard(~all(is.na(.x))) %>%
  map_df(~.x)
if(exists('z')){
datatable(z, rownames = F, escape= F, extensions = 'Buttons',
          options = list(dom = 'Bit', scrollX = TRUE, scrollY = '350px',
                         pageLength = nrow(z), 
                         buttons=list('copy',#list(extend='excel',filename=paste0('CEDSFieldAnalyteData',input$station, Sys.Date())),
                                      'colvis')), selection = 'none')
} else {
  datatable(stationFieldAnalyte1, rownames = F, escape= F, extensions = 'Buttons',
            options = list(dom = 'Bit', scrollX = TRUE, scrollY = '350px',
                           pageLength = nrow(stationFieldAnalyte1), 
                           buttons=list('copy',#list(extend='excel',filename=paste0('CEDSFieldAnalyteData',input$station, Sys.Date())),
                                        'colvis')), selection = 'none')
}

# Collection info
uniqueCollector(stationFieldAnalyte1)

# Sample Codes Summary
uniqueSampleCodes(stationFieldAnalyte1)

# Special Comments
uniqueComments(stationFieldAnalyte1)

# Basic Dataset people will actually use
basicData <- basicSummary(stationFieldAnalyte1)

# parameter graph
parameterPlotly(basicData, 'Dissolved Oxygen', unitData, WQSlookup) 


# Compare to Prob Estimates section

# for now only keep estimates we can crosswalk to WQM data
#probEst <- filter(probEst, Indicator %in% filter(unitData, AltName %in% names(basicData))$Indicator) 

# Find central tendency of each parameter based on filtered window
probComparison <- centralTendenciesCalculation(basicData)  
  


# User chooses an indicator
#indicatorOptions <- probIndicators$AltName[1]
#parameter <- filter(probIndicators, Parameter %in% indicatorOptions)$Parameter


statsAndPercentiles <- percentileList(probEst, probComparison, probIndicators, userSuperBasin, userBasin, userEcoregion, userOrder)
                    
                    
# Plot mean and median
#cdfdata <- probEst
prettyParameterName <- 'Silver' #'Potassium'#probIndicators$AltName[1]

parameterSwitch <- filter(probIndicators, AltName %in% prettyParameterName)$Parameter

parameter <- parameterSwitch
subpopulation <- userBasin
dataset <- statsAndPercentiles#statsAndPercentiles[[parameter]]
#CDFsettings <- CDFsettingsList[[parameter]] #paste0(dataset,'settingsCDF')

cdfplot(probEst, prettyParameterName , parameterSwitch,  
        as.character(unique(WQM_Station_Full_REST$EPA_ECO_US_L3NAME)), statsAndPercentiles, CDFsettingsList[[parameterSwitch]] )




#






