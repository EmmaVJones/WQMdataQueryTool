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
labCommentCodes <- pin_get("labCommentCodes", board = 'rsconnect')



WQSlookup <- pin_get("WQSlookup-withStandards",  board = "rsconnect")
# while server is down
#readRDS('C:/HardDriveBackup/R/pins11182020/ejones/WQSlookup-withStandards.RDS')

## For testing: connect to ODS production
pool <- dbPool(
  drv = odbc::odbc(),
  Driver = "ODBC Driver 11 for SQL Server",#"SQL Server Native Client 11.0",
  Server= "DEQ-SQLODS-PROD,50000",
  dbname = "ODS",
  trusted_connection = "yes"
)


## Pull one station- this brings everything back based on these parameters and futher refining is allowed in the app
station <- '1BNFS011.81'#'2-PWT003.98'#'2-JKS023.61'#'2-JKS067.00'#'2-JKS023.61'#'1AOCC002.47'##'2-JKS006.67'#'2-JKS023.61'#'4AROA217.38'# not in WQM_full on REST service '2-JKS023.61'#
dateRange <- c(as.Date('1970-01-01'),  as.Date(Sys.Date())) #as.Date('1985-01-01'))#

# make sure station has data
# z <- pool %>% tbl( "Wqm_Field_data_View") %>%
#   filter(Fdt_Sta_Id %in% !! toupper(station)) %>%  
#   as_tibble()

# pool %>% tbl("Wqm_Analytes_View") %>%
#   filter(Ana_Sam_Fdt_Id %in% !! z$Fdt_Id)

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
           between(as.Date(Fdt_Date_Time), !! dateRange[1], !! dateRange[2]) & # x >= left & x <= right
           Ssc_Description != "INVALID DATA SET QUALITY ASSURANCE FAILURE") %>% 
  as_tibble()

### Analyte information

stationAnalyteData <- pool %>% tbl("Wqm_Analytes_View") %>%
  filter(Ana_Sam_Fdt_Id %in% !! stationFieldData$Fdt_Id &
           between(as.Date(Ana_Received_Date), !! dateRange[1], !! dateRange[2]) & # x >= left & x <= right
           Pg_Parm_Name != "STORET STORAGE TRANSACTION DATE YR/MO/DAY") %>% 
  as_tibble() %>%
  left_join(dplyr::select(stationFieldData, Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time), by = c("Ana_Sam_Fdt_Id" = "Fdt_Id")) 



# User filters
dateRangeFilter <-  c(as.Date('1970-01-01'), as.Date(Sys.Date()))#as.Date('2011-01-01'), as.Date('2011-12-31'))#c(as.Date('2015-02-24'), as.Date(Sys.Date()))#
labCodesDropped <- c('QF')#sort(unique(stationAnalyteData$Ana_Com_Code))
repFilter <- c('R')

stationFieldDataUserFilter <- filter(stationFieldData,between(as.Date(Fdt_Date_Time), dateRangeFilter[1], dateRangeFilter[2]) )
  
stationAnalyteDataUserFilter <- filter(stationAnalyteData, between(as.Date(Fdt_Date_Time), dateRangeFilter[1], dateRangeFilter[2]) )  %>% 
  filter(Ana_Sam_Mrs_Container_Id_Desc %in% repFilter) %>% 
  filter(! Ana_Com_Code %in% labCodesDropped)


### Organize field and analyte info into prettier table

stationFieldAnalyte1 <- stationFieldAnalyteDataPretty(stationAnalyteDataUserFilter, stationFieldDataUserFilter, averageResults = TRUE)

#Ana_Sam_Mrs_Lcc_Parm_Group_Cd,
# report out where results averaged

z <- stationFieldAnalyte1 %>% # drop all empty columns ( this method longer but handles dttm issues)
  map(~.x) %>%
  discard(~all(is.na(.x))) %>%
  map_df(~.x)
if("Associated Analyte Records" %in% names(z)){ # highlight rows where field data duplicated bc multiple analytes on same datetime
  datatable(z, rownames = F, escape= F, extensions = 'Buttons',
            options = list(dom = 'Bift', scrollX = TRUE, scrollY = '350px',
                           pageLength = nrow(z), 
                           buttons=list('copy',#list(extend='excel',filename=paste0('CEDSFieldAnalyteData',input$station, Sys.Date())),
                                        'colvis')), selection = 'none') %>% 
    formatStyle("Associated Analyte Records", target = 'row', backgroundColor = styleEqual(c(1,2,3,4,5,6,7,8,9,10), 
                                                                                           c(NA, 'yellow','yellow','yellow','yellow','yellow','yellow','yellow','yellow','yellow')))
}else {datatable(z, rownames = F, escape= F, extensions = 'Buttons',
                 options = list(dom = 'Bift', scrollX = TRUE, scrollY = '350px',
                                pageLength = nrow(z), 
                                buttons=list('copy',list(extend='excel',filename=paste0('CEDSFieldAnalyteData',station, Sys.Date())),
                                             'colvis')), selection = 'none') }
          
              
              
              

# Collection info
uniqueCollector(stationFieldAnalyte1)

# Sample Codes Summary
uniqueSampleCodes(stationFieldAnalyte1)

# Special Comments
uniqueComments(stationFieldAnalyte1)

# Basic Dataset people will actually use
basicData <- basicSummary(stationFieldAnalyte1)

# parameter graph
parameterPlotly(basicData, 'Dissolved Oxygen', unitData, WQSlookup) #unique(filter(unitData, !is.na(AltName))$AltName)
parameterPlotly(basicData, "Secci Depth", unitData, WQSlookup) 

names(basicData)[names(basicData) %in% unitData$AltName]
dplyr::select(basicData, parameterPlot = !! parameter) %>% # rename clutch for nse
  filter(!is.na(parameterPlot)) 

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

ggplot(cdfsubset, aes(x=Value,y=Estimate.P)) + 
  labs(x=paste(prettyParameterName,unique(cdfsubset$Units),sep=" "),y="Percentile") +
  ggtitle(paste(subpopulation,'\n', prettyParameterName,"\n Percentile Graph ( n = ",m,")",sep=" "))



# BSA tool data output
BSAtooloutput <- stationFieldAnalyteDataPretty(stationAnalyteDataUserFilter, stationFieldDataUserFilter, averageResults = FALSE) %>%
  dplyr::select(one_of(c('Fdt_Sta_Id', 'Fdt_Date_Time', 'Fdt_Temp_Celcius', 'Fdt_Field_Ph', 'Fdt_Do_Probe', 'Fdt_Do_Optical', 'Fdt_Do_Winkler', 
                'Fdt_Specific_Conductance','NITROGEN, TOTAL (MG/L AS N)', 'PHOSPHORUS, TOTAL (MG/L AS P)', 'SULFATE, TOTAL (MG/L AS SO4)',
                'TDS RESIDUE,TOTAL FILTRABLE (DRIED AT 180C),MG/L', 'SULFATE, TOTAL (MG/L AS SO4)','CHLORIDE,TOTAL IN WATER MG/L',
                'SODIUM, DISSOLVED (MG/L AS NA)'))) %>% 
  mutate(`Dissolved Oxygen` = case_when(!is.na(Fdt_Do_Probe) ~ Fdt_Do_Probe,
                                        !is.na(Fdt_Do_Optical) ~ Fdt_Do_Optical,
                                        !is.na(Fdt_Do_Winkler) ~ Fdt_Do_Winkler,
                                        TRUE ~ as.numeric(NA))) %>%
  left_join(dplyr::select(stationInfo_sf, STATION_ID, Latitude, Longitude) %>% distinct(STATION_ID), by = c('Fdt_Sta_Id'='STATION_ID')) %>% 
  rename("StationID"=  'Fdt_Sta_Id',
         "CollectionDateTime" = "Fdt_Date_Time") %>% 
  dplyr::select(StationID, CollectionDateTime, Latitude, Longitude,  Fdt_Temp_Celcius)

## stopped here bc it's a mess if you don't have exactly the data you need in the filtered dataset (use 2-jks023.61 matching the template dates (2011)
##  for example... sodium is missing)
# also waiting on Roger about lab methods that make the most sense

