source('global.R')



# Critical Data
# goes in server

assessmentRegions <- st_read( 'data/GIS/AssessmentRegions_simple.shp')
ecoregion <- st_read('data/GIS/vaECOREGIONlevel3__proj84.shp')
assessmentLayer <- st_read('data/GIS/AssessmentRegions_VA84_basins.shp') %>%
  st_transform( st_crs(4326)) 
subbasins <- st_read('data/GIS/DEQ_VAHUSB_subbasins_EVJ.shp') %>%
  rename('SUBBASIN' = 'SUBBASIN_1') %>%
  mutate(SUBBASIN = ifelse(is.na(SUBBASIN), as.character(BASIN_NAME), as.character(SUBBASIN)))
subbasinVAHU6crosswalk <- read_csv('data/basinAssessmentReg_clb_EVJ.csv') %>%
  filter(!is.na(SubbasinVAHU6code)) %>%
  mutate(SUBBASIN = ifelse(is.na(SUBBASIN), BASIN_NAME, SUBBASIN)) %>%
  dplyr::select(SUBBASIN, SubbasinVAHU6code)




## Pull one station

station <- '2-JKS023.61'
dateRange <- c(as.Date('2015-01-01'), as.Date(Sys.Date()))
#dateRangeFilter <- c(as.Date(Sys.Date())-years(3), as.Date(Sys.Date()))
dateRangeFilter <- c(as.Date('2015-02-24'), as.Date(Sys.Date()))


### Geospatial Information

# Basic station info and make sure station in CEDS
stationInfo <- pool %>% tbl( "Wqm_Stations_View") %>%
  filter(Sta_Id %in% !! toupper(station)) %>%
  as_tibble()

# Pull as many details about station from REST service (if available). Work around provided in case station isn't on REST server
## Pull station info from REST service
WQM_Station_Full_REST <- WQM_Station_Full_REST_request(pool, '2-JKS023.61')

## Pull CEDS Station Information 
stationInfoFin <- stationInfoConsolidated(pool, station, WQM_Station_Full_REST)

#extra step, kinda seems unnecessary
stationInfo_sf <- WQM_Station_Full_REST

# Quick Station Sampling Information
stationInfoSampleMetrics <- stationSummarySampingMetrics(stationInfo_sf)


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
                                                      averageResults = TRUE)
stationFieldAnalyte2 <- stationFieldAnalyteDataPretty(filter(stationAnalyteData, between(as.Date(Fdt_Date_Time), dateRangeFilter[1], dateRangeFilter[2]) ), 
                                                      filter(stationFieldData, between(as.Date(Fdt_Date_Time), dateRangeFilter[1], dateRangeFilter[2]) ), 
                                                      repFilter = c('R','S1'),
                                                      averageResults = FALSE)
View(
stationFieldAnalyte1 %>%
  map(~.x) %>%
  discard(~all(is.na(.x))) %>%
  map_df(~.x) )


# Collection info
uniqueCollector(stationFieldAnalyte1)

# Sample Codes Summary
uniqueSampleCodes(stationFieldAnalyte1)

# Special Comments
uniqueComments(stationFieldAnalyte1)


basicData <- basicSummary(stationFieldAnalyte1)
parameter <- 'Temperature'

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

parameterPlotly(basicData, 'Temperature', unitData, WQSlookup) 





