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

WQSlookup <- pin_get("WQSlookup-withStandards",  board = "rsconnect")
# while server is down
#readRDS('C:/HardDriveBackup/R/pins11182020/ejones/WQSlookup-withStandards.RDS')



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
#stationFieldAnalyte2 <- stationFieldAnalyteDataPretty(filter(stationAnalyteData, between(as.Date(Fdt_Date_Time), dateRangeFilter[1], dateRangeFilter[2]) ), 
#                                                      filter(stationFieldData, between(as.Date(Fdt_Date_Time), dateRangeFilter[1], dateRangeFilter[2]) ), 
#                                                      repFilter = c('R','S1'),
#                                                      averageResults = FALSE)
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

# Basic Dataset people will actually use
basicData <- basicSummary(stationFieldAnalyte1)

# parameter graph
parameterPlotly(basicData, 'Temperature', unitData, WQSlookup) 


# Compare to Prob Estimates section

# for now only keep estimates we can crosswalk to WQM data
#probEst <- filter(probEst, Indicator %in% filter(unitData, AltName %in% names(basicData))$Indicator) 

# Find central tendency of each parameter based on filtered window
probComparison <- centralTendencies(basicData)  
  
# User chooses an indicator
indicatorOptions <- probIndicators$AltName[1]
parameter <- filter(probIndicators, Parameter %in% indicatorOptions)$Parameter

# Compare median of selected indicator to prob estimates
percentiles <- percentileTable(probEst, probComparison, 'DO', 'James Basin', "Blue Ridge Mountains" , NA)

# Plot mean and median
cdfplot(probEst, 'Dissolved Oxygen', 'DO',  "Blue Ridge Mountains", percentiles, DOsettingsCDF)
  

























# Return percentile 
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




































cdfdata <- probEst
statsTable<- probComparison
parameter <- 'DO'
userBasin <- 'James Basin'
userEco <- "Blue Ridge Mountains" 

out <- statsTable %>%
  dplyr::select(StationID, Statistic, starts_with(parameter))# %>%
  #group_by(StationID) %>%
  #pivot_wider(names_from = 'Statistic', values_from = starts_with(parameter))
  #pivot_longer(cols = starts_with(parameter), names_to = 'Statistic', values_to = 'Value')
va <- data.frame(filter(cdfdata,Subpopulation=='Virginia',Indicator==parameter)%>%select(Value,Estimate.P)) # needs to be df for vlookup to work


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
  
                                                     
va2 <- data.frame(Subpopulation = 'Virginia',
                  Average = vlookup(filter(out, str_detect(Statistic, 'Mean')) %>% dplyr::select(starts_with(parameter)) %>% pull(), va, 2, range=TRUE),
                  Median = vlookup(filter(out, str_detect(Statistic, 'Median')) %>% dplyr::select(starts_with(parameter)) %>% pull(), va, 2, range=TRUE)) 
basin2 <- data.frame(Subpopulation = userBasin,
                  Average = vlookup(filter(out, str_detect(Statistic, 'Mean')) %>% dplyr::select(starts_with(parameter)) %>% pull(), basin, 2, range=TRUE),
                  Median = vlookup(filter(out, str_detect(Statistic, 'Median')) %>% dplyr::select(starts_with(parameter)) %>% pull(), basin, 2, range=TRUE))
eco2 <- data.frame(Subpopulation = userEco,
                  Average = vlookup(filter(out, str_detect(Statistic, 'Mean')) %>% dplyr::select(starts_with(parameter)) %>% pull(), eco, 2, range=TRUE),
                  Median = vlookup(filter(out, str_detect(Statistic, 'Median')) %>% dplyr::select(starts_with(parameter)) %>% pull(), eco, 2, range=TRUE))


percentileTable(statsTable = probComparison, parameter = "pH",userBasin = 'Roanoke Basin',userEco = 'Blue Ridge Mountains',userOrder = 'Fourth',stationName= '2-JKS023.61')
#percentileTable(stats(),"pH",input$Basin,input$Ecoregion,input$StreamOrder,unique(inputFile()$StationID))