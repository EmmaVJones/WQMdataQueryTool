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
median_n <- list(
  median = ~signif(median(.x, na.rm = TRUE), digits = 2), 
  mean = ~signif(mean(.x, na.rm = TRUE), digits = 2), 
  n = ~sum(ifelse(!is.na(.x), 1,0))#, na.rm = TRUE)
)

centralTendencies <- function(basicData){
  dplyr::select(basicData, StationID, probIndicators$AltName) %>%
    group_by(StationID) %>%
    summarise(across(where(is.numeric), median_n)) }

probComparison <- centralTendencies(basicData)  
  
# User chooses an indicator
indicatorOptions <- probIndicators$AltName[1]
indicator <- filter(probIndicators, Indicator %in% indicatorOptions)$Indicator

# Compare median of selected indicator to prob estimates
subFunction <- function(cdftable,parameter,userInput){
  return(filter(cdftable,Subpopulation%in%userInput & Indicator%in%parameter))
}
View(subFunction(probEst, parameter = indicator, 'Virginia'))

subFunction2 <- function(cdftable,userValue){
  return(filter(cdftable,Estimate.P%in%userValue))
}

subFunction2(subFunction(probEst, parameter = indicator, 'Virginia'), 50)

vlookup(probComparison$indicator,filter(probEst, Indicator),2,range=TRUE)



# CDF plot function
cdfplot <- function(prettyParameterName,parameter,indicator,dataset,CDFsettings){
  cdfsubset <- subFunction(cdfdata,parameter,indicator)
  avg1 <- as.numeric(filter(dataset,Statistic==indicator)[,2])
  avg <- subFunction2(cdfsubset,avg1)
  med1 <- as.numeric(filter(dataset,Statistic==indicator)[,3]) 
  med <- subFunction2(cdfsubset,med1)
  m <- max(cdfsubset$NResp)
  p1 <- ggplot(cdfsubset, aes(x=Value,y=Estimate.P)) + 
    labs(x=paste(prettyParameterName,unique(cdfsubset$units),sep=" "),y="Percentile") +
    ggtitle(paste(indicator,prettyParameterName,"Percentile Graph ( n=",m,")",sep=" ")) + 
    theme(plot.title = element_text(hjust=0.5,face='bold',size=15)) +
    theme(axis.title = element_text(face='bold',size=12))+
    
    CDFsettings  +
    
    geom_point() +
    geom_point(data=avg,color='orange',size=4) + geom_text(data=avg,label='Average',hjust=1.2) +
    geom_point(data=med,color='gray',size=4)+ geom_text(data=med,label='Median',hjust=1.2) 
  return(p1)
}




# Return percentile 
percentileTable <- function(statsTable,parameter,userBasin,userEco,userOrder,stationName){
  out <- statsTable%>%select_("Statistic",parameter)%>% spread_("Statistic",parameter)%>%
    mutate(Statistic=stationName)%>%select(Statistic,everything())
  va <- data.frame(filter(cdfdata,Subpopulation=='Virginia',Indicator==parameter)%>%select(Value,Estimate.P)) # needs to be df for vlookup to work
  basin <- data.frame(filter(cdfdata,Subpopulation==userBasin,Indicator==parameter)%>%select(Value,Estimate.P)) # needs to be df for vlookup to work
  eco <- data.frame(filter(cdfdata,Subpopulation==userEco,Indicator==parameter)%>%select(Value,Estimate.P)) # needs to be df for vlookup to work
  order <- data.frame(filter(cdfdata,Subpopulation==userOrder,Indicator==parameter)%>%select(Value,Estimate.P)) # needs to be df for vlookup to work
  va2 <- data.frame(Statistic='Virginia',Average=vlookup(out$Average,va,2,range=TRUE),Median=vlookup(out$Median,va,2,range=TRUE))
  basin2 <- data.frame(Statistic=userBasin,Average=vlookup(out$Average,basin,2,TRUE),Median=vlookup(out$Median,basin,2,TRUE))
  eco2 <- data.frame(Statistic=userEco,Average=vlookup(out$Average,eco,2,TRUE),Median=vlookup(out$Median,eco,2,TRUE))
  order2 <- data.frame(Statistic=userOrder,Average=vlookup(out$Average,order,2,TRUE),Median=vlookup(out$Median,order,2,TRUE))
  out_final <- rbind(out,va2,basin2,eco2,order2)
  return(out_final)
}
cdfdata <- probEst
out <- filter()

out <- statsTable %>%
  dplyr::select(StationID, starts_with(parameter)) %>%
  group_by(StationID) %>%
  pivot_longer(cols = starts_with(parameter), names_to = 'Statistic', values_to = 'Value')
    

va2 <- data.frame(Statistic='Virginia',
                  Average=vlookup(filter(out, str_detect(Statistic, 'mean')) %>% pull(Value), va, 2, range=TRUE),
                  Median=vlookup(filter(out, str_detect(Statistic, 'median')) %>% pull(Value), va, 2, range=TRUE))
basin2 <- data.frame(Statistic=userBasin,
                  Average=vlookup(filter(out, str_detect(Statistic, 'mean')) %>% pull(Value), basin, 2, range=TRUE),
                  Median=vlookup(filter(out, str_detect(Statistic, 'median')) %>% pull(Value), basin, 2, range=TRUE))
eco2 <- data.frame(Statistic=userEco,
                  Average=vlookup(filter(out, str_detect(Statistic, 'mean')) %>% pull(Value), eco, 2, range=TRUE),
                  Median=vlookup(filter(out, str_detect(Statistic, 'median')) %>% pull(Value), eco, 2, range=TRUE))


percentileTable(statsTable = probComparison, parameter = "pH",userBasin = 'Roanoke Basin',userEco = 'Blue Ridge Mountains',userOrder = 'Fourth',stationName= '2-JKS023.61')
#percentileTable(stats(),"pH",input$Basin,input$Ecoregion,input$StreamOrder,unique(inputFile()$StationID))