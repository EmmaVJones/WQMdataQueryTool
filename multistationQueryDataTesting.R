source('global.R')



# Critical Data
# goes in server

assessmentRegions <- st_read( 'data/GIS/AssessmentRegions_simple.shp')
ecoregion <- st_read('data/GIS/vaECOREGIONlevel3__proj84.shp')
ecoregionLevel4 <- st_read('data/GIS/vaECOREGIONlevel4__proj84.shp')
county <- st_read('data/GIS/VACountyBoundaries.shp')
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
WQM_Stations_Spatial <- pin_get("ejones/WQM-Stations-Spatial", board = "rsconnect") %>%
  rename("Basin_Name" = "Basin_Code") # can't have same name different case when using sqldf
WQM_Stations_Full <- st_as_sf(pin_get('ejones/WQM-Station-Full', board = 'rsconnect'))
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

# analyte options 
Wqm_Parameter_Grp_Cds_Codes_Wqm_View <- pool %>% tbl(in_schema("wqm", 'Wqm_Parameter_Grp_Cds_Codes_Wqm_View')) %>% 
  filter(Pg_Parm_Name != "STORET STORAGE TRANSACTION DATE YR/MO/DAY") %>% 
  distinct(Pg_Parm_Name) %>% arrange(Pg_Parm_Name) %>% as_tibble() %>% drop_na()
labMediaCodes <-  pool %>% tbl(in_schema("wqm", "Wqm_Lab_Catalogs_View")) %>% as_tibble()
programCodes <- pool %>% tbl(in_schema("wqm", "Wqm_Survey_Pgm_Cds_Codes_Wqm_View")) %>% as_tibble()



# user inputs
#queryType <- 'Manually Specify Stations'#'Spatial Filters' #Interactive Selection 

runIDfilter <-""# NULL isn't what happens in the app# "W19%"#
labGroupCodeFilter <- NULL#'TNUTL'
programCodeFilter <- NULL#'HF'#c('AW','TR')
countyFilter <- NULL#"Roanoke City"#
ecoregionFilter <- NULL#"Middle Atlantic Coastal Plain"#NULL#"Blue Ridge"#unique(ecoregion$US_L3NAME)
ecoregionLevel4Filter <- NULL
dateRange_multistation <- c(as.Date('2016-01-01'), as.Date('2017-12-31'))#as.Date(Sys.Date()- 7))
## pull based on parameter
analyte_Filter <- NULL#
  c('SODIUM (NA), ATM DEP, WET, DISS, MG/L', 'SODIUM, DISSOLVED (MG/L AS NA)', 'SODIUM, TOTAL (MG/L AS NA)', 'SODIUM-TOTAL  UG/L (AS NA)')
# 'FECAL COLIFORM,7 HR,M-7HR FC MED MF,41.5C,#/100ML', 'FECAL COLIFORM,A-1 MOD,WATER,44.5C,24HR MPN/100ML', 
# 'FECAL COLIFORM,MEMBR FILTER,M-FC BROTH,44.5 C', 'FECAL COLIFORM,MPN,BORIC ACID LACTOSE BR,43C,48HR', 
# 'FECAL COLIFORM,MPN,EC MED,44.5C (TUBE 31614)', 'FECAL COLIFORM,MPN,TUBE CONFIGURATION',
# 'HARDNESS, CA MG CALCULATED (MG/L AS CACO3)', 'HARDNESS, CA MG CALCULATED (MG/L AS CACO3) AS DISSOLVED', 
# 'HARDNESS, CARBONATE (MG/L AS CACO3)', 'HARDNESS, TOTAL (MG/L AS CACO3)')

  # %>% 
  #   left_join(dplyr::select(labMediaCodes, Lc_Parm_Group_Code, Lc_Description, Act_Media_Desc), by = c('Pg_Parm_Group_Code' = 'Lc_Parm_Group_Code'))

  
# manually specify troubleshooting
manualSelection1 <- '1BDUR000.11'#c('2-JKS028.69', '2-JKS023.61')#4AROA000.00'#c('1BSMT001.53','1BSMT006.62','1BSMT009.08')#1AFOU002.06')
#WQM_Stations_Filter <- filter(WQM_Stations_Spatial, StationID %in% as.character(manualSelection1))  
WQM_Stations_Filter <- WQM_Stations_Filter_function('Manually Specify Stations (takes a few seconds for the station text box to appear)', 
                                                    pool, WQM_Stations_Spatial, VAHU6Filter = NULL, subbasinFilter = NULL, assessmentRegionFilter = NULL,
                                                    ecoregionFilter = ecoregionFilter, ecoregionLevel4Filter = ecoregionLevel4Filter, countyFilter = countyFilter, dateRange_multistation, analyte_Filter, 
                                                    programCodeFilter = programCodeFilter, labGroupCodeFilter = labGroupCodeFilter, runIDfilter = runIDfilter,
                                                    manualSelection = manualSelection1, wildcardSelection = NULL)

# wildcard troubleshooting
wildcardText1 <- '4aroa2%'#'2-JKS02%'#'4aroa%'#'2-JKS02%'#'3-RPP10%'
# wildcardResults <- sqldf(paste0('SELECT * FROM WQM_Stations_Spatial WHERE StationID like "',
#                                 wildcardText1, '"'))
WQM_Stations_Filter <- WQM_Stations_Filter_function('Wildcard Selection', 
                                                    pool, WQM_Stations_Spatial, VAHU6Filter = NULL, subbasinFilter = NULL, assessmentRegionFilter = NULL,
                                                    ecoregionFilter = ecoregionFilter, ecoregionLevel4Filter = ecoregionLevel4Filter, countyFilter, dateRange_multistation, analyte_Filter= NULL, 
                                                    programCodeFilter = programCodeFilter, labGroupCodeFilter = labGroupCodeFilter, runIDfilter = runIDfilter, 
                                                    manualSelection = NULL, wildcardSelection = wildcardText1)



# Spatial filters troubleshooting
### begin
assessmentRegionFilter <- c("BRRO")#c("BRRO")#NULL#c("PRO")#unique(subbasins$ASSESS_REG)
subbasinFilter <- "New"#"James-Upper"#NULL#"Appomattox"#"James-Upper"# c("James-Middle",'Potomac-Lower')#NULL# c("James River - Middle",'Potomac River')#NULL#"James River - Lower"
# filter(WQM_Stations_Spatial, ASSESS_REG %in% assessmentRegionFilter) %>%
#   distinct(Basin_Name) %>%  pull()
VAHU6Filter <- NULL#'JU11'#NULL 
# filter(WQM_Stations_Spatial, ASSESS_REG %in% assessmentRegionFilter) %>%
#   filter(Basin_Name %in% subbasinFilter) %>% 
#   distinct(VAHU6) %>%  pull()



WQM_Stations_Filter <- WQM_Stations_Filter_function('Spatial Filters', pool, WQM_Stations_Spatial, VAHU6Filter, subbasinFilter, assessmentRegionFilter,
                                         ecoregionFilter, ecoregionLevel4Filter, countyFilter, dateRange_multistation, analyte_Filter, 
                                         programCodeFilter = programCodeFilter, labGroupCodeFilter = labGroupCodeFilter,runIDfilter= runIDfilter,
                                         manualSelection = NULL, wildcardSelection = NULL)

 
### end filter options





# Pull as many details about station from REST service (if available). Work around provided in case station isn't on REST server
## Pull station info from REST service
WQM_Station_Full_REST <- filter(WQM_Stations_Full, WQM_STA_ID %in% WQM_Stations_Filter$StationID)
#WQM_Station_Full_REST <- WQM_Station_Full_REST_request(pool, station, subbasinVAHU6crosswalk, subbasins, ecoregion)

# Quick Station Sampling Information
stationInfoSampleMetrics <- stationSummarySampingMetrics(WQM_Station_Full_REST, 'multi')

## Pull CEDS Station Information 
# if(nrow(WQM_Stations_Filter) > 0){
#   stationInfoFin <- stationInfoConsolidated(pool, WQM_Stations_Filter$StationID, WQM_Station_Full_REST, WQM_Stations_Spatial)
# }



## For map stuff
multistationInfoFin <- left_join(Wqm_Stations_View %>%  # need to repull data instead of calling stationInfo bc app crashes
                                   filter(Sta_Id %in% WQM_Stations_Filter$StationID) %>%
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
                                 ########filter(WQM_Station_View, Sta_Id %in% toupper(input$station)), # need to filter instead of calling stationInfo bc app crashes
                                 dplyr::select(WQM_Station_Full, 
                                               STATION_ID, Latitude, Longitude, WQM_STA_STRAHER_ORDER, EPA_ECO_US_L3CODE,
                                               EPA_ECO_US_L3NAME,EPA_ECO_US_L4CODE, EPA_ECO_US_L4NAME, BASINS_HUC_8_NAME, 
                                               BASINS_VAHU6, WQS_WATER_NAME, WQS_SEC, WQS_CLASS, 
                                               WQS_SPSTDS, WQS_PWS, WQS_TROUT, WQS_TIER_III, WQM_YRS_YEAR, WQM_YRS_SPG_CODE),
                                 by = c('Sta_Id' = 'STATION_ID')) %>%
  left_join(dplyr::select(WQM_Stations_Spatial, StationID, ASSESS_REG, CountyCityName), by = c('Sta_Id' = 'StationID')) %>% 
  dplyr::select(Sta_Id, Sta_Desc, `CEDS Station View Link`, `DEQ GIS Web App Link`, Latitude, Longitude, WQM_STA_STRAHER_ORDER, 
                ASSESS_REG, CountyCityName, EPA_ECO_US_L3CODE,EPA_ECO_US_L3NAME, EPA_ECO_US_L4CODE, EPA_ECO_US_L4NAME, 
                BASINS_HUC_8_NAME, BASINS_VAHU6, WQS_WATER_NAME, WQS_SEC, WQS_CLASS, 
                WQS_SPSTDS, WQS_PWS, WQS_TROUT, WQS_TIER_III, everything()) 



# Basic station info for conventionals
multiStationInfo <- pool %>% tbl(in_schema("wqm",  "Wqm_Stations_View")) %>%
  filter(Sta_Id %in% !! toupper(multistationInfoFin$Sta_Id)) %>%
  as_tibble()
multiStationGIS_View <-  pool %>% tbl(in_schema("wqm",  "Wqm_Sta_GIS_View")) %>%
  filter(Station_Id %in% !! toupper(multistationInfoFin$Sta_Id)) %>%
  as_tibble()


# map stuff
# # Empty station user selection to start with
# selectedSites <- NULL 
# 
# 
# # color palette for assessment polygons
# pal <- colorFactor(
#   palette = topo.colors(7),
#   domain = assessmentRegions$ASSESS_REG)
# pal2 <- colorFactor(
#   palette = rainbow(7),
#   domain = ecoregion$US_L3NAME)
# 
# assessmentLayerFilter <- filter(assessmentLayer, VAHU6 %in% WQM_Stations_Filter$VAHU6)
# 
# 
# CreateWebMap(maps = c("Topo","Imagery","Hydrography"), collapsed = TRUE,
#              options= leafletOptions(zoomControl = TRUE,minZoom = 3, maxZoom = 20,
#                                      preferCanvas = TRUE)) %>%
#   setView(-79.1, 37.7, zoom=7)  %>%
#   # for when going to points
#   flyToBounds(lng1 = min(WQM_Stations_Filter$Longitude)+0.01,
#             lat1 = min(WQM_Stations_Filter$Latitude)+0.01,
#             lng2 = max(WQM_Stations_Filter$Longitude)+0.01,
#             lat2 = max(WQM_Stations_Filter$Latitude)+0.01) %>%
#   addPolygons(data= ecoregion,  color = 'gray', weight = 1,
#               fillColor= ~pal2(ecoregion$US_L3NAME), fillOpacity = 0.5,stroke=0.1,
#               group="Level III Ecoregions",label = ~US_L3NAME) %>% hideGroup('Level III Ecoregions') %>%
#   addPolygons(data= assessmentRegions,  color = 'black', weight = 1,
#               fillColor= ~pal(assessmentRegions$ASSESS_REG), fillOpacity = 0.5,stroke=0.1,
#               group="Assessment Regions", label = ~ASSESS_REG) %>% hideGroup('Assessment Regions') %>%
# 
# 
#   addCircleMarkers(data = WQM_Stations_Filter,
#                    color='blue', fillColor='gray', radius = 4,
#                    fillOpacity = 0.5,opacity=0.8,weight = 2,stroke=T, group="Spatial Filter Station(s)",
#                    label = ~StationID, layerId = ~StationID,
#                    popup = leafpop::popupTable(WQM_Stations_Filter, zcol=c('StationID'))) %>%
#   {if(nrow(assessmentLayerFilter) > 0)
#     addPolygons(., data= assessmentLayerFilter,  color = 'black', weight = 1,
#                 fillColor= 'gray', fillOpacity = 0.5,stroke=0.1,
#                 group="VAHU6", label = ~VAHU6) %>% hideGroup('VAHU6')
#     else . } %>%
# 
#   inlmisc::AddHomeButton(raster::extent(-83.89, -74.80, 36.54, 39.98), position = "topleft") %>%
#   addDrawToolbar(
#     targetGroup='Selected',
#     polylineOptions=FALSE,
#     markerOptions = FALSE,
#     polygonOptions = drawPolygonOptions(shapeOptions=drawShapeOptions(fillOpacity = 0, color = 'white', weight = 3)),
#     rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0, color = 'white', weight = 3)),
#     circleOptions = FALSE,
#     circleMarkerOptions = FALSE,
#     editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions())) %>%
#   addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
#                    overlayGroups = c("Spatial Filter Station(s)", "VAHU6","Level III Ecoregions", 'Assessment Regions'),
#                    options=layersControlOptions(collapsed=T),
#                    position='topleft')




## Actual data querying bit

### Field Data Information

multistationFieldData <- pool %>% tbl(in_schema("wqm", "Wqm_Field_Data_View")) %>%
  filter(Fdt_Sta_Id %in% !! WQM_Stations_Filter$StationID &
           between(as.Date(Fdt_Date_Time), !! dateRange_multistation[1], !! dateRange_multistation[2])) %>% # & # x >= left & x <= right
           #Ssc_Description != "INVALID DATA SET QUALITY ASSURANCE FAILURE") %>%  # don't drop QA failure on SQL part bc also drops any is.na(Ssc_Description)
  as_tibble() %>% 
  filter(Ssc_Description != "INVALID DATA SET QUALITY ASSURANCE FAILURE")


### Analyte information

multistationAnalyteData <- pool %>% tbl(in_schema("wqm", "Wqm_Analytes_View")) %>%
  filter(Ana_Sam_Fdt_Id %in% !! multistationFieldData$Fdt_Id &
           #between(as.Date(Ana_Received_Date), !! dateRange_multistation[1], !! dateRange_multistation[2]) & # x >= left & x <= right
           Pg_Parm_Name != "STORET STORAGE TRANSACTION DATE YR/MO/DAY") %>% 
  as_tibble() %>%
  left_join(dplyr::select(multistationFieldData, Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time), by = c("Ana_Sam_Fdt_Id" = "Fdt_Id"))



# User filters
multistationDateRangeFilter <-  c(as.Date('2016-01-01'),as.Date('2016-12-31')) #as.Date(Sys.Date()))#as.Date('2011-01-01'), as.Date('2011-12-31'))#c(as.Date('2015-02-24'), as.Date(Sys.Date()))#
multistationLabCodesDropped <- c('QF')#sort(unique(stationAnalyteData$Ana_Com_Code))
multistationRepFilter <- c('R')
multistationDepthFilter <- T

multistationFieldDataUserFilter <- filter(multistationFieldData, between(as.Date(Fdt_Date_Time), multistationDateRangeFilter[1], multistationDateRangeFilter[2]) ) %>%
  {if(multistationDepthFilter == TRUE)
    filter(., Fdt_Depth <= 0.3)
    else . }

multistationAnalyteDataUserFilter <- filter(multistationAnalyteData, between(as.Date(Fdt_Date_Time), multistationDateRangeFilter[1], multistationDateRangeFilter[2]) )  %>% 
  filter(Ana_Sam_Mrs_Container_Id_Desc %in% multistationRepFilter) %>% 
  filter(! Ana_Com_Code %in% multistationLabCodesDropped)

# reactive_objects$multistationAnalyteDataUserFilter <- filter(reactive_objects$multistationAnalyteData, between(as.Date(Fdt_Date_Time), input$multistationDateRangeFilter[1], input$multistationDateRangeFilter[2]) )  %>%
#   filter(Ana_Sam_Mrs_Container_Id_Desc %in% input$multistationRepFilter) %>%
#   filter(! Ana_Com_Code %in% input$multistationLabCodesDropped)})




### Organize field and analyte info into prettier table

multistationFieldAnalyte1 <- stationFieldAnalyteDataPretty(multistationAnalyteDataUserFilter, multistationFieldDataUserFilter, averageResults = FALSE)

#Ana_Sam_Mrs_Lcc_Parm_Group_Cd,
# report out where results averaged

z <- multistationFieldAnalyte1 %>% # drop all empty columns ( this method longer but handles dttm issues)
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
uniqueCollector(multistationFieldAnalyte1)

# Sample Codes Summary
uniqueSampleCodes(multistationFieldAnalyte1)

# Special Comments
uniqueComments(multistationFieldAnalyte1)


# conventionals dataset to correctly consolidate data
multistationOrganizeData <- conventionalsSummary(conventionals= pin_get("conventionals2022IRfinalWithSecchi", board = "rsconnect")[0,],
                                          stationFieldDataUserFilter= multistationFieldDataUserFilter, 
                                          stationAnalyteDataUserFilter = multistationAnalyteDataUserFilter, 
                                          stationInfo = multiStationInfo,
                                          stationGIS_View = multiStationGIS_View,
                                          dropCodes = c('QF', multistationLabCodesDropped),
                                          assessmentUse = F) #%>% 
 # arrange(FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH)



# Basic Dataset people will actually use
basicData <- basicSummaryConventionals(multistationOrganizeData$More, multistationFieldAnalyte1)
####################################################################################################################################
### old method that relied on name matching vs actual storet codes and standardized data consolidation steps (conventionals method)
### Basic Dataset people will actually use
###basicData <- basicSummary(multistationFieldAnalyte1)
####################################################################################################################################

# parameter graph
parameterPlotly(basicData, 'Dissolved Oxygen', unitData, WQSlookup, addBSAcolors = T) #unique(filter(unitData, !is.na(AltName))$AltName)
parameterPlotly(basicData, "Specific Conductance", unitData, WQSlookup, addBSAcolors = T) 





basicLoessPlotFunction(basicData, 'pH')

z <- dplyr::select(basicData, parameterPlot = !! parameter) %>% # rename clutch for nse
  filter(!is.na(parameterPlot)) 


# %>% style(hoverinfo="text",
#               text=~paste(sep="<br>",
#                           paste("StationID: ",StationID),
#                           paste("Sample Date: ",`Collection Date`),
#                           paste("Depth: ",Depth, "m"),
#                           paste(parameter, ": ",Measure," (mg/L)")) )
# 

fig <- plotly_build(fig)



# Parameter graph with loess smoother
loessTest <- dplyr::select(basicData, DO = `Dissolved Oxygen`, CD=  `Collection Date`) %>% 
  mutate(CD1 = as.Date(CD))
m <- loess(DO ~ CD1, data = as.data.frame(loessTest))


plot_ly(mtcars, x = ~disp, color = I("black"))
fig <- fig %>% add_markers(y = ~mpg, text = rownames(mtcars), showlegend = FALSE)
fig <- fig %>% add_lines(y = ~fitted(loess(mpg ~ disp)),
                         line = list(color = 'rgba(7, 164, 181, 1)'),
                         name = "Loess Smoother")
fig <- fig %>% add_ribbons(data = augment(m),
                           ymin = ~.fitted - 1.96 * .se.fit,
                           ymax = ~.fitted + 1.96 * .se.fit,
                           line = list(color = 'rgba(7, 164, 181, 0.05)'),
                           fillcolor = 'rgba(7, 164, 181, 0.2)',
                           name = "Standard Error")
fig <- fig %>% layout(xaxis = list(title = 'Displacement (cu.in.)'),
                      yaxis = list(title = 'Miles/(US) gallon'),
                      legend = list(x = 0.80, y = 0.90))

fig



# individual parameter boxplot
parameter <- 'pH'
addJitter <- T

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
parameterBoxplotFunction(basicData, 'Dissolved Oxygen', unitData, WQSlookup, addJitter = F)
     