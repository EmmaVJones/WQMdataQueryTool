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
Wqm_Parameter_Grp_Cds_Codes_Wqm_View <- pool %>% tbl('Wqm_Parameter_Grp_Cds_Codes_Wqm_View') %>% 
  filter(Pg_Parm_Name != "STORET STORAGE TRANSACTION DATE YR/MO/DAY") %>% 
  distinct(Pg_Parm_Name) %>% arrange(Pg_Parm_Name) %>% as_tibble() %>% drop_na()



# user inputs
queryType <- 'Manually Specify Stations'#'Spatial Filters' #Interactive Selection 

ecoregionFilter <- NULL#"Middle Atlantic Coastal Plain"#NULL#"Blue Ridge"#unique(ecoregion$US_L3NAME)
dateRange_multistation <- c(as.Date('1970-01-01'), as.Date(Sys.Date()- 7))
## pull based on parameter
analyte_Filter <- #NULL#
  c('SODIUM (NA), ATM DEP, WET, DISS, MG/L', 'SODIUM, DISSOLVED (MG/L AS NA)', 'SODIUM, TOTAL (MG/L AS NA)', 'SODIUM-TOTAL  UG/L (AS NA)')
# 'FECAL COLIFORM,7 HR,M-7HR FC MED MF,41.5C,#/100ML', 'FECAL COLIFORM,A-1 MOD,WATER,44.5C,24HR MPN/100ML', 
# 'FECAL COLIFORM,MEMBR FILTER,M-FC BROTH,44.5 C', 'FECAL COLIFORM,MPN,BORIC ACID LACTOSE BR,43C,48HR', 
# 'FECAL COLIFORM,MPN,EC MED,44.5C (TUBE 31614)', 'FECAL COLIFORM,MPN,TUBE CONFIGURATION',
# 'HARDNESS, CA MG CALCULATED (MG/L AS CACO3)', 'HARDNESS, CA MG CALCULATED (MG/L AS CACO3) AS DISSOLVED', 
# 'HARDNESS, CARBONATE (MG/L AS CACO3)', 'HARDNESS, TOTAL (MG/L AS CACO3)')


# manually specify troubleshooting
manualSelection1 <- c('1BSMT001.53','1BSMT006.62','1BSMT009.08')#1AFOU002.06')
#WQM_Stations_Filter <- filter(WQM_Stations_Spatial, StationID %in% as.character(manualSelection1))  
WQM_Stations_Filter <- WQM_Stations_Filter_function('Manually Specify Stations (takes a few seconds for the station text box to appear)', 
                                                    pool, WQM_Stations_Spatial, VAHU6Filter = NULL, subbasinFilter = NULL, assessmentRegionFilter = NULL,
                                                    ecoregionFilter = "Ridge and Valley", dateRange_multistation, analyte_Filter, 
                                                    manualSelection = manualSelection1, wildcardSelection = NULL)

# wildcard troubleshooting
wildcardText1 <- '3-RPP%'
# wildcardResults <- sqldf(paste0('SELECT * FROM WQM_Stations_Spatial WHERE StationID like "',
#                                 wildcardText1, '"'))
WQM_Stations_Filter <- WQM_Stations_Filter_function('Wildcard Selection', 
                                                    pool, WQM_Stations_Spatial, VAHU6Filter = NULL, subbasinFilter = NULL, assessmentRegionFilter = NULL,
                                                    ecoregionFilter = "Ridge and Valley", dateRange_multistation, analyte_Filter= NULL, 
                                                    manualSelection = NULL, wildcardSelection = wildcardText1)



# Spatial filters troubleshooting
### begin
assessmentRegionFilter <- c("PRO")#c("BRRO")#NULL#c("PRO")#unique(subbasins$ASSESS_REG)
subbasinFilter <- "Appomattox"#"James-Upper"# c("James-Middle",'Potomac-Lower')#NULL# c("James River - Middle",'Potomac River')#NULL#"James River - Lower"
# filter(WQM_Stations_Spatial, ASSESS_REG %in% assessmentRegionFilter) %>%
#   distinct(Basin_Name) %>%  pull()
VAHU6Filter <- NULL#'JU11'#NULL 
# filter(WQM_Stations_Spatial, ASSESS_REG %in% assessmentRegionFilter) %>%
#   filter(Basin_Name %in% subbasinFilter) %>% 
#   distinct(VAHU6) %>%  pull()



WQM_Stations_Filter <- WQM_Stations_Filter_function('Spatial Filters', pool, WQM_Stations_Spatial, VAHU6Filter, subbasinFilter, assessmentRegionFilter,
                                         ecoregionFilter, dateRange_multistation, analyte_Filter, manualSelection = NULL, wildcardSelection = NULL)

 
### end filter options

# Pull as many details about station from REST service (if available). Work around provided in case station isn't on REST server
## Pull station info from REST service
WQM_Station_Full_REST <- filter(WQM_Stations_Full, WQM_STA_ID %in% WQM_Stations_Filter$StationID)
#WQM_Station_Full_REST <- WQM_Station_Full_REST_request(pool, station, subbasinVAHU6crosswalk, subbasins, ecoregion)

# Quick Station Sampling Information
stationInfoSampleMetrics <- stationSummarySampingMetrics(WQM_Station_Full_REST, 'multi')

## Pull CEDS Station Information 
if(nrow(WQM_Stations_Filter) > 0){
  stationInfoFin <- stationInfoConsolidated(pool, WQM_Stations_Filter$StationID, WQM_Station_Full_REST)
}

# Quick Station Sampling Information
stationInfoSampleMetrics <- stationSummarySampingMetrics(WQM_Station_Full_REST, 'multi')



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
                                               EPA_ECO_US_L3NAME, BASINS_HUC_8_NAME, BASINS_VAHU6, WQS_WATER_NAME, WQS_SEC, WQS_CLASS, 
                                               WQS_SPSTDS, WQS_PWS, WQS_TROUT, WQS_TIER_III, WQM_YRS_YEAR, WQM_YRS_SPG_CODE),
                                 by = c('Sta_Id' = 'STATION_ID')) %>%
  dplyr::select(Sta_Id, Sta_Desc, `CEDS Station View Link`, `DEQ GIS Web App Link`, Latitude, Longitude, WQM_STA_STRAHER_ORDER, EPA_ECO_US_L3CODE,
                EPA_ECO_US_L3NAME, BASINS_HUC_8_NAME, BASINS_VAHU6, WQS_WATER_NAME, WQS_SEC, WQS_CLASS, 
                WQS_SPSTDS, WQS_PWS, WQS_TROUT, WQS_TIER_III, everything()) 
# Empty station user selection to start with
selectedSites <- NULL 


# color palette for assessment polygons
pal <- colorFactor(
  palette = topo.colors(7),
  domain = assessmentRegions$ASSESS_REG)
pal2 <- colorFactor(
  palette = rainbow(7),
  domain = ecoregion$US_L3NAME)

assessmentLayerFilter <- filter(assessmentLayer, VAHU6 %in% WQM_Stations_Filter$VAHU6) 


CreateWebMap(maps = c("Topo","Imagery","Hydrography"), collapsed = TRUE,
             options= leafletOptions(zoomControl = TRUE,minZoom = 3, maxZoom = 20,
                                     preferCanvas = TRUE)) %>%
  setView(-79.1, 37.7, zoom=7)  %>%
  addPolygons(data= ecoregion,  color = 'gray', weight = 1,
              fillColor= ~pal2(ecoregion$US_L3NAME), fillOpacity = 0.5,stroke=0.1,
              group="Level III Ecoregions",label = ~US_L3NAME) %>% hideGroup('Level III Ecoregions') %>%
  addPolygons(data= assessmentRegions,  color = 'black', weight = 1,
              fillColor= ~pal(assessmentRegions$ASSESS_REG), fillOpacity = 0.5,stroke=0.1,
              group="Assessment Regions", label = ~ASSESS_REG) %>% hideGroup('Assessment Regions') %>%
  
  
  addCircleMarkers(data = WQM_Stations_Filter,
                   color='blue', fillColor='gray', radius = 4,
                   fillOpacity = 0.5,opacity=0.8,weight = 2,stroke=T, group="Spatial Filter Station(s)",
                   label = ~StationID, layerId = ~StationID,
                   popup = leafpop::popupTable(WQM_Stations_Filter, zcol=c('StationID'))) %>%
  {if(nrow(assessmentLayerFilter) > 0)
    addPolygons(., data= assessmentLayerFilter,  color = 'black', weight = 1,
                fillColor= 'gray', fillOpacity = 0.5,stroke=0.1,
                group="VAHU6", label = ~VAHU6) %>% hideGroup('VAHU6')
    else . } %>%
    
  inlmisc::AddHomeButton(raster::extent(-83.89, -74.80, 36.54, 39.98), position = "topleft") %>%
  addDrawToolbar(
    targetGroup='Selected',
    polylineOptions=FALSE,
    markerOptions = FALSE,
    polygonOptions = drawPolygonOptions(shapeOptions=drawShapeOptions(fillOpacity = 0, color = 'white', weight = 3)),
    rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0, color = 'white', weight = 3)),
    circleOptions = FALSE,
    circleMarkerOptions = FALSE,
    editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions())) %>%
  addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                   overlayGroups = c("Spatial Filter Station(s)", "VAHU6","Level III Ecoregions", 'Assessment Regions'),
                   options=layersControlOptions(collapsed=T),
                   position='topleft') 


