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
WQM_Stations_Spatial <- pin_get("ejones/WQM-Stations-Spatial", board = "rsconnect")
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
queryType <- 'Manually Specify Stations'#'Spatial Filters' #Interactive Selection #'Manually Specify Stations'


# manually specify troubleshooting
manualSelection1 <- c('1BSMT001.53','1BSMT006.62','1BSMT009.08')#1AFOU002.06')
WQM_Stations_Filter <- filter(WQM_Stations_Spatial, StationID %in% as.character(manualSelection1))  
# skip down to multistationInfoFin




# Spatial filters troubleshooting
### begin
assessmentRegionFilter <- c("PRO")#c("BRRO")#NULL#c("PRO")#unique(subbasins$ASSESS_REG)
subbasinFilter <- "Appomattox"#"James-Upper"# c("James-Middle",'Potomac-Lower')#NULL# c("James River - Middle",'Potomac River')#NULL#"James River - Lower"
# filter(WQM_Stations_Spatial, ASSESS_REG %in% assessmentRegionFilter) %>%
#   distinct(Basin_Code) %>%  pull()
VAHU6Filter <- NULL#'JU11'#NULL 
# filter(WQM_Stations_Spatial, ASSESS_REG %in% assessmentRegionFilter) %>%
#   filter(Basin_Code %in% subbasinFilter) %>% 
#   distinct(VAHU6) %>%  pull()
ecoregionFilter <- "Middle Atlantic Coastal Plain"#NULL#"Blue Ridge"#unique(ecoregion$US_L3NAME)
dateRange_multistation <- c(as.Date('1970-01-01'), as.Date(Sys.Date()- 7))
## pull based on parameter
analyte_Filter <- NULL#c('SODIUM (NA), ATM DEP, WET, DISS, MG/L', 'SODIUM, DISSOLVED (MG/L AS NA)', 
                    #'SODIUM, TOTAL (MG/L AS NA)', 'SODIUM-TOTAL  UG/L (AS NA)')#,
                    # 'FECAL COLIFORM,7 HR,M-7HR FC MED MF,41.5C,#/100ML', 'FECAL COLIFORM,A-1 MOD,WATER,44.5C,24HR MPN/100ML', 
                    # 'FECAL COLIFORM,MEMBR FILTER,M-FC BROTH,44.5 C', 'FECAL COLIFORM,MPN,BORIC ACID LACTOSE BR,43C,48HR', 
                    # 'FECAL COLIFORM,MPN,EC MED,44.5C (TUBE 31614)', 'FECAL COLIFORM,MPN,TUBE CONFIGURATION',
                    # 'HARDNESS, CA MG CALCULATED (MG/L AS CACO3)', 'HARDNESS, CA MG CALCULATED (MG/L AS CACO3) AS DISSOLVED', 
                    # 'HARDNESS, CARBONATE (MG/L AS CACO3)', 'HARDNESS, TOTAL (MG/L AS CACO3)')



WQM_Stations_Filter <- WQM_Stations_Filter_function(pool, WQM_Stations_Spatial, VAHU6Filter, subbasinFilter, assessmentRegionFilter,
                                         ecoregionFilter, dateRange_multistation, analyte_Filter)


### end  

# Pull as many details about station from REST service (if available). Work around provided in case station isn't on REST server
## Pull station info from REST service
WQM_Station_Full_REST <- filter(WQM_Stations_Full, WQM_STA_ID %in% WQM_Stations_Filter$StationID)
#WQM_Station_Full_REST <- WQM_Station_Full_REST_request(pool, station, subbasinVAHU6crosswalk, subbasins, ecoregion)

# Quick Station Sampling Information
stationInfoSampleMetrics <- stationSummarySampingMetrics(WQM_Station_Full_REST, 'multi')

## Pull CEDS Station Information 
stationInfoFin <- stationInfoConsolidated(pool, WQM_Stations_Filter$StationID, WQM_Station_Full_REST)

# Quick Station Sampling Information
stationInfoSampleMetrics <- stationSummarySampingMetrics(WQM_Station_Full_REST, 'multi')


