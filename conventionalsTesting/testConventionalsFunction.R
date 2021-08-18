## Function Demo
library(tidyverse)
library(config)
library(sf)
library(lubridate)
library(pool)
library(pins)
library(dbplyr)


# Server connection things
conn <- config::get("connectionSettings") # get configuration settings

# set up pool
pool <- dbPool(
  drv = odbc::odbc(),
  Driver = "ODBC Driver 11 for SQL Server",#"SQL Server Native Client 11.0",
  Server= "DEQ-SQLODS-PROD,50000",
  dbname = "ODS",
  trusted_connection = "yes"
)

station <- '4AROA216.75'#'2-ANG003.35'#'2-HRE000.44'#'2-JKS030.65'

dateRange <- c(as.Date('2015-01-01'), as.Date('2021-12-31'))
stationFieldData <- pool %>% tbl(in_schema("wqm", "Wqm_Field_Data_View")) %>%
  filter(Fdt_Sta_Id %in% !! station &
           between(as.Date(Fdt_Date_Time), !! dateRange[1], !! dateRange[2]) ) %>%
  as_tibble() %>%
  filter(! Ssc_Description %in% "INVALID DATA SET QUALITY ASSURANCE FAILURE")
stationAnalyteData <- pool %>% tbl(in_schema("wqm", "Wqm_Analytes_View")) %>%
  filter(Ana_Sam_Fdt_Id %in% !! stationFieldData$Fdt_Id &
           #between(as.Date(Ana_Received_Date), !! dateRange[1], !! dateRange[2]) & # x >= left & x <= right
           Pg_Parm_Name != "STORET STORAGE TRANSACTION DATE YR/MO/DAY") %>%
  as_tibble() %>%
  left_join(dplyr::select(stationFieldData, Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time), by = c("Ana_Sam_Fdt_Id" = "Fdt_Id"))
stationInfo <- pool %>% tbl(in_schema("wqm",  "Wqm_Stations_View")) %>%
  filter(Sta_Id %in% !! toupper(station)) %>%
  as_tibble()
stationGIS_View <-  pool %>% tbl(in_schema("wqm",  "Wqm_Sta_GIS_View")) %>%
  filter(Station_Id %in% !! toupper(station)) %>%
  as_tibble()


station <- pin_get("conventionals2022IRfinalWithSecchi", board = "rsconnect") %>% 
  filter(STA_REC_CODE == 'BRRO') %>% 
  distinct(FDT_STA_ID)
station <- station$FDT_STA_ID[1:500]


dateRange <- c(as.Date('2015-01-01'), as.Date('2020-12-31'))
stationFieldData <- pool %>% tbl(in_schema("wqm", "Wqm_Field_Data_View")) %>%
  filter(Fdt_Sta_Id %in% !! station &
           between(as.Date(Fdt_Date_Time), !! dateRange[1], !! dateRange[2]) ) %>%
  as_tibble() %>%
  filter(! Ssc_Description %in% "INVALID DATA SET QUALITY ASSURANCE FAILURE")
stationAnalyteData <- pool %>% tbl(in_schema("wqm", "Wqm_Analytes_View")) %>%
  filter(Ana_Sam_Fdt_Id %in% !! stationFieldData$Fdt_Id &
           #between(as.Date(Ana_Received_Date), !! dateRange[1], !! dateRange[2]) & # x >= left & x <= right
           Pg_Parm_Name != "STORET STORAGE TRANSACTION DATE YR/MO/DAY") %>%
  as_tibble() %>%
  left_join(dplyr::select(stationFieldData, Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time), by = c("Ana_Sam_Fdt_Id" = "Fdt_Id"))
stationInfo <- pool %>% tbl(in_schema("wqm",  "Wqm_Stations_View")) %>%
  filter(Sta_Id %in% !! toupper(station)) %>%
  as_tibble()
stationGIS_View <-  pool %>% tbl(in_schema("wqm",  "Wqm_Sta_GIS_View")) %>%
  filter(Station_Id %in% !! toupper(station)) %>%
  as_tibble()
zz <- conventionalsSummary(conventionals= pin_get("conventionals2022IRfinalWithSecchi", board = "rsconnect")[0,],
                           stationFieldDataUserFilter= stationFieldData, stationAnalyteDataUserFilter = stationAnalyteData,
                           stationInfo,
                           stationGIS_View,
                           dropCodes = c('QF'))%>% 
  arrange(FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH) #%>% 
# mutate(UID = paste0(FDT_DATE_TIME, FDT_DEPTH)) %>% 
# group_by(FDT_STA_ID, FDT_DATE_TIME) %>% 
# #rowid_to_column()
# mutate(UID = n())%>% 
#   dplyr::select(UID, everything())



conventionals2 <- pin_get("conventionals2022IRfinalWithSecchi", board = "rsconnect") %>% 
  mutate_at(vars(contains('RMK_')), as.character) %>% 
  mutate_at(vars(contains('LEVEL_')), as.character) %>% 
  filter(FDT_STA_ID %in% station) %>% 
  arrange(FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH) #%>% 
#  mutate(UID = paste0(FDT_DATE_TIME, FDT_DEPTH)) %>% 
# group_by(FDT_STA_ID, FDT_DATE_TIME) %>% 
# #rowid_to_column()
# mutate(UID = n()) %>% 
#   dplyr::select(UID, everything()) #%>% 
# group_by(UID) %>% 
# mutate(n = 1:n()) %>% 
# filter(n > 1)



library(lazyeval)
oneForOne <- function(df,metricName){
  z <- mutate_(df, realValue = interp(~v, v= as.name(paste(metricName,'.y',sep='')))) %>%
    mutate_(newValue = interp(~v, v= as.name(paste(metricName,'.x',sep='')))) %>%
    rowwise() %>% 
    mutate(diff = abs(round(sum(realValue, - newValue, na.rm= T),digits = 3))) %>%
    select(UID,diff)
  names(z) <- c('UID',paste(metricName,'_diff',sep=''))
  return(z)
}
#oneForOne(finalVSCI,'%Ephem')
diff2 <- left_join(zz, conventionals2,  c('FDT_STA_ID', 'FDT_DATE_TIME', 'FDT_DEPTH', 'FDT_DEPTH_DESC')) %>% 
  mutate(UID = row_number()) 

diff3 <- mutate(diff2, FDT_SPG_CODE = ifelse(FDT_SPG_CODE.x == FDT_SPG_CODE.y, T, F)) %>% 
  left_join(oneForOne(diff2, 'FDT_FIELD_PH')) %>% 
  left_join(oneForOne(diff2, 'FDT_TEMP_CELCIUS')) %>% 
  left_join(oneForOne(diff2, 'DO_mg_L')) %>% 
  left_join(oneForOne(diff2, 'DISSOLVED_OXYGEN_00300_mg_L')) %>% 
  left_join(oneForOne(diff2, 'DISSOLVED_OXYGEN_DOOPT_mg_L')) %>% 
  left_join(oneForOne(diff2, 'DISSOLVED_OXYGEN_WINK_mg_L')) %>% 
  left_join(oneForOne(diff2, 'FDT_SPECIFIC_CONDUCTANCE')) %>% 
  left_join(oneForOne(diff2, 'FDT_SALINITY')) %>% 
  left_join(oneForOne(diff2, 'NITROGEN_mg_L')) %>% 
  left_join(oneForOne(diff2, 'AMMONIA_mg_L')) %>% 
  left_join(oneForOne(diff2, 'NITRATE_mg_L')) %>% 
  left_join(oneForOne(diff2, 'NOX_mg_L')) %>% 
  left_join(oneForOne(diff2, 'NITROGEN_TOTAL_00600_mg_L')) %>% 
  left_join(oneForOne(diff2, 'NITROGEN_AMMONIA_DISSOLVED_00608_mg_L')) %>% 
  left_join(oneForOne(diff2, 'NITROGEN_AMMONIA_TOTAL_00610_mg_L')) %>% 
  left_join(oneForOne(diff2, 'NITROGEN_NITRITE_DISSOLVED_00613_mg_L')) %>% 
  left_join(oneForOne(diff2, 'NITROGEN_NITRITE_TOTAL_00615_mg_L')) %>% 
  left_join(oneForOne(diff2, 'NITROGEN_NITRATE_DISSOLVED_00618_mg_L')) %>% 
  left_join(oneForOne(diff2, 'NITROGEN_NITRATE_TOTAL_00620_mg_L')) %>% 
  left_join(oneForOne(diff2, 'NITROGEN_KJELDAHL_TOTAL_00625_mg_L')) %>% 
  left_join(oneForOne(diff2, 'NITRITE+NITRATE_TOTAL_00630_mg_L')) %>% 
  left_join(oneForOne(diff2, 'NITRITE+NITRATE_DISSOLVED_00631_mg_L')) %>% 
  left_join(oneForOne(diff2, 'NITROGEN_PARTICULATE_49570_mg_L')) %>% 
  left_join(oneForOne(diff2, 'NITROGEN_TOTAL_DISSOLVED_49571_mg_L')) %>% 
  left_join(oneForOne(diff2, 'NITROGEN_TOTAL_DISSOLVED_TDNLF_mg_L')) %>% 
  left_join(oneForOne(diff2, 'PHOSPHORUS_mg_L')) %>% 
  left_join(oneForOne(diff2, 'PHOSPHORUS_TOTAL_00665_mg_L')) %>% 
  left_join(oneForOne(diff2, 'PHOSPHORUS_DISSOLVED_00666_mg_L')) %>% 
  left_join(oneForOne(diff2, 'PHOSPHORUS_DISSOLVED_ORTHOPHOSPHATE_00671_mg_L')) %>% 
  left_join(oneForOne(diff2, 'PHOSPHOROUS_PARTICULATE_49567_mg_L')) %>% 
  left_join(oneForOne(diff2, 'PHOSPHOROUS_TOTAL_DISSOLVED_49572_mg_L')) %>% 
  left_join(oneForOne(diff2, 'PHOSPHORUS_TOTAL_ORTHOPHOSPHATE_70507_mg_L')) %>% 
  left_join(oneForOne(diff2, 'ORTHOPHOSPHATE_DISSOLVED_OPWLF_mg_L')) %>% 
  left_join(oneForOne(diff2, 'PHOSPHORUS_SUSPENDED_INORGANIC_PIPLF_mg_L')) %>% 
  left_join(oneForOne(diff2, 'PHOSPHORUS_PARTICULATE_PPWLF_mg_L')) %>% 
  left_join(oneForOne(diff2, 'PHOSPHORUS_TOTAL_DISSOLVED_TDPLF_mg_L')) %>% 
  left_join(oneForOne(diff2, 'HARDNESS_TOTAL_00900_mg_L')) %>% 
  left_join(oneForOne(diff2, 'CHLORIDE_mg_L')) %>% 
  left_join(oneForOne(diff2, 'CHLORIDE_TOTAL_00940_mg_L')) %>% 
  left_join(oneForOne(diff2, 'CHLORIDE_DISSOLVED_00941_mg_L')) %>% 
  left_join(oneForOne(diff2, 'SULFATE_mg_L')) %>% 
  left_join(oneForOne(diff2, 'SULFATE_TOTAL_mg_L')) %>% 
  left_join(oneForOne(diff2, 'SULFATE_DISS_mg_L')) %>% 
  left_join(oneForOne(diff2, 'ECOLI')) %>% 
  left_join(oneForOne(diff2, 'ECOLI_31648_NO_100mL')) %>% 
  left_join(oneForOne(diff2, 'ENTEROCOCCI')) %>% 
  left_join(oneForOne(diff2, 'FECAL_COLI')) %>% 
  left_join(oneForOne(diff2, 'CHLOROPHYLL_A_ug_L')) %>% 
  left_join(oneForOne(diff2, 'TSS_mg_L')) %>% 
  left_join(oneForOne(diff2, 'TOTAL_SUSPENDED_SOLIDS_00530_mg_L')) %>% 
  left_join(oneForOne(diff2, 'SSC_mg_L')) %>% 
  left_join(oneForOne(diff2, 'TOTAL_SUSPENDED_SOLIDS_TSS45_mg_L')) %>% 
  left_join(oneForOne(diff2, 'SECCHI_DEPTH_M')) %>% 
  select(UID, FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, FDT_DEPTH_DESC, ends_with('_diff')) %>%
  filter_at(vars(ends_with('_diff')), any_vars(. != 0))

diff4 <- diff3 
x <- diff4 %>% #filter(diff4, FDT_STA_ID == i) %>% 
  group_by(UID, FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, FDT_DEPTH_DESC) %>% 
  pivot_longer(cols= -c(UID, FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, FDT_DEPTH_DESC), names_to = 'parameter', values_to = 'value') %>% 
  filter(value != 0)

issues <- filter(x, ! parameter %in% c( #"PHOSPHORUS_mg_L_diff",
  "SECCHI_DEPTH_M_diff", # secchi depth issues all over the place with where I tacked it on in roger's conventionals vs what line it occurs on in CEDS
  "NOX_mg_L_diff",  # NOX calculation off in conventionals
  "FDT_SALINITY_diff")) %>% # unknown salinity data coming in 
  arrange(parameter, FDT_STA_ID) %>% 
  # lots of rows identified that are duplicate records and issue is that all data is teh same but on different lines
  # drop those
  group_by(FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, FDT_DEPTH_DESC, parameter, value) %>% 
  mutate(n = n()) %>% 
  filter(n == 1)
unique(issues$parameter)


stationFieldDataUserFilter <- stationFieldData
stationAnalyteDataUserFilter <- stationAnalyteData



View(filter(conventionals2, FDT_STA_ID == station) %>% dplyr::select(FDT_DATE_TIME, contains('FDT_SALINITY')))
View(filter(stationFieldDataUserFilter1, FDT_STA_ID == station) %>% dplyr::select(FDT_DATE_TIME, contains('FDT_SALINITY')))


View(filter(stationAnalyteDataUserFilter1, FDT_STA_ID == station) %>% dplyr::select(FDT_DATE_TIME, contains('DISSOLVED_OXYGEN_DOOPT_mg_L')))

































# Older QA attempts with minimal success
View(full_join(conventionals2, zz, by = 'UID'))

#View(filter(zz, ! rowid %in% conventionals2$rowid))
View(left_join(dplyr::select(zz, FDT_STA_ID, UID),
               dplyr::select(conventionals2, FDT_STA_ID, UID), by = c('FDT_STA_ID', 'FDT_DATE_TIME')) %>% 
       mutate(diff = UID.x == UID.y) %>% 
       filter(diff == FALSE))

View(anti_join(dplyr::select(zz, -c(UID, GROUP_STA_ID:STA_REC_CODE)), 
               dplyr::select(conventionals2, -c(UID, GROUP_STA_ID:STA_REC_CODE))))

dplyr::all_equal(conventionals2, zz)
janitor::compare_df_cols(conventionals2, zz)
janitor::compare_df_cols(conventionals2, zz, return = "mismatch")



library(daff)
library(tidyselect)
diff_data(conventionals2, zz)
render_diff(diff_data(conventionals2, zz))

View(setdiff(conventionals2, zz))

View(dplyr::select(zz, FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, NITROGEN_mg_L:LEVEL_AMMONIA))
View(dplyr::select(conventionals2, FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, NITROGEN_mg_L:LEVEL_AMMONIA))

View(setdiff(zz, conventionals2))

differences <- 
  left_join(
    dplyr::select(zz, FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, PHOSPHORUS_mg_L:LEVEL_TDPLF),# NITROGEN_mg_L:LEVEL_AMMONIA),
    dplyr::select(conventionals2, FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, PHOSPHORUS_mg_L:LEVEL_TDPLF),#NITROGEN_mg_L:LEVEL_AMMONIA),
    by = c('FDT_STA_ID', 'FDT_DATE_TIME', 'FDT_DEPTH')) %>% 
  select(sort(peek_vars()))
rowwise() %>% 
  mutate(nitrogen = sum(NITROGEN_mg_L.x, - NITROGEN_mg_L.y, na.rm = T),
         ammonia = sum(AMMONIA_mg_L.x, - AMMONIA_mg_L.y, na.rm = T)) 


