mCCUmetals <- c("HARDNESS, CA MG CALCULATED (MG/L AS CACO3) AS DISSOLVED", "ARSENIC, DISSOLVED  (UG/L AS AS)",
                "CHROMIUM, DISSOLVED (UG/L AS CR)", "COPPER, DISSOLVED (UG/L AS CU)",
                "LEAD, DISSOLVED (UG/L AS PB)", "NICKEL, DISSOLVED (UG/L AS NI)","ZINC, DISSOLVED (UG/L AS ZN)")



BSAtooloutputFunction <- function(pool, station, dateRangeFilter, LRBS, stationInfo_sf, stationAnalyteDataUserFilter, stationFieldDataUserFilter){
  # Get habitat Information
  totHab <- BSAhabitatQuery(pool, station, dateRangeFilter) %>% 
    distinct(CollDate, .keep_all = T) %>% 
    dplyr::select(-c(HabParameter, HabValue))
  
  # Get LRBS information
  lrbs <- filter(LRBS, StationID %in% station & between(Date, dateRangeFilter[1], dateRangeFilter[2])) %>% 
    dplyr::select(StationID, Date, LRBS = LRBS2)
  
  # Get Metals Information
  metalsCCU <- filter(stationAnalyteDataUserFilter, Pg_Parm_Name %in% mCCUmetals)
  if(nrow(metalsCCU) > 0){
    metalsCCU <- metalsCCU %>% 
      mutate(Parameter = recode(`Pg_Parm_Name`,  "CALCIUM, DISSOLVED (MG/L AS CA)" = "Calcium",
                                "ARSENIC, DISSOLVED  (UG/L AS AS)" = "Arsenic",
                                "CHROMIUM, DISSOLVED (UG/L AS CR)" = "Chromium",
                                "COPPER, DISSOLVED (UG/L AS CU)" = "Copper", 
                                "LEAD, DISSOLVED (UG/L AS PB)" = "Lead",
                                "NICKEL, DISSOLVED (UG/L AS NI)" = "Nickel",
                                "ZINC, DISSOLVED (UG/L AS ZN)" = "Zinc",
                                "HARDNESS, CA MG CALCULATED (MG/L AS CACO3) AS DISSOLVED" = "Hardness")) %>% 
      dplyr::select(StationID = Fdt_Sta_Id, `Collection Date` =  Fdt_Date_Time, Parameter, Ana_Uncensored_Value) %>% 
      group_by(StationID, `Collection Date`) %>% 
      pivot_wider(names_from = Parameter, values_from = Ana_Uncensored_Value) %>%
      mutate(criteriaHardness = ifelse(Hardness<25,25,ifelse(Hardness>400,400,Hardness)),
             ArsenicChronic = Arsenic/150,
             ChromiumChronic = Chromium/((exp(0.819*(log(criteriaHardness))+0.6848))*0.86),
             CopperChronic = Copper/((exp(0.8545*(log(criteriaHardness))-1.702))*0.96),
             LeadChronic = Lead/((exp(1.273*(log(criteriaHardness))-3.259))*(1.46203-(log(criteriaHardness)*(0.145712)))),
             NickelChronic = Nickel/((exp(0.846*(log(criteriaHardness))-0.884))*0.997),
             ZincChronic = Zinc/((exp(0.8473*(log(criteriaHardness))+0.884))*0.986)) %>% 
      summarise(MetalsCCU = sum(ArsenicChronic, ChromiumChronic, CopperChronic, LeadChronic, NickelChronic, ZincChronic, na.rm = T))
  } else {metalsCCU <- tibble(StationID = station,  `Collection Date`= NA, MetalsCCU = NA)}
  
  
  z <- bind_rows(tibble('Fdt_Sta_Id'= 'FakeRow', 'Fdt_Date_Time'= NA, 'Fdt_Depth' = NA, 'Fdt_Temp_Celcius'= NA, 'Fdt_Field_Ph'= NA, 'Fdt_Do_Probe'= NA, 'Fdt_Do_Optical'= NA, 'Fdt_Do_Winkler'= NA, 
                        'Fdt_Specific_Conductance'= NA,'NITROGEN, TOTAL (MG/L AS N)'= NA, 'PHOSPHORUS, TOTAL (MG/L AS P)'= NA, 'TDS RESIDUE,TOTAL FILTRABLE (DRIED AT 180C),MG/L'= NA,
                        'SULFATE, TOTAL (MG/L AS SO4)'= NA, 'CHLORIDE,TOTAL IN WATER MG/L'= NA,  'SODIUM, DISSOLVED (MG/L AS NA)'= NA, 'POTASSIUM, DISSOLVED (MG/L AS K)' = NA),
                 stationFieldAnalyteDataPretty(stationAnalyteDataUserFilter, stationFieldDataUserFilter, averageResults = FALSE) %>%
                   dplyr::select(one_of(c('Fdt_Sta_Id', 'Fdt_Date_Time', 'Fdt_Depth', 'Fdt_Temp_Celcius', 'Fdt_Field_Ph', 'Fdt_Do_Probe', 'Fdt_Do_Optical', 'Fdt_Do_Winkler', 
                                          'Fdt_Specific_Conductance','NITROGEN, TOTAL (MG/L AS N)', 'PHOSPHORUS, TOTAL (MG/L AS P)', 'SULFATE, TOTAL (MG/L AS SO4)',
                                          'TDS RESIDUE,TOTAL FILTRABLE (DRIED AT 180C),MG/L','CHLORIDE,TOTAL IN WATER MG/L', 'SODIUM, DISSOLVED (MG/L AS NA)',
                                          'POTASSIUM, DISSOLVED (MG/L AS K)')))) %>% 
    filter(Fdt_Sta_Id != 'FakeRow') %>% 
    mutate(`Dissolved Oxygen` = case_when(!is.na(Fdt_Do_Probe) ~ Fdt_Do_Probe,
                                          !is.na(Fdt_Do_Optical) ~ Fdt_Do_Optical,
                                          !is.na(Fdt_Do_Winkler) ~ Fdt_Do_Winkler,
                                          TRUE ~ as.numeric(NA))) %>%
    full_join(totHab, by = c('Fdt_Sta_Id'= 'StationID', 'Fdt_Date_Time' = 'CollDate')) %>% # full join in case benthic date/time doesn't match somewhere in CEDS
    full_join(lrbs, by = c('Fdt_Sta_Id'= 'StationID', 'Fdt_Date_Time' = 'Date')) %>% # full join in case LRBS date/time doesn't match somewhere in CEDS
    full_join(metalsCCU, by = c('Fdt_Sta_Id'= 'StationID', 'Fdt_Date_Time' = 'Collection Date')) %>% # full join in case LRBS date/time doesn't match somewhere in CEDS
    filter(!is.na(Fdt_Date_Time)) %>%  #drop any empty rows after joining other datasets
    left_join(dplyr::select(stationInfo_sf, STATION_ID, Latitude, Longitude) %>% 
                distinct(STATION_ID, .keep_all= T) %>% 
                st_drop_geometry(), by = c('Fdt_Sta_Id'='STATION_ID')) %>% 
    dplyr::select(StationID = Fdt_Sta_Id, CollectionDateTime = Fdt_Date_Time, Depth = Fdt_Depth, Longitude,  Latitude,
                  `pH (unitless)` = Fdt_Field_Ph, `DO (mg/L)` = `Dissolved Oxygen`, `TN (mg/L)` = `NITROGEN, TOTAL (MG/L AS N)`,
                  `TP (mg/L)` = `PHOSPHORUS, TOTAL (MG/L AS P)`, `Total Habitat (unitless)` = `Total Habitat Score`, 
                  `LRBS (unitless)` = LRBS, `MetalsCCU (unitless)` = MetalsCCU,
                  `SpCond (uS/cm)` = Fdt_Specific_Conductance, `TDS (mg/L)` = `TDS RESIDUE,TOTAL FILTRABLE (DRIED AT 180C),MG/L`,
                  `DSulfate (mg/L)` = `SULFATE, TOTAL (MG/L AS SO4)`, `DChloride (mg/L)` = `CHLORIDE,TOTAL IN WATER MG/L`, 
                  `DPotassium (mg/L)` = `POTASSIUM, DISSOLVED (MG/L AS K)`, `DSodium (mg/L)` = `SODIUM, DISSOLVED (MG/L AS NA)`, 
                  `Temperature (C)` = Fdt_Temp_Celcius) %>% 
    arrange(StationID, CollectionDateTime) %>% 
    mutate(row_number()) #number rows
  # drop empty rows
  z2 <- filter_at(z, vars(`pH (unitless)`:`Temperature (C)`), all_vars(is.na(.)))
  z <- filter(z, ! `row_number()` %in% z2$`row_number()`)  %>% 
    dplyr::select(-`row_number()`)
  return(z)
} 
# BSA tool data output

# Habitat Data pull for BSA
BSAhabitatQuery <- function(pool, station, dateRangeFilter){
  totalHabitatSample <- pool %>% tbl(in_schema("wqm", "Edas_Habitat_Sample_View")) %>%
    filter(STA_ID %in% !! toupper(station) &  between(as.Date(FDT_DATE_TIME), !!dateRangeFilter[1], !!dateRangeFilter[2])) %>%
    as_tibble() %>% 
    rename("StationID" = "STA_ID",
           "HabSampID" = "WHS_SAMP_ID",
           "Entered Date" = "WHS_INSERTED_DATE",
           "Entered By" = "WHS_INSERTED_BY",
           "Field Team" = "WHS_FIELD_TEAM",
           "HabSample Comment" = "WHS_COMMENT",
           "Gradient" = "WSGC_DESCRIPTION",
           "Collection Date" = "FDT_DATE_TIME") %>%
    # Add sample season 
    mutate(monthday = as.numeric(paste0(sprintf("%02d",month(`Collection Date`)),
                                        sprintf("%02d",day(`Collection Date`)))),
           Season = case_when(monthday >= 0215 & monthday <= 0615 ~ 'Spring',
                              monthday >= 0815 & monthday <= 1215 ~ 'Fall',
                              TRUE ~ as.character("Outside Sample Window"))) %>%
    dplyr::select(HabSampID, StationID, `Collection Date`, `Entered By`, `Entered Date`, `Field Team`, `HabSample Comment`, Gradient, Season)
  if(nrow(totalHabitatSample) > 0){
    totalHabitat <- pool %>% tbl(in_schema("wqm", "Edas_Habitat_Values_View")) %>%
      filter(WHS_SAMP_ID %in% !! totalHabitatSample$HabSampID) %>%
      as_tibble() %>%
      rename("HabSampID" = "WHS_SAMP_ID",
             "HabParameter" = "WHVP_CODE",
             "HabParameterDescription" = "WHVP_DESCRIPTION",
             "HabValue" = "WHV_HAB_VALUE",
             "HabValue Comment" = "WHV_COMMENT") %>%
      left_join(dplyr::select(totalHabitatSample, StationID, HabSampID, `Collection Date`), by = 'HabSampID') %>% 
      # what I really want after BSA update
      #dplyr::select(StationID, `Collection Date`, HabParameter, HabParameterDescription, HabValue, `HabValue Comment`, Gradient, Season)
      dplyr::select(StationID, CollDate = `Collection Date`, HabParameter, HabValue) %>% 
      group_by(StationID, CollDate) %>% 
      mutate(`Total Habitat Score` = sum(HabValue, na.rm = T)) %>%  ungroup()
  } else {
    totalHabitat <- tibble(StationID = station, CollDate = NA, HabParameter = NA, HabValue = NA, `Total Habitat Score` = NA) }
  return(totalHabitat)
}
#BSAhabitatQuery(pool, station, dateRangeFilter)


BSAtooloutputFunction <- function(pool, station, dateRangeFilter, LRBS, stationInfo_sf, stationAnalyteDataUserFilter, stationFieldDataUserFilter){
  # Get habitat Information
  totHab <- BSAhabitatQuery(pool, station, dateRangeFilter) %>% 
    distinct(CollDate, .keep_all = T) %>% 
    dplyr::select(-c(HabParameter, HabValue))
  
  # Get LRBS information
  lrbs <- filter(LRBS, StationID %in% station & between(Date, dateRangeFilter[1], dateRangeFilter[2])) %>% 
    dplyr::select(StationID, Date, LRBS = LRBS2)
  
  # Get Metals Information
  metalsCCU <- filter(stationAnalyteDataUserFilter, Pg_Parm_Name %in% mCCUmetals)
  if(nrow(metalsCCU) > 0){
    metalsCCU <- metalsCCU %>% 
      mutate(Parameter = recode(`Pg_Parm_Name`,  "CALCIUM, DISSOLVED (MG/L AS CA)" = "Calcium",
                                "ARSENIC, DISSOLVED  (UG/L AS AS)" = "Arsenic",
                                "CHROMIUM, DISSOLVED (UG/L AS CR)" = "Chromium",
                                "COPPER, DISSOLVED (UG/L AS CU)" = "Copper", 
                                "LEAD, DISSOLVED (UG/L AS PB)" = "Lead",
                                "NICKEL, DISSOLVED (UG/L AS NI)" = "Nickel",
                                "ZINC, DISSOLVED (UG/L AS ZN)" = "Zinc",
                                "HARDNESS, CA MG CALCULATED (MG/L AS CACO3) AS DISSOLVED" = "Hardness")) %>% 
      dplyr::select(StationID = Fdt_Sta_Id, `Collection Date` =  Fdt_Date_Time, Parameter, Ana_Uncensored_Value) %>% 
      group_by(StationID, `Collection Date`) %>% 
      pivot_wider(names_from = Parameter, values_from = Ana_Uncensored_Value) %>%
      mutate(criteriaHardness = ifelse(Hardness<25,25,ifelse(Hardness>400,400,Hardness)),
             ArsenicChronic = Arsenic/150,
             ChromiumChronic = Chromium/((exp(0.819*(log(criteriaHardness))+0.6848))*0.86),
             CopperChronic = Copper/((exp(0.8545*(log(criteriaHardness))-1.702))*0.96),
             LeadChronic = Lead/((exp(1.273*(log(criteriaHardness))-3.259))*(1.46203-(log(criteriaHardness)*(0.145712)))),
             NickelChronic = Nickel/((exp(0.846*(log(criteriaHardness))-0.884))*0.997),
             ZincChronic = Zinc/((exp(0.8473*(log(criteriaHardness))+0.884))*0.986)) %>% 
      summarise(MetalsCCU = sum(ArsenicChronic, ChromiumChronic, CopperChronic, LeadChronic, NickelChronic, ZincChronic, na.rm = T))
  } else {metalsCCU <- tibble(StationID = station,  `Collection Date`= NA, MetalsCCU = NA)}
  
  
  z <- bind_rows(tibble('Fdt_Sta_Id'= 'FakeRow', 'Fdt_Date_Time'= NA, 'Fdt_Depth' = NA, 'Fdt_Temp_Celcius'= NA, 'Fdt_Field_Ph'= NA, 'Fdt_Do_Probe'= NA, 'Fdt_Do_Optical'= NA, 'Fdt_Do_Winkler'= NA, 
                        'Fdt_Specific_Conductance'= NA,'NITROGEN, TOTAL (MG/L AS N)'= NA, 'PHOSPHORUS, TOTAL (MG/L AS P)'= NA, 'TDS RESIDUE,TOTAL FILTRABLE (DRIED AT 180C),MG/L'= NA,
                        'SULFATE, TOTAL (MG/L AS SO4)'= NA, 'CHLORIDE,TOTAL IN WATER MG/L'= NA,  'SODIUM, DISSOLVED (MG/L AS NA)'= NA, 'POTASSIUM, DISSOLVED (MG/L AS K)' = NA),
                 stationFieldAnalyteDataPretty(stationAnalyteDataUserFilter, stationFieldDataUserFilter, averageResults = FALSE) %>%
                   dplyr::select(one_of(c('Fdt_Sta_Id', 'Fdt_Date_Time', 'Fdt_Depth', 'Fdt_Temp_Celcius', 'Fdt_Field_Ph', 'Fdt_Do_Probe', 'Fdt_Do_Optical', 'Fdt_Do_Winkler', 
                                          'Fdt_Specific_Conductance','NITROGEN, TOTAL (MG/L AS N)', 'PHOSPHORUS, TOTAL (MG/L AS P)', 'SULFATE, TOTAL (MG/L AS SO4)',
                                          'TDS RESIDUE,TOTAL FILTRABLE (DRIED AT 180C),MG/L','CHLORIDE,TOTAL IN WATER MG/L', 'SODIUM, DISSOLVED (MG/L AS NA)',
                                          'POTASSIUM, DISSOLVED (MG/L AS K)')))) %>% 
    filter(Fdt_Sta_Id != 'FakeRow') %>% 
    mutate(`Dissolved Oxygen` = case_when(!is.na(Fdt_Do_Probe) ~ Fdt_Do_Probe,
                                          !is.na(Fdt_Do_Optical) ~ Fdt_Do_Optical,
                                          !is.na(Fdt_Do_Winkler) ~ Fdt_Do_Winkler,
                                          TRUE ~ as.numeric(NA))) %>%
    full_join(totHab, by = c('Fdt_Sta_Id'= 'StationID', 'Fdt_Date_Time' = 'CollDate')) %>% # full join in case benthic date/time doesn't match somewhere in CEDS
    full_join(lrbs, by = c('Fdt_Sta_Id'= 'StationID', 'Fdt_Date_Time' = 'Date')) %>% # full join in case LRBS date/time doesn't match somewhere in CEDS
    full_join(metalsCCU, by = c('Fdt_Sta_Id'= 'StationID', 'Fdt_Date_Time' = 'Collection Date')) %>% # full join in case LRBS date/time doesn't match somewhere in CEDS
    filter(!is.na(Fdt_Date_Time)) %>%  #drop any empty rows after joining other datasets
    left_join(dplyr::select(stationInfo_sf, STATION_ID, Latitude, Longitude) %>% 
                distinct(STATION_ID, .keep_all= T) %>% 
                st_drop_geometry(), by = c('Fdt_Sta_Id'='STATION_ID')) %>% 
    dplyr::select(StationID = Fdt_Sta_Id, CollectionDateTime = Fdt_Date_Time, Depth = Fdt_Depth, Longitude,  Latitude,
                  `pH (unitless)` = Fdt_Field_Ph, `DO (mg/L)` = `Dissolved Oxygen`, `TN (mg/L)` = `NITROGEN, TOTAL (MG/L AS N)`,
                  `TP (mg/L)` = `PHOSPHORUS, TOTAL (MG/L AS P)`, `Total Habitat (unitless)` = `Total Habitat Score`, 
                  `LRBS (unitless)` = LRBS, `MetalsCCU (unitless)` = MetalsCCU,
                  `SpCond (uS/cm)` = Fdt_Specific_Conductance, `TDS (mg/L)` = `TDS RESIDUE,TOTAL FILTRABLE (DRIED AT 180C),MG/L`,
                  `DSulfate (mg/L)` = `SULFATE, TOTAL (MG/L AS SO4)`, `DChloride (mg/L)` = `CHLORIDE,TOTAL IN WATER MG/L`, 
                  `DPotassium (mg/L)` = `POTASSIUM, DISSOLVED (MG/L AS K)`, `DSodium (mg/L)` = `SODIUM, DISSOLVED (MG/L AS NA)`, 
                  `Temperature (C)` = Fdt_Temp_Celcius) %>% 
    arrange(StationID, CollectionDateTime) %>% 
    mutate(row_number()) #number rows
  # drop empty rows
  z2 <- filter_at(z, vars(`pH (unitless)`:`Temperature (C)`), all_vars(is.na(.)))
  z <- filter(z, ! `row_number()` %in% z2$`row_number()`)  %>% 
    dplyr::select(-`row_number()`)
  return(z)
} 


BSAtoolMetalsFunction <- function(station, stationInfo_sf, stationAnalyteDataUserFilter){
  
  z <- stationAnalyteDataUserFilter %>%
    mutate(Parameter = recode(`Pg_Parm_Name`,
                              "SELENIUM, DISSOLVED (UG/L AS SE)" = "Selenium",
                              "CALCIUM, DISSOLVED (MG/L AS CA)" = "Calcium",
                              "MAGNESIUM, DISSOLVED (MG/L AS MG)" = "Magnesium",
                              "ARSENIC, DISSOLVED  (UG/L AS AS)" = "Arsenic",
                              "BARIUM, DISSOLVED (UG/L AS BA)" = "Barium",
                              "ALUMINUM, DISSOLVED (UG/L AS AL)" = "Aluminum",
                              "BERYLLIUM, DISSOLVED (UG/L AS BE)" = "Beryllium",
                              "CADMIUM, DISSOLVED (UG/L AS CD)" = "Cadmium",
                              "CHROMIUM, DISSOLVED (UG/L AS CR)" = "Chromium",
                              "COPPER, DISSOLVED (UG/L AS CU)" = "Copper", 
                              "IRON, DISSOLVED (UG/L AS FE)" = "Iron",
                              "LEAD, DISSOLVED (UG/L AS PB)" = "Lead",
                              "MANGANESE, DISSOLVED (UG/L AS MN)" = "Manganese",
                              "THALLIUM, DISSOLVED (UG/L AS TL)" = "Thallium",
                              "NICKEL, DISSOLVED (UG/L AS NI)" = "Nickel",
                              "SILVER, DISSOLVED (UG/L AS AG)" = "Silver",
                              "ZINC, DISSOLVED (UG/L AS ZN)" = "Zinc",
                              "ANTIMONY, DISSOLVED (UG/L AS SB)" = "Antimony", 
                              "HARDNESS, CA MG CALCULATED (MG/L AS CACO3) AS DISSOLVED" = "Hardness")) %>% 
    filter(Parameter %in% c('Calcium', 'Magnesium', 'Arsenic', 'Barium', 'Beryllium', 'Cadmium', 'Chromium', 'Copper', 
                            'Iron', 'Lead', 'Manganese', 'Thallium', 'Nickel', 'Silver', 'Zinc', 'Antimony', 'Aluminum', 'Selenium', 'Hardness'))
  if(nrow(z)> 0){
    z <- bind_rows(tibble(StationID = 'FakeRow',	CollectionDateTime = NA, Longitude = NA, Latitude = NA, Calcium = NA, Magnesium = NA, 
                          Arsenic = NA, Barium = NA, Beryllium = NA, Cadmium = NA, Chromium = NA, Copper = NA, Iron = NA, Lead = NA, Manganese = NA, Thallium = NA, 
                          Nickel = NA, Silver = NA, Zinc = NA, Antimony = NA, Aluminum = NA, Selenium = NA, Hardness = NA),
                   z %>% 
                     dplyr::select(StationID = Fdt_Sta_Id, CollectionDateTime =  Fdt_Date_Time, Parameter, Ana_Uncensored_Value) %>% 
                     group_by(StationID, CollectionDateTime) %>% 
                     pivot_wider(names_from = Parameter, values_from = Ana_Uncensored_Value)  %>% 
                     left_join(dplyr::select(stationInfo_sf, StationID = STATION_ID, Longitude, Latitude) %>% 
                                 distinct(StationID, .keep_all= T) %>% st_drop_geometry(), by = 'StationID') %>% 
                     dplyr::select(StationID, CollectionDateTime,  Longitude, Latitude, everything())) %>% 
      filter(StationID != 'FakeRow')
  } else  {
    z <- tibble(StationID = NA, CollectionDateTime = NA, Longitude = NA, Latitude = NA, Calcium = NA, Magnesium = NA, 
                Arsenic = NA, Barium = NA, Beryllium = NA, Cadmium = NA, Chromium = NA, Copper = NA, Iron = NA, Lead = NA, Manganese = NA, Thallium = NA, 
                Nickel = NA, Silver = NA, Zinc = NA, Antimony = NA, Aluminum = NA, Selenium = NA, Hardness = NA)    }
  
  return(z)
}   
#BSAtoolMetalsFunction(station, stationInfo_sf, stationAnalyteDataUserFilter)


