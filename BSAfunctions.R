mCCUmetals <- c("HARDNESS, CA MG CALCULATED (MG/L AS CACO3) AS DISSOLVED", "ARSENIC, DISSOLVED  (UG/L AS AS)",
                "CHROMIUM, DISSOLVED (UG/L AS CR)", "COPPER, DISSOLVED (UG/L AS CU)",
                "LEAD, DISSOLVED (UG/L AS PB)", "NICKEL, DISSOLVED (UG/L AS NI)","ZINC, DISSOLVED (UG/L AS ZN)")



BSAtooloutputFunctionMultistation <- function(pool, station, dateRangeFilter, LRBS, conventionalsData){
  
  # Get habitat Information
  totHab <- BSAhabitatQuery(pool, station, dateRangeFilter) %>% 
    distinct(CollDate, .keep_all = T) %>% 
    dplyr::select(-c(HabParameter, HabValue))
  
  # Get LRBS information
  lrbs <- filter(LRBS, StationID %in% station & between(as.Date(Date), dateRangeFilter[1], dateRangeFilter[2])) %>% 
    dplyr::select(StationID, Date, LRBS = LRBS2) %>% mutate(DateJoin = as.Date(Date))
  
  # Get Metals Information
  metalsCCU <- dplyr::select(conventionalsData, StationID = FDT_STA_ID, `Collection Date`  = FDT_DATE_TIME, Depth = FDT_DEPTH,
                             Ana_Sam_Mrs_Container_Id_Desc, mCCUmetals) %>% 
    pivot_longer(cols = -c(StationID, `Collection Date`, Depth, Ana_Sam_Mrs_Container_Id_Desc), 
                 names_to = 'Pg_Parm_Name', values_to = 'Ana_Uncensored_Value') %>% 
    filter(!is.na(Ana_Uncensored_Value))
  if(nrow(metalsCCU) > 0){
    metalsCCUout <- tibble(StationID = 'FakeRow', `Collection Date`  = as.POSIXct(NA), 
                           Ana_Sam_Mrs_Container_Id_Desc  = as.character(NA), Depth = as.numeric(NA), MetalsCCU = as.numeric(NA))
    for(i in unique(metalsCCU$StationID)){
      metalsCCUstation <- suppressMessages(suppressWarnings(
        metalsCCU %>% 
          filter(StationID %in% i) %>% 
          mutate(Parameter = recode(`Pg_Parm_Name`,  "CALCIUM, DISSOLVED (MG/L AS CA)" = "Calcium",
                                    "ARSENIC, DISSOLVED  (UG/L AS AS)" = "Arsenic",
                                    "CHROMIUM, DISSOLVED (UG/L AS CR)" = "Chromium",
                                    "COPPER, DISSOLVED (UG/L AS CU)" = "Copper", 
                                    "LEAD, DISSOLVED (UG/L AS PB)" = "Lead",
                                    "NICKEL, DISSOLVED (UG/L AS NI)" = "Nickel",
                                    "ZINC, DISSOLVED (UG/L AS ZN)" = "Zinc",
                                    "HARDNESS, CA MG CALCULATED (MG/L AS CACO3) AS DISSOLVED" = "Hardness")) %>% 
          # dplyr::select(StationID = Fdt_Sta_Id, `Collection Date` =  Fdt_Date_Time,Ana_Sam_Mrs_Container_Id_Desc, Fdt_Depth, #Ana_Sam_Lab_Num,
          #               Parameter, Ana_Uncensored_Value) %>% 
          group_by(StationID, `Collection Date`,Ana_Sam_Mrs_Container_Id_Desc, Depth) %>%#, Ana_Sam_Lab_Num) %>% 
          pivot_wider(names_from = Parameter, values_from = Ana_Uncensored_Value) %>%
          mutate(criteriaHardness = ifelse(Hardness<25,25,ifelse(Hardness>400,400,Hardness)),
                 ArsenicChronic = Arsenic/150,
                 ChromiumChronic = Chromium/((exp(0.819*(log(criteriaHardness))+0.6848))*0.86),
                 CopperChronic = Copper/((exp(0.8545*(log(criteriaHardness))-1.702))*0.96),
                 LeadChronic = Lead/((exp(1.273*(log(criteriaHardness))-3.259))*(1.46203-(log(criteriaHardness)*(0.145712)))),
                 NickelChronic = Nickel/((exp(0.846*(log(criteriaHardness))-0.884))*0.997),
                 ZincChronic = Zinc/((exp(0.8473*(log(criteriaHardness))+0.884))*0.986)) %>% 
          summarise(MetalsCCU = sum(ArsenicChronic, ChromiumChronic, CopperChronic, LeadChronic, NickelChronic, ZincChronic, na.rm = T)) ))
      metalsCCUout <- bind_rows(metalsCCUout, metalsCCUstation) %>% 
        filter(StationID !='FakeRow')
      
    }
    
  } else {metalsCCUout <- tibble(StationID = station, `Collection Date`  = as.POSIXct(NA), 
                                 Ana_Sam_Mrs_Container_Id_Desc  = as.character(NA),Depth = as.numeric(NA),  MetalsCCU = as.numeric(NA))}
  
  
  z <- conventionalsData %>% 
    dplyr::select(one_of(c('FDT_STA_ID', 'FDT_DATE_TIME','Latitude', 'Longitude','Ana_Sam_Mrs_Container_Id_Desc', 'FDT_DEPTH', 
                           'FDT_TEMP_CELCIUS', 'FDT_FIELD_PH', 'DO_mg_L',  
                           'FDT_SPECIFIC_CONDUCTANCE','NITROGEN_mg_L', 'PHOSPHORUS_mg_L', 'SULFATE_mg_L',
                           'TDS_mg_L','CHLORIDE_mg_L', 'SODIUM, DISSOLVED (MG/L AS NA)',
                           'POTASSIUM, DISSOLVED (MG/L AS K)'))) %>% 
    mutate(DateJoin = as.Date(FDT_DATE_TIME)) %>% 
    full_join(totHab, by = c('FDT_STA_ID'= 'StationID', 'FDT_DATE_TIME' = 'CollDate')) %>% # full join in case benthic date/time doesn't match somewhere in CEDS
    full_join(lrbs, by = c('FDT_STA_ID'= 'StationID', 'DateJoin')) %>% #'FDT_DATE_TIME' = 'Date')) %>% # full join in case LRBS date/time doesn't match somewhere in CEDS
    full_join(metalsCCUout, by = c('FDT_STA_ID'= 'StationID', 'FDT_DATE_TIME' = 'Collection Date', 
                                   'FDT_DEPTH' = 'Depth', 'Ana_Sam_Mrs_Container_Id_Desc')) %>% # full join in case LRBS date/time doesn't match somewhere in CEDS
    
    
    filter(!is.na(FDT_DATE_TIME)) %>%  #drop any empty rows after joining other datasets
    # left_join(dplyr::select(stationInfo_sf, STATION_ID, Latitude, Longitude) %>% 
    #             distinct(STATION_ID, .keep_all= T) %>% 
    #             st_drop_geometry(), by = c('Fdt_Sta_Id'='STATION_ID')) %>% 
    dplyr::select(StationID = FDT_STA_ID, CollectionDateTime = FDT_DATE_TIME, Depth = FDT_DEPTH, Longitude,  Latitude,
                  `pH (unitless)` = FDT_FIELD_PH, `DO (mg/L)` = DO_mg_L, `TN (mg/L)` = NITROGEN_mg_L,
                  `TP (mg/L)` = PHOSPHORUS_mg_L, `Total Habitat (unitless)` = `Total Habitat Score`, 
                  `LRBS (unitless)` = LRBS, `MetalsCCU (unitless)` = MetalsCCU,
                  `SpCond (uS/cm)` = FDT_SPECIFIC_CONDUCTANCE, `TDS (mg/L)` = TDS_mg_L,
                  `DSulfate (mg/L)` = SULFATE_mg_L, `DChloride (mg/L)` = CHLORIDE_mg_L, 
                  `DPotassium (mg/L)` = `POTASSIUM, DISSOLVED (MG/L AS K)`, `DSodium (mg/L)` = `SODIUM, DISSOLVED (MG/L AS NA)`, 
                  `Temperature (C)` = FDT_TEMP_CELCIUS) %>% 
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


