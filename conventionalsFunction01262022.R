# Conventionals function to replace SAS method for assessments and
# create standardized dataset for many other application purposes

# Built in R 3.6.2
# source as needed for other applications

# multiple parameter clean up function
# newMethod <-  stationAnalyteDataUserFilter0 %>% 
#   
#   # filter(Ana_Sam_Fdt_Id %in% '2880155') %>% 
#   # 
#   # dplyr::select(Ana_Sam_Fdt_Id, Ana_Sam_Mrs_Container_Id_Desc, Ana_Sam_Mrs_Lcc_Parm_Group_Cd, Pg_Storet_Code, ParameterName, Ana_Uncensored_Value, Ana_Uncensored_Val_Comment) %>% 
#   group_by(Ana_Sam_Fdt_Id, Pg_Storet_Code) %>% 
#   mutate(n = n()) %>% #filter(Ana_Sam_Fdt_Id == '3001273') 
#   arrange(desc(n), Ana_Sam_Fdt_Id, Pg_Storet_Code) 

fixMultipleParameterResultsFunction <- function(newMethod){
  potentialIssues <- filter(newMethod, n > 1)
  if(nrow(potentialIssues) > 0){
    nonIssues <- filter(newMethod, n == 1)
    #i = unique(potentialIssues$Ana_Sam_Fdt_Id)[1]
    for(i in unique(potentialIssues$Ana_Sam_Fdt_Id)){
      sampleIssue <- filter(potentialIssues, Ana_Sam_Fdt_Id == i)
      
      parameterOut <- sampleIssue[0,] %>% dplyr::select(-c(Ana_Uncensored_Val_Comment, n))
      for(k in unique(sampleIssue$ParameterName)){
        parameter <- filter(sampleIssue, ParameterName == k)
        # catch in case all nondetect
        if(all(parameter$Ana_Uncensored_Val_Comment %in% c('T', 'QQ'))){
          parameter <- suppressWarnings(suppressMessages(
            parameter %>% 
              group_by(Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Fdt_Id, Ana_Sam_Mrs_Container_Id_Desc, Pg_Storet_Code, ParameterName) %>%
              summarise(Ana_Uncensored_Value = max(Ana_Uncensored_Value)) ))
          parameterOut <- bind_rows(parameterOut, parameter)
        } else {
          parameter <- parameter %>% 
            {if(any(parameter$Ana_Uncensored_Val_Comment %in% c('T', 'QQ')))
              filter(., ! Ana_Uncensored_Val_Comment %in% c('T', 'QQ'))
              else . } %>% 
            ungroup() %>% 
            group_by(Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Fdt_Id, Ana_Sam_Mrs_Container_Id_Desc, Pg_Storet_Code, ParameterName) %>%
            summarise(Ana_Uncensored_Value = median(Ana_Uncensored_Value))
          parameterOut <- bind_rows(parameterOut, parameter) }
      }
      
      fixedSample <- suppressWarnings(suppressMessages(
        parameterOut %>% 
          left_join(sampleIssue %>% 
                      ungroup() %>% 
                      group_by(Ana_Sam_Fdt_Id, Ana_Sam_Mrs_Container_Id_Desc, Pg_Storet_Code, ParameterName) %>% 
                      summarise(Ana_Uncensored_Val_Comment = paste0(Ana_Uncensored_Val_Comment, collapse = ' | ')),
                    by = c('Ana_Sam_Fdt_Id', 'Ana_Sam_Mrs_Container_Id_Desc', 'Pg_Storet_Code', 'ParameterName') )
      ))
      nonIssues <- bind_rows(nonIssues, fixedSample) 
    }
  } else {
    nonIssues <- newMethod
  }
  return(nonIssues %>% arrange(Ana_Id))
}

# From Steve, slick way to create template with all character NA values
char_name_fun=function(x){
  tibble(!!sym(x) := NA_character_)
}
dbl_name_fun=function(x){
  tibble(!!sym(x) := NA_real_)
}

# Conventionals Summary
conventionalsSummary <- function(conventionals, stationFieldDataUserFilter, stationAnalyteDataUserFilter, stationInfo, stationGIS_View, dropCodes, assessmentUse,
                                 overwriteUncensoredZeros){
  # make template to provide columns that may not exist for coalesce step
  parameterTemplate <- bind_cols(map_dfc(c("Ana_Sam_Fdt_Id", "Ana_Sam_Mrs_Container_Id_Desc"), ~char_name_fun(.x)),
                                 map_dfc(c("TOTAL_SUSPENDED_SOLIDS_00530_mg_L", "NITROGEN_TOTAL_00600_mg_L", 
                                           "NITROGEN_AMMONIA_DISSOLVED_00608_mg_L", "NITROGEN_AMMONIA_TOTAL_00610_mg_L", 
                                           "NITROGEN_NITRITE_DISSOLVED_00613_mg_L", "NITROGEN_NITRITE_TOTAL_00615_mg_L", 
                                           "NITROGEN_NITRATE_DISSOLVED_00618_mg_L", "NITROGEN_NITRATE_TOTAL_00620_mg_L", 
                                           "NITROGEN_KJELDAHL_TOTAL_00625_mg_L", "NITRITE+NITRATE_TOTAL_00630_mg_L", 
                                           "NITRITE+NITRATE_DISSOLVED_00631_mg_L", "PHOSPHORUS_TOTAL_00665_mg_L", 
                                           "PHOSPHORUS_DISSOLVED_00666_mg_L", "PHOSPHORUS_DISSOLVED_ORTHOPHOSPHATE_00671_mg_L", 
                                           "HARDNESS_TOTAL_00900_mg_L", "CHLORIDE_TOTAL_00940_mg_L", 
                                           "CHLORIDE_DISSOLVED_00941_mg_L", "SULFATE_TOTAL_00945_mg_L", 
                                           "SULFATE_DISSOLVED_00946_mg_L", "FECAL_COLIFORM_31616_NO_100mL", 
                                           "E._COLI_31648_NO_100mL", "ENTEROCOCCI_31649_NO_100mL", 
                                           "CHLOROPHYLL_32211_ug_L", "PHOSPHOROUS_PARTICULATE_49567_mg_L", 
                                           "NITROGEN_PARTICULATE_49570_mg_L", "NITROGEN_TOTAL_DISSOLVED_49571_mg_L", 
                                           "PHOSPHOROUS_TOTAL_DISSOLVED_49572_mg_L", "PHOSPHORUS_TOTAL_ORTHOPHOSPHATE_70507_mg_L", 
                                           "E.COLI_ECOLI_CFU_100mL", "ORTHOPHOSPHATE_DISSOLVED_OPWLF_mg_L", 
                                           "PHOSPHORUS_SUSPENDED_INORGANIC_PIPLF_mg_L", "PHOSPHORUS_PARTICULATE_PPWLF_mg_L", 
                                           "SSC-TOTAL_00530_mg_L", "NITROGEN_TOTAL_DISSOLVED_TDNLF_mg_L", 
                                           "PHOSPHORUS_TOTAL_DISSOLVED_TDPLF_mg_L", "TOTAL_SUSPENDED_SOLIDS_TSS45_mg_L",
                                           # added for probmon
                                           "TDS_mg_L", "SSC%Finer", "SSC_COARSE", "SSC_FINE", "SSC_TOTAL",
                                           
                                           'BENTHIC ASH FREE DRY MASS, GM/M2', 'BENTHIC CHLOROPHYLL A, MG/M2' ),  ~dbl_name_fun(.x)) )
  remarkTemplate <-  map_dfc(c("Ana_Sam_Fdt_Id", "Ana_Sam_Mrs_Container_Id_Desc", "RMK_00530", "RMK_00600", "RMK_00608", "RMK_00610", 
                               "RMK_00613", "RMK_00615", "RMK_00618", "RMK_00620", "RMK_00625", "RMK_00630", "RMK_00631", "RMK_00665", 
                               "RMK_00666", "RMK_00671", "RMK_00900", "RMK_00940", "RMK_00941", "RMK_00945", "RMK_00946", "RMK_31616", 
                               "RMK_31648", "RMK_31649", "RMK_32211", "RMK_49567", "RMK_49570", "RMK_49571", "RMK_49572", "RMK_70507", 
                               "RMK_ECOLI", "RMK_OPWLF", "RMK_PIPLF", "RMK_PPWLF", "RMK_SSC_TOTAL_00530","RMK_TDNLF","RMK_TDPLF","RMK_TSS45",
                               # added for probmon
                               "RMK_70300", "RMK_70331", "RMK_SSC-COARSE", "RMK_SSC-FINE", "RMK_SSC-TOTAL",
                               
                               'RMK_BENCM2', 'RMK_CHLCM2'  ), ~char_name_fun(.x))
    
  # Step 1: Organize station information to match conventionals format
  stationData <- left_join(stationInfo, stationGIS_View, by = c('Sta_Id' = 'Station_Id')) %>%
    
    # special step if data organization method being used for assessment. TRUE drops program codes that aren't valid for assessment purposes. 
    #  FALSE allows data organization method to be used by multiple applications
    {if(assessmentUse == TRUE)
      filter(., ! Sta_Lv1_Code %in% c('LND', 'PIPE', 'UNK', 'WELL')) %>% #drop unwanted Level 1 Codes
        filter(! Sta_Lv2_Code %in% c('INPLNT', 'NONAMB', 'TREATD', 'SEWER')) %>% #drop unwanted Level 2 Codes
        filter(! Sta_Lv3_Code %in% c('IR', 'IND', 'AGRI'))   #drop unwanted Level 3 Codes
      else . } %>% 
    dplyr::select(FDT_STA_ID = Sta_Id, 
                  STA_DESC = Sta_Desc,
                  Deq_Region = Admin_Region, # EVJ change bc Roger has opposite
                  STA_REC_CODE = Sta_Rec_Code, # EVJ change bc Roger has opposite
                  Latitude, Longitude, Huc6_Huc_8, Huc6_Huc_8_Name, Huc6_Name, Huc6_Vahu5, Huc6_Huc_12, Huc6_Huc_12_Name, Huc6_Vahu6, 
                  STA_LV1_CODE = Sta_Lv1_Code, 
                  LV1_DESCRIPTION = Lv1_Description,
                  STA_LV2_CODE = Sta_Lv2_Code,
                  LV2_DESCRIPTION = Lv2_Description, 
                  STA_LV3_CODE = Sta_Lv3_Code, 
                  LV3_DESCRIPTION = Lv3_Description, 
                  STA_CBP_NAME = Sta_Cbp_Name) %>% 
  mutate(GROUP_STA_ID = as.character(NA),
         Waterbody = as.character(NA),
         OTHER_CITMON_NONAGENCY_INFO = as.character(NA),
         Data_Source = as.character(NA))
  

  if(nrow(stationData) > 0){                             
    
    # Step 2: Make field data match conventionals format
    stationFieldDataUserFilter1 <- stationFieldDataUserFilter %>% 
     
       # special step if data organization method being used for assessment. TRUE drops program codes that aren't valid for assessment purposes. 
      #  FALSE allows data organization method to be used by multiple applications
      {if(assessmentUse == TRUE)
        filter(., ! Fdt_Spg_Code %in% c('IR', 'PC', 'FI')) %>% # exclude targeted incident response and facility data by survey program code
          # drop undesired comment codes
          filter(! grepl('= S|=S|storm|target|duplicate|blank', Fdt_Comment, ignore.case = TRUE))  
        else . } %>% 
      
      # Change field measurements to NA if Comment field in dropCodes
      mutate(
        Fdt_Air_Per_Sat = ifelse(coalesce(Fdt_Air_Per_Sat_Rmk, Air_Per_Sat_Remark) %in% dropCodes, NA, Fdt_Air_Per_Sat),
        Fdt_Air_Temp = ifelse(coalesce(Fdt_Air_Temp_Rmk, Air_Temp_Remark) %in% dropCodes, NA, Fdt_Air_Temp),
        Fdt_Baro_Pressure = ifelse(coalesce(Fdt_Baro_Pressure_Rmk, Baro_Pressure_Remark) %in% dropCodes, NA, Fdt_Baro_Pressure),
        Fdt_Chlorine_Residual = ifelse(coalesce(Fdt_Chlorine_Residual_Rmk, Chlorine_Residual_Remark) %in% dropCodes, NA, Fdt_Chlorine_Residual),
        Fdt_Conductivity = ifelse(coalesce(Fdt_Conductivity_Rmk, Conductivity_Remark) %in% dropCodes, NA, Fdt_Conductivity),
        Fdt_Do_Optical = ifelse(coalesce(Fdt_Do_Optical_Rmk, Do_Optical_Remark) %in% dropCodes, NA, Fdt_Do_Optical),
        Fdt_Do_Probe = ifelse(coalesce(Fdt_Do_Probe_Rmk, Do_Probe_Remark) %in% dropCodes, NA, Fdt_Do_Probe),
        Fdt_Do_Winkler = ifelse(coalesce(Fdt_Do_Winkler_Rmk, Do_Winkler_Remark) %in% dropCodes, NA, Fdt_Do_Winkler),
        Fdt_Field_Ph = ifelse(coalesce(Fdt_Field_Ph_Rmk, Field_Ph_Remark) %in% dropCodes, NA, Fdt_Field_Ph),
        Fdt_Flow_Cfs = ifelse(coalesce(Fdt_Flow_Cfs_Rmk, Flow_Cfs_Remark) %in% dropCodes, NA, Fdt_Flow_Cfs),
        Fdt_Gauge_Height = ifelse(coalesce(Fdt_Gauge_Height_Rmk, Gauge_Height_Remark) %in% dropCodes, NA, Fdt_Gauge_Height),
        Fdt_Salinity = ifelse(coalesce(Fdt_Salinity_Rmk, Salinity_Remark) %in% dropCodes, NA, Fdt_Salinity),
        Fdt_Secchi_Depth = ifelse(coalesce(Fdt_Secchi_Depth_Rmk, Secchi_Depth_Remark) %in% dropCodes, NA, Fdt_Secchi_Depth),
        Fdt_Specific_Conductance = ifelse(coalesce(Fdt_Specific_Conductance_Rmk, Specific_Conductance_Remark) %in% dropCodes, NA, Fdt_Specific_Conductance),
        Fdt_Temp_Celcius = ifelse(coalesce(Fdt_Temp_Celcius_Rmk, Temp_Celcius_Remark) %in% dropCodes, NA, Fdt_Temp_Celcius),
        Fdt_Turbidity = ifelse(coalesce(Fdt_Turbidity_Rmk, Turbidity_Remark) %in% dropCodes, NA, Fdt_Turbidity)) %>% 
      rename_with( toupper) %>% 
      mutate(RMK_FDT_FIELD_PH = FDT_FIELD_PH_RMK, LEVEL_FDT_FIELD_PH = as.character(NA),
             RMK_FDT_TEMP_CELCIUS = FDT_TEMP_CELCIUS_RMK, LEVEL_FDT_TEMP_CELCIUS = as.character(NA),
             DO_mg_L = coalesce( FDT_DO_PROBE, FDT_DO_OPTICAL, FDT_DO_WINKLER), # double check logic here
             RMK_DO = coalesce(FDT_DO_PROBE_RMK, FDT_DO_OPTICAL_RMK, FDT_DO_WINKLER_RMK), # double check logic here
             LEVEL_DO = as.character(NA),
             DISSOLVED_OXYGEN_00300_mg_L = FDT_DO_PROBE, RMK_FDT_DO_PROBE = FDT_DO_PROBE_RMK, LEVEL_FDT_DO_PROBE = as.character(NA),
             DISSOLVED_OXYGEN_DOOPT_mg_L = FDT_DO_OPTICAL, RMK_FDT_DO_OPTICAL = FDT_DO_OPTICAL_RMK, LEVEL_FDT_DO_OPTICAL = as.character(NA),
             DISSOLVED_OXYGEN_WINK_mg_L =  FDT_DO_WINKLER, RMK_FDT_DO_WINKLER = FDT_DO_WINKLER_RMK, LEVEL_FDT_DO_WINKLER = as.character(NA),
             RMK_FDT_SPECIFIC_CONDUCTANCE = FDT_SPECIFIC_CONDUCTANCE_RMK, LEVEL_FDT_SPECIFIC_CONDUCTANCE = as.character(NA),
             RMK_FDT_SALINITY = FDT_SALINITY_RMK, LEVEL_FDT_SALINITY = as.character(NA), 
             SECCHI_DEPTH_M = FDT_SECCHI_DEPTH, RMK_SECCHI_DEPTH = FDT_SECCHI_DEPTH_RMK, LEVEL_SECCHI_DEPTH = as.character(NA)) %>% 
      dplyr::select(
        FDT_ID, # necessary for joining field to analyte data
        FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, FDT_DEPTH_DESC, FDT_PERCENT_FRB, FDT_SSC_CODE, FDT_SPG_CODE, FDT_COMMENT,
        FDT_FIELD_PH, RMK_FDT_FIELD_PH, LEVEL_FDT_FIELD_PH, FDT_TEMP_CELCIUS, RMK_FDT_TEMP_CELCIUS, LEVEL_FDT_TEMP_CELCIUS, 
        DO_mg_L, RMK_DO, LEVEL_DO, DISSOLVED_OXYGEN_00300_mg_L, RMK_FDT_DO_PROBE, LEVEL_FDT_DO_PROBE, DISSOLVED_OXYGEN_DOOPT_mg_L,
        RMK_FDT_DO_OPTICAL, LEVEL_FDT_DO_OPTICAL, DISSOLVED_OXYGEN_WINK_mg_L, RMK_FDT_DO_WINKLER, LEVEL_FDT_DO_WINKLER, FDT_SPECIFIC_CONDUCTANCE,
        RMK_FDT_SPECIFIC_CONDUCTANCE, LEVEL_FDT_SPECIFIC_CONDUCTANCE, FDT_SALINITY, RMK_FDT_SALINITY, LEVEL_FDT_SALINITY, 
        SECCHI_DEPTH_M, RMK_SECCHI_DEPTH, LEVEL_SECCHI_DEPTH)
    
    
    # Step 3: Make Analytes match conventionals format, multistep process for remarks and data
    # Step 3.1: drop certain lab codes
    stationAnalyteDataUserFilter <- filter(stationAnalyteDataUserFilter, Ana_Lab_Seq_Num == 1) %>% # drop analyte lab sequence number != 1
      mutate(Ana_Uncensored_Value = case_when(Ana_Com_Code %in% c('IF', 'J', 'O', 'PE', 'Q1', 'QF', 'QFQ', 'V') ~ as.numeric(NA), 
                                              TRUE ~ Ana_Uncensored_Value)) %>% # drop results from invalid lab codes
      filter(Ana_Sam_Mrs_Container_Id_Desc %in% c('', 'C', 'H', 'HV', 'R', 'S1', 'S2','V', 'EB')) # only keep samples with select container descriptions
    

    # Step 3.2: Rename by storet codes to match conventionals format
    stationAnalyteDataUserFilter0 <- stationAnalyteDataUserFilter %>% 
      
      # only keep data we will do something with to avoid unnecessary data duplication
      filter(Pg_Storet_Code %in% c('00530', '00600', '00608', '00610', '00613' ,'00615', '00618', '00620',
                                   '00625', '00630', '00631', '00665', '00666', '00671', '00900', '00940',
                                   '00941', '00945', '00946', '31616', '31648', '31649', '32211', '49567',
                                   '49570', '49571', '49572', '70507', 'ECOLI', 'OPWLF', 'PIPLF', 'PPWLF',
                                   'SSC-TOTAL', 'TDNLF', 'TDPLF', 'TSS45',
                                   
                                   # metals for assessment
                                   '01106','01095','01000','01005','01010','01025','00915','01030','01040',
                                   'DHARD','01046','01049','00925','01056','01065','00935','01145','01075',
                                   '00930','01080','01057','01090',
                                   
                                   # others
                                   '00681','00680','50091','50092','82079', 
                                   '70300', '70331', 'SSC-COARSE', 'SSC-FINE', 'SSC-TOTAL',
                                   
                                   'BENCM2', 'CHLCM2' )) %>% 
      
      mutate(ParameterName = case_when(Pg_Storet_Code == '00530' ~ 'TOTAL_SUSPENDED_SOLIDS_00530_mg_L',
                                       Pg_Storet_Code == '00600' ~ 'NITROGEN_TOTAL_00600_mg_L',
                                       Pg_Storet_Code == '00608' ~ 'NITROGEN_AMMONIA_DISSOLVED_00608_mg_L',
                                       Pg_Storet_Code == '00610' ~ 'NITROGEN_AMMONIA_TOTAL_00610_mg_L',
                                       Pg_Storet_Code == '00613' ~ 'NITROGEN_NITRITE_DISSOLVED_00613_mg_L',
                                       Pg_Storet_Code == '00615' ~ 'NITROGEN_NITRITE_TOTAL_00615_mg_L',
                                       Pg_Storet_Code == '00618' ~ 'NITROGEN_NITRATE_DISSOLVED_00618_mg_L',
                                       Pg_Storet_Code == '00620' ~ 'NITROGEN_NITRATE_TOTAL_00620_mg_L',
                                       Pg_Storet_Code == '00625' ~ 'NITROGEN_KJELDAHL_TOTAL_00625_mg_L',
                                       Pg_Storet_Code == '00630' ~ 'NITRITE+NITRATE_TOTAL_00630_mg_L',
                                       Pg_Storet_Code == '00631' ~ 'NITRITE+NITRATE_DISSOLVED_00631_mg_L',
                                       Pg_Storet_Code == '00665' ~ 'PHOSPHORUS_TOTAL_00665_mg_L',
                                       Pg_Storet_Code == '00666' ~ 'PHOSPHORUS_DISSOLVED_00666_mg_L',
                                       Pg_Storet_Code == '00671' ~ 'PHOSPHORUS_DISSOLVED_ORTHOPHOSPHATE_00671_mg_L',
                                       Pg_Storet_Code == '00900' ~ 'HARDNESS_TOTAL_00900_mg_L',
                                       Pg_Storet_Code == '00940' ~ 'CHLORIDE_TOTAL_00940_mg_L',
                                       Pg_Storet_Code == '00941' ~ 'CHLORIDE_DISSOLVED_00941_mg_L',
                                       Pg_Storet_Code == '00945' ~ 'SULFATE_TOTAL_00945_mg_L',
                                       Pg_Storet_Code == '00946' ~ 'SULFATE_DISSOLVED_00946_mg_L',
                                       Pg_Storet_Code == '31616' ~ 'FECAL_COLIFORM_31616_NO_100mL',
                                       Pg_Storet_Code == '31648' ~ 'E._COLI_31648_NO_100mL',
                                       Pg_Storet_Code == '31649' ~ 'ENTEROCOCCI_31649_NO_100mL',
                                       Pg_Storet_Code == '32211' ~ 'CHLOROPHYLL_32211_ug_L',
                                       Pg_Storet_Code == '49567' ~ 'PHOSPHOROUS_PARTICULATE_49567_mg_L',
                                       Pg_Storet_Code == '49570' ~ 'NITROGEN_PARTICULATE_49570_mg_L',
                                       Pg_Storet_Code == '49571' ~ 'NITROGEN_TOTAL_DISSOLVED_49571_mg_L',
                                       Pg_Storet_Code == '49572' ~ 'PHOSPHOROUS_TOTAL_DISSOLVED_49572_mg_L',
                                       Pg_Storet_Code == '70507' ~ 'PHOSPHORUS_TOTAL_ORTHOPHOSPHATE_70507_mg_L',
                                       Pg_Storet_Code == 'ECOLI' ~ 'E.COLI_ECOLI_CFU_100mL',
                                       Pg_Storet_Code == 'OPWLF' ~ 'ORTHOPHOSPHATE_DISSOLVED_OPWLF_mg_L',
                                       Pg_Storet_Code == 'PIPLF' ~ 'PHOSPHORUS_SUSPENDED_INORGANIC_PIPLF_mg_L',
                                       Pg_Storet_Code == 'PPWLF' ~ 'PHOSPHORUS_PARTICULATE_PPWLF_mg_L',
                                       Pg_Storet_Code == 'SSC-TOTAL' ~ 'SSC-TOTAL_00530_mg_L',
                                       Pg_Storet_Code == 'TDNLF' ~ 'NITROGEN_TOTAL_DISSOLVED_TDNLF_mg_L',
                                       Pg_Storet_Code == 'TDPLF' ~ 'PHOSPHORUS_TOTAL_DISSOLVED_TDPLF_mg_L',
                                       Pg_Storet_Code == 'TSS45' ~ 'TOTAL_SUSPENDED_SOLIDS_TSS45_mg_L',
                                       
                                       # metals for assessment
                                       Pg_Storet_Code == '01106' ~ 'ALUMINUM, DISSOLVED (UG/L AS AL)',
                                       Pg_Storet_Code == '01095' ~ 'ANTIMONY, DISSOLVED (UG/L AS SB)',
                                       Pg_Storet_Code == '01000' ~ 'ARSENIC, DISSOLVED  (UG/L AS AS)',
                                       Pg_Storet_Code == '01005' ~ 'BARIUM, DISSOLVED (UG/L AS BA)',
                                       Pg_Storet_Code == '01010' ~ 'BERYLLIUM, DISSOLVED (UG/L AS BE)',
                                       Pg_Storet_Code == '01025' ~ 'CADMIUM, DISSOLVED (UG/L AS CD)',
                                       Pg_Storet_Code == '00915' ~ 'CALCIUM, DISSOLVED (MG/L AS CA)',
                                       Pg_Storet_Code == '01030' ~ 'CHROMIUM, DISSOLVED (UG/L AS CR)',
                                       Pg_Storet_Code == '01040' ~ 'COPPER, DISSOLVED (UG/L AS CU)',
                                       Pg_Storet_Code == 'DHARD' ~ 'HARDNESS, CA MG CALCULATED (MG/L AS CACO3) AS DISSOLVED',
                                       Pg_Storet_Code == '01046' ~ 'IRON, DISSOLVED (UG/L AS FE)',
                                       Pg_Storet_Code == '01049' ~ 'LEAD, DISSOLVED (UG/L AS PB)',
                                       Pg_Storet_Code == '00925' ~ 'MAGNESIUM, DISSOLVED (MG/L AS MG)',
                                       Pg_Storet_Code == '01056' ~ 'MANGANESE, DISSOLVED (UG/L AS MN)',
                                       Pg_Storet_Code == '01065' ~ 'NICKEL, DISSOLVED (UG/L AS NI)',
                                       Pg_Storet_Code == '00935' ~ 'POTASSIUM, DISSOLVED (MG/L AS K)',
                                       Pg_Storet_Code == '01145' ~ 'SELENIUM, DISSOLVED (UG/L AS SE)',
                                       Pg_Storet_Code == '01075' ~ 'SILVER, DISSOLVED (UG/L AS AG)',
                                       Pg_Storet_Code == '00930' ~ 'SODIUM, DISSOLVED (MG/L AS NA)',
                                       Pg_Storet_Code == '01080' ~ 'STRONTIUM, DISSOLVED (UG/L AS SR)',
                                       Pg_Storet_Code == '01057' ~ 'THALLIUM, DISSOLVED (UG/L AS TL)',
                                       Pg_Storet_Code == '01090' ~ 'ZINC, DISSOLVED (UG/L AS ZN)',
                                       
                                       # others
                                       Pg_Storet_Code == '00681' ~ 'CARBON, DISSOLVED ORGANIC (MG/L AS C)',
                                       Pg_Storet_Code == '00680' ~ 'CARBON, TOTAL ORGANIC (MG/L AS C)',
                                       Pg_Storet_Code == '50091' ~ 'MERCURY-TL,FILTERED WATER,ULTRATRACE METHOD NG/L',
                                       Pg_Storet_Code == '50092' ~ 'MERCURY-TL,UNFILTERED WATER,ULTRATRACE METHOD NG/L',
                                       Pg_Storet_Code == '82079' ~ 'TURBIDITY,LAB NEPHELOMETRIC TURBIDITY UNITS, NTU',
                                       
                                       Pg_Storet_Code == '70300' ~ 'TDS_mg_L',
                                       Pg_Storet_Code == '70331' ~ 'SSC%Finer',
                                       Pg_Storet_Code == 'SSC-COARSE' ~ 'SSC_COARSE',
                                       Pg_Storet_Code == 'SSC-FINE' ~ 'SSC_FINE',
                                       
                                       Pg_Storet_Code == 'BENCM2' ~ 'BENTHIC ASH FREE DRY MASS, GM/M2',
                                       Pg_Storet_Code == 'CHLCM2' ~ 'BENTHIC CHLOROPHYLL A, MG/M2',
                                       
                                       TRUE ~ as.character(Pg_Storet_Code))) 
    
    # Step 3.2.0 Overwrite 0 if reported in Ana_Uncensored_Value field
    # sometimes the lab reports "0" for the uncensored value, which can throw off analyses. If this option is selected, users
    #  will instead see the Ana_Value for that parameter value
    if(overwriteUncensoredZeros == TRUE){
      stationAnalyteDataUserFilter0 <- stationAnalyteDataUserFilter0 %>% 
        mutate(Ana_Uncensored_Value = case_when(Ana_Uncensored_Value == 0 ~ Ana_Value,
                                                TRUE ~ Ana_Uncensored_Value))
    }
    
    
    
    # Step 3.2.1: Fix issues associated with multiple parameter values returned for a single sample for same parameter (when multiple lab group codes call for same analysis)
    # This step averages only the values that are not non detects (T, QQ) only when multiple parameters exist for the same storet code
    # All remark codes are concatenated regardless of non detect status
    stationAnalyteDataUserFilter0_0 <-  suppressMessages(suppressWarnings(
      stationAnalyteDataUserFilter0 %>% 
      group_by(Ana_Sam_Fdt_Id, Pg_Storet_Code, Ana_Sam_Mrs_Container_Id_Desc) %>% 
      mutate(n = n()) %>% 
      arrange(desc(n), Ana_Sam_Fdt_Id, Pg_Storet_Code, ) %>% 
      fixMultipleParameterResultsFunction() ))
    
    # Step 3.3: Organize lab data
    stationAnalyteDataUserFilter1 <- bind_rows(parameterTemplate, 
                                               suppressWarnings(
                                                 stationAnalyteDataUserFilter0_0 %>% 
                                                 #stationAnalyteDataUserFilter0 %>% 
                                                   dplyr::select(Ana_Sam_Fdt_Id, Ana_Sam_Mrs_Container_Id_Desc,  ParameterName, Ana_Uncensored_Value) %>% 
                                                   pivot_wider(id_cols = c(Ana_Sam_Fdt_Id, Ana_Sam_Mrs_Container_Id_Desc),
                                                               names_from = ParameterName, values_from = Ana_Uncensored_Value, values_fn = list()) %>% 
                                                   unnest(-Ana_Sam_Fdt_Id)) ) %>% 
      rowwise() %>% 
      # For sum in this step, use bazar::sumNA to return NA when none of the fields are populated (instead of 0 which is returned using sum(..., na.rm=T))
      mutate(ECOLI = coalesce(`E.COLI_ECOLI_CFU_100mL`, `E._COLI_31648_NO_100mL`),
             NITROGEN_mg_L = coalesce(NITROGEN_TOTAL_00600_mg_L,
                                      bazar::sumNA(NITROGEN_KJELDAHL_TOTAL_00625_mg_L, `NITRITE+NITRATE_TOTAL_00630_mg_L`, na.rm = F)),
             NITROGEN_mg_L = ifelse(is.na(NITROGEN_TOTAL_00600_mg_L) && !is.na(NITROGEN_NITRITE_TOTAL_00615_mg_L) && 
                                      !is.na(NITROGEN_NITRATE_TOTAL_00620_mg_L) && !is.na(NITROGEN_KJELDAHL_TOTAL_00625_mg_L), 
                                    bazar::sumNA(NITROGEN_NITRITE_TOTAL_00615_mg_L, NITROGEN_NITRATE_TOTAL_00620_mg_L, 
                                                 NITROGEN_KJELDAHL_TOTAL_00625_mg_L, na.rm = F), 
                                    NITROGEN_mg_L),  # Is this even right? Roger doesn't provide an alternate option
             NITROGEN_mg_L = ifelse(is.na(NITROGEN_TOTAL_00600_mg_L) && !is.na(NITROGEN_PARTICULATE_49570_mg_L) && 
                                      !is.na(NITROGEN_TOTAL_DISSOLVED_49571_mg_L), 
                                    bazar::sumNA(NITROGEN_PARTICULATE_49570_mg_L, NITROGEN_TOTAL_DISSOLVED_49571_mg_L, na.rm = F), 
                                    NITROGEN_mg_L),  # Is this even right? Roger doesn't provide an alternate option
             NOX_mg_L = ifelse(is.na(`NITRITE+NITRATE_TOTAL_00630_mg_L`), 
                               bazar::sumNA(NITROGEN_NITRITE_TOTAL_00615_mg_L, NITROGEN_NITRATE_TOTAL_00620_mg_L, na.rm = F),
                               `NITRITE+NITRATE_TOTAL_00630_mg_L`), # Is this even right? Roger doesn't provide an alternate option if NITRITE+NITRATE_TOTAL_00630_mg_L exists so just assuming things here
             AMMONIA_mg_L = coalesce(NITROGEN_AMMONIA_TOTAL_00610_mg_L, NITROGEN_AMMONIA_DISSOLVED_00608_mg_L),
             # secondary AMMONIA_mg_L adjustment based on remark codes when dataset later combined with remarks
             NITRATE_mg_L = coalesce(NITROGEN_NITRATE_TOTAL_00620_mg_L, NITROGEN_NITRATE_DISSOLVED_00618_mg_L),
             
             
             ## Phosphorus, how it is written in Conventionals with overwriting previous data
             PHOSPHORUS_mg_L = coalesce(PHOSPHORUS_TOTAL_00665_mg_L, PHOSPHORUS_TOTAL_ORTHOPHOSPHATE_70507_mg_L, 
                                        PHOSPHORUS_DISSOLVED_00666_mg_L, PHOSPHORUS_DISSOLVED_ORTHOPHOSPHATE_00671_mg_L),
             PHOSPHORUS_mg_L = ifelse(is.na(PHOSPHORUS_TOTAL_00665_mg_L) && !is.na(PHOSPHOROUS_PARTICULATE_49567_mg_L) &&
                                        !is.na(PHOSPHOROUS_TOTAL_DISSOLVED_49572_mg_L),
                                      bazar::sumNA(PHOSPHOROUS_PARTICULATE_49567_mg_L, PHOSPHOROUS_TOTAL_DISSOLVED_49572_mg_L, na.rm = F),
                                      PHOSPHORUS_mg_L), # should the order really be total, ortho, then dissolved???
             
             # ## Phosphorus, how it should be?
             # PHOSPHORUS_mg_L = coalesce(PHOSPHORUS_TOTAL_00665_mg_L, PHOSPHORUS_TOTAL_ORTHOPHOSPHATE_70507_mg_L, 
             #                            PHOSPHORUS_DISSOLVED_00666_mg_L, PHOSPHORUS_DISSOLVED_ORTHOPHOSPHATE_00671_mg_L),
             # PHOSPHORUS_mg_L = ifelse(is.na(PHOSPHORUS_mg_L) && !is.na(PHOSPHOROUS_PARTICULATE_49567_mg_L) &&
             #                            !is.na(PHOSPHOROUS_TOTAL_DISSOLVED_49572_mg_L),
             #                          bazar::sumNA(PHOSPHOROUS_PARTICULATE_49567_mg_L, PHOSPHOROUS_TOTAL_DISSOLVED_49572_mg_L, na.rm = F),
             #                          PHOSPHORUS_mg_L), # should the order really be total, ortho, then dissolved???
             # 
             
             TSS_mg_L = coalesce(TOTAL_SUSPENDED_SOLIDS_00530_mg_L, `SSC-TOTAL_00530_mg_L`, TOTAL_SUSPENDED_SOLIDS_TSS45_mg_L),
             # Chloride logic not spelled out in Roger's code but how his scripts work
             CHLORIDE_mg_L = coalesce(CHLORIDE_TOTAL_00940_mg_L, CHLORIDE_DISSOLVED_00941_mg_L), 
             CHLORIDE_mg_L = ifelse(!is.na(CHLORIDE_DISSOLVED_00941_mg_L) && !is.na(CHLORIDE_TOTAL_00940_mg_L) &&
                                      CHLORIDE_DISSOLVED_00941_mg_L > CHLORIDE_TOTAL_00940_mg_L, CHLORIDE_DISSOLVED_00941_mg_L, CHLORIDE_mg_L),
             # Sulfate logic not spelled out in Roger's code but how his scripts work
             SULFATE_mg_L = coalesce(SULFATE_TOTAL_00945_mg_L, SULFATE_DISSOLVED_00946_mg_L), 
             SULFATE_mg_L = ifelse(!is.na(SULFATE_DISSOLVED_00946_mg_L) && !is.na(SULFATE_TOTAL_00945_mg_L) &&
                                      SULFATE_DISSOLVED_00946_mg_L > SULFATE_TOTAL_00945_mg_L, SULFATE_DISSOLVED_00946_mg_L, SULFATE_mg_L),
             
             # EVJ added these even though not called for in this part of conventionals, no idea where comes from otherwise
             SULFATE_TOTAL_mg_L = SULFATE_TOTAL_00945_mg_L,
             SULFATE_DISS_mg_L = SULFATE_DISSOLVED_00946_mg_L,
             ECOLI_31648_NO_100mL = `E._COLI_31648_NO_100mL`,
             ENTEROCOCCI = ENTEROCOCCI_31649_NO_100mL,
             FECAL_COLI = FECAL_COLIFORM_31616_NO_100mL,
             CHLOROPHYLL_A_ug_L = CHLOROPHYLL_32211_ug_L,
             SSC_mg_L = `SSC-TOTAL_00530_mg_L`,
             SSC_TOTAL = `SSC-TOTAL_00530_mg_L`) # for probmon
      # now limit to just necessary columns for Conventionals format
    stationAnalyteDataUserFilter1conventionals <- dplyr::select(stationAnalyteDataUserFilter1, #Ana_Id, 
        Ana_Sam_Fdt_Id, Ana_Sam_Mrs_Container_Id_Desc,
        NITROGEN_mg_L,  AMMONIA_mg_L, NITRATE_mg_L, NOX_mg_L, NITROGEN_TOTAL_00600_mg_L, NITROGEN_AMMONIA_DISSOLVED_00608_mg_L,
        NITROGEN_AMMONIA_TOTAL_00610_mg_L, NITROGEN_NITRITE_DISSOLVED_00613_mg_L, NITROGEN_NITRITE_TOTAL_00615_mg_L,
        NITROGEN_NITRATE_DISSOLVED_00618_mg_L, NITROGEN_NITRATE_TOTAL_00620_mg_L, NITROGEN_KJELDAHL_TOTAL_00625_mg_L,
        `NITRITE+NITRATE_TOTAL_00630_mg_L`, `NITRITE+NITRATE_DISSOLVED_00631_mg_L`, NITROGEN_PARTICULATE_49570_mg_L,
        NITROGEN_TOTAL_DISSOLVED_49571_mg_L, NITROGEN_TOTAL_DISSOLVED_TDNLF_mg_L, PHOSPHORUS_mg_L, PHOSPHORUS_TOTAL_00665_mg_L, 
        PHOSPHORUS_DISSOLVED_00666_mg_L, PHOSPHORUS_DISSOLVED_ORTHOPHOSPHATE_00671_mg_L, PHOSPHOROUS_PARTICULATE_49567_mg_L,
        PHOSPHOROUS_TOTAL_DISSOLVED_49572_mg_L, PHOSPHORUS_TOTAL_ORTHOPHOSPHATE_70507_mg_L, ORTHOPHOSPHATE_DISSOLVED_OPWLF_mg_L, 
        PHOSPHORUS_SUSPENDED_INORGANIC_PIPLF_mg_L, PHOSPHORUS_PARTICULATE_PPWLF_mg_L, PHOSPHORUS_TOTAL_DISSOLVED_TDPLF_mg_L,
        HARDNESS_TOTAL_00900_mg_L, CHLORIDE_mg_L, CHLORIDE_TOTAL_00940_mg_L, CHLORIDE_DISSOLVED_00941_mg_L,
        SULFATE_mg_L, SULFATE_TOTAL_mg_L, SULFATE_DISS_mg_L, ECOLI, ECOLI_31648_NO_100mL, ENTEROCOCCI, FECAL_COLI, 
        CHLOROPHYLL_A_ug_L, TSS_mg_L, TOTAL_SUSPENDED_SOLIDS_00530_mg_L, SSC_mg_L, TOTAL_SUSPENDED_SOLIDS_TSS45_mg_L) %>% 
      filter(! is.na(Ana_Sam_Fdt_Id)) #%>%  # drop blank row from row_bind
      # double check things
      #left_join(dplyr::select(stationFieldDataUserFilter1, FDT_ID:FDT_DEPTH_DESC), by = c("Ana_Sam_Fdt_Id"= "FDT_ID")) %>% 
      #dplyr::select(FDT_STA_ID:FDT_DEPTH_DESC, everything())
    
    # everything
    stationAnalyteDataUserFilter1all <- stationAnalyteDataUserFilter1 %>% 
      dplyr::select(any_of(c(#Ana_Id, 
        'Ana_Sam_Fdt_Id', 'Ana_Sam_Mrs_Container_Id_Desc',
        # metals for assessment
        'ALUMINUM, DISSOLVED (UG/L AS AL)', 'ANTIMONY, DISSOLVED (UG/L AS SB)', 'ARSENIC, DISSOLVED  (UG/L AS AS)', 'BARIUM, DISSOLVED (UG/L AS BA)',
        'BERYLLIUM, DISSOLVED (UG/L AS BE)', 'CADMIUM, DISSOLVED (UG/L AS CD)', 'CALCIUM, DISSOLVED (MG/L AS CA)', 'CHROMIUM, DISSOLVED (UG/L AS CR)',
        'COPPER, DISSOLVED (UG/L AS CU)', 'HARDNESS, CA MG CALCULATED (MG/L AS CACO3) AS DISSOLVED', 'IRON, DISSOLVED (UG/L AS FE)', 'LEAD, DISSOLVED (UG/L AS PB)',
        'MAGNESIUM, DISSOLVED (MG/L AS MG)', 'MANGANESE, DISSOLVED (UG/L AS MN)', 'NICKEL, DISSOLVED (UG/L AS NI)', 'POTASSIUM, DISSOLVED (MG/L AS K)',
        'SELENIUM, DISSOLVED (UG/L AS SE)', 'SILVER, DISSOLVED (UG/L AS AG)', 'SODIUM, DISSOLVED (MG/L AS NA)', 'STRONTIUM, DISSOLVED (UG/L AS SR)',
        'THALLIUM, DISSOLVED (UG/L AS TL)', 'ZINC, DISSOLVED (UG/L AS ZN)',
        # others
        'CARBON, DISSOLVED ORGANIC (MG/L AS C)', 'CARBON, TOTAL ORGANIC (MG/L AS C)', 'MERCURY-TL,FILTERED WATER,ULTRATRACE METHOD NG/L',
        'MERCURY-TL,UNFILTERED WATER,ULTRATRACE METHOD NG/L', 'TURBIDITY,LAB NEPHELOMETRIC TURBIDITY UNITS, NTU',
        'TDS_mg_L', 'SSC%Finer', 'SSC_COARSE', 'SSC_FINE', 'SSC_TOTAL', 'BENTHIC ASH FREE DRY MASS, GM/M2','BENTHIC CHLOROPHYLL A, MG/M2'))) %>% 
      filter(! is.na(Ana_Sam_Fdt_Id)) #%>%  # drop blank row from row_bind
               
    # old method that bombs out if a field is missing                                       
    # stationAnalyteDataUserFilter1all <- dplyr::select(stationAnalyteDataUserFilter1, #Ana_Id, 
    #                                                   Ana_Sam_Fdt_Id, Ana_Sam_Mrs_Container_Id_Desc,
    #                                                   # metals for assessment
    #                                                   `ALUMINUM, DISSOLVED (UG/L AS AL)`, `ANTIMONY, DISSOLVED (UG/L AS SB)`, `ARSENIC, DISSOLVED  (UG/L AS AS)`, `BARIUM, DISSOLVED (UG/L AS BA)`,
    #                                                   `BERYLLIUM, DISSOLVED (UG/L AS BE)`, `CADMIUM, DISSOLVED (UG/L AS CD)`, `CALCIUM, DISSOLVED (MG/L AS CA)`, `CHROMIUM, DISSOLVED (UG/L AS CR)`,
    #                                                   `COPPER, DISSOLVED (UG/L AS CU)`, `HARDNESS, CA MG CALCULATED (MG/L AS CACO3) AS DISSOLVED`, `IRON, DISSOLVED (UG/L AS FE)`, `LEAD, DISSOLVED (UG/L AS PB)`,
    #                                                   `MAGNESIUM, DISSOLVED (MG/L AS MG)`, `MANGANESE, DISSOLVED (UG/L AS MN)`, `NICKEL, DISSOLVED (UG/L AS NI)`, `POTASSIUM, DISSOLVED (MG/L AS K)`,
    #                                                   `SELENIUM, DISSOLVED (UG/L AS SE)`, `SILVER, DISSOLVED (UG/L AS AG)`, `SODIUM, DISSOLVED (MG/L AS NA)`, `STRONTIUM, DISSOLVED (UG/L AS SR)`,
    #                                                   `THALLIUM, DISSOLVED (UG/L AS TL)`, `ZINC, DISSOLVED (UG/L AS ZN)`,
    #                                                   # others
    #                                                   `CARBON, DISSOLVED ORGANIC (MG/L AS C)`, `CARBON, TOTAL ORGANIC (MG/L AS C)`, `MERCURY-TL,FILTERED WATER,ULTRATRACE METHOD NG/L`,
    #                                                   `MERCURY-TL,UNFILTERED WATER,ULTRATRACE METHOD NG/L`, `TURBIDITY,LAB NEPHELOMETRIC TURBIDITY UNITS, NTU`)
    #                                                   
                                                      
    
    
    # Step 3.4: Organize lab remark fields
    # now fix remark fields
    stationAnalyteDataUserFilter2 <- bind_rows(remarkTemplate, 
                                               suppressWarnings(
                                                 stationAnalyteDataUserFilter0_0 %>% 
                                                   #stationAnalyteDataUserFilter %>% 
                                                   
                                                   # only keep codes we will do something with to avoid unnecessary data duplication
                                                   
                                                   filter(Pg_Storet_Code %in% c('00530', '00600', '00608', '00610', '00613' ,'00615', '00618', '00620',
                                                                                '00625', '00630', '00631', '00665', '00666', '00671', '00900', '00940',
                                                                                '00941', '00945', '00946', '31616', '31648', '31649', '32211', '49567',
                                                                                '49570', '49571', '49572', '70507', 'ECOLI', 'OPWLF', 'PIPLF', 'PPWLF',
                                                                                'SSC-TOTAL', 'TDNLF', 'TDPLF', 'TSS45',
                                                                                
                                                                                # metals for assessment
                                                                                '01106','01095','01000','01005','01010','01025','00915','01030','01040',
                                                                                'DHARD','01046','01049','00925','01056','01065','00935','01145','01075',
                                                                                '00930','01080','01057','01090',
                                                                                
                                                                                # others
                                                                                '00681','00680','50091','50092','82079',
                                                                                "70300", "70331", "SSC-COARSE", "SSC-FINE", "SSC-TOTAL",
                                                                                'BENCM2', 'CHLCM2')) %>% 
                                                   
                                                   dplyr::select(Ana_Sam_Fdt_Id, Ana_Sam_Mrs_Container_Id_Desc, Pg_Storet_Code, Ana_Uncensored_Val_Comment) %>% 
                                                   pivot_wider(id_cols = c(Ana_Sam_Fdt_Id, 'Ana_Sam_Mrs_Container_Id_Desc'),
                                                               names_from = 'Pg_Storet_Code', names_prefix = "RMK_", values_from = 'Ana_Uncensored_Val_Comment', values_fn = list()) %>% 
                                                   unnest(-Ana_Sam_Fdt_Id)) ) %>% 
      mutate(RMK_ECOLI = coalesce(`RMK_ECOLI`, `RMK_31648`),
             RMK_NITROGEN = coalesce(RMK_49570, RMK_49571), # what about NITROGEN_TOTAL_00600_mg_L remarks????
             RMK_NOX = coalesce(`RMK_00630`, RMK_00615, RMK_00620), # not in conventionals query, EVJ added logic
             RMK_AMMONIA = coalesce(RMK_00610, RMK_00608),
             RMK_NITRATE = coalesce(RMK_00620, RMK_00618),
             RMK_PHOSPHORUS = coalesce(RMK_00665, RMK_70507, RMK_00666, RMK_00671, 
                                       RMK_49567, RMK_49572), # don't love the logic on this one, double check me
             RMK_TSS = coalesce(RMK_00530, RMK_SSC_TOTAL_00530, RMK_TSS45),
             RMK_CHLORIDE = coalesce(RMK_00940, RMK_00941), # placeholder for now, fixed once combined with parameter data
             RMK_SULFATE = coalesce(RMK_00945, RMK_00946), # placeholder for now, fixed once combined with parameter data
             
             # EVJ added these even though not called for in this part of conventionals, no idea where comes from otherwise
             RMK_SULFATE_TOTAL = RMK_00945,
             RMK_SULFATE_DISS = RMK_00946,
             RMK_ENTEROCOCCI = RMK_31649,
             RMK_FECAL_COLI = RMK_31616,
             RMK_CHLOROPHYLL_A = RMK_32211,
             RMK_SSC = RMK_00530,
             
             RMK_TDS = RMK_70300,
             `RMK_SSC%Finer` = RMK_70331,
             RMK_SSC_COARSE = `RMK_SSC-COARSE`, 
             RMK_SSC_FINE = `RMK_SSC-FINE`,
             RMK_SSC_TOTAL = `RMK_SSC-TOTAL`,
             
             LEVEL_NITROGEN = as.character(NA), LEVEL_AMMONIA = as.character(NA), LEVEL_NITRATE = as.character(NA),
             LEVEL_NOX = as.character(NA), LEVEL_00600 = as.character(NA), LEVEL_00608 = as.character(NA),
             LEVEL_00610 = as.character(NA), LEVEL_00613 = as.character(NA), LEVEL_00615 = as.character(NA),
             LEVEL_00618 = as.character(NA), LEVEL_00620 = as.character(NA), LEVEL_00625 = as.character(NA),
             LEVEL_00630 = as.character(NA), LEVEL_00631 = as.character(NA), LEVEL_49570 = as.character(NA), 
             LEVEL_49571 = as.character(NA), LEVEL_TDNLF = as.character(NA), LEVEL_PHOSPHORUS = as.character(NA), 
             LEVEL_00665 = as.character(NA), LEVEL_00666 = as.character(NA), LEVEL_00671 = as.character(NA),
             LEVEL_49567 = as.character(NA), LEVEL_49572 = as.character(NA), LEVEL_70507 = as.character(NA),
             LEVEL_OPWLF = as.character(NA), LEVEL_PIPLF = as.character(NA), LEVEL_PPWLF = as.character(NA),
             LEVEL_TDPLF = as.character(NA), LEVEL_00900 = as.character(NA), LEVEL_CHLORIDE = as.character(NA),
             LEVEL_00940 = as.character(NA), LEVEL_00941 = as.character(NA), LEVEL_SULFATE = as.character(NA),
             LEVEL_SULFATE_TOTAL = as.character(NA), LEVEL_SULFATE_DISS = as.character(NA), LEVEL_ECOLI = as.character(NA),
             LEVEL_31648 = as.character(NA), LEVEL_ENTEROCOCCI = as.character(NA), LEVEL_FECAL_COLI = as.character(NA),
             LEVEL_CHLOROPHYLL_A = as.character(NA), LEVEL_TSS = as.character(NA), LEVEL_00530 = as.character(NA),
             LEVEL_SSC = as.character(NA), LEVEL_TSS45 = as.character(NA),
            
             # don't need level info of stuff not in conventionals query
             # LEVEL_TDS = as.character(NA), LEVEL_70331 = as.character(NA), LEVEL_SSC_COARSE = as.character(NA), 
             # LEVEL_SSC_FINE = as.character(NA), LEVEL_SSC_TOTAL = as.character(NA)
             ) 
    
    stationAnalyteDataUserFilter2conventionals <- dplyr::select(stationAnalyteDataUserFilter2,#Ana_Id, 
                                                                Ana_Sam_Fdt_Id, Ana_Sam_Mrs_Container_Id_Desc,
                                                                RMK_NITROGEN, LEVEL_NITROGEN, RMK_AMMONIA, LEVEL_AMMONIA, RMK_NITRATE, LEVEL_NITRATE,
                                                                RMK_NOX, LEVEL_NOX, RMK_00600, LEVEL_00600, RMK_00608, LEVEL_00608,
                                                                RMK_00610, LEVEL_00610, RMK_00613, LEVEL_00613, RMK_00615, LEVEL_00615, RMK_00618, LEVEL_00618,
                                                                RMK_00620, LEVEL_00620, RMK_00625, LEVEL_00625, RMK_00630, LEVEL_00630, RMK_00631, LEVEL_00631, RMK_49570, LEVEL_49570,
                                                                RMK_49571, LEVEL_49571, RMK_TDNLF, LEVEL_TDNLF, RMK_PHOSPHORUS, LEVEL_PHOSPHORUS, RMK_00665, LEVEL_00665,
                                                                RMK_00666, LEVEL_00666, RMK_00671, LEVEL_00671, RMK_49567, LEVEL_49567, RMK_49572, LEVEL_49572, RMK_70507, 
                                                                LEVEL_70507, RMK_OPWLF, LEVEL_OPWLF, RMK_PIPLF, LEVEL_PIPLF, RMK_PPWLF, LEVEL_PPWLF, RMK_TDPLF, LEVEL_TDPLF,
                                                                RMK_00900, LEVEL_00900, RMK_CHLORIDE, LEVEL_CHLORIDE, RMK_00940, LEVEL_00940, RMK_00941, LEVEL_00941,
                                                                RMK_SULFATE, LEVEL_SULFATE, RMK_SULFATE_TOTAL, LEVEL_SULFATE_TOTAL, RMK_SULFATE_DISS, LEVEL_SULFATE_DISS,
                                                                RMK_ECOLI, LEVEL_ECOLI, RMK_31648, LEVEL_31648, RMK_ENTEROCOCCI, LEVEL_ENTEROCOCCI, RMK_FECAL_COLI, LEVEL_FECAL_COLI,
                                                                RMK_CHLOROPHYLL_A, LEVEL_CHLOROPHYLL_A, RMK_TSS, LEVEL_TSS, RMK_00530, LEVEL_00530, RMK_SSC, LEVEL_SSC,
                                                                RMK_TSS45, LEVEL_TSS45 ) %>% 
      filter(! is.na(Ana_Sam_Fdt_Id)) # drop blank row from row_bind
    # double check things
    #left_join(dplyr::select(stationFieldDataUserFilter1, FDT_ID:FDT_DEPTH_DESC), by = c("Ana_Sam_Fdt_Id"= "FDT_ID")) %>% 
    #dplyr::select(FDT_STA_ID:FDT_DEPTH_DESC, everything())
    
    
    stationAnalyteDataUserFilter2all <- dplyr::select(stationAnalyteDataUserFilter2,
                                                      any_of(c(#Ana_Id,
                                                      'Ana_Sam_Fdt_Id', 'Ana_Sam_Mrs_Container_Id_Desc',
                                                      'RMK_01106', 'RMK_01095', 'RMK_01000', 'RMK_01005', 'RMK_01010', 'RMK_01025', 'RMK_00915', 'RMK_01030', 'RMK_01040', 'RMK_DHARD', 'RMK_01046', 'RMK_01049', 'RMK_00925', 'RMK_01056',
                                                      'RMK_01065', 'RMK_00935', 'RMK_01145', 'RMK_01075', 'RMK_00930', 'RMK_01080', 'RMK_01057', 'RMK_01090', 'RMK_01106', 'RMK_01095', 'RMK_01000', 'RMK_01005', 'RMK_01010', 'RMK_01025',
                                                      'RMK_00915', 'RMK_01030', 'RMK_01040', 'RMK_DHARD', 'RMK_01046', 'RMK_01049', 'RMK_00925', 'RMK_01056', 'RMK_01065', 'RMK_00935', 'RMK_01145', 'RMK_01075', 'RMK_00930', 'RMK_01080',
                                                      'RMK_01057', 'RMK_01090', 'RMK_00681', 'RMK_00680', 'RMK_50091', 'RMK_50092', 'RMK_82079',
                                                      'RMK_TDS', 'RMK_SSC%Finer', 'RMK_SSC_COARSE', 'RMK_SSC_FINE', 'RMK_SSC_TOTAL',
                                                      'RMK_BENCM2', 'RMK_CHLCM2'))) %>% 
      filter(! is.na(Ana_Sam_Fdt_Id)) #%>%  # drop blank row from row_bind
    # old method that bombs out if any field missing
    # stationAnalyteDataUserFilter2all <- dplyr::select(stationAnalyteDataUserFilter2,#Ana_Id,
    #                                                   Ana_Sam_Fdt_Id, Ana_Sam_Mrs_Container_Id_Desc,
    #                                                   RMK_01106, RMK_01095, RMK_01000, RMK_01005, RMK_01010, RMK_01025, RMK_00915, RMK_01030, RMK_01040, RMK_DHARD, RMK_01046, RMK_01049, RMK_00925, RMK_01056,
    #                                                   RMK_01065, RMK_00935, RMK_01145, RMK_01075, RMK_00930, RMK_01080, RMK_01057, RMK_01090, RMK_01106, RMK_01095, RMK_01000, RMK_01005, RMK_01010, RMK_01025,
    #                                                   RMK_00915, RMK_01030, RMK_01040, RMK_DHARD, RMK_01046, RMK_01049, RMK_00925, RMK_01056, RMK_01065, RMK_00935, RMK_01145, RMK_01075, RMK_00930, RMK_01080,
    #                                                   RMK_01057, RMK_01090, RMK_00681, RMK_00680, RMK_50091, RMK_50092, RMK_82079)
    
    
      
    # Step 3.5: Combine analyte data and remarks (and empty Level fields)
    stationAnalyteDataUserFilter3conventionals <- left_join(stationAnalyteDataUserFilter1conventionals, stationAnalyteDataUserFilter2conventionals,
                                               by = c(#"Ana_Id", 
                                                 "Ana_Sam_Fdt_Id", "Ana_Sam_Mrs_Container_Id_Desc")) %>% 
      # secondary adjustment based on remark codes when combined with remarks
      mutate(
        AMMONIA_mg_L = ifelse(!is.na(NITROGEN_AMMONIA_DISSOLVED_00608_mg_L) &&
                                NITROGEN_AMMONIA_DISSOLVED_00608_mg_L > NITROGEN_AMMONIA_TOTAL_00610_mg_L && 
                                RMK_00610 %in% c('U','QQ') && is.na(RMK_00608), NITROGEN_AMMONIA_DISSOLVED_00608_mg_L, AMMONIA_mg_L),
        RMK_AMMONIA = ifelse(!is.na(NITROGEN_AMMONIA_DISSOLVED_00608_mg_L) &&
                               NITROGEN_AMMONIA_DISSOLVED_00608_mg_L > NITROGEN_AMMONIA_TOTAL_00610_mg_L && 
                               RMK_00610 %in% c('U','QQ'),
                             RMK_00608, RMK_AMMONIA),
        RMK_CHLORIDE = ifelse(!is.na(CHLORIDE_DISSOLVED_00941_mg_L) && !is.na(CHLORIDE_TOTAL_00940_mg_L) &&
                                CHLORIDE_DISSOLVED_00941_mg_L > CHLORIDE_TOTAL_00940_mg_L, RMK_00941, RMK_00940), # fix from above
        RMK_SULFATE =  ifelse(!is.na(SULFATE_DISS_mg_L) && !is.na(SULFATE_TOTAL_mg_L) &&  # conventionals changes these names for some reason: SULFATE_TOTAL_mg_L = SULFATE_TOTAL_00945_mg_L;  SULFATE_DISS_mg_L = SULFATE_DISSOLVED_00946_mg_L,
                                SULFATE_DISS_mg_L > SULFATE_TOTAL_mg_L, RMK_SULFATE_DISS, RMK_SULFATE_TOTAL)) %>%  # fix from above; # conventionals changes these names for some reason: RMK_SULFATE_DISS = RMK_00946; RMK_SULFATE_TOTAL = RMK_00945
      dplyr::select(#Ana_Id, # maybe don't need?
        Ana_Sam_Fdt_Id, Ana_Sam_Mrs_Container_Id_Desc,
        NITROGEN_mg_L, RMK_NITROGEN, LEVEL_NITROGEN, AMMONIA_mg_L, 
        RMK_AMMONIA, LEVEL_AMMONIA, NITRATE_mg_L, RMK_NITRATE, LEVEL_NITRATE, NOX_mg_L, RMK_NOX, LEVEL_NOX, NITROGEN_TOTAL_00600_mg_L, 
        RMK_00600, LEVEL_00600, NITROGEN_AMMONIA_DISSOLVED_00608_mg_L, RMK_00608, LEVEL_00608, NITROGEN_AMMONIA_TOTAL_00610_mg_L, 
        RMK_00610, LEVEL_00610, NITROGEN_NITRITE_DISSOLVED_00613_mg_L, RMK_00613, LEVEL_00613, NITROGEN_NITRITE_TOTAL_00615_mg_L, 
        RMK_00615, LEVEL_00615, NITROGEN_NITRATE_DISSOLVED_00618_mg_L, RMK_00618, LEVEL_00618, NITROGEN_NITRATE_TOTAL_00620_mg_L, 
        RMK_00620, LEVEL_00620, NITROGEN_KJELDAHL_TOTAL_00625_mg_L, RMK_00625, LEVEL_00625, `NITRITE+NITRATE_TOTAL_00630_mg_L`, RMK_00630, 
        LEVEL_00630, `NITRITE+NITRATE_DISSOLVED_00631_mg_L`, RMK_00631, LEVEL_00631, NITROGEN_PARTICULATE_49570_mg_L, RMK_49570, LEVEL_49570, 
        NITROGEN_TOTAL_DISSOLVED_49571_mg_L, RMK_49571, LEVEL_49571, NITROGEN_TOTAL_DISSOLVED_TDNLF_mg_L, RMK_TDNLF, LEVEL_TDNLF, 
        PHOSPHORUS_mg_L, RMK_PHOSPHORUS, LEVEL_PHOSPHORUS, PHOSPHORUS_TOTAL_00665_mg_L, RMK_00665, LEVEL_00665, 
        PHOSPHORUS_DISSOLVED_00666_mg_L, RMK_00666, LEVEL_00666, PHOSPHORUS_DISSOLVED_ORTHOPHOSPHATE_00671_mg_L, RMK_00671, LEVEL_00671, 
        PHOSPHOROUS_PARTICULATE_49567_mg_L, RMK_49567, LEVEL_49567, PHOSPHOROUS_TOTAL_DISSOLVED_49572_mg_L, RMK_49572, LEVEL_49572, 
        PHOSPHORUS_TOTAL_ORTHOPHOSPHATE_70507_mg_L, RMK_70507, LEVEL_70507, ORTHOPHOSPHATE_DISSOLVED_OPWLF_mg_L, RMK_OPWLF, LEVEL_OPWLF, 
        PHOSPHORUS_SUSPENDED_INORGANIC_PIPLF_mg_L, RMK_PIPLF, LEVEL_PIPLF, PHOSPHORUS_PARTICULATE_PPWLF_mg_L, RMK_PPWLF, LEVEL_PPWLF, 
        PHOSPHORUS_TOTAL_DISSOLVED_TDPLF_mg_L, RMK_TDPLF, LEVEL_TDPLF, HARDNESS_TOTAL_00900_mg_L, RMK_00900, LEVEL_00900, CHLORIDE_mg_L, 
        RMK_CHLORIDE, LEVEL_CHLORIDE, CHLORIDE_TOTAL_00940_mg_L, RMK_00940, LEVEL_00940, CHLORIDE_DISSOLVED_00941_mg_L, RMK_00941, 
        LEVEL_00941, SULFATE_mg_L, RMK_SULFATE, LEVEL_SULFATE, SULFATE_TOTAL_mg_L, RMK_SULFATE_TOTAL, LEVEL_SULFATE_TOTAL, 
        SULFATE_DISS_mg_L, RMK_SULFATE_DISS, LEVEL_SULFATE_DISS, ECOLI, RMK_ECOLI, LEVEL_ECOLI, ECOLI_31648_NO_100mL, RMK_31648, 
        LEVEL_31648, ENTEROCOCCI, RMK_ENTEROCOCCI, LEVEL_ENTEROCOCCI, FECAL_COLI, RMK_FECAL_COLI, LEVEL_FECAL_COLI, CHLOROPHYLL_A_ug_L, 
        RMK_CHLOROPHYLL_A, LEVEL_CHLOROPHYLL_A, TSS_mg_L, RMK_TSS, LEVEL_TSS, TOTAL_SUSPENDED_SOLIDS_00530_mg_L, RMK_00530, LEVEL_00530, 
        SSC_mg_L, RMK_SSC, LEVEL_SSC, TOTAL_SUSPENDED_SOLIDS_TSS45_mg_L, RMK_TSS45, LEVEL_TSS45) #%>% 
      # QUick double check
      # left_join(dplyr::select(stationFieldDataUserFilter1, FDT_ID:FDT_DEPTH_DESC), by = c("Ana_Sam_Fdt_Id"= "FDT_ID")) %>% 
      # dplyr::select(FDT_STA_ID:FDT_DEPTH_DESC, everything())
      
    # everything
    stationAnalyteDataUserFilter3all <- left_join(stationAnalyteDataUserFilter3conventionals,
                                                  left_join(stationAnalyteDataUserFilter1all, stationAnalyteDataUserFilter2all,
                                                            by = c(#"Ana_Id", 
                                                              "Ana_Sam_Fdt_Id", "Ana_Sam_Mrs_Container_Id_Desc")) ,
                                                  by = c(#"Ana_Id", 
                                                    "Ana_Sam_Fdt_Id", "Ana_Sam_Mrs_Container_Id_Desc")) %>% 
      dplyr::select(any_of(c(names(stationAnalyteDataUserFilter3conventionals), 
                    # metals for assessment
                    'ALUMINUM, DISSOLVED (UG/L AS AL)', 'RMK_01106', 'ANTIMONY, DISSOLVED (UG/L AS SB)', 'RMK_01095',
                    'ARSENIC, DISSOLVED  (UG/L AS AS)', 'RMK_01000', 'BARIUM, DISSOLVED (UG/L AS BA)', 'RMK_01005',
                    'BERYLLIUM, DISSOLVED (UG/L AS BE)', 'RMK_01010', 'CADMIUM, DISSOLVED (UG/L AS CD)', 'RMK_01025',
                    'CALCIUM, DISSOLVED (MG/L AS CA)', 'RMK_00915', 'CHROMIUM, DISSOLVED (UG/L AS CR)', 'RMK_01030',
                    'COPPER, DISSOLVED (UG/L AS CU)', 'RMK_01040', 'HARDNESS, CA MG CALCULATED (MG/L AS CACO3) AS DISSOLVED', 'RMK_DHARD',
                    'IRON, DISSOLVED (UG/L AS FE)', 'RMK_01046', 'LEAD, DISSOLVED (UG/L AS PB)', 'RMK_01049', 
                    'MAGNESIUM, DISSOLVED (MG/L AS MG)', 'RMK_00925', 'MANGANESE, DISSOLVED (UG/L AS MN)', 'RMK_01056',
                    'NICKEL, DISSOLVED (UG/L AS NI)',  'RMK_01065', 'POTASSIUM, DISSOLVED (MG/L AS K)', 'RMK_00935',
                    'SELENIUM, DISSOLVED (UG/L AS SE)', 'RMK_01145', 'SILVER, DISSOLVED (UG/L AS AG)', 'RMK_01075',
                    'SODIUM, DISSOLVED (MG/L AS NA)', 'RMK_00930', 'STRONTIUM, DISSOLVED (UG/L AS SR)', 'RMK_01080',
                    'THALLIUM, DISSOLVED (UG/L AS TL)', 'RMK_01057', 'ZINC, DISSOLVED (UG/L AS ZN)', 'RMK_01090',
                    
                    # others
                    'CARBON, DISSOLVED ORGANIC (MG/L AS C)', 'RMK_00681', 'CARBON, TOTAL ORGANIC (MG/L AS C)', 'RMK_00680',
                    'MERCURY-TL,FILTERED WATER,ULTRATRACE METHOD NG/L', 'RMK_50091', 'MERCURY-TL,UNFILTERED WATER,ULTRATRACE METHOD NG/L', 'RMK_50092',
                     'TURBIDITY,LAB NEPHELOMETRIC TURBIDITY UNITS, NTU', 'RMK_82079',
                    'TDS_mg_L', 'RMK_TDS', 'SSC%Finer', 'RMK_SSC%Finer', 'SSC_COARSE', 'RMK_SSC_COARSE', 'SSC_FINE', 'RMK_SSC_FINE', 
                    'SSC_TOTAL', 'RMK_SSC_TOTAL')))
    # old method that bombed out when fields weren't there
    # dplyr::select(names(stationAnalyteDataUserFilter3conventionals), 
    #               # metals for assessment
    #               `ALUMINUM, DISSOLVED (UG/L AS AL)`, RMK_01106, `ANTIMONY, DISSOLVED (UG/L AS SB)`, RMK_01095,
    #               `ARSENIC, DISSOLVED  (UG/L AS AS)`, RMK_01000, `BARIUM, DISSOLVED (UG/L AS BA)`, RMK_01005,
    #               `BERYLLIUM, DISSOLVED (UG/L AS BE)`, RMK_01010, `CADMIUM, DISSOLVED (UG/L AS CD)`, RMK_01025,
    #               `CALCIUM, DISSOLVED (MG/L AS CA)`, RMK_00915, `CHROMIUM, DISSOLVED (UG/L AS CR)`, RMK_01030,
    #               `COPPER, DISSOLVED (UG/L AS CU)`, RMK_01040, `HARDNESS, CA MG CALCULATED (MG/L AS CACO3) AS DISSOLVED`, RMK_DHARD,
    #               `IRON, DISSOLVED (UG/L AS FE)`, RMK_01046, `LEAD, DISSOLVED (UG/L AS PB)`, RMK_01049, 
    #               `MAGNESIUM, DISSOLVED (MG/L AS MG)`, RMK_00925, `MANGANESE, DISSOLVED (UG/L AS MN)`, RMK_01056,
    #               `NICKEL, DISSOLVED (UG/L AS NI)`,  RMK_01065, `POTASSIUM, DISSOLVED (MG/L AS K)`, RMK_00935,
    #               `SELENIUM, DISSOLVED (UG/L AS SE)`, RMK_01145, `SILVER, DISSOLVED (UG/L AS AG)`, RMK_01075,
    #               `SODIUM, DISSOLVED (MG/L AS NA)`, RMK_00930, `STRONTIUM, DISSOLVED (UG/L AS SR)`, RMK_01080,
    #               `THALLIUM, DISSOLVED (UG/L AS TL)`, RMK_01057, `ZINC, DISSOLVED (UG/L AS ZN)`, RMK_01090,
    #               
    #               # others
    #               `CARBON, DISSOLVED ORGANIC (MG/L AS C)`, RMK_00681, `CARBON, TOTAL ORGANIC (MG/L AS C)`, RMK_00680,
    #               `MERCURY-TL,FILTERED WATER,ULTRATRACE METHOD NG/L`, RMK_50091, `MERCURY-TL,UNFILTERED WATER,ULTRATRACE METHOD NG/L`, RMK_50092,
    #               `TURBIDITY,LAB NEPHELOMETRIC TURBIDITY UNITS, NTU`, RMK_82079)
    
    
    # Step 4: Combine field and analyte data
    # use left join here bc stationFieldDataUserFilter1 removes any unwanted sample types (incident response, facility data)
    comboConventionals <- left_join(stationFieldDataUserFilter1, stationAnalyteDataUserFilter3conventionals, by = c("FDT_ID" = "Ana_Sam_Fdt_Id")) %>% 
      dplyr::select(-FDT_ID) # no longer needed
    comboAll <- left_join(stationFieldDataUserFilter1, stationAnalyteDataUserFilter3all, by = c("FDT_ID" = "Ana_Sam_Fdt_Id")) %>% 
      dplyr::select(-FDT_ID) # no longer needed
    
    # Step 5: Combine station and combo data
    combo2 <- full_join(stationData, comboConventionals, by = 'FDT_STA_ID') %>% 
      dplyr::select(names(conventionals))
    combo2all <- full_join(stationData, comboAll, by = 'FDT_STA_ID') 
        
    return(list(Conventionals = combo2,
                More = combo2all))
    
  } else {
    return(list(Conventionals = conventionals,
                More = tibble()))
  }
}
    
# 
# ## Function Demo
# library(tidyverse)
# library(config)
# library(sf)
# library(lubridate)
# library(pool)
# library(pins)
# library(dbplyr)
# 
# 
# # Server connection things
# conn <- config::get("connectionSettings") # get configuration settings
# 
# # set up pool
# pool <- dbPool(
#   drv = odbc::odbc(),
#   Driver = "ODBC Driver 11 for SQL Server",#"SQL Server Native Client 11.0",
#   Server= "DEQ-SQLODS-PROD,50000",
#   dbname = "ODS",
#   trusted_connection = "yes"
# )


# single station
# station <- '2-JKS023.61'
# dateRange <- c(as.Date('2015-01-01'), as.Date('2021-01-01'))
# stationFieldData <- pool %>% tbl(in_schema("wqm", "Wqm_Field_Data_View")) %>%
#   filter(Fdt_Sta_Id %in% !! station &
#            between(as.Date(Fdt_Date_Time), !! dateRange[1], !! dateRange[2]) ) %>%
#   as_tibble() %>%
#   filter(! Ssc_Description %in% "INVALID DATA SET QUALITY ASSURANCE FAILURE")
# stationAnalyteData <- pool %>% tbl(in_schema("wqm", "Wqm_Analytes_View")) %>%
#   filter(Ana_Sam_Fdt_Id %in% !! stationFieldData$Fdt_Id  &
#            between(as.Date(Ana_Received_Date), !! dateRange[1], !! dateRange[2]) & # x >= left & x <= right
#            Pg_Parm_Name != "STORET STORAGE TRANSACTION DATE YR/MO/DAY") %>%
#   as_tibble() %>%
#   left_join(dplyr::select(stationFieldData, Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time), by = c("Ana_Sam_Fdt_Id" = "Fdt_Id"))
# stationInfo <- pool %>% tbl(in_schema("wqm",  "Wqm_Stations_View")) %>%
#   filter(Sta_Id %in% !! toupper(station)) %>%
#   as_tibble()
# stationGIS_View <-  pool %>% tbl(in_schema("wqm",  "Wqm_Sta_GIS_View")) %>%
#   filter(Station_Id %in% !! toupper(station)) %>%
#   as_tibble()
#
# zz <- conventionalsSummary(conventionals = pin_get("conventionals2022IRfinalWithSecchi", board = "rsconnect")[0,],
#                            stationFieldData,
#                            stationAnalyteData,
#                            stationInfo, 
#                            stationGIS_View, 
#                            dropCodes = c('QF'))


# multistation
# station <- pin_get("conventionals2022IRfinalWithSecchi", board = "rsconnect") %>% 
#   filter(STA_REC_CODE == 'BRRO') %>% 
#   distinct(FDT_STA_ID)
# station <- station$FDT_STA_ID[1:100]
# 
# 
# dateRange <- c(as.Date('2015-01-01'), as.Date('2020-12-31'))
# stationFieldData <- pool %>% tbl(in_schema("wqm", "Wqm_Field_Data_View")) %>%
#   filter(Fdt_Sta_Id %in% !! station &
#            between(as.Date(Fdt_Date_Time), !! dateRange[1], !! dateRange[2]) ) %>%
#   as_tibble() %>%
#   filter(! Ssc_Description %in% "INVALID DATA SET QUALITY ASSURANCE FAILURE")
# stationAnalyteData <- pool %>% tbl(in_schema("wqm", "Wqm_Analytes_View")) %>%
#   filter(Ana_Sam_Fdt_Id %in% !! stationFieldData$Fdt_Id &
#            #between(as.Date(Ana_Received_Date), !! dateRange[1], !! dateRange[2]) & # x >= left & x <= right
#            Pg_Parm_Name != "STORET STORAGE TRANSACTION DATE YR/MO/DAY") %>%
#   as_tibble() %>%
#   left_join(dplyr::select(stationFieldData, Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time), by = c("Ana_Sam_Fdt_Id" = "Fdt_Id"))
# stationInfo <- pool %>% tbl(in_schema("wqm",  "Wqm_Stations_View")) %>%
#   filter(Sta_Id %in% !! toupper(station)) %>%
#   as_tibble()
# stationGIS_View <-  pool %>% tbl(in_schema("wqm",  "Wqm_Sta_GIS_View")) %>%
#   filter(Station_Id %in% !! toupper(station)) %>%
#   as_tibble()
# zz <- conventionalsSummary(conventionals= pin_get("conventionals2022IRfinalWithSecchi", board = "rsconnect")[0,],
#                            stationFieldDataUserFilter= stationFieldData, stationAnalyteDataUserFilter = stationAnalyteData,
#                            stationInfo,
#                            stationGIS_View,
#                            dropCodes = c('QF'))