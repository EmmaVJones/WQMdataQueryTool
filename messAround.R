wqm_samples_view <- pool %>% tbl()


samplesView <- pool %>% tbl(in_schema("wqm", "Wqm_Samples_View")) %>%
  distinct(Sam_Mrs_Container_Id_Desc) %>% 
  as_tibble()
  
analytesView <- pool %>% tbl(in_schema("wqm", "Wqm_Analytes_View")) %>%
  distinct(Ana_Sam_Mrs_Container_Id_Desc) %>% 
  as_tibble()

commentsView <- pool %>% tbl(in_schema("wqm", "Wqm_Field_Data_View")) %>%
  distinct(Fdt_Comment) %>% 
  as_tibble()

commentsView2 <- commentsView %>% 
  filter( str_detect(Fdt_Comment, '= S')) %>%
  filter( str_detect(Fdt_Comment, '=S')) %>%
  filter( str_detect(Fdt_Comment, '= s')) %>%
  filter( str_detect(Fdt_Comment, '=s')) %>%
  filter( str_detect(Fdt_Comment, 'STORM EVENT')) %>%
  filter( str_detect(Fdt_Comment, 'storm event')) %>%
  filter( str_detect(Fdt_Comment, 'STORM SAMPL')) %>%
  filter( str_detect(Fdt_Comment, 'storm sampl')) %>%
  filter( str_detect(Fdt_Comment, 'TARGETED')) %>%
  filter( str_detect(Fdt_Comment, 'targeted')) %>%
  filter( str_detect(Fdt_Comment, 'DUPLICATE')) %>%
  filter( str_detect(Fdt_Comment, 'duplicate')) %>%
  filter( str_detect(Fdt_Comment, 'BLANK')) %>%
  filter( str_detect(Fdt_Comment, 'blank'))

  #filter(Ana_Sam_Fdt_Id %in% !! stationFieldData$Fdt_Id  &
           between(as.Date(Ana_Received_Date), !! dateRange[1], !! dateRange[2]) & # x >= left & x <= right
           Pg_Parm_Name != "STORET STORAGE TRANSACTION DATE YR/MO/DAY") %>% 
  as_tibble() 



# Fix field measurement to NA if remark field indicates QC failure
View(dplyr::select(stationFieldDataUserFilter, Fdt_Temp_Celcius:Fdt_Gauge_Height, Fdt_Do_Satr_Per:Fdt_Secchi_Depth ) %>% 
  select(sort(tidyselect::peek_vars())))
# need to remove Fdt?

# flip long by parameter and remark?
View(
  stationFieldDataUserFilter %>% 
    group_by(Fdt_Id) %>% 
    dplyr::select( Fdt_Flow_Cfs:Fdt_Gauge_Height, Fdt_Do_Satr_Per:Fdt_Secchi_Depth ) %>% 
    mutate_if(is.numeric, as.character) %>% 
    pivot_longer(cols = -Fdt_Id, names_to = 'parameter', values_to = 'value') %>%
    mutate(parameter1 = case_when(str_detect(parameter, 'Fdt_') ~ str_replace(parameter, 'Fdt_', ''),
                                  TRUE ~ as.character(parameter))) %>% 
    arrange(parameter1) %>% 
    ungroup %>% 
    distinct(parameter1, .keep_all = T) %>% 
    pull(parameter1)
)

# na_if and go column by column
test <- stationFieldDataUserFilter[1:30,] %>% 
  dplyr::select(Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Fdt_Temp_Celcius, Fdt_Temp_Celcius_Rmk, Temp_Celcius_Remark)
test$Fdt_Temp_Celcius_Rmk[2] <- 'QF'
test$Fdt_Temp_Celcius_Rmk[3] <- 'QF'#'XX'
test$Temp_Celcius_Remark[3] <- 'XX'#'QF'
dropCodes <- c('QF')
View(test %>% 
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
         Fdt_Turbidity = ifelse(coalesce(Fdt_Turbidity_Rmk, Turbidity_Remark) %in% dropCodes, NA, Fdt_Turbidity))
)





stationAnalyteDataUserFilter <- filter(stationAnalyteDataUserFilter, Ana_Lab_Seq_Num == 1) %>% # drop analyte lab sequence number != 1
  mutate(Ana_Uncensored_Value = case_when(Ana_Com_Code %in% c('IF', 'J', 'O', 'PE', 'Q1', 'QF', 'QFQ', 'V') ~ as.numeric(NA), 
                                          TRUE ~ Ana_Uncensored_Value)) %>% # drop results from invalid lab codes
  filter(Ana_Sam_Mrs_Container_Id_Desc %in% c('', 'C', 'H', 'HV', 'R', 'S1', 'V')) # only keep samples with select container descriptions


stationAnalyteDataUserFilter0 <- stationAnalyteDataUserFilter %>% 
  mutate(ParameterName = case_when(Pg_Storet_Code == '31648' ~ 'ECOLI_CFU/100mL',
                                   Pg_Storet_Code == '00530' ~ 'TOTAL_SUSPENDED_SOLIDS_00530_mg/L',
                                   Pg_Storet_Code == '00600' ~ 'NITROGEN_TOTAL_00600_mg/L',
                                   Pg_Storet_Code == '00608' ~ 'NITROGEN_AMMONIA_DISSOLVED_00608_mg/L',
                                   Pg_Storet_Code == '00610' ~ 'NITROGEN_AMMONIA_TOTAL_00610_mg/L',
                                   Pg_Storet_Code == '00613' ~ 'NITROGEN_NITRITE_DISSOLVED_00613_mg/L',
                                   Pg_Storet_Code == '00615' ~ 'NITROGEN_NITRITE_TOTAL_00615_mg/L',
                                   Pg_Storet_Code == '00618' ~ 'NITROGEN_NITRATE_DISSOLVED_00618_mg/L',
                                   Pg_Storet_Code == '00620' ~ 'NITROGEN_NITRATE_TOTAL_00620_mg/L',
                                   Pg_Storet_Code == '00625' ~ 'NITROGEN_KJELDAHL_TOTAL_00625_mg/L',
                                   Pg_Storet_Code == '00630' ~ 'NITRITE+NITRATE_TOTAL_00630_mg/L',
                                   Pg_Storet_Code == '00631' ~ 'NITRITE+NITRATE_DISSOLVED_00631_mg/L',
                                   Pg_Storet_Code == '00665' ~ 'PHOSPHORUS_TOTAL_00665_mg/L',
                                   Pg_Storet_Code == '00666' ~ 'PHOSPHORUS_DISSOLVED_00666_mg/L',
                                   Pg_Storet_Code == '00671' ~ 'PHOSPHORUS_DISSOLVED_ORTHOPHOSPHATE_00671_mg/L',
                                   Pg_Storet_Code == '00900' ~ 'HARDNESS_TOTAL_00900_mg/L',
                                   Pg_Storet_Code == '00940' ~ 'CHLORIDE_TOTAL_00940_mg/L',
                                   Pg_Storet_Code == '00941' ~ 'CHLORIDE_DISSOLVED_00941_mg/L',
                                   Pg_Storet_Code == '00945' ~ 'SULFATE_TOTAL_00945_mg/L',
                                   Pg_Storet_Code == '00946' ~ 'SULFATE_DISSOLVED_00946_mg/L',
                                   Pg_Storet_Code == '31616' ~ 'FECAL_COLIFORM_31616_NO/100mL',
                                   Pg_Storet_Code == '31648' ~ 'E._COLI_31648_NO/100mL',
                                   Pg_Storet_Code == '31649' ~ 'ENTEROCOCCI_31649_NO/100mL',
                                   Pg_Storet_Code == '32211' ~ 'CHLOROPHYLL_32211_ug/L',
                                   Pg_Storet_Code == '49567' ~ 'PHOSPHOROUS_PARTICULATE_49567_mg/L',
                                   Pg_Storet_Code == '49570' ~ 'NITROGEN_PARTICULATE_49570_mg/L',
                                   Pg_Storet_Code == '49571' ~ 'NITROGEN_TOTAL_DISSOLVED_49571_mg/L',
                                   Pg_Storet_Code == '49572' ~ 'PHOSPHOROUS_TOTAL_DISSOLVED_49572_mg/L',
                                   Pg_Storet_Code == '70507' ~ 'PHOSPHORUS_TOTAL_ORTHOPHOSPHATE_70507_mg/L',
                                   Pg_Storet_Code == 'ECOLI' ~ 'E.COLI_ECOLI_CFU/100mL',
                                   Pg_Storet_Code == 'OPWLF' ~ 'ORTHOPHOSPHATE_DISSOLVED_OPWLF_mg/L',
                                   Pg_Storet_Code == 'PIPLF' ~ 'PHOSPHORUS_SUSPENDED_INORGANIC_PIPLF_mg/L',
                                   Pg_Storet_Code == 'PPWLF' ~ 'PHOSPHORUS_PARTICULATE_PPWLF_mg/L',
                                   Pg_Storet_Code == 'SSC-TOTAL' ~ 'SSC-TOTAL_00530_mg/L',
                                   Pg_Storet_Code == 'TDNLF' ~ 'NITROGEN_TOTAL_DISSOLVED_TDNLF_mg/L',
                                   Pg_Storet_Code == 'TDPLF' ~ 'PHOSPHORUS_TOTAL_DISSOLVED_TDPLF_mg/L',
                                   Pg_Storet_Code == 'TSS45' ~ 'TOTAL_SUSPENDED_SOLIDS_TSS45_mg/L',
                                   TRUE ~ as.character(Pg_Storet_Code)))
       

stationAnalyteDataUserFilter1 <- stationAnalyteDataUserFilter0 %>% 
  dplyr::select(Ana_Id, Ana_Sam_Fdt_Id, Ana_Sam_Mrs_Container_Id_Desc,  ParameterName, Ana_Uncensored_Value) %>% 
  pivot_wider( names_from = 'ParameterName', values_from = 'Ana_Uncensored_Value')

  #stationAnalyteDataUserFilter %>%
  #dplyr::select(Ana_Id, Ana_Sam_Fdt_Id, Ana_Sam_Mrs_Container_Id_Desc,  Pg_Storet_Code,Ana_Uncensored_Value) %>% 
  #pivot_wider( names_from = 'Pg_Storet_Code', values_from = 'Ana_Uncensored_Value')

stationAnalyteDataUserFilter2 <- stationAnalyteDataUserFilter0 %>% 
  dplyr::select(Ana_Id, Ana_Sam_Fdt_Id, Ana_Sam_Mrs_Container_Id_Desc, Pg_Storet_Code, Ana_Uncensored_Val_Comment) %>% 
  pivot_wider( names_from = 'Pg_Storet_Code', names_prefix = "Rmk", values_from = 'Ana_Uncensored_Val_Comment')




