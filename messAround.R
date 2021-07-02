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
                                   TRUE ~ as.character(Pg_Storet_Code))) 

# make template to provide columns that may not exist for coalesce step
parameterTemplate <- tibble(Ana_Id = as.character(NA), Ana_Sam_Fdt_Id = as.character(NA), Ana_Sam_Mrs_Container_Id_Desc = as.character(NA), 
                            `TOTAL_SUSPENDED_SOLIDS_00530_mg_L` = as.numeric(NA), `NITROGEN_TOTAL_00600_mg_L` = as.numeric(NA), 
                            `NITROGEN_AMMONIA_DISSOLVED_00608_mg_L` = as.numeric(NA), `NITROGEN_AMMONIA_TOTAL_00610_mg_L` = as.numeric(NA), 
                            `NITROGEN_NITRITE_DISSOLVED_00613_mg_L` = as.numeric(NA), `NITROGEN_NITRITE_TOTAL_00615_mg_L` = as.numeric(NA), 
                            `NITROGEN_NITRATE_DISSOLVED_00618_mg_L` = as.numeric(NA), `NITROGEN_NITRATE_TOTAL_00620_mg_L` = as.numeric(NA), 
                            `NITROGEN_KJELDAHL_TOTAL_00625_mg_L` = as.numeric(NA), `NITRITE+NITRATE_TOTAL_00630_mg_L` = as.numeric(NA), 
                            `NITRITE+NITRATE_DISSOLVED_00631_mg_L` = as.numeric(NA), `PHOSPHORUS_TOTAL_00665_mg_L` = as.numeric(NA), 
                            `PHOSPHORUS_DISSOLVED_00666_mg_L` = as.numeric(NA), `PHOSPHORUS_DISSOLVED_ORTHOPHOSPHATE_00671_mg_L` = as.numeric(NA), 
                            `HARDNESS_TOTAL_00900_mg_L` = as.numeric(NA), `CHLORIDE_TOTAL_00940_mg_L` = as.numeric(NA), 
                            `CHLORIDE_DISSOLVED_00941_mg_L` = as.numeric(NA), `SULFATE_TOTAL_00945_mg_L` = as.numeric(NA), 
                            `SULFATE_DISSOLVED_00946_mg_L` = as.numeric(NA), `FECAL_COLIFORM_31616_NO_100mL` = as.numeric(NA), 
                            `E._COLI_31648_NO_100mL` = as.numeric(NA), `ENTEROCOCCI_31649_NO_100mL` = as.numeric(NA), 
                            `CHLOROPHYLL_32211_ug_L` = as.numeric(NA), `PHOSPHOROUS_PARTICULATE_49567_mg_L` = as.numeric(NA), 
                            `NITROGEN_PARTICULATE_49570_mg_L` = as.numeric(NA), `NITROGEN_TOTAL_DISSOLVED_49571_mg_L` = as.numeric(NA), 
                            `PHOSPHOROUS_TOTAL_DISSOLVED_49572_mg_L` = as.numeric(NA), `PHOSPHORUS_TOTAL_ORTHOPHOSPHATE_70507_mg_L` = as.numeric(NA), 
                            `E.COLI_ECOLI_CFU_100mL` = as.numeric(NA), `ORTHOPHOSPHATE_DISSOLVED_OPWLF_mg_L` = as.numeric(NA), 
                            `PHOSPHORUS_SUSPENDED_INORGANIC_PIPLF_mg_L` = as.numeric(NA), `PHOSPHORUS_PARTICULATE_PPWLF_mg_L` = as.numeric(NA), 
                            `SSC-TOTAL_00530_mg_L` = as.numeric(NA), `NITROGEN_TOTAL_DISSOLVED_TDNLF_mg_L` = as.numeric(NA), 
                            `PHOSPHORUS_TOTAL_DISSOLVED_TDPLF_mg_L` = as.numeric(NA), `TOTAL_SUSPENDED_SOLIDS_TSS45_mg_L` = as.numeric(NA) )
remarkTemplate <- tibble(Ana_Id = as.character(NA), Ana_Sam_Fdt_Id = as.character(NA), Ana_Sam_Mrs_Container_Id_Desc = as.character(NA), 
                         `RMK_00530` = as.character(NA), `RMK_00600` = as.character(NA), 
                         `RMK_00608` = as.character(NA), `RMK_00610` = as.character(NA), 
                         `RMK_00613` = as.character(NA), `RMK_00615` = as.character(NA), 
                         `RMK_00618` = as.character(NA), `RMK_00620` = as.character(NA), 
                         `RMK_00625` = as.character(NA), `RMK_00630` = as.character(NA), 
                         `RMK_00631` = as.character(NA), `RMK_00665` = as.character(NA), 
                         `RMK_00666` = as.character(NA), `RMK_00671` = as.character(NA), 
                         `RMK_00900` = as.character(NA), `RMK_00940` = as.character(NA), 
                         `RMK_00941` = as.character(NA), `RMK_00945` = as.character(NA), 
                         `RMK_00946` = as.character(NA), `RMK_31616` = as.character(NA), 
                         `RMK_31648` = as.character(NA), `RMK_31649` = as.character(NA), 
                         `RMK_32211` = as.character(NA), `RMK_49567` = as.character(NA), 
                         `RMK_49570` = as.character(NA), `RMK_49571` = as.character(NA), 
                         `RMK_49572` = as.character(NA), `RMK_70507` = as.character(NA), 
                         `RMK_ECOLI` = as.character(NA), `RMK_OPWLF` = as.character(NA), 
                         `RMK_PIPLF` = as.character(NA), `RMK_PPWLF` = as.character(NA), 
                         `RMK_SSC_TOTAL_00530` = as.character(NA), `RMK_TDNLF` = as.character(NA), 
                         `RMK_TDPLF` = as.character(NA), `RMK_TSS45` = as.character(NA))

stationAnalyteDataUserFilter1 <- 
  bind_rows(parameterTemplate, 
            stationAnalyteDataUserFilter0 %>% 
              dplyr::select(Ana_Id, Ana_Sam_Fdt_Id, Ana_Sam_Mrs_Container_Id_Desc,  ParameterName, Ana_Uncensored_Value) %>% 
              pivot_wider( names_from = 'ParameterName', values_from = 'Ana_Uncensored_Value'))  #%>%
stationAnalyteDataUserFilter1 <- stationAnalyteDataUserFilter1 %>% 
#   dplyr::select(`NITROGEN_TOTAL_00600_mg_L`, NITROGEN_KJELDAHL_TOTAL_00625_mg_L, `NITRITE+NITRATE_TOTAL_00630_mg_L`)
# stationAnalyteDataUserFilter1[1,1] <- 4
# stationAnalyteDataUserFilter1[2,2] <- 3
# stationAnalyteDataUserFilter1[2,3] <- 2
# stationAnalyteDataUserFilter1[3,2] <- 1
# View(stationAnalyteDataUserFilter1 %>%
       rowwise() %>% 
  
  # For sum in this step, use bazar::sumNA to return NA when none of the fields are populated (instead of 0 which is returned using sum(..., na.rm=T))
  mutate(ECOLI = coalesce(`E.COLI_ECOLI_CFU_100mL`, `E._COLI_31648_NO_100mL`),
         NITROGEN_mg_L = coalesce(NITROGEN_TOTAL_00600_mg_L,
                                  bazar::sumNA(NITROGEN_KJELDAHL_TOTAL_00625_mg_L, `NITRITE+NITRATE_TOTAL_00630_mg_L`, na.rm = T)),
         NOX_mg_L = ifelse(is.na(`NITRITE+NITRATE_TOTAL_00630_mg_L`), 
                           bazar::sumNA(NITROGEN_NITRITE_TOTAL_00615_mg_L, NITROGEN_NITRATE_TOTAL_00620_mg_L, na.rm = T),
                           `NITRITE+NITRATE_TOTAL_00630_mg_L`), # Is this even right? Roger doesn't provide an alternate option if NITRITE+NITRATE_TOTAL_00630_mg_L exists so just assuming things here
         NITROGEN_mg_L = ifelse(is.na(NITROGEN_TOTAL_00600_mg_L), 
                                bazar::sumNA(NITROGEN_NITRITE_TOTAL_00615_mg_L, NITROGEN_NITRATE_TOTAL_00620_mg_L, 
                                    NITROGEN_KJELDAHL_TOTAL_00625_mg_L, na.rm = T), 
                                NITROGEN_mg_L),  # Is this even right? Roger doesn't provide an alternate option
         NITROGEN_mg_L = ifelse(is.na(NITROGEN_TOTAL_00600_mg_L), 
                                bazar::sumNA(NITROGEN_PARTICULATE_49570_mg_L, NITROGEN_TOTAL_DISSOLVED_49571_mg_L, na.rm = T), 
                                NITROGEN_mg_L),  # Is this even right? Roger doesn't provide an alternate option
         AMMONIA_mg_L = coalesce(NITROGEN_AMMONIA_TOTAL_00610_mg_L, NITROGEN_AMMONIA_DISSOLVED_00608_mg_L),
         # secondary AMMONIA_mg_L adjustment based on remark codes when dataset later combined with remarks
         NITRATE_mg_L = coalesce(NITROGEN_NITRATE_TOTAL_00620_mg_L, NITROGEN_NITRATE_DISSOLVED_00618_mg_L),
         PHOSPHORUS_mg_L = coalesce(PHOSPHORUS_TOTAL_00665_mg_L, PHOSPHORUS_TOTAL_ORTHOPHOSPHATE_70507_mg_L, 
                                    PHOSPHORUS_DISSOLVED_00666_mg_L, PHOSPHORUS_DISSOLVED_ORTHOPHOSPHATE_00671_mg_L, 
                                    bazar::sumNA(PHOSPHOROUS_PARTICULATE_49567_mg_L, PHOSPHOROUS_TOTAL_DISSOLVED_49572_mg_L, na.rm = T)), # should the order really be total, ortho, then dissolved???
         TSS_mg_L = coalesce(TOTAL_SUSPENDED_SOLIDS_00530_mg_L, `SSC-TOTAL_00530_mg_L`, TOTAL_SUSPENDED_SOLIDS_TSS45_mg_L),
         CHLORIDE_mg_L = ifelse(CHLORIDE_DISSOLVED_00941_mg_L > CHLORIDE_TOTAL_00940_mg_L, CHLORIDE_DISSOLVED_00941_mg_L, CHLORIDE_TOTAL_00940_mg_L),
         SULFATE_mg_L = ifelse(SULFATE_DISSOLVED_00946_mg_L > SULFATE_TOTAL_00945_mg_L, SULFATE_DISSOLVED_00946_mg_L, SULFATE_TOTAL_00945_mg_L),
         
         # EVJ added these even though not called for in this part of conventionals, no idea where comes from otherwise
         
         SULFATE_TOTAL_mg_L = SULFATE_TOTAL_00945_mg_L,
         SULFATE_DISS_mg_L = SULFATE_DISSOLVED_00946_mg_L,
         ECOLI_31648_NO_100mL = `E._COLI_31648_NO_100mL`,
         ENTEROCOCCI = ENTEROCOCCI_31649_NO_100mL,
         FECAL_COLI = FECAL_COLIFORM_31616_NO_100mL,
         CHLOROPHYLL_A_ug_L = CHLOROPHYLL_32211_ug_L,
         SSC_mg_L = `SSC-TOTAL_00530_mg_L`
         ) %>% 
  # now limit to just necessary columns for Conventionals format
  dplyr::select(Ana_Id, Ana_Sam_Fdt_Id, Ana_Sam_Mrs_Container_Id_Desc,
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
  filter(! is.na(Ana_Id)) # drop blank row from row_bind
                

         
         
         
# now fix remark fields
stationAnalyteDataUserFilter2 <- 
  bind_rows(remarkTemplate, 
            stationAnalyteDataUserFilter %>% 
              dplyr::select(Ana_Id, Ana_Sam_Fdt_Id, Ana_Sam_Mrs_Container_Id_Desc, Pg_Storet_Code, Ana_Uncensored_Val_Comment) %>% 
              pivot_wider( names_from = 'Pg_Storet_Code', names_prefix = "RMK_", values_from = 'Ana_Uncensored_Val_Comment') ) %>% 
  mutate(RMK_ECOLI = coalesce(`RMK_ECOLI`, `RMK_31648`),
         RMK_NITROGEN = coalesce(RMK_49570, RMK_49571), # what about NITROGEN_TOTAL_00600_mg_L remarks????
         RMK_AMMONIA = coalesce(RMK_00610, RMK_00608),
         RMK_NITRATE = coalesce(RMK_00620, RMK_00618),
         RMK_PHOSPHORUS = coalesce(RMK_00665, RMK_70507, RMK_00666, RMK_00671, 
                                    RMK_49567, RMK_49572), # don't love the logic on this one, double check me
         RMK_TSS = coalesce(RMK_00530, RMK_SSC_TOTAL_00530, RMK_TSS45),
         RMK_CHLORIDE = RMK_00940, # placeholder for now, fixed once combined with parameter data
         RMK_SULFATE = RMK_00945,  # placeholder for now, fixed once combined with parameter data
         LEVEL_NITROGEN = as.character(NA),
         LEVEL_AMMONIA = as.character(NA),
         LEVEL_NITRATE = as.character(NA),
         
         # rinse and repeat
         
  ) %>% 
  dplyr::select(Ana_Id, Ana_Sam_Fdt_Id, Ana_Sam_Mrs_Container_Id_Desc,
                RMK_NITROGEN, LEVEL_NITROGEN, RMK_AMMONIA, LEVEL_AMMONIA, RMK_NITRATE, LEVEL_NITRATE
                
                
                # rinse and repeat
                )

         
            


## Combine data and remarks (and empty Level fields)

mutate(
# secondary adjustment based on remark codes when combined with remarks
AMMONIA_mg_L = ifelse(NITROGEN_AMMONIA_DISSOLVED_00608_mg_L > NITROGEN_AMMONIA_TOTAL_00610_mg_L && RMK_00610 %in% c('U','QQ') && is.na(RMK_00608),
                      NITROGEN_AMMONIA_DISSOLVED_00608_mg_L, AMMONIA_mg_L),
RMK_AMMONIA = ifelse(NITROGEN_AMMONIA_DISSOLVED_00608_mg_L > NITROGEN_AMMONIA_TOTAL_00610_mg_L && RMK_00610 %in% c('U','QQ'),
                      RMK_00608, RMK_AMMONIA),
RMK_CHLORIDE = ifelse(CHLORIDE_DISSOLVED_00941_mg_L > CHLORIDE_TOTAL_00940_mg_L, RMK_00941, RMK_00940), # fix from above
RMK_SULFATE = ifelse(SULFATE_DISSOLVED_00946_mg_L > SULFATE_TOTAL_00945_mg_L, RMK_00946, RMK_00945)) # fix from above

)
