
# Metals functions

# Metals criteria analysis
metalsCriteriaFunction <- function(ID, Hardness, WER){
  # Remember: ln is really log() in R; exp() is natural antilog in R
  # Per 9VAC25-260-140, criteria to 2 sig figs #https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section140/
  
  # If no WER supplied, use 1
  WER <- ifelse(is.na(WER), 1, WER)
  # Establish Hardness Criteria
  criteriaHardness <- ifelse(Hardness < 25, 25, ifelse(Hardness > 400, 400, Hardness))
  
  metalsCriteria <- suppressWarnings(
    tibble(ID= ID, `Antimony PWS` = 5.6, `Antimony All Other Surface Waters` = 640,
           `Arsenic Acute Freshwater` = 340, `Arsenic Chronic Freshwater` = 150, `Arsenic PWS` = 10,
           `Arsenic Acute Saltwater` = 69, `Arsenic Chronic Saltwater` = 36,
           `Barium PWS` = 2000,
           `Cadmium Acute Freshwater` =  signif(WER * exp(0.9789 * (log(criteriaHardness))-3.866) * (1.136672 - (log(criteriaHardness) * 0.041838)), digits = 2),
           `Cadmium Chronic Freshwater` = signif(WER * exp(0.7977 * log(criteriaHardness) - 3.909) * (1.101672 - (log(criteriaHardness) * (0.041838))), digits = 2),
           `Cadmium Acute Saltwater` = signif(33 * WER, digits = 2), `Cadmium Chronic Saltwater` = signif(7.9 * WER, digits = 2), `Cadmium PWS` = 5,
           `ChromiumIII Acute Freshwater` = signif(WER *  (exp(0.8190 * (log(criteriaHardness)) + 3.7256)) * 0.316, digits = 2), 
           `ChromiumIII Chronic Freshwater` = signif(WER *  (exp(0.8190 * (log(criteriaHardness))  +0.6848)) * 0.860, digits = 2), `ChromiumIII PWS` = 100,
           `ChromiumVI Acute Freshwater` = 16, `ChromiumVI Chronic Freshwater` = 11, `ChromiumVI Acute Saltwater` = 1100, `ChromiumVI Chronic Saltwater` = 50, 
           `Copper Acute Freshwater` =  signif(WER * (exp(0.9422 * log(criteriaHardness) - 1.700)) * 0.960, digits = 2),
           `Copper Chronic Freshwater` = signif(WER * (exp(0.8545 * log(criteriaHardness) - 1.702)) * 0.960, digits = 2),
           `Copper Acute Saltwater` =  signif(9.3 * WER, digits = 2), `Copper Chronic Saltwater` =  signif(6.0 * WER, digits = 2), `Copper PWS` = 1300,
           `Lead Acute Freshwater` = signif(WER * (exp(1.273 * log(criteriaHardness) - 1.084)) * (1.46203 - (log(criteriaHardness) * 0.145712)), digits = 2),
           `Lead Chronic Freshwater` = signif(WER * (exp(1.273 * log(criteriaHardness) - 3.259)) * (1.46203 - (log(criteriaHardness) * 0.145712)), digits = 2),
           `Lead Acute Saltwater` = signif(230 * WER, digits = 2), `Lead Chronic Saltwater` = signif(8.8 * WER, digits = 2), `Lead PWS` = 15,
           `Mercury Acute Freshwater` = 1.4, `Mercury Chronic Freshwater` = 0.77, `Mercury Acute Saltwater` = 1.8, `Mercury Chronic Saltwater` = 0.94,
           `Nickel Acute Freshwater` = signif(WER * (exp (0.8460 * log(criteriaHardness) + 1.312)) * 0.998, digits = 2), 
           `Nickel Chronic Freshwater` = signif(WER * (exp(0.8460 * log(criteriaHardness) - 0.8840)) * 0.997, digits = 2),
           `Nickel Acute Saltwater` = signif(74 * WER, digits = 2), `Nickel Chronic Saltwater` = signif(8.2 * WER, digits = 2), `Nickel PWS` = 610,  `Nickel All Other Surface Waters` = 4600,
           `Uranium PWS` = 30,
           `Selenium Acute Freshwater` = 20, `Selenium Chronic Freshwater` = 5.0, 
           `Selenium Acute Saltwater` = signif(290 * WER, digits = 2), `Selenium Chronic Saltwater` = signif(71 * WER, digits = 2),
           `Selenium PWS` = 170, `Selenium All Other Surface Waters` = 4200,
           `Silver Acute Freshwater` = signif(WER * (exp(1.72 * log(criteriaHardness) - 6.52)) * 0.85, digits = 2), `Silver Acute Saltwater` = signif(1.9 * WER, digits = 2),
           `Thallium PWS` = 0.24, `Thallium All Other Surface Waters` = 0.47,
           `Zinc Acute Freshwater` = signif(WER * (exp(0.8473 * log(criteriaHardness) + 0.884)) * 0.978, digits = 2),
           `Zinc Chronic Freshwater` = signif(WER * (exp(0.8473 * log(criteriaHardness) + 0.884)) * 0.986, digits = 2),
           `Zinc Acute Saltwater` = signif(90 * WER, digits = 2), `Zinc Chronic Saltwater` = signif(81 * WER, digits = 2), 
           `Zinc PWS` = 7400, `Zinc All Other Surface Waters` = 26000) %>% 
      pivot_longer(!ID, names_to = 'Criteria', values_to = 'CriteriaValue') %>% 
      mutate(Criteria2 = Criteria) %>%  #duplicate column to split
      separate(Criteria2, c("Metal", "Criteria Type", "Waterbody"), sep = " ") %>% 
      mutate(`Criteria Type` = ifelse(`Criteria Type` == 'All', 'All Other Waters', `Criteria Type`),
             Waterbody = ifelse(Waterbody == 'Other', NA, Waterbody)) %>% 
      dplyr::select(ID, Metal, Criteria, `Criteria Type`, Waterbody, CriteriaValue))
  return(metalsCriteria)
}
#criteria <- metalsCriteriaFunction(stationMetalsData[1,]$Hardness, WER = 1)

# Single station Metals analysis
metalsAnalysisFlexible <- function(stationMetalsData, CLASS, ZONE, PWS, WER){
  # If no WER supplied, use 1
  WER <- ifelse(is.na(WER), 1, WER)
  
  # Get WQS from stationData so correct criteria can be applied
  stationMetalsData <- stationMetalsData %>% 
    mutate(CLASS = CLASS, ZONE = ZONE, PWS = PWS) %>% # add these based on user selection
    mutate(`Assess As` = case_when(CLASS == "I" ~ 'Saltwater',
                                   CLASS == "II" & ZONE == 'Estuarine' ~ 'Saltwater',
                                   CLASS == "II" & ZONE == 'Transition' ~ 'More Stringent',
                                   CLASS == "II" & ZONE == 'Tidal Fresh' ~ 'Freshwater',
                                   CLASS %in% c('III', "IV","V","VI","VII") ~ 'Freshwater',
                                   TRUE ~ as.character(NA)),
           ChromiumIII= Chromium, RMK_ChromiumIII = RMK_Chromium, 
           ChromiumVI= Chromium, RMK_ChromiumVI = RMK_Chromium ) %>% # add individual Chromium variables to make joining to assessment criteria easier
    # Roger uses ChromiumIII and VI to flag any potential chromium issues, likely further lab analyses needed if either chromium criteria blown
    dplyr::select(Station_Id:RMK_Cadmium, ChromiumIII:RMK_ChromiumVI, Cadmium:RMK_Hardness, CLASS, PWS, ZONE, `Assess As`)
  
  # make a place to store raw analysis results
  rawCriteriaResults <- tibble(Station_Id = as.character(NA), WindowDateTimeStart = as.POSIXct(NA), FDT_DEPTH = as.numeric(NA),
                               CLASS = as.factor(NA), PWS = as.factor(NA), ZONE = as.factor(NA), `Assess As` = as.character(NA),
                               Metal = as.character(NA), ValueType = as.character(NA), Value = as.numeric(NA), Criteria = as.character(NA), 
                               `Criteria Type` = as.character(NA), Waterbody = as.character(NA), CriteriaValue = as.numeric(NA),
                               parameterRound = as.numeric(NA), Exceedance = as.numeric(NA))
  acuteCriteriaResults <- rawCriteriaResults 
  chronicCriteriaResults <- acuteCriteriaResults 
  
  # loop through each row of data to correctly calculate criteria and find any chronic scenarios
  for(k in stationMetalsData$FDT_DATE_TIME){
    rawDataWindow <- filter(stationMetalsData, FDT_DATE_TIME == k)
    acuteDataWindow <- filter(stationMetalsData,  between(FDT_DATE_TIME, k, k + hours(1)))
    chronicDataWindow <- filter(stationMetalsData,  between(FDT_DATE_TIME, k, k + days(4)))
    # Run any analyses requiring raw data if data exists
    if(nrow(rawDataWindow) > 0){
      rawData <- rawDataWindow %>% 
        group_by(Station_Id, FDT_DATE_TIME, FDT_DEPTH, CLASS, PWS, ZONE, `Assess As`) %>% 
        dplyr::select(-c(contains('RMK_'))) %>% 
        pivot_longer(cols = Antimony:Hardness, names_to = "Metal", values_to = "Value") %>% 
        mutate(ValueType = 'Raw Result',
               ID = paste(Station_Id, FDT_DATE_TIME, FDT_DEPTH, sep = '_')) %>% # make a uniqueID in case >1 sample for given datetime
        ungroup()
      # Calculate criteria based on raw data
      rawDataCriteria <- metalsCriteriaFunction(filter(rawData, Metal == "Hardness")$ID, filter(rawData, Metal == "Hardness")$Value, WER = 1) %>% 
        filter(`Criteria Type` %in% c('All Other Waters', 'PWS')) %>% # don't need other criteria for acute window
        {if(is.na(unique(rawData$PWS)))
          filter(., `Criteria Type` != 'PWS')
          else .}
      # Join appropriate criteria to rawData for comparison to averaged data
      rawDataCriteriaAnalysis <- left_join(rawData, rawDataCriteria, by = c('ID', 'Metal')) %>% 
        mutate(parameterRound = signif(Value, digits = 2), # two significant figures based on WQS https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section140/
               Exceedance = ifelse(parameterRound > parameterRound, 1, 0 ),
               WindowDateTimeStart = min(rawDataWindow$FDT_DATE_TIME)) %>%  # use 1/0 to easily summarize multiple results later
        filter(!is.na(Criteria)) %>%  # filter out metals that don't have chronic criteria
        dplyr::select(Station_Id, WindowDateTimeStart, everything()) %>% 
        dplyr::select(-c(FDT_DATE_TIME, ID))
      # Save the results for viewing later
      rawCriteriaResults <- bind_rows(rawCriteriaResults, rawDataCriteriaAnalysis) 
    } else {rawCriteriaResults <- rawCriteriaResults }
    # Run acute analysis if data exists
    if(nrow(acuteDataWindow) > 0){
      acuteData <- acuteDataWindow %>% 
        group_by(Station_Id, FDT_DEPTH, CLASS, PWS, ZONE, `Assess As`) %>% # can't group by datetime or summary can't happen
        dplyr::select(-c(contains('RMK_'))) %>% 
        pivot_longer(cols = Antimony:Hardness, names_to = "Metal", values_to = "CriteriaValue") %>% 
        ungroup() %>% group_by(Station_Id, FDT_DEPTH, CLASS, PWS, ZONE, `Assess As`, Metal) %>% 
        summarise(Value = mean(CriteriaValue, na.rm=T)) %>%  # get hourly average
        mutate(ValueType = 'Hourly Average',
               ID = paste(Station_Id, FDT_DEPTH, sep = '_')) # make a uniqueID in case >1 sample for given datetime
      # Calculate criteria based on hourly averaged data
      acuteDataCriteria <- metalsCriteriaFunction(filter(acuteData, Metal == "Hardness")$ID, filter(acuteData, Metal == "Hardness")$Value, WER = 1) %>% 
        filter(`Criteria Type` == 'Acute') %>% # don't need other criteria for acute window
        # Keep only the criteria needed 
        {if(unique(acuteData$`Assess As`) %in% c('Freshwater', 'Saltwater'))
          filter(., Waterbody %in% c(NA, !!unique(acuteData$`Assess As`)))
          # if in Transition Zone then use the more stringent standard
          else group_by(., Metal) %>% 
            mutate(MoreStringent = min(CriteriaValue)) %>% 
            filter(CriteriaValue == MoreStringent) %>% 
            dplyr::select(-MoreStringent)} 
      # Join appropriate criteria to acuteData for comparison to averaged data
      acuteDataCriteriaAnalysis <- left_join(acuteData, acuteDataCriteria, by = c('ID', 'Metal')) %>% 
        mutate(parameterRound = signif(Value, digits = 2), # two significant figures based on WQS https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section140/
               Exceedance = ifelse(parameterRound > CriteriaValue, 1, 0 ), # use 1/0 to easily summarize multiple results later
               WindowDateTimeStart = min(acuteDataWindow$FDT_DATE_TIME)) %>% 
        filter(!is.na(Criteria)) %>% # filter out metals that don't have chronic criteria
        dplyr::select(Station_Id, WindowDateTimeStart, everything(), -ID)
      # Save the results for viewing later
      acuteCriteriaResults <- bind_rows(acuteCriteriaResults, acuteDataCriteriaAnalysis) 
    } else {acuteCriteriaResults <- acuteCriteriaResults }
    # Run chronic analysis if data exists
    if(nrow(chronicDataWindow) > 0){
      chronicData <- chronicDataWindow %>% 
        group_by(Station_Id, FDT_DEPTH, CLASS, PWS, ZONE, `Assess As`) %>% # can't group by datetime or summary can't happen
        dplyr::select(-c(contains('RMK_'))) %>% 
        pivot_longer(cols = Antimony:Hardness, names_to = "Metal", values_to = "CriteriaValue") %>% 
        ungroup() %>% group_by(Station_Id, FDT_DEPTH, CLASS, PWS, ZONE, `Assess As`, Metal) %>% 
        summarise(Value = mean(CriteriaValue, na.rm=T)) %>% # get four day average
        mutate(ValueType = 'Four Day Average',
               ID = paste(Station_Id, FDT_DEPTH, sep = '_')) # make a uniqueID in case >1 sample for given datetime
      # Calculate criteria based on hourly averaged data
      chronicDataCriteria <- metalsCriteriaFunction(filter(chronicData, Metal == "Hardness")$ID, filter(chronicData, Metal == "Hardness")$Value, WER = 1) %>% 
        filter(`Criteria Type` == 'Chronic') %>% # don't need other criteria for chronic window analysis
        # Keep only the criteria needed 
        {if(unique(chronicData$`Assess As`) %in% c('Freshwater', 'Saltwater'))
          filter(., Waterbody %in% c(NA, !!unique(chronicData$`Assess As`)))
          # if in Transition Zone then use the more stringent standard
          else group_by(., Metal) %>% 
            mutate(MoreStringent = min(CriteriaValue)) %>% 
            filter(CriteriaValue == MoreStringent) %>% 
            dplyr::select(-MoreStringent)} 
      # Join appropriate criteria to chronicData for comparison to averaged data
      chronicDataCriteriaAnalysis <- left_join(chronicData, chronicDataCriteria, by = c('ID', 'Metal')) %>% 
        mutate(parameterRound = signif(Value, digits = 2), # two significant figures based on WQS https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section140/
               Exceedance = ifelse(parameterRound > CriteriaValue, 1, 0 ), # use 1/0 to easily summarize multiple results later
               WindowDateTimeStart = min(chronicDataWindow$FDT_DATE_TIME)) %>% 
        filter(!is.na(Criteria)) %>% # filter out metals that don't have chronic criteria
        dplyr::select(Station_Id, WindowDateTimeStart, everything(), -ID)
      # Save the results for viewing later
      chronicCriteriaResults <- bind_rows(chronicCriteriaResults, chronicDataCriteriaAnalysis) 
    } else {chronicCriteriaResults <- chronicCriteriaResults }
  }
  stationCriteriaResults <- bind_rows(rawCriteriaResults, acuteCriteriaResults, chronicCriteriaResults) %>% 
    filter(!is.na(Station_Id)) %>% # drop placeholder rows
    distinct(Station_Id, WindowDateTimeStart, FDT_DEPTH, Criteria, .keep_all = T) %>% # remove duplicates in case > 1 depth per datetime
    arrange(Station_Id, WindowDateTimeStart, FDT_DEPTH, Metal)
  return(stationCriteriaResults)
}
# Metals Assessment function that makes sense of output from metalsAnalysis()
metalsAssessmentFunction <- function(metalsAnalysisResults){
  metalsAnalysisResults %>% 
    group_by(Station_Id, Criteria) %>% 
    summarise(Exceedances = sum(Exceedance, na.rm = T)) %>% 
    arrange(Criteria) %>% # arrange on just Criteria to make column order make more sense 
    pivot_wider(names_from = Criteria, values_from = Exceedances)
}


dissolvedMetalsCriteriaUI <- function(id){
  ns <- NS(id)
  tagList(
    h5('All water column metals data available for the selected site and additional filters are available below. 
       If no data is presented, then the station does not have any water column metals data available.'),
    helpText("The criteria applicable to a given station for dissolved metals depends on the station's Water 
             Quality Standards (WQS) and a Water Effects Ratio (WER). Where a station does not have WQS assigned
             by Regional Assessment staff, the user may assign applicable WQS for the analysis. Please consult the",
             span(strong(a('DEQ GIS Staff App', href='https://gis.deq.virginia.gov/GISStaffApplication/',  target='_blank'))),
             "for information on WQS. The WER default is 1 unless additional information supports using a different number."),
    fluidRow(column(3, uiOutput(ns('stationSelected_'))),
             column(3, uiOutput(ns('WQSclass_')),
                    uiOutput(ns('WQSzone_'))),
             column(3, uiOutput(ns('PWSinfo_'))),
             column(3, numericInput(ns('WER'), strong('Water Effects Ratio (WER) used for module analysis'), value = 1))),
    h5('A summary of the exceedances of water column metals criteria available for the selected site and additional filters 
        are available below. If no data is presented, then the station does not have any water column metals data available. 
       Hardness based criteria are calculated as applicable.'),
    DT::dataTableOutput(ns('WCmetalsSingleSiteSummary')),
    #verbatimTextOutput(ns('test')),
    hr(), 
    fluidRow(
      column(7, helpText("Below are the calculated results associated with the selected site and additional filters. You can view all the
                          analyzed data associated with the site by using the 'All' selection, or you may choose a particular 
                          criteria to investigate further by choosing that in the drop down. The criteria chosen in the drop down 
                         will filter the table to just that selected criteria."),
             h5(strong('Analyzed Data')),
             uiOutput(ns('criteriaChoice_')),
             DT::dataTableOutput(ns('analyzedData'))),
      column(5, helpText('Choose a specific criteria using the drop down to left to reveal a detailed interactive plot of the data
                         included in the selected criteria window. When criteria with static limits are selected, a
                         black dashed line appears corresponding to the appropriate criteria. When criteria with hardness based limits 
                         are selected, measured values appear as gray if they fall below the calculated criteria and red if they exceed 
                         the calculated criteria.'),
             plotlyOutput(ns('plotlyZoom'))))
  )
  }
             
    
dissolvedMetalsCriteria <-  function(input,output,session, multistationFieldAnalyte, WQSlookup, staticLimit){
  ns <- session$ns
  # Choose a station to work with
  output$stationSelected_ <- renderUI({req(multistationFieldAnalyte)
    selectInput(ns('stationSelected'), 'Select Station:', choices = sort(unique(multistationFieldAnalyte$Fdt_Sta_Id)))})
  
  # Join WQS to basicData
  metalsDataWQS <- reactive({multistationFieldAnalyte %>% 
      {if(! 'Uranium, total, ug/L' %in% names(multistationFieldAnalyte))
        mutate(., `Uranium, total, ug/L` = NA, `Uranium, total, ug/L RMK` = NA)
        else . } %>% 
      dplyr::select(Station_Id = Fdt_Sta_Id, FDT_DATE_TIME = Fdt_Date_Time, FDT_DEPTH = Fdt_Depth, # include depth bc a few samples taken same datetime but different depths
                    METAL_Antimony = `ANTIMONY, DISSOLVED (UG/L AS SB)`, RMK_Antimony = `ANTIMONY, DISSOLVED (UG/L AS SB) RMK`,
                    METAL_Arsenic = `ARSENIC, DISSOLVED  (UG/L AS AS)`, RMK_Arsenic = `ARSENIC, DISSOLVED  (UG/L AS AS) RMK`, 
                    METAL_Barium = `BARIUM, DISSOLVED (UG/L AS BA)`, RMK_Barium = `BARIUM, DISSOLVED (UG/L AS BA) RMK`, 
                    METAL_Cadmium = `CADMIUM, DISSOLVED (UG/L AS CD)`, RMK_Cadmium = `CADMIUM, DISSOLVED (UG/L AS CD) RMK`,
                    METAL_Chromium = `CHROMIUM, DISSOLVED (UG/L AS CR)`, RMK_Chromium = `CHROMIUM, DISSOLVED (UG/L AS CR) RMK`, 
                    # Chromium III and ChromiumVI dealt with inside metalsAnalysis()
                    METAL_Copper = `COPPER, DISSOLVED (UG/L AS CU)`, RMK_Copper = `COPPER, DISSOLVED (UG/L AS CU) RMK`, 
                    METAL_Lead = `LEAD, DISSOLVED (UG/L AS PB)`, RMK_Lead = `LEAD, DISSOLVED (UG/L AS PB) RMK`, 
                    METAL_Mercury = `MERCURY-TL,FILTERED WATER,ULTRATRACE METHOD NG/L`, RMK_Mercury = `MERCURY-TL,FILTERED WATER,ULTRATRACE METHOD NG/L RMK`,
                    METAL_Nickel = `NICKEL, DISSOLVED (UG/L AS NI)`, RMK_Nickel = `NICKEL, DISSOLVED (UG/L AS NI) RMK`, 
                    METAL_Uranium = `Uranium, total, ug/L`, RMK_Uranium = `Uranium, total, ug/L RMK`, 
                    METAL_Selenium = `SELENIUM, DISSOLVED (UG/L AS SE)`, RMK_Selenium = `SELENIUM, DISSOLVED (UG/L AS SE) RMK`, 
                    METAL_Silver = `SILVER, DISSOLVED (UG/L AS AG)`, RMK_Silver = `SILVER, DISSOLVED (UG/L AS AG) RMK`, 
                    METAL_Thallium = `THALLIUM, DISSOLVED (UG/L AS TL)`, RMK_Thallium = `THALLIUM, DISSOLVED (UG/L AS TL) RMK`,
                    METAL_Zinc = `ZINC, DISSOLVED (UG/L AS ZN)`, RMK_Zinc = `ZINC, DISSOLVED (UG/L AS ZN) RMK`,
                    METAL_Hardness = `HARDNESS, CA MG CALCULATED (MG/L AS CACO3) AS DISSOLVED`, 
                    RMK_Hardness = `HARDNESS, CA MG CALCULATED (MG/L AS CACO3) AS DISSOLVED RMK`) %>% 
      mutate(METAL_Mercury = METAL_Mercury/1000) %>% # convert to ug/L
      group_by(Station_Id, FDT_DATE_TIME, FDT_DEPTH) %>% 
      mutate_if(is.numeric, as.character) %>% 
      pivot_longer(cols = METAL_Antimony:RMK_Hardness, #RMK_Antimony:RMK_Hardness, 
                   names_to = c('Type', 'Metal'),
                   names_sep = "_",
                   values_to = 'Value') %>% 
      ungroup() %>% group_by(Station_Id, FDT_DATE_TIME, FDT_DEPTH, Metal) %>% 
      filter(!is.na(Value)) %>%  #drop NA rows to pivot better
      pivot_wider(id_cols = c(Station_Id, FDT_DATE_TIME, FDT_DEPTH, Metal), names_from = Type, values_from = Value) %>% # pivot remark wider so the appropriate metal value is dropped when filtering on lab comment codes
      filter(! RMK %in% c('IF', 'J', 'O', 'QF', 'V')) %>% # lab codes dropped from further analysis
      pivot_longer(cols= METAL:RMK, names_to = 'Type', values_to = 'Value') %>% # get in appropriate format to flip wide again
      pivot_wider(id_cols = c(Station_Id, FDT_DATE_TIME, FDT_DEPTH), names_from = c(Type, Metal), names_sep = "_", values_from = Value) %>% 
      mutate_at(vars(contains('METAL')), as.numeric) %>%# change metals values back to numeric
      rename_with(~str_remove(., 'METAL_')) # drop METAL_ prefix for easier analyses
      
      })
  
  stationMetalsDataWQS <- reactive({req(input$stationSelected)
    filter(metalsDataWQS(), Station_Id %in% input$stationSelected) %>% 
      left_join(WQSlookup, by=c('Station_Id' = 'StationID')) })
 
  # Identify Class
  output$WQSclass_ <- renderUI({req(stationMetalsDataWQS())
    selectInput(ns('WQSclass'),strong('WQS For Analysis'), choices= c(NA, WQSvalues$CLASS),
                selected = unique(stationMetalsDataWQS()$CLASS)) })
  # Identify Zone
  output$WQSzone_ <- renderUI({req(stationMetalsDataWQS())
    selectInput(ns('WQSzone'),strong('WQS Zone For Analysis'), choices= c(NA, 'Estuarine', 'Transition', 'Tidal Fresh'),
                selected = unique(stationMetalsDataWQS()$ZONE)) })
  # Identify PWS
  output$PWSinfo_ <- renderUI({req(stationMetalsDataWQS())
    selectInput(ns('PWSinfo'),strong('PWS Status For Analysis'), choices= c(NA, 'Yes'),
                selected = unique(stationMetalsDataWQS()$PWS)) })
  
  stationMetalsAnalysis <- reactive({req(input$WQSclass, input$WQSzone, input$PWSinfo, input$WER, stationMetalsDataWQS())
    metalsAnalysisFlexible(dplyr::select(stationMetalsDataWQS(), Station_Id:RMK_Hardness),
                           input$WQSclass, input$WQSzone, input$PWSinfo, input$WER) })

  stationMetalsAnalysisSummary <- reactive({req(stationMetalsAnalysis())
    metalsAssessmentFunction(stationMetalsAnalysis())})
                 
  output$WCmetalsSingleSiteSummary <- DT::renderDataTable({ req(stationMetalsAnalysisSummary())
    datatable(stationMetalsAnalysisSummary(),
              rownames = F, escape= F, extensions = 'Buttons',
              options = list(dom = 'Bift', scrollX= TRUE, scrollY = '75px',
                             pageLength = nrow(stationMetalsAnalysisSummary()),
                             buttons=list('copy','colvis')))  })
  
  
  # Zoomed plot section
  output$criteriaChoice_ <- renderUI({req(stationMetalsAnalysis())
    selectizeInput(ns("criteriaChoice"), "Choose Criteria to visualize", choices = c('All', unique(stationMetalsAnalysis()$Criteria)), width = '40%')})
  
  output$analyzedData <- DT::renderDataTable({req(stationMetalsAnalysis(), input$criteriaChoice)
    z <- stationMetalsAnalysis() %>% 
      {if(input$criteriaChoice != 'All')
        filter(., Criteria %in% input$criteriaChoice)
        else . }
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "300px", dom='Bti', buttons=list('copy')),
                  selection = 'none') })
  #output$test <- renderPrint({input$criteriaChoice    })
  
  output$plotlyZoom <- renderPlotly({ req(stationMetalsAnalysis(), input$criteriaChoice !='All' )#input$analyzedData_rows_selected, 
    criteriaSelection <- input$criteriaChoice #stationMetalsAnalysis()[input$analyzedData_rows_selected, ]$Criteria
    dat <- filter(stationMetalsAnalysis(), Criteria %in%  criteriaSelection) %>%
      filter(Value != 'NaN') # drop any unmeasured values
    dat$SampleDate <- as.POSIXct(dat$WindowDateTimeStart, format="%m/%d/%y")
    
    plot_ly(data=dat) %>%
      {if(criteriaSelection %in% staticLimit)
        add_markers(., x= ~SampleDate, y= ~Value,mode = 'scatter', name=~Metal, marker = list(color= '#535559'),
                    hoverinfo="text",text=~paste(sep="<br>",
                                                 paste("StationID: ",Station_Id),
                                                 paste("Date: ",SampleDate),
                                                 paste("Depth: ",FDT_DEPTH, "m"),
                                                 paste(Metal,":",Value, "ug/L"),
                                                 paste('Static Criteria:', CriteriaValue, "ug/L"))) %>%
          add_lines(data=dat, x=~SampleDate,y=~CriteriaValue, mode='line', line = list(color = '#484a4c',dash = 'dot'),
                    hoverinfo = "text", text= ~paste(criteriaSelection, "Criteria:",  CriteriaValue, "ug/L"), name="Static Criteria")
        else add_markers(., data=dat, x= ~SampleDate, y= ~Value, mode = 'scatter', name=~Metal, marker = list(color= ~Exceedance), colors = c('#535559', 'red'), #color= ~Exceedance, #colors = c('#535559', 'red'),#marker = list(color= '#535559'),
                         symbol =  ~Exceedance, symbols = c(16,15),
                         hoverinfo="text",text=~paste(sep="<br>",
                                                      paste("StationID: ",Station_Id),
                                                      paste("Date: ",SampleDate),
                                                      paste("Depth: ",FDT_DEPTH, "m"),
                                                      paste(Metal,":",Value, "ug/L"),
                                                      paste('Hardness Based Criteria:', CriteriaValue, "ug/L")))       } %>%
      layout(showlegend=FALSE,
             yaxis=list(title=paste(stringr::word(criteriaSelection, 1), "ug/L")),
             xaxis=list(title="Sample Date",tickfont = list(size = 10))) })
  
  #output$test <- renderPrint({stationMetalsAnalysis()})#glimpse(stationMetalsDataWQS())})#input$stationSelected})

}
# 
# 
# ui <- fluidPage(  dissolvedMetalsCriteriaUI('metals'))
# 
# server <- function(input,output,session){
#   callModule(dissolvedMetalsCriteria,'metals', multistationFieldAnalyte1, WQSlookup, staticLimit)}
# 
# shinyApp(ui,server)
# 
