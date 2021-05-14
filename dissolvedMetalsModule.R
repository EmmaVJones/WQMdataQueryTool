
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
    fluidRow(column(4, uiOutput(ns('stationSelected_'))),
             column(4, uiOutput(ns('WQSclass_'))),
             column(4, uiOutput(ns('PWSinfo_'))),
             column(4, numericInput(ns('WER'), strong('Water Effects Ratio (WER) used for module analysis'), value = 1))),
    h5('A summary of the exceedances of water column metals criteria available for the selected site and additional filters 
        are available below. If no data is presented, then the station does not have any water column metals data available. 
       Hardness based criteria are calculated as applicable.'),
    DT::dataTableOutput(ns('WCmetalsSingleSiteSummary')),
    verbatimTextOutput(ns('test')),
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
  metalsDataWQS <- reactive({left_join(multistationFieldAnalyte, WQSlookup, by=c('Fdt_Sta_Id' = 'StationID')) %>% 
      dplyr::select(Station_Id = Fdt_Sta_Id, FDT_DATE_TIME = Fdt_Date_Time, FDT_DEPTH = Fdt_Depth, # include depth bc a few samples taken same datetime but different depths
                    METAL_Antimony = `STORET_01095_ANTIMONY, DISSOLVED (UG/L AS SB)`, RMK_Antimony = RMK_01097, 
                    METAL_Arsenic = `STORET_01000_ARSENIC, DISSOLVED  (UG/L AS AS)`, RMK_Arsenic = RMK_01002, 
                    METAL_Barium = `STORET_01005_BARIUM, DISSOLVED (UG/L AS BA)`, RMK_Barium = RMK_01005, 
                    METAL_Cadmium = `STORET_01025_CADMIUM, DISSOLVED (UG/L AS CD)`, RMK_Cadmium = RMK_01025,
                    METAL_Chromium = `STORET_01030_CHROMIUM, DISSOLVED (UG/L AS CR)`, RMK_Chromium = RMK_01030, 
                    # Chromium III and ChromiumVI dealt with inside metalsAnalysis()
                    METAL_Copper = `STORET_01040_COPPER, DISSOLVED (UG/L AS CU)`, RMK_Copper = RMK_01040, 
                    METAL_Lead = `STORET_01049_LEAD, DISSOLVED (UG/L AS PB)`, RMK_Lead = RMK_01049, 
                    METAL_Mercury = `STORET_50091_MERCURY-TL,FILTERED WATER,ULTRATRACE METHOD UG/L`, RMK_Mercury = RMK_50091,
                    METAL_Nickel = `STORET_01065_NICKEL, DISSOLVED (UG/L AS NI)`, RMK_Nickel = RMK_01067, 
                    METAL_Uranium = `URANIUM_TOT`, RMK_Uranium = `RMK_7440-61-1T`, 
                    METAL_Selenium = `STORET_01145_SELENIUM, DISSOLVED (UG/L AS SE)`, RMK_Selenium = RMK_01145, 
                    METAL_Silver = `STORET_01075_SILVER, DISSOLVED (UG/L AS AG)`, RMK_Silver = RMK_01075, 
                    METAL_Thallium = `STORET_01057_THALLIUM, DISSOLVED (UG/L AS TL)`, RMK_Thallium = RMK_01057,
                    METAL_Zinc = `STORET_01090_ZINC, DISSOLVED (UG/L AS ZN)`, RMK_Zinc = RMK_01092,
                    METAL_Hardness = `STORET_DHARD_HARDNESS, CA MG CALCULATED (MG/L AS CACO3) AS DISSOLVED`, RMK_Hardness = RMK_DHARD) %>% 
      group_by(Station_Id, FDT_DATE_TIME, FDT_DEPTH) %>% 
      mutate_if(is.numeric, as.character) %>% 
      pivot_longer(cols = METAL_Antimony:RMK_Hardness, #RMK_Antimony:RMK_Hardness, 
                   names_to = c('Type', 'Metal'),
                   names_sep = "_",
                   values_to = 'Value') %>% 
      ungroup() %>% group_by(Station_Id, FDT_DATE_TIME, FDT_DEPTH, Metal) %>% 
      pivot_wider(id_cols = c(Station_Id, FDT_DATE_TIME, FDT_DEPTH, Metal), names_from = Type, values_from = Value) %>% # pivot remark wider so the appropriate metal value is dropped when filtering on lab comment codes
      filter(! RMK %in% c('IF', 'J', 'O', 'QF', 'V')) %>% # lab codes dropped from further analysis
      pivot_longer(cols= METAL:RMK, names_to = 'Type', values_to = 'Value') %>% # get in appropriate format to flip wide again
      pivot_wider(id_cols = c(Station_Id, FDT_DATE_TIME, FDT_DEPTH), names_from = c(Type, Metal), names_sep = "_", values_from = Value) %>% 
      mutate_at(vars(contains('METAL')), as.numeric) %>%# change metals values back to numeric
      rename_with(~str_remove(., 'METAL_')) # drop METAL_ prefix for easier analyses
      
      })
  
  stationMetalsDataWQS <- reactive({req(input$stationSelected)
    filter(metalsDataWQS(), Fdt_Sta_Id %in% input$stationSelected)})
 
  # Identify Class
  # output$WQSclass_ <- renderUI({req(basicDataWQS())
  #   choice <- ifelse(unique(basicDataWQS()$)
    # 
    # })
  
  
  output$test <- renderPrint({glimpse(stationMetalsDataWQS())})#input$stationSelected})
    #glimpse(stationDataWQS())})#left_join(multistationFieldAnalyte, WQSlookup, by=c('Fdt_Sta_Id' = 'StationID')))})#basicDataWQS())})
  
}

# # for Testing
# basicData1 <- basicData
# rm(basicData)

ui <- fluidPage(  dissolvedMetalsCriteriaUI('metals'))

server <- function(input,output,session){
  # stationData <- eventReactive( input$stationSelection, {
  #   filter(multistationFieldAnalyte1 , Fdt_Sta_Id %in% input$stationSelection) })
  # stationSelected <- reactive({input$stationSelection})
  # 
  #basicData <- reactive({basicData })
  
  callModule(dissolvedMetalsCriteria,'metals', multistationFieldAnalyte1, WQSlookup, staticLimit)
  
}

shinyApp(ui,server)

