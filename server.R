source('global.R')

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

# labCommentCodes <- pool %>% tbl( "Wqm_Comment_Cds_Codes_Wqm_View") %>%
#   as_tibble()
# pin(labCommentCodes, description = 'Lab Comment Codes', board = 'rsconnect')
labCommentCodes <- pin_get("labCommentCodes", board = 'rsconnect')

WQSlookup <- pin_get("WQSlookup-withStandards",  board = "rsconnect")



shinyServer(function(input, output, session) {
  
  # empty reactive objects list
  reactive_objects = reactiveValues() 
  
  
  ###----------------------------------------Single Station Query ---------------------------------------------------------------------------------------------------
  
  # test station viability in separate object
  observeEvent(input$begin, {
    ## Station Information
    reactive_objects$stationInfo <- pool %>% tbl( "Wqm_Stations_View") %>%
      filter(Sta_Id %in% !! toupper(input$station)) %>%
      as_tibble() 
    
    if(nrow(reactive_objects$stationInfo) == 0){
      showNotification("Not a valid StationID.")   }  })
  
  ## Pull station info from REST service
  WQM_Station_Full_REST <- reactive({req(input$begin, nrow(reactive_objects$stationInfo) != 0)
    WQM_Station_Full_REST_request(pool, input$station, subbasinVAHU6crosswalk, subbasins, ecoregion)})
  
  
  ## Pull Station Information 
  observeEvent(nrow(reactive_objects$stationInfo) > 0, {
    ## update Station Information after ensuring station valid
    reactive_objects$stationInfoFin <- stationInfoConsolidated(pool, input$station, WQM_Station_Full_REST())
    
    ## Station Geospatial Information
    reactive_objects$stationInfo_sf <- WQM_Station_Full_REST()#filter(WQM_STATIONS_FINAL, STATION_ID %in% toupper(input$station) )
    
    ## Station Sampling Information
    reactive_objects$stationInfoSampleMetrics <- stationSummarySampingMetrics(WQM_Station_Full_REST())
    
    ## Field Data Information
    reactive_objects$stationFieldData <- pool %>% tbl("Wqm_Field_Data_View") %>%
      filter(Fdt_Sta_Id %in% !! input$station &
               between(as.Date(Fdt_Date_Time), !! input$dateRange[1], !! input$dateRange[2]) & # x >= left & x <= right
               Ssc_Description != "INVALID DATA SET QUALITY ASSURANCE FAILURE") %>% 
      as_tibble()
    
    
    ### Analyte information
    if(nrow(reactive_objects$stationFieldData) == 0){
      showNotification("No data for selected window.", duration = 30, type = 'error')   
    } else {
      reactive_objects$stationAnalyteData <- pool %>% tbl("Wqm_Analytes_View") %>%
        filter(Ana_Sam_Fdt_Id %in% !! reactive_objects$stationFieldData$Fdt_Id &
                 between(as.Date(Ana_Received_Date), !! input$dateRange[1], !! input$dateRange[2])& # x >= left & x <= right
                 Pg_Parm_Name != "STORET STORAGE TRANSACTION DATE YR/MO/DAY") %>% 
        as_tibble() %>%
        left_join(dplyr::select(reactive_objects$stationFieldData, Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time), 
                  by = c("Ana_Sam_Fdt_Id" = "Fdt_Id"))  }     })
  
  ## Display Station Information
  output$stationInfoTable <- DT::renderDataTable({
    req(reactive_objects$stationInfoFin)
    datatable(reactive_objects$stationInfoFin %>% distinct(Sta_Id, .keep_all = T), 
              rownames = F, escape= F, extensions = 'Buttons',
              options = list(dom = 'Bt', scrollX= TRUE, scrollY = '100px',
                             pageLength = nrow(reactive_objects$stationInfoFin %>% distinct(Sta_Id, .keep_all = T)),
                             buttons=list('copy','colvis')))  })
  
  output$stationInfoSampleCodeMetrics <- DT::renderDataTable({
    req(reactive_objects$stationInfoFin)
    datatable(reactive_objects$stationInfoSampleMetrics, 
              rownames = F, escape= F, extensions = 'Buttons',
              options = list(dom = 'Bt', scrollX= TRUE, scrollY = '300px',
                             pageLength = nrow(reactive_objects$stationInfoSampleMetrics),
                             buttons=list('copy','colvis')) ) })
  ## Map Station Information
  output$stationMap <- renderLeaflet({
    req(reactive_objects$stationInfo)
    # color palette for assessment polygons
    pal <- colorFactor(
      palette = topo.colors(7),
      domain = assessmentRegions$ASSESS_REG)
    pal2 <- colorFactor(
      palette = rainbow(7),
      domain = ecoregion$US_L3NAME)
    
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
      inlmisc::AddHomeButton(raster::extent(-83.89, -74.80, 36.54, 39.98), position = "topleft") %>%
      addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                       overlayGroups = c("Level III Ecoregions", 'Assessment Regions'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')    
  })
  
  map_proxy <- leafletProxy("stationMap")
  
  # Add layers to map as requested
  observe({
    req(nrow(reactive_objects$stationInfo) > 0)
    map_proxy %>%
      addCircleMarkers(data = reactive_objects$stationInfo_sf,
                       color='blue', fillColor='yellow', radius = 6,
                       fillOpacity = 0.5,opacity=0.8,weight = 4,stroke=T, group="Selected Station(s)",
                       label = ~STATION_ID, layerId = ~STATION_ID) %>%
      addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                       overlayGroups = c("Selected Station(s)", "Level III Ecoregions", 'Assessment Regions'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')  })
  
  ## Field and Analyte Data Date Range 
  output$dateRangeFilter_ <- renderUI({ req(nrow(reactive_objects$stationFieldData) > 0)
    dateRangeInput('dateRangeFilter',
                   label = 'Filter Available Data Further By User Selected Date Range (YYYY-MM-DD)',
                   start = min(as.Date(reactive_objects$stationFieldData$Fdt_Date_Time)), 
                   end = max(as.Date(reactive_objects$stationFieldData$Fdt_Date_Time)))  })
  
  ## Lab codes based on original dataset brought back from ODS
  output$labCodesDropped_ <- renderUI({ req(nrow(reactive_objects$stationAnalyteData) > 0)
    codeOptions <- sort(unique(reactive_objects$stationAnalyteData$Ana_Com_Code))
    list(helpText('Below are the unique lab codes retrieved based on the initial query parameters of StationID and Date Range filters on 
                  the Station Data tab. Please check any lab codes you do not want included in any further analyses. For assistance with
                  lab code descriptions, please click the button below.'),
         actionButton('reviewLabCodes',"Lab Code Table",class='btn-block'),
         checkboxGroupInput('labCodesDropped', 'Lab Codes Revoved From Futher Analyses',
                            choices = codeOptions, inline = TRUE, selected = c('QF'))   ) })
  
  ## Lab Code Module
  observeEvent(input$reviewLabCodes,{
    showModal(modalDialog(
      title="Lab Code Descriptions",
      DT::dataTableOutput('labCodeTable'),
      easyClose = TRUE))  })
  
  output$labCodeTable <- renderDataTable({req(input$reviewLabCodes)
    datatable(labCommentCodes %>% arrange(Com_Code), rownames = F, escape= F, extensions = 'Buttons',
              options = list(dom = 'Bift', scrollX = TRUE, scrollY = '350px',
                             pageLength = nrow(labCommentCodes), 
                             buttons=list('copy',list(extend='excel',filename=paste0('CEDSlabCodes')),
                                          'colvis')), selection = 'none')   })
  
  
  ## Drop any unwanted Analyte codes
  observe({req(nrow(reactive_objects$stationFieldData) > 0, input$dateRangeFilter, nrow(reactive_objects$stationAnalyteData)>0)
    reactive_objects$stationFieldDataUserFilter <- filter(reactive_objects$stationFieldData, between(as.Date(Fdt_Date_Time), input$dateRangeFilter[1], input$dateRangeFilter[2]) )
    reactive_objects$stationAnalyteDataUserFilter <- filter(reactive_objects$stationAnalyteData, between(as.Date(Fdt_Date_Time), input$dateRangeFilter[1], input$dateRangeFilter[2]) )  %>% 
      filter(Ana_Sam_Mrs_Container_Id_Desc %in% input$repFilter) %>% 
      filter(! Ana_Com_Code %in% input$labCodesDropped)})
  
  
  ### Filter by user input
  stationFieldAnalyteDateRange <- reactive({req(reactive_objects$stationFieldDataUserFilter, input$dateRangeFilter, reactive_objects$stationAnalyteDataUserFilter, input$repFilter, input$averageParameters)
    stationFieldAnalyteDataPretty(reactive_objects$stationAnalyteDataUserFilter, reactive_objects$stationFieldDataUserFilter, 
                                  averageResults = ifelse(input$averageParameters == 'Average parameters by sample date time.', TRUE, FALSE) ) })

  #output$test <- renderPrint({stationFieldAnalyteDateRange()})
  
  ## Data Summary
  output$stationFieldAnalyte <-  renderDataTable({ req(stationFieldAnalyteDateRange())
    z <- stationFieldAnalyteDateRange() %>% # drop all empty columns ( this method longer but handles dttm issues)
      map(~.x) %>%
      discard(~all(is.na(.x))) %>%
      map_df(~.x)
    if("Associated Analyte Records" %in% names(z)){ # highlight rows where field data duplicated bc multiple analytes on same datetime
      datatable(z, rownames = F, escape= F, extensions = 'Buttons',
                options = list(dom = 'Bift', scrollX = TRUE, scrollY = '350px',
                               pageLength = nrow(z),
                               buttons=list('copy',list(extend='excel',filename=paste0('CEDSFieldAnalyteData',input$station, Sys.Date())),
                                            'colvis')), selection = 'none') %>%
        formatStyle("Associated Analyte Records", target = 'row',
                    backgroundColor = styleEqual(c(1,2,3,4,5,6,7,8,9,10), c(NA, 'yellow','yellow','yellow','yellow','yellow','yellow','yellow','yellow','yellow')))

    } else {
      datatable(z, rownames = F, escape= F, extensions = 'Buttons',
                options = list(dom = 'Bift', scrollX = TRUE, scrollY = '350px',
                               pageLength = nrow(z),
                               buttons=list('copy',list(extend='excel',filename=paste0('CEDSFieldAnalyteData',input$station, Sys.Date())),
                                            'colvis')), selection = 'none')    } })

  ## Collector Summary
  output$collectorSummary <- renderDataTable({ req(stationFieldAnalyteDateRange())
    z <- uniqueCollector(stationFieldAnalyteDateRange())
    datatable(z, rownames = F, escape= F, extensions = 'Buttons',
              options = list(dom = 'Bit', scrollX = TRUE, scrollY = '350px',
                             pageLength = nrow(z), buttons=list('copy')), selection = 'none')})

  ## Sample Code Summary
  output$sampleCodeSummary <- renderDataTable({ req(stationFieldAnalyteDateRange())
    z <- uniqueSampleCodes(stationFieldAnalyteDateRange())
    datatable(z, rownames = F, escape= F, extensions = 'Buttons',
              options = list(dom = 'Bit', scrollX = TRUE, scrollY = '350px',
                             pageLength = nrow(z), buttons=list('copy')), selection = 'none')})

  ## Sample Comment Summary
  output$sampleCommentSummary <- renderDataTable({ req(stationFieldAnalyteDateRange())
    z <- uniqueComments(stationFieldAnalyteDateRange())
    datatable(z, rownames = F, escape= F, extensions = 'Buttons',
              options = list(dom = 'Bit', scrollX = TRUE, scrollY = '350px',
                             pageLength = nrow(z), buttons=list('copy')), selection = 'none')})


  ## Visualization Tools: Simplified Dataset Tab
  basicStationSummary <- reactive({req(stationFieldAnalyteDateRange())
    basicSummary(stationFieldAnalyteDateRange()) })

  output$basicSummary <- renderDataTable({ req(basicStationSummary())
    datatable(basicStationSummary(), rownames = F, escape= F, extensions = 'Buttons',
              options = list(dom = 'Bit', scrollX = TRUE, scrollY = '350px', pageLength = nrow(basicStationSummary()),
                             buttons=list('copy',list(extend='excel',filename=paste0('CEDSbasicFieldAnalyteData',input$station, Sys.Date())),
                                          'colvis')), selection = 'none')})

  ## Visualization Tools: Parameter Plot Tab
  observe({req(nrow(basicStationSummary()) > 0, input$parameterPlotlySelection)
    if(input$parameterPlotlySelection %in% names(basicStationSummary())){
      z <- dplyr::select(basicStationSummary(), parameterPlot = !! input$parameterPlotlySelection) %>% # rename clutch for nse
        filter(!is.na(parameterPlot)) 
      if(nrow(z) == 0){
        showNotification('No data to plot for selected parameter.',duration = 3 )}
    } else {showNotification('No data to plot for selected parameter.', duration = 3  )}      })

  output$parameterPlot <- renderPlotly({ req(nrow(filter(basicStationSummary(), !is.na(input$parameterPlotlySelection))) > 0, input$parameterPlotlySelection)
   suppressWarnings(suppressMessages(
     parameterPlotly(basicStationSummary(), input$parameterPlotlySelection, unitData, WQSlookup) ))   })
  
  # observe(  
  #      })
  # 

  ## Visualization Tools: Probabilistic Estimates Tab
  centralTendencies <- reactive({ req(basicStationSummary())
    centralTendenciesCalculation(basicStationSummary())})

  output$centralTendencies <- renderDataTable({ req(centralTendencies())
    datatable(centralTendencies(), rownames = F, escape= F, extensions = 'Buttons',
              options = list(dom = 'Bit', scrollX = TRUE, scrollY = '125px', pageLength = nrow(centralTendencies()),
                             buttons=list('copy',list(extend='excel',filename=paste0('CEDSsummaryFieldAnalyteData',input$station, Sys.Date())),
                                          'colvis')), selection = 'none') })
  # %>% the row number gets colored as well making table ineffective
  #formatStyle("pH", backgroundColor = styleInterval(pHRiskTable$brks, pHRiskTable$clrs))%>%
  #formatStyle("Dissolved Oxygen", backgroundColor = styleInterval(DORiskTable$brks, DORiskTable$clrs)) %>%
  #formatStyle("Total Nitrogen", backgroundColor = styleInterval(TNRiskTable$brks, TNRiskTable$clrs))%>%
  #formatStyle("Total Phosphorus", backgroundColor = styleInterval(TPRiskTable$brks, TPRiskTable$clrs))%>%
  ##formatStyle("TotalHabitat", backgroundColor = styleInterval(TotHabRiskTable$brks, TotHabRiskTable$clrs))%>%
  ##formatStyle("LRBS", backgroundColor = styleInterval(LRBSRiskTable$brks, LRBSRiskTable$clrs))%>%
  ##formatStyle("MetalsCCU", backgroundColor = styleInterval(MetalsCCURiskTable$brks, MetalsCCURiskTable$clrs))%>%
  #formatStyle("Specific Conductance", backgroundColor = styleInterval(SpCondRiskTable$brks, SpCondRiskTable$clrs))%>%
  #formatStyle("Total Dissolved Solids", backgroundColor = styleInterval(TDSRiskTable$brks, TDSRiskTable$clrs))%>%
  #formatStyle("Sulfate", backgroundColor = styleInterval(DSulfateRiskTable$brks, DSulfateRiskTable$clrs))%>%
  #formatStyle("Chloride", backgroundColor = styleInterval(DChlorideRiskTable$brks, DChlorideRiskTable$clrs))%>%
  #formatStyle("Potassium", backgroundColor = styleInterval(DPotassiumRiskTable$brks, DPotassiumRiskTable$clrs))%>%
  #formatStyle("Sodium", backgroundColor = styleInterval(DSodiumRiskTable$brks, DSodiumRiskTable$clrs)) })




  # Percentile Data
  observe({ req(nrow(basicStationSummary()) > 0, nrow(WQM_Station_Full_REST()) > 0)
    reactive_objects$percentiles <- percentileList(probEst, centralTendencies(), probIndicators, unique(WQM_Station_Full_REST()$ProbSuperBasin),
                                                   unique(WQM_Station_Full_REST()$ProbBasin), unique(WQM_Station_Full_REST()$EPA_ECO_US_L3NAME), NA)  })


  # Statewide CDF plot
  output$Statewide <- renderPlot({ req(reactive_objects$percentiles, input$parameterSelect)
    parameterSwitch <- as.character(filter(probIndicators, AltName %in% input$parameterSelect)$Parameter)
    cdfplot(probEst, as.character(input$parameterSelect), parameterSwitch,
            'Virginia', reactive_objects$percentiles, CDFsettingsList[[parameterSwitch]] )  })


  # SuperBasin CDF plot
  output$SuperBasin <- renderPlot({ req(reactive_objects$percentiles, input$parameterSelect)
    if(!is.na(as.character(unique(WQM_Station_Full_REST()$ProbSuperBasin)))){
      parameterSwitch <- as.character(filter(probIndicators, AltName %in% input$parameterSelect)$Parameter)
      cdfplot(probEst, as.character(input$parameterSelect), parameterSwitch,
              as.character(unique(WQM_Station_Full_REST()$ProbSuperBasin)), reactive_objects$percentiles, CDFsettingsList[[parameterSwitch]] )
    } })

  # Basin CDF plot
  output$Basin <- renderPlot({ req(reactive_objects$percentiles, input$parameterSelect)
    parameterSwitch <- as.character(filter(probIndicators, AltName %in% input$parameterSelect)$Parameter)
    cdfplot(probEst, as.character(input$parameterSelect), parameterSwitch,
            as.character(unique(WQM_Station_Full_REST()$ProbBasin)), reactive_objects$percentiles, CDFsettingsList[[parameterSwitch]] )  })


  # Ecoregion CDF plot
  output$Ecoregion <- renderPlot({ req(reactive_objects$percentiles, input$parameterSelect)
    parameterSwitch <- as.character(filter(probIndicators, AltName %in% input$parameterSelect)$Parameter)
    cdfplot(probEst, as.character(input$parameterSelect), parameterSwitch,
            as.character(unique(WQM_Station_Full_REST()$EPA_ECO_US_L3NAME)), reactive_objects$percentiles, CDFsettingsList[[parameterSwitch]] )  })




  # Raw Field Data
  output$fieldDataRaw <- renderDataTable({ req(reactive_objects$stationFieldData)
    datatable(reactive_objects$stationFieldDataUserFilter, rownames = F, escape= F, extensions = 'Buttons',
              options = list(dom = 'Bift', scrollX = TRUE, scrollY = '300px', pageLength = nrow(reactive_objects$stationFieldData),
                             buttons=list('copy',list(extend='excel',filename=paste0('CEDSrawFieldData',input$station, Sys.Date())),
                                          'colvis')), selection = 'none') })

  output$analyteDataRaw <- renderDataTable({ req(reactive_objects$stationAnalyteData)
    datatable(reactive_objects$stationAnalyteDataUserFilter, rownames = F, escape= F, extensions = 'Buttons',
              options = list(dom = 'Bift', scrollX = TRUE, scrollY = '300px', pageLength = nrow(reactive_objects$stationAnalyteData),
                             buttons=list('copy',list(extend='excel',filename=paste0('CEDSrawAnalyteData',input$station, Sys.Date())),
                                          'colvis')), selection = 'none') })


  
  #output$test1 <- renderPrint({ 
  #  parameterSwitch <- as.character(filter(probIndicators, AltName %in% input$parameterSelect)$Parameter)
  #  #print(parameterSwitch)}) 
  #  glimpse(reactive_objects$percentiles )})
  
  #output$test2 <- renderPrint({reactive_objects$percentiles$DO$percentiles})
})