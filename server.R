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
WQM_Stations_Spatial <- pin_get("ejones/WQM-Stations-Spatial", board = "rsconnect") %>%
  rename("Basin_Name" = "Basin_Code") # can't have same name different case when using sqldf
WQM_Stations_Full <- st_as_sf(pin_get('ejones/WQM-Station-Full', board = 'rsconnect'))



# analyte options
Wqm_Parameter_Grp_Cds_Codes_Wqm_View <- pool %>% tbl('Wqm_Parameter_Grp_Cds_Codes_Wqm_View') %>%
  filter(Pg_Parm_Name != "STORET STORAGE TRANSACTION DATE YR/MO/DAY") %>%
  distinct(Pg_Parm_Name) %>% arrange(Pg_Parm_Name) %>% as_tibble() %>% drop_na()



shinyServer(function(input, output, session) {
  
  # empty reactive objects list
  reactive_objects = reactiveValues() 
  
  
  ###----------------------------------------Single Station Query ---------------------------------------------------------------------------------------------------
  
  # # test station viability in separate object
  # observeEvent(input$begin, {
  #   ## Station Information
  #   reactive_objects$stationInfo <- pool %>% tbl( "Wqm_Stations_View") %>%
  #     filter(Sta_Id %in% !! toupper(input$station)) %>%
  #     as_tibble() 
  #   
  #   if(nrow(reactive_objects$stationInfo) == 0){
  #     showNotification("Not a valid StationID.")   }  })
  # 
  # ## Pull station info from REST service
  # WQM_Station_Full_REST <- reactive({req(input$begin, nrow(reactive_objects$stationInfo) != 0)
  #   WQM_Station_Full_REST_request(pool, input$station, subbasinVAHU6crosswalk, subbasins, ecoregion)})
  # 
  # 
  # ## Pull Station Information 
  # observeEvent(nrow(reactive_objects$stationInfo) > 0, {
  #   ## update Station Information after ensuring station valid
  #   reactive_objects$stationInfoFin <- stationInfoConsolidated(pool, input$station, WQM_Station_Full_REST())
  #   
  #   ## Station Geospatial Information
  #   reactive_objects$stationInfo_sf <- WQM_Station_Full_REST()#filter(WQM_STATIONS_FINAL, STATION_ID %in% toupper(input$station) )
  #   
  #   ## Station Sampling Information
  #   reactive_objects$stationInfoSampleMetrics <- stationSummarySampingMetrics(WQM_Station_Full_REST(), 'single')
  #   
  #   ## Field Data Information
  #   reactive_objects$stationFieldData <- pool %>% tbl("Wqm_Field_Data_View") %>%
  #     filter(Fdt_Sta_Id %in% !! input$station &
  #              between(as.Date(Fdt_Date_Time), !! input$dateRange[1], !! input$dateRange[2]) & # x >= left & x <= right
  #              Ssc_Description != "INVALID DATA SET QUALITY ASSURANCE FAILURE") %>% 
  #     as_tibble()
  #   
  #   
  #   ### Analyte information
  #   if(nrow(reactive_objects$stationFieldData) == 0){
  #     showNotification("No data for selected window.", duration = 30, type = 'error')   
  #   } else {
  #     reactive_objects$stationAnalyteData <- pool %>% tbl("Wqm_Analytes_View") %>%
  #       filter(Ana_Sam_Fdt_Id %in% !! reactive_objects$stationFieldData$Fdt_Id &
  #                between(as.Date(Ana_Received_Date), !! input$dateRange[1], !! input$dateRange[2])& # x >= left & x <= right
  #                Pg_Parm_Name != "STORET STORAGE TRANSACTION DATE YR/MO/DAY") %>% 
  #       as_tibble() %>%
  #       left_join(dplyr::select(reactive_objects$stationFieldData, Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time), 
  #                 by = c("Ana_Sam_Fdt_Id" = "Fdt_Id"))  }     })
  # 
  # ## Display Station Information
  # output$stationInfoTable <- DT::renderDataTable({
  #   req(reactive_objects$stationInfoFin)
  #   datatable(reactive_objects$stationInfoFin %>% distinct(Sta_Id, .keep_all = T), 
  #             rownames = F, escape= F, extensions = 'Buttons',
  #             options = list(dom = 'Bt', scrollX= TRUE, scrollY = '100px',
  #                            pageLength = nrow(reactive_objects$stationInfoFin %>% distinct(Sta_Id, .keep_all = T)),
  #                            buttons=list('copy','colvis')))  })
  # 
  # output$stationInfoSampleCodeMetrics <- DT::renderDataTable({
  #   req(reactive_objects$stationInfoFin)
  #   datatable(reactive_objects$stationInfoSampleMetrics, 
  #             rownames = F, escape= F, extensions = 'Buttons',
  #             options = list(dom = 'Bt', scrollX= TRUE, scrollY = '300px',
  #                            pageLength = nrow(reactive_objects$stationInfoSampleMetrics),
  #                            buttons=list('copy','colvis')) ) })
  # ## Map Station Information
  # output$stationMap <- renderLeaflet({
  #   req(reactive_objects$stationInfo)
  #   # color palette for assessment polygons
  #   pal <- colorFactor(
  #     palette = topo.colors(7),
  #     domain = assessmentRegions$ASSESS_REG)
  #   pal2 <- colorFactor(
  #     palette = rainbow(7),
  #     domain = ecoregion$US_L3NAME)
  #   
  #   CreateWebMap(maps = c("Topo","Imagery","Hydrography"), collapsed = TRUE, 
  #                options= leafletOptions(zoomControl = TRUE,minZoom = 3, maxZoom = 20,
  #                                        preferCanvas = TRUE)) %>%
  #     setView(-79.1, 37.7, zoom=7)  %>% 
  #     addPolygons(data= ecoregion,  color = 'gray', weight = 1,
  #                 fillColor= ~pal2(ecoregion$US_L3NAME), fillOpacity = 0.5,stroke=0.1,
  #                 group="Level III Ecoregions",label = ~US_L3NAME) %>% hideGroup('Level III Ecoregions') %>%
  #     addPolygons(data= assessmentRegions,  color = 'black', weight = 1,
  #                 fillColor= ~pal(assessmentRegions$ASSESS_REG), fillOpacity = 0.5,stroke=0.1,
  #                 group="Assessment Regions", label = ~ASSESS_REG) %>% hideGroup('Assessment Regions') %>% 
  #     inlmisc::AddHomeButton(raster::extent(-83.89, -74.80, 36.54, 39.98), position = "topleft") %>%
  #     addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
  #                      overlayGroups = c("Level III Ecoregions", 'Assessment Regions'),
  #                      options=layersControlOptions(collapsed=T),
  #                      position='topleft')    
  # })
  # 
  # map_proxy <- leafletProxy("stationMap")
  # 
  # # Add layers to map as requested
  # observe({
  #   req(nrow(reactive_objects$stationInfo) > 0)
  #   map_proxy %>%
  #     addCircleMarkers(data = reactive_objects$stationInfo_sf,
  #                      color='blue', fillColor='yellow', radius = 6,
  #                      fillOpacity = 0.5,opacity=0.8,weight = 4,stroke=T, group="Selected Station(s)",
  #                      label = ~STATION_ID, layerId = ~STATION_ID) %>%
  #     addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
  #                      overlayGroups = c("Selected Station(s)", "Level III Ecoregions", 'Assessment Regions'),
  #                      options=layersControlOptions(collapsed=T),
  #                      position='topleft')  })
  # 
  # ## Field and Analyte Data Date Range 
  # output$dateRangeFilter_ <- renderUI({ req(nrow(reactive_objects$stationFieldData) > 0)
  #   dateRangeInput('dateRangeFilter',
  #                  label = 'Filter Available Data Further By User Selected Date Range (YYYY-MM-DD)',
  #                  start = min(as.Date(reactive_objects$stationFieldData$Fdt_Date_Time)), 
  #                  end = max(as.Date(reactive_objects$stationFieldData$Fdt_Date_Time)))  })
  # 
  # ## Lab codes based on original dataset brought back from ODS
  # output$labCodesDropped_ <- renderUI({ req(nrow(reactive_objects$stationAnalyteData) > 0)
  #   codeOptions <- sort(unique(reactive_objects$stationAnalyteData$Ana_Com_Code))
  #   list(helpText('Below are the unique lab codes retrieved based on the initial query parameters of StationID and Date Range filters on 
  #                 the Station Data tab. Please check any lab codes you do not want included in any further analyses. For assistance with
  #                 lab code descriptions, please click the button below.'),
  #        actionButton('reviewLabCodes',"Lab Code Table",class='btn-block'),
  #        checkboxGroupInput('labCodesDropped', 'Lab Codes Revoved From Futher Analyses',
  #                           choices = codeOptions, inline = TRUE, selected = c('QF'))   ) })
  # 
  # ## Lab Code Module
  # observeEvent(input$reviewLabCodes,{
  #   showModal(modalDialog(
  #     title="Lab Code Descriptions",
  #     DT::dataTableOutput('labCodeTable'),
  #     easyClose = TRUE))  })
  # 
  # output$labCodeTable <- renderDataTable({req(input$reviewLabCodes)
  #   datatable(labCommentCodes %>% arrange(Com_Code), rownames = F, escape= F, extensions = 'Buttons',
  #             options = list(dom = 'Bift', scrollX = TRUE, scrollY = '350px',
  #                            pageLength = nrow(labCommentCodes), 
  #                            buttons=list('copy',list(extend='excel',filename=paste0('CEDSlabCodes')),
  #                                         'colvis')), selection = 'none')   })
  # 
  # 
  # ## Drop any unwanted Analyte codes
  # observe({req(nrow(reactive_objects$stationFieldData) > 0, input$dateRangeFilter, nrow(reactive_objects$stationAnalyteData)>0)
  #   reactive_objects$stationFieldDataUserFilter <- filter(reactive_objects$stationFieldData, between(as.Date(Fdt_Date_Time), input$dateRangeFilter[1], input$dateRangeFilter[2]) )
  #   reactive_objects$stationAnalyteDataUserFilter <- filter(reactive_objects$stationAnalyteData, between(as.Date(Fdt_Date_Time), input$dateRangeFilter[1], input$dateRangeFilter[2]) )  %>% 
  #     filter(Ana_Sam_Mrs_Container_Id_Desc %in% input$repFilter) %>% 
  #     filter(! Ana_Com_Code %in% input$labCodesDropped)})
  # 
  # 
  # ### Filter by user input
  # stationFieldAnalyteDateRange <- reactive({req(reactive_objects$stationFieldDataUserFilter, input$dateRangeFilter, reactive_objects$stationAnalyteDataUserFilter, input$repFilter, input$averageParameters)
  #   stationFieldAnalyteDataPretty(reactive_objects$stationAnalyteDataUserFilter, reactive_objects$stationFieldDataUserFilter, 
  #                                 averageResults = ifelse(input$averageParameters == 'Average parameters by sample date time.', TRUE, FALSE) ) })
  # 
  # #output$test <- renderPrint({stationFieldAnalyteDateRange()})
  # 
  # ## Data Summary
  # output$stationFieldAnalyte <-  renderDataTable({ req(stationFieldAnalyteDateRange())
  #   z <- stationFieldAnalyteDateRange() %>% # drop all empty columns ( this method longer but handles dttm issues)
  #     map(~.x) %>%
  #     discard(~all(is.na(.x))) %>%
  #     map_df(~.x)
  #   if("Associated Analyte Records" %in% names(z)){ # highlight rows where field data duplicated bc multiple analytes on same datetime
  #     datatable(z, rownames = F, escape= F, extensions = 'Buttons',
  #               options = list(dom = 'Bift', scrollX = TRUE, scrollY = '350px',
  #                              pageLength = nrow(z),
  #                              buttons=list('copy',list(extend='excel',filename=paste0('CEDSFieldAnalyteData',input$station, Sys.Date())),
  #                                           'colvis')), selection = 'none') %>%
  #       formatStyle("Associated Analyte Records", target = 'row',
  #                   backgroundColor = styleEqual(c(1,2,3,4,5,6,7,8,9,10), c(NA, 'yellow','yellow','yellow','yellow','yellow','yellow','yellow','yellow','yellow')))
  # 
  #   } else {
  #     datatable(z, rownames = F, escape= F, extensions = 'Buttons',
  #               options = list(dom = 'Bift', scrollX = TRUE, scrollY = '350px',
  #                              pageLength = nrow(z),
  #                              buttons=list('copy',list(extend='excel',filename=paste0('CEDSFieldAnalyteData',input$station, Sys.Date())),
  #                                           'colvis')), selection = 'none')    } })
  # 
  # ## Collector Summary
  # output$collectorSummary <- renderDataTable({ req(stationFieldAnalyteDateRange())
  #   z <- uniqueCollector(stationFieldAnalyteDateRange())
  #   datatable(z, rownames = F, escape= F, extensions = 'Buttons',
  #             options = list(dom = 'Bit', scrollX = TRUE, scrollY = '350px',
  #                            pageLength = nrow(z), buttons=list('copy')), selection = 'none')})
  # 
  # ## Sample Code Summary
  # output$sampleCodeSummary <- renderDataTable({ req(stationFieldAnalyteDateRange())
  #   z <- uniqueSampleCodes(stationFieldAnalyteDateRange())
  #   datatable(z, rownames = F, escape= F, extensions = 'Buttons',
  #             options = list(dom = 'Bit', scrollX = TRUE, scrollY = '350px',
  #                            pageLength = nrow(z), buttons=list('copy')), selection = 'none')})
  # 
  # ## Sample Comment Summary
  # output$sampleCommentSummary <- renderDataTable({ req(stationFieldAnalyteDateRange())
  #   z <- uniqueComments(stationFieldAnalyteDateRange())
  #   datatable(z, rownames = F, escape= F, extensions = 'Buttons',
  #             options = list(dom = 'Bit', scrollX = TRUE, scrollY = '350px',
  #                            pageLength = nrow(z), buttons=list('copy')), selection = 'none')})
  # 
  # 
  # ## Visualization Tools: Simplified Dataset Tab
  # basicStationSummary <- reactive({req(stationFieldAnalyteDateRange())
  #   basicSummary(stationFieldAnalyteDateRange()) })
  # 
  # output$basicSummary <- renderDataTable({ req(basicStationSummary())
  #   datatable(basicStationSummary(), rownames = F, escape= F, extensions = 'Buttons',
  #             options = list(dom = 'Bit', scrollX = TRUE, scrollY = '350px', pageLength = nrow(basicStationSummary()),
  #                            buttons=list('copy',list(extend='excel',filename=paste0('CEDSbasicFieldAnalyteData',input$station, Sys.Date())),
  #                                         'colvis')), selection = 'none')})
  # 
  # ## Visualization Tools: Parameter Plot Tab
  # observe({req(nrow(basicStationSummary()) > 0, input$parameterPlotlySelection)
  #   if(input$parameterPlotlySelection %in% names(basicStationSummary())){
  #     z <- dplyr::select(basicStationSummary(), parameterPlot = !! input$parameterPlotlySelection) %>% # rename clutch for nse
  #       filter(!is.na(parameterPlot)) 
  #     if(nrow(z) == 0){
  #       showNotification('No data to plot for selected parameter.',duration = 3 )}
  #   } else {showNotification('No data to plot for selected parameter.', duration = 3  )}      })
  # 
  # output$parameterPlot <- renderPlotly({ req(nrow(filter(basicStationSummary(), !is.na(input$parameterPlotlySelection))) > 0, input$parameterPlotlySelection)
  #  suppressWarnings(suppressMessages(
  #    parameterPlotly(basicStationSummary(), input$parameterPlotlySelection, unitData, WQSlookup) ))   })
  # 
  # # observe(  
  # #      })
  # # 
  # 
  # ## Visualization Tools: Probabilistic Estimates Tab
  # centralTendencies <- reactive({ req(basicStationSummary())
  #   centralTendenciesCalculation(basicStationSummary())})
  # 
  # output$centralTendencies <- renderDataTable({ req(centralTendencies())
  #   datatable(centralTendencies(), rownames = F, escape= F, extensions = 'Buttons',
  #             options = list(dom = 'Bit', scrollX = TRUE, scrollY = '125px', pageLength = nrow(centralTendencies()),
  #                            buttons=list('copy',list(extend='excel',filename=paste0('CEDSsummaryFieldAnalyteData',input$station, Sys.Date())),
  #                                         'colvis')), selection = 'none') })
  # # %>% the row number gets colored as well making table ineffective
  # #formatStyle("pH", backgroundColor = styleInterval(pHRiskTable$brks, pHRiskTable$clrs))%>%
  # #formatStyle("Dissolved Oxygen", backgroundColor = styleInterval(DORiskTable$brks, DORiskTable$clrs)) %>%
  # #formatStyle("Total Nitrogen", backgroundColor = styleInterval(TNRiskTable$brks, TNRiskTable$clrs))%>%
  # #formatStyle("Total Phosphorus", backgroundColor = styleInterval(TPRiskTable$brks, TPRiskTable$clrs))%>%
  # ##formatStyle("TotalHabitat", backgroundColor = styleInterval(TotHabRiskTable$brks, TotHabRiskTable$clrs))%>%
  # ##formatStyle("LRBS", backgroundColor = styleInterval(LRBSRiskTable$brks, LRBSRiskTable$clrs))%>%
  # ##formatStyle("MetalsCCU", backgroundColor = styleInterval(MetalsCCURiskTable$brks, MetalsCCURiskTable$clrs))%>%
  # #formatStyle("Specific Conductance", backgroundColor = styleInterval(SpCondRiskTable$brks, SpCondRiskTable$clrs))%>%
  # #formatStyle("Total Dissolved Solids", backgroundColor = styleInterval(TDSRiskTable$brks, TDSRiskTable$clrs))%>%
  # #formatStyle("Sulfate", backgroundColor = styleInterval(DSulfateRiskTable$brks, DSulfateRiskTable$clrs))%>%
  # #formatStyle("Chloride", backgroundColor = styleInterval(DChlorideRiskTable$brks, DChlorideRiskTable$clrs))%>%
  # #formatStyle("Potassium", backgroundColor = styleInterval(DPotassiumRiskTable$brks, DPotassiumRiskTable$clrs))%>%
  # #formatStyle("Sodium", backgroundColor = styleInterval(DSodiumRiskTable$brks, DSodiumRiskTable$clrs)) })
  # 
  # 
  # 
  # 
  # # Percentile Data
  # observe({ req(nrow(basicStationSummary()) > 0, nrow(WQM_Station_Full_REST()) > 0)
  #   reactive_objects$percentiles <- percentileList(probEst, centralTendencies(), probIndicators, unique(WQM_Station_Full_REST()$ProbSuperBasin),
  #                                                  unique(WQM_Station_Full_REST()$ProbBasin), unique(WQM_Station_Full_REST()$EPA_ECO_US_L3NAME), NA)  })
  # 
  # 
  # # Statewide CDF plot
  # output$Statewide <- renderPlot({ req(reactive_objects$percentiles, input$parameterSelect)
  #   parameterSwitch <- as.character(filter(probIndicators, AltName %in% input$parameterSelect)$Parameter)
  #   cdfplot(probEst, as.character(input$parameterSelect), parameterSwitch,
  #           'Virginia', reactive_objects$percentiles, CDFsettingsList[[parameterSwitch]] )  })
  # 
  # 
  # # SuperBasin CDF plot
  # output$SuperBasin <- renderPlot({ req(reactive_objects$percentiles, input$parameterSelect)
  #   if(!is.na(as.character(unique(WQM_Station_Full_REST()$ProbSuperBasin)))){
  #     parameterSwitch <- as.character(filter(probIndicators, AltName %in% input$parameterSelect)$Parameter)
  #     cdfplot(probEst, as.character(input$parameterSelect), parameterSwitch,
  #             as.character(unique(WQM_Station_Full_REST()$ProbSuperBasin)), reactive_objects$percentiles, CDFsettingsList[[parameterSwitch]] )
  #   } })
  # 
  # # Basin CDF plot
  # output$Basin <- renderPlot({ req(reactive_objects$percentiles, input$parameterSelect)
  #   parameterSwitch <- as.character(filter(probIndicators, AltName %in% input$parameterSelect)$Parameter)
  #   cdfplot(probEst, as.character(input$parameterSelect), parameterSwitch,
  #           as.character(unique(WQM_Station_Full_REST()$ProbBasin)), reactive_objects$percentiles, CDFsettingsList[[parameterSwitch]] )  })
  # 
  # 
  # # Ecoregion CDF plot
  # output$Ecoregion <- renderPlot({ req(reactive_objects$percentiles, input$parameterSelect)
  #   parameterSwitch <- as.character(filter(probIndicators, AltName %in% input$parameterSelect)$Parameter)
  #   cdfplot(probEst, as.character(input$parameterSelect), parameterSwitch,
  #           as.character(unique(WQM_Station_Full_REST()$EPA_ECO_US_L3NAME)), reactive_objects$percentiles, CDFsettingsList[[parameterSwitch]] )  })
  # 
  # 
  # 
  # 
  # # Raw Field Data
  # output$fieldDataRaw <- renderDataTable({ req(reactive_objects$stationFieldData)
  #   datatable(reactive_objects$stationFieldDataUserFilter, rownames = F, escape= F, extensions = 'Buttons',
  #             options = list(dom = 'Bift', scrollX = TRUE, scrollY = '300px', pageLength = nrow(reactive_objects$stationFieldData),
  #                            buttons=list('copy',list(extend='excel',filename=paste0('CEDSrawFieldData',input$station, Sys.Date())),
  #                                         'colvis')), selection = 'none') })
  # 
  # output$analyteDataRaw <- renderDataTable({ req(reactive_objects$stationAnalyteData)
  #   datatable(reactive_objects$stationAnalyteDataUserFilter, rownames = F, escape= F, extensions = 'Buttons',
  #             options = list(dom = 'Bift', scrollX = TRUE, scrollY = '300px', pageLength = nrow(reactive_objects$stationAnalyteData),
  #                            buttons=list('copy',list(extend='excel',filename=paste0('CEDSrawAnalyteData',input$station, Sys.Date())),
  #                                         'colvis')), selection = 'none') })
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  
  ### -------------------------------Multistation Query-------------------------------------------------------------------------------------------------------
  
  
  # Query by spatial filters
  output$spatialFilters_assessmentRegion <- renderUI({req(input$queryType == 'Spatial Filters')
    list(helpText('Interactive cross validation between filters applies in this section.'),
         selectInput('assessmentRegionFilter','Assessment Region',
                     choices = sort(unique(WQM_Stations_Spatial$ASSESS_REG)), multiple = T)) })

  output$spatialFilters_subbasin <- renderUI({req(input$queryType == 'Spatial Filters')
    if(is.null(input$assessmentRegionFilter)){
      choices <- distinct(WQM_Stations_Spatial, Basin_Name) %>% drop_na() %>% arrange(Basin_Name) %>% pull()
      }else{
        choices <- filter(WQM_Stations_Spatial, ASSESS_REG %in% input$assessmentRegionFilter) %>%
          distinct(Basin_Name) %>% arrange(Basin_Name) %>%  pull()  }
    selectInput('subbasinFilter','Basin', choices = choices, multiple = T) })

  output$spatialFilters_VAHU6 <- renderUI({  req(input$queryType == 'Spatial Filters')
    if(is.null(input$assessmentRegionFilter) & is.null(input$subbasinFilter)){choices <- sort(unique(assessmentLayer$VAHU6))}
    if(is.null(input$assessmentRegionFilter) & !is.null(input$subbasinFilter)){
      choices <-  filter(WQM_Stations_Spatial, Basin_Name %in% input$subbasinFilter) %>%
        distinct(VAHU6) %>% arrange(VAHU6) %>% pull() }
    if(!is.null(input$assessmentRegionFilter) & is.null(input$subbasinFilter)){
      choices <- filter(WQM_Stations_Spatial, ASSESS_REG %in% input$assessmentRegionFilter) %>%
        distinct(VAHU6) %>% arrange(VAHU6) %>% pull() }
    if(!is.null(input$assessmentRegionFilter) & !is.null(input$subbasinFilter)){
      choices <- filter(WQM_Stations_Spatial, ASSESS_REG %in% input$assessmentRegionFilter) %>%
        filter(Basin_Name %in% input$subbasinFilter) %>%
        distinct(VAHU6) %>% arrange(VAHU6) %>%  pull()  }
    selectInput('VAHU6Filter','VAHU6', choices = choices, multiple = T) })

  output$spatialFilters_Ecoregion <- renderUI({#req(input$queryType == 'Spatial Filters')
    selectInput('ecoregionFilter','Level 3 Ecoregion', choices = unique(ecoregion$US_L3NAME), multiple = T) })

  output$dateRange_multistationUI <- renderUI({#req(input$queryType == 'Spatial Filters')
    dateRangeInput('dateRange_multistation',
                   label = 'Filter Stations By Sample Date Range (YYYY-MM-DD)',
                   start = as.Date("1970-01-01"),
                   end = as.Date(Sys.Date()- 7))   })

  output$analyte_FilterUI <- renderUI({#req(input$queryType == 'Spatial Filters')
    selectInput('analyte_Filter', 'Filter Stations By Analytes Collected',
                choices = Wqm_Parameter_Grp_Cds_Codes_Wqm_View, multiple = TRUE) })

  # Query by spatial filter selection
  observeEvent(input$begin_multistation_spatial,{
    reactive_objects$WQM_Stations_Filter <- WQM_Stations_Filter_function('Spatial Filters', pool, WQM_Stations_Spatial, input$VAHU6Filter, input$subbasinFilter,
                                                          input$assessmentRegionFilter, input$ecoregionFilter, input$dateRange_multistation, input$analyte_Filter,
                                                          manualSelection = NULL, wildcardSelection = NULL) })
  
  # Query by wildcard selection
  output$wildcardSelection <- renderUI({req(input$queryType == 'Wildcard Selection')
    list(
      helpText('Remember, use % as your wildcard, not *'),
      textInput('wildcardText', 'Filter by StationID LIKE', value = NULL, placeholder = '2A%') )     })
  
  observeEvent(input$begin_multistation_wildcard,{
    reactive_objects$WQM_Stations_Filter <- WQM_Stations_Filter_function('Wildcard Selection', 
                                                                         pool, WQM_Stations_Spatial, VAHU6Filter = NULL, subbasinFilter = NULL, assessmentRegionFilter = NULL,
                                                                         ecoregionFilter = input$ecoregionFilter, dateRange_multistation = input$dateRange_multistation,
                                                                         analyte_Filter = input$analyte_Filter, manualSelection = NULL, 
                                                                         wildcardSelection = as.character(toupper(input$wildcardText))) })
                                                  
  
  #output$test <- renderPrint({input$wildcardText})
  
  
  
  
  # Query by manual selection
  output$manualSelectionUI <- renderUI({req(input$queryType == 'Manually Specify Stations (takes a few seconds for the station text box to appear)')
    list(helpText('Begin typing station names and the app will filter available data by input text. Multiple stations are allowed.'),
         selectInput('manualSelection','Station ID', choices = sort(unique(WQM_Stations_Spatial$StationID)), multiple = T)) })
  
  observeEvent(input$begin_multistation_manual, {
    reactive_objects$WQM_Stations_Filter <- WQM_Stations_Filter_function('Manually Specify Stations (takes a few seconds for the station text box to appear)', 
                                                                         pool, WQM_Stations_Spatial, VAHU6Filter = NULL, subbasinFilter = NULL, assessmentRegionFilter = NULL,
                                                                         ecoregionFilter = input$ecoregionFilter, dateRange_multistation = input$dateRange_multistation,
                                                                         analyte_Filter = input$analyte_Filter, manualSelection = as.character(input$manualSelection),
                                                                         wildcardSelection = NULL) })

  
  
  
  
  
  observe({ req(reactive_objects$WQM_Stations_Filter)
    ## Basic Station Info
    reactive_objects$multistationInfoFin <- left_join(Wqm_Stations_View %>%  # need to repull data instead of calling stationInfo bc app crashes
                                                        filter(Sta_Id %in% reactive_objects$WQM_Stations_Filter$StationID) %>%
                                                        as_tibble() %>%
                                                        # add link to data and add link to internal GIS web app with WQS layer on there
                                                        mutate(`CEDS Station View Link` = paste0("<b><a href='https://ceds.deq.virginia.gov/ui#wqmStations/",
                                                                                                 Sta_Id,"'", 
                                                                                                 " target= '_blank'> View Monitoring Station in CEDS</a></b>"),
                                                               `DEQ GIS Web App Link` =  paste0("<b><a href='https://gis.deq.virginia.gov/GISStaffApplication/?query=WQM%20Stations%20(All%20stations%20with%20full%20attributes),STATION_ID,",
                                                                                                Sta_Id, 
                                                                                                "&showLayers=DEQInternalDataViewer_1723;WATER%20LAYERS;WQM%20Stations%20(All%20stations%20with%20full%20attributes);", 
                                                                                                ";2020%20Draft%20ADB%20WQA%20Layers;2020%20Rivers%20(Any%20Use)&level=14' target='_blank'>View Monitoring Station in DEQ Staff App</a></b>" )) %>%
                                                        dplyr::select(Sta_Id, Sta_Desc, `CEDS Station View Link`, `DEQ GIS Web App Link`, everything()), 
                                                      ########filter(WQM_Station_View, Sta_Id %in% toupper(input$station)), # need to filter instead of calling stationInfo bc app crashes
                                                      dplyr::select(WQM_Station_Full, 
                                                                    STATION_ID, Latitude, Longitude, WQM_STA_STRAHER_ORDER, EPA_ECO_US_L3CODE,
                                                                    EPA_ECO_US_L3NAME, BASINS_HUC_8_NAME, BASINS_VAHU6, WQS_WATER_NAME, WQS_SEC, WQS_CLASS, 
                                                                    WQS_SPSTDS, WQS_PWS, WQS_TROUT, WQS_TIER_III, WQM_YRS_YEAR, WQM_YRS_SPG_CODE),
                                                      by = c('Sta_Id' = 'STATION_ID')) %>%
      dplyr::select(Sta_Id, Sta_Desc, `CEDS Station View Link`, `DEQ GIS Web App Link`, Latitude, Longitude, WQM_STA_STRAHER_ORDER, EPA_ECO_US_L3CODE,
                    EPA_ECO_US_L3NAME, BASINS_HUC_8_NAME, BASINS_VAHU6, WQS_WATER_NAME, WQS_SEC, WQS_CLASS, 
                    WQS_SPSTDS, WQS_PWS, WQS_TROUT, WQS_TIER_III, everything()) 
    
    # Empty station user selection to start with
    reactive_objects$selectedSites <- NULL  })

  output$multistationMap <- renderLeaflet({req(reactive_objects$WQM_Stations_Filter)
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
      addDrawToolbar(
        targetGroup='Selected',
        polylineOptions=FALSE,
        markerOptions = FALSE,
        polygonOptions = drawPolygonOptions(shapeOptions=drawShapeOptions(fillOpacity = 0, color = 'white', weight = 3)),
        rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0, color = 'white', weight = 3)),
        circleOptions = FALSE,
        circleMarkerOptions = FALSE,
        editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions())) %>%
      addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                       overlayGroups = c("Level III Ecoregions", 'Assessment Regions'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')        })

  map_proxy_multi <- leafletProxy("multistationMap")

  # # Add layers to map as requested
  assessmentLayerFilter <- reactive({req(nrow(reactive_objects$WQM_Stations_Filter) > 0)
    filter(assessmentLayer, VAHU6 %in% reactive_objects$WQM_Stations_Filter$VAHU6) })

  observe({req(nrow(reactive_objects$WQM_Stations_Filter) > 0)
    map_proxy_multi %>%
      addCircleMarkers(data = reactive_objects$WQM_Stations_Filter,
                       color='blue', fillColor='gray', radius = 4,
                       fillOpacity = 0.5,opacity=0.8,weight = 2,stroke=T, group="Spatial Filter Station(s)",
                       label = ~StationID, layerId = ~StationID,
                       popup = leafpop::popupTable(reactive_objects$WQM_Stations_Filter, zcol=c('StationID'))) %>%
      {if(nrow(assessmentLayerFilter()) > 0)
        addPolygons(., data= assessmentLayerFilter(),  color = 'black', weight = 1,
                    fillColor= 'gray', fillOpacity = 0.5,stroke=0.1,
                    group="VAHU6", label = ~VAHU6) %>% hideGroup('VAHU6')
        else . } %>%
      addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                       overlayGroups = c("Spatial Filter Station(s)", "VAHU6","Level III Ecoregions", 'Assessment Regions'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')  })
  
  
  
  ## User polygon selection feature
  observeEvent(input$multistationMap_draw_new_feature,{
    
    shape = input$multistationMap_draw_new_feature
    
    # derive polygon coordinates and feature_type from shape input
    polygon_coordinates <- shape$geometry$coordinates
    feature_type <- shape$properties$feature_type
    
    if(feature_type %in% c("rectangle","polygon")) {
      # change user coordinates into sf multipolygon
      poly <- st_sf(what = 'user selected polygon',
                    geom = st_sfc(st_cast(st_polygon(list(do.call(rbind,lapply(polygon_coordinates[[1]],function(x){c(x[[1]][1],x[[2]][1])})))), 'MULTIPOLYGON') ))
      st_crs(poly) <- 4326 # set crs (can't do in one step???)
      
      # select sites inside polygon
      if(is.null(reactive_objects$selectedSites)){
        reactive_objects$selectedSites <- reactive_objects$WQM_Stations_Filter %>% 
          st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
                   remove = F, # don't remove these lat/lon cols from df
                   crs = 4326) %>% 
          st_intersection(poly)
      } else {
        addedSites <- reactive_objects$WQM_Stations_Filter %>% 
          st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
                   remove = F, # don't remove these lat/lon cols from df
                   crs = 4326) %>% 
          st_intersection(poly)
        reactive_objects$selectedSites <- rbind(reactive_objects$selectedSites, addedSites)
      }
    } })
  
  # Highlight selected sites from polygon
  observe({req(nrow(reactive_objects$selectedSites) > 0)
    map_proxy_multi %>%
      addCircleMarkers(data = reactive_objects$selectedSites,
                       color='blue', fillColor='yellow', radius = 4,
                       fillOpacity = 0.5,opacity=0.8,weight = 2,stroke=T, group="User Selected Station(s)",
                       label = ~StationID, layerId = ~StationID,
                       popup = leafpop::popupTable(reactive_objects$selectedSites, zcol=c('StationID'))) %>%
      addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                       overlayGroups = c("User Selected Station(s)","Spatial Filter Station(s)","VAHU6", "Level III Ecoregions", 'Assessment Regions'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')  })
  
  # redraw all sites if user selection deleted
  observeEvent(input$multistationMap_draw_deleted_features,{
    reactive_objects$selectedSites <- NULL
    
    map_proxy_multi %>%
      clearGroup(group="User Selected Station(s)") %>%
      addCircleMarkers(data = reactive_objects$WQM_Stations_Filter,
                       color='blue', fillColor='gray', radius = 4,
                       fillOpacity = 0.5,opacity=0.8,weight = 2,stroke=T, group="Spatial Filter Station(s)",
                       label = ~StationID, layerId = ~StationID,
                       popup = leafpop::popupTable(reactive_objects$selectedSites, zcol=c('StationID'))) %>%
      addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                       overlayGroups = c("Spatial Filter Station(s)","VAHU6","Level III Ecoregions", 'Assessment Regions'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')    })
  
  # Update "final" site selection after user input
  observe({req(reactive_objects$multistationInfoFin)
    # "final sites"
    reactive_objects$multistationSelection <- reactive_objects$multistationInfoFin %>%
      {if(!is.null(reactive_objects$selectedSites))
        filter(., Sta_Id %in% reactive_objects$selectedSites$StationID)
        else . }
    
    ## Station Sampling Information
    reactive_objects$multistationInfoSampleMetrics <- reactive_objects$multistationSelection %>%
      group_by(Sta_Id) %>%
      mutate(`Years Sampled` = paste0(year(WQM_YRS_YEAR))) %>%
      dplyr::select(Sta_Id, WQM_YRS_SPG_CODE,WQM_YRS_YEAR,`Years Sampled`) %>%
      group_by(Sta_Id, `Years Sampled`) %>%
      summarise(`Sample Codes` = paste0(WQM_YRS_SPG_CODE, collapse = ' | '))           })
  
  ## Display Station Information
  output$multistationInfoTable <- DT::renderDataTable({
    req(reactive_objects$multistationSelection)
    datatable(reactive_objects$multistationSelection %>% distinct(Sta_Id, .keep_all = T) %>% arrange(Sta_Id),
              rownames = F, escape= F, extensions = 'Buttons',
              options = list(dom = 'Bift', scrollX= TRUE, scrollY = '300px',
                             pageLength = nrow(reactive_objects$multistationSelection %>% distinct(Sta_Id, .keep_all = T)),
                             buttons=list('copy','colvis')))  })
  
  output$multistationInfoSampleMetrics <- DT::renderDataTable({req(reactive_objects$multistationInfoSampleMetrics)
    datatable(reactive_objects$multistationInfoSampleMetrics, 
              rownames = F, escape= F, extensions = 'Buttons',
              options = list(dom = 'Bift', scrollX= TRUE, scrollY = '300px',
                             pageLength = nrow(reactive_objects$multistationInfoSampleMetrics),
                             buttons=list('copy','colvis')) ) })
  
  
  
  
  
  
  
  output$test <- renderPrint({ reactive_objects$multistationInfoFin })#WQM_Stations_Filter }) #input$begin_multistation_spatial})#
  


  
})



#output$test1 <- renderPrint({ 
#  parameterSwitch <- as.character(filter(probIndicators, AltName %in% input$parameterSelect)$Parameter)
#  #print(parameterSwitch)}) 
#  glimpse(reactive_objects$percentiles )})

#output$test2 <- renderPrint({reactive_objects$percentiles$DO$percentiles})