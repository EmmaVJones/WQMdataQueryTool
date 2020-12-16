source('global.R')

#assessmentRegions <- st_read( 'data/GIS/AssessmentRegions_simple.shp')
#ecoregion <- st_read('data/GIS/vaECOREGIONlevel3__proj84.shp')
#assessmentLayer <- st_read('data/GIS/AssessmentRegions_VA84_basins.shp') %>%
#  st_transform( st_crs(4326)) 
#subbasins <- st_read('data/GIS/DEQ_VAHUSB_subbasins_EVJ.shp') %>%
#  rename('SUBBASIN' = 'SUBBASIN_1') %>%
#  mutate(SUBBASIN = ifelse(is.na(SUBBASIN), as.character(BASIN_NAME), as.character(SUBBASIN)))
#subbasinVAHU6crosswalk <- read_csv('data/basinAssessmentReg_clb_EVJ.csv') %>%
#  filter(!is.na(SubbasinVAHU6code)) %>%
#  mutate(SUBBASIN = ifelse(is.na(SUBBASIN), BASIN_NAME, SUBBASIN)) %>%
#  dplyr::select(SUBBASIN, SubbasinVAHU6code)


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
    WQM_Station_Full_REST_request(pool, input$station)})
  
  
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
               between(as.Date(Fdt_Date_Time), !! input$dateRange[1], !! input$dateRange[2]) ) %>% # x >= left & x <= right
      as_tibble()
    
    ### Analyte information
    reactive_objects$stationAnalyteData <- pool %>% tbl("Wqm_Analytes_View") %>%
      filter(Ana_Sam_Fdt_Id %in% !! reactive_objects$stationFieldData$Fdt_Id &
               between(as.Date(Ana_Received_Date), !! input$dateRange[1], !! input$dateRange[2]) ) %>% # x >= left & x <= right
      as_tibble() %>%
      left_join(dplyr::select(reactive_objects$stationFieldData, Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time), 
                by = c("Ana_Sam_Fdt_Id" = "Fdt_Id")) 
    
  })
  
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
  output$dateRangeFilter_ <- renderUI({ req(reactive_objects$stationFieldData)
    dateRangeInput('dateRangeFilter',
                   label = 'Filter Data Further By Date Range (YYYY-MM-DD)',
                   start = min(as.Date(reactive_objects$stationFieldData$Fdt_Date_Time)), 
                   end = max(as.Date(reactive_objects$stationFieldData$Fdt_Date_Time)))  })
  
  ### Filter by user input
  stationFieldAnalyteDateRange <- reactive({req(reactive_objects$stationFieldData, input$dateRangeFilter)
    stationFieldAnalyteDataPretty( filter(reactive_objects$stationAnalyteData, between(as.Date(Fdt_Date_Time), input$dateRangeFilter[1], input$dateRangeFilter[2]) ),
                                     filter(reactive_objects$stationFieldData, between(as.Date(Fdt_Date_Time), input$dateRangeFilter[1], input$dateRangeFilter[2]) ),
                                     repFilter = input$repFilter) })
    
    
  
  
  output$stationFieldAnalyte <-  renderDataTable({ req(stationFieldAnalyteDateRange())
    datatable(stationFieldAnalyteDateRange(), rownames = F, escape= F, extensions = 'Buttons',
              options = list(dom = 'Bit', scrollX = TRUE, scrollY = '350px',
                             pageLength = nrow(stationFieldAnalyteDateRange()), buttons=list('copy','colvis')), selection = 'none')})
  
  
  #output$test1 <- renderPrint({reactive_objects$stationFieldData})
  #output$test2 <- renderPrint({reactive_objects$stationAnalyteData})
})