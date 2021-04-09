source('global.R')
subbasinVAHU6crosswalk <- read_csv('data/basinAssessmentReg_clb_EVJ.csv') %>%
  filter(!is.na(SubbasinVAHU6code)) %>%
  mutate(SUBBASIN = ifelse(is.na(SUBBASIN), BASIN_NAME, SUBBASIN)) #%>%
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

ecoregion <- st_read('data/GIS/vaECOREGIONlevel3__proj84.shp')

ui <- shinyUI(fluidPage(tags$head(
  tags$style(
    HTML(".shiny-notification {position:fixed; top: calc(60%); left: calc(10%); }"))),
  theme= "yeti.css",
  navbarPage("CEDS Water Quality Monitoring Data Query Tool",
             
             tabPanel("Single Station Query (Live CEDS Connection)",
                      tabsetPanel(
                        tabPanel("Station Data",
                                 sidebarPanel(
                                   helpText("Query will pull directly from CEDS. Data is refreshed nightly."),
                                   helpText("Begin typing a DEQ StationID and available options will auto-filter based on the user input."),
                                   selectInput('station', 'DEQ Station ID', choices = c("", unique(stationOptions$Station_Id))),#textInput('station', 'DEQ Station ID', placeholder = "DEQ Station ID"),
                                   dateRangeInput('dateRange',
                                                  label = 'Filter Data By Sample Date Range (YYYY-MM-DD)',
                                                  start = as.Date("1970-01-01"), ##################################################as.Date("2015-01-01"),
                                                  end = as.Date(Sys.Date()- 1)),
                                   br(),
                                   actionButton('begin', 'Pull Station',class='btn-block'),
                                   width = 3),
                                 mainPanel(
                                   helpText('This interactive map allows users to zoom and pan across different basemaps. Basemap option and Level III
                                                            Ecoregion and Assessment Region information are available by using the checkboxes in the layers drop down
                                                            on the left panel of the map.'),
                                   verbatimTextOutput('test'),
                                   leafletOutput('stationMap'),br()
                                 )))))))


server <- shinyServer(function(input, output, session) {
  
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
    WQM_Station_Full_REST <- suppressWarnings(
      geojson_sf(
        paste0("http://gis.deq.virginia.gov/arcgis/rest/services/staff/DEQInternalDataViewer/MapServer/104/query?&where=STATION_ID%3D%27",
               toupper(input$station),"%27&outFields=*&f=geojson")))
    WQM_Station_Full_REST })
    
    #WQM_Station_Full_REST_request(pool, input$station, subbasinVAHU6crosswalk, subbasins, ecoregion)})
  
  output$test <- renderPrint({WQM_Station_Full_REST()})
  
})



shinyApp(ui, server)