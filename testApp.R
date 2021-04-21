httr::set_config(httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))

library(tidyverse)
library(sf)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(inlmisc)
library(DT)
library(DBI)
#library(measurements) #only necessary if don't use Rex's dataset for points
library(plotly)
library(lubridate)
library(pool)
library(geojsonsf)
library(pins)
library(sqldf)
library(config)
library(readxl)

# get configuration settings
conn <- config::get("connectionSettings")

board_register_rsconnect(key = conn$CONNECT_API_KEY,  #Sys.getenv("CONNECT_API_KEY"),
                         server = conn$CONNECT_SERVER)#Sys.getenv("CONNECT_SERVER"))


# Set up pool connection to production environment
pool <- dbPool(
  drv = odbc::odbc(),
  Driver = "SQLServer",   # note the LACK OF space between SQL and Server ( how RStudio named driver)
  # Production Environment
  Server= "DEQ-SQLODS-PROD,50000",
  dbname = "ODS",
  UID = conn$UID_prod,
  PWD = conn$PWD_prod,
  #UID = Sys.getenv("userid_production"), # need to change in Connect {vars}
  #PWD = Sys.getenv("pwd_production")   # need to change in Connect {vars}
  # Test environment
  #Server= "WSQ04151,50000",
  #dbname = "ODS_test",
  #UID = Sys.getenv("userid"),  # need to change in Connect {vars}
  #PWD = Sys.getenv("pwd"),  # need to change in Connect {vars}
  trusted_connection = "yes"
)
onStop(function() {
  poolClose(pool)
})

# ## For testing: connect to ODS production
# pool <- dbPool(
#   drv = odbc::odbc(),
#   Driver = "ODBC Driver 11 for SQL Server",#Driver = "SQL Server Native Client 11.0",
#   Server= "DEQ-SQLODS-PROD,50000",
#   dbname = "ODS",
#   trusted_connection = "yes"
# )

stationOptions <- pin_get('ejones/WQM-Sta-GIS-View-Stations', board= 'rsconnect')


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
                                   
                                   br(),
                                   actionButton('begin', 'Pull Station',class='btn-block'),
                                   width = 3),
                                 mainPanel(
                                   helpText('The URL sent from the R server to the GIS REST service was:'),
                                   verbatimTextOutput('test'),
                                   helpText('And the geojson call returned:'),
                                   verbatimTextOutput('test1'),
                                   br(),
                                   helpText('Trying with https results in:'),
                                   verbatimTextOutput('test2')
                                   
                                   
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
  
  output$test <- renderPrint({paste0("http://gis.deq.virginia.gov/arcgis/rest/services/staff/DEQInternalDataViewer/MapServer/104/query?&where=STATION_ID%3D%27",
                                     toupper(input$station),"%27&outFields=*&f=geojson")})
  
  output$test1 <- renderPrint({glimpse(WQM_Station_Full_REST())})
  
  output$test2 <- renderPrint({req(input$begin, nrow(reactive_objects$stationInfo) != 0)
    glimpse(suppressWarnings(
    geojson_sf(
      paste0("https://gis.deq.virginia.gov/arcgis/rest/services/staff/DEQInternalDataViewer/MapServer/104/query?&where=STATION_ID%3D%27",
             toupper(input$station),"%27&outFields=*&f=geojson"))))})
  
})



shinyApp(ui, server)