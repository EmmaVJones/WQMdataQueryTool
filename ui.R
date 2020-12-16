shinyUI(fluidPage(theme= "yeti.css",
                  navbarPage("CEDS Water Quality Monitoring Data Query Tool",
                             
                             #tabPanel('How To',
                            #          htmlOutput("BenthicQueryToolHowTo") ),
                             
                             tabPanel("Single Station Query (Live CEDS Connection)",
                                      tabsetPanel(
                                        tabPanel("Station Data",
                                                 sidebarPanel(
                                                   helpText("Query will pull directly from CEDS. Data is refreshed nightly."),
                                                   textInput('station', 'DEQ Station ID', placeholder = "DEQ Station ID"),
                                                   dateRangeInput('dateRange',
                                                                  label = 'Filter Data By Last Sample Date Range (YYYY-MM-DD)',
                                                                  start = as.Date("1970-01-01"), 
                                                                  end = as.Date(Sys.Date()- 1)),
                                                   br(),
                                                   actionButton('begin', 'Pull Station',class='btn-block')),
                                                 mainPanel(
                                                   leafletOutput('stationMap'),br(),
                                                   h4('Station Information'),
                                                   DT::dataTableOutput('stationInfoTable'),br(),
                                                   h4('Sampling Summary'),
                                                   DT::dataTableOutput('stationInfoSampleCodeMetrics'),br(),
                                                   helpText('Eventually maybe some stats on n sampling events or raw field data?'),
                                                   br(), br(), br() # a little breathing room
                                                 )),
                                        tabPanel("Water Quality Data",
                                                 sidebarPanel(
                                                   uiOutput('dateRangeFilter_'),
                                                   checkboxGroupInput('repFilter', "Filter Reps", 
                                                                      choices = c('R','S1', 'S2'), selected = 'R')   ),
                                                 mainPanel(
                                                   tabsetPanel(
                                                     tabPanel('Raw Data',
                                                              h4('Field and Chemistry Combined'),
                                                              DT::dataTableOutput('stationFieldAnalyte')),
                                                             # verbatimTextOutput('test1'),
                                                             # verbatimTextOutput('test2')),
                                                     tabPanel('Visualization Tools'), # a little breathing room
                                                     tabPanel('Other Stuff') # a little breathing room
                                                     ) 
                                                 ))
                                      )))))
                                                 
                                                     
                                                   