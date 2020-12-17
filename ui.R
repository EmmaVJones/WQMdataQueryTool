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
                                                   h4('Full Station Sampling Summary'),
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
                                                     tabPanel('Station Data',
                                                              h4('Field and Chemistry Data Combined'),
                                                              helpText("This table presents all available field and analyte data 
                                                                        filtered by the date filter in the sidebar panel. Analyte 
                                                                        data collected more than once for a given sample event 
                                                                        (e.g. more than one sample group code that reports the same parameter)
                                                                       are presented as separate columns with the sample group code appended to 
                                                                       the column name. If you wish to consolidate this information into a single 
                                                                       measure by averaging these values, please choose the `Average similar parameters by sample date`
                                                                       option below."),
                                                              radioButtons('averageParameters', "Display Options", 
                                                                           choices = c('Report all available parameters', 'Average parameters by sample date')),
                                                              DT::dataTableOutput('stationFieldAnalyte'),
                                                              br(),
                                                              h4('Sample Metrics'),
                                                              fluidRow(
                                                                column(6,h5('Collector Information'),
                                                                       DT::dataTableOutput('collectorSummary')),
                                                                column(6, h5('Sample Code Summary'),
                                                                       DT::dataTableOutput('sampleCodeSummary'))),
                                                              h5('Sample Comment Summary'),
                                                              DT::dataTableOutput('sampleCommentSummary'),br(), br(), br()) ,
                                                     tabPanel('Visualization Tools',
                                                              tabsetPanel(
                                                                tabPanel('Simplified Dataset',
                                                                         h4('Simplified Field and Chemistry Dataset'),
                                                                         helpText('This dataset cleans up the CEDS default parameter names and simplifies 
                                                                         results to one measure per sample event (using a median statistic where more than 
                                                                                  one measure per sampel event is available).'),
                                                                         DT::dataTableOutput('basicSummary'), br(), br(), br()),
                                                                tabPanel('Parameter Plot',
                                                                         h4('Interactive Parameter Plot'),
                                                                         helpText('Based on the Simplified Field and Chemistry Dataset, users may plot 
                                                                                  all available parameters in the selected data window to visualize temporal changes.',
                                                                                  span(strong('Where the information is available, the appropriate Water Quality Standard is
                                                                                              plotted with the station data'))),
                                                                         selectInput('parameterPlotlySelection', 'Select a Parameter to Visualize', choices = unique(unitData$AltName)),
                                                                         plotlyOutput('parameterPlot'), br(), br(), br()),
                                                                tabPanel('Probabilistic Estimates',
                                                                         h4('Comparison to Freshwater Probabilistic Estimates'),
                                                                         helpText('By comparing the median parameter measures from the Simplified Field and 
                                                                                  Chemistry Dataset to weighted probabilistic estimates generated by the 
                                                                                  Freshwater Probabilistic Monitoring Program, we can give additional 
                                                                                  context to the station data. These estimates place median parameter measures
                                                                                  from the selected station in a statewide, basin, ecoregion, and stream order context.'),
                                                                         h5('more stuff')) )), # a little breathing room
                                                     tabPanel('Raw Data',
                                                              h4('Field Data'),
                                                              helpText('This table displays field data for the selected station and filters exactly how the data are
                                                                       stored in CEDS.'),
                                                              DT::dataTableOutput('fieldDataRaw'), br(),
                                                              h4('Analyte Data'),
                                                              helpText('This table displays analyte data for the selected station and filters exactly how the data are
                                                                       stored in CEDS.'),
                                                              DT::dataTableOutput('analyteDataRaw'),br(),br(),br()) # a little breathing room
                                                     ) 
                                                 ))
                                      )))))
                                         
# verbatimTextOutput('test1'),
# verbatimTextOutput('test2')),        
                                                     
                                                   