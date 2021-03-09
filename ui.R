shinyUI(fluidPage(tags$head(
  tags$style(
    HTML(".shiny-notification {position:fixed; top: calc(60%); left: calc(10%); }"))),
  theme= "yeti.css",
                  navbarPage("CEDS Water Quality Monitoring Data Query Tool",
                             
                             #tabPanel('How To',
                            #          htmlOutput("BenthicQueryToolHowTo") ),
                             
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
                                                   leafletOutput('stationMap'),br(),
                                                   h4('Station Information'),
                                                   DT::dataTableOutput('stationInfoTable'),br(),
                                                   h4('Station Sampling Summary- Based on WQM Station Information Shared with GIS Services'),
                                                   helpText('This information may not be inclusive of all historical sample codes due to database structure.'),
                                                   DT::dataTableOutput('stationInfoSampleCodeMetrics'),br(),
                                                   br(), br(), br() # a little breathing room
                                                 )),
                                        tabPanel("Water Quality Data",
                                                 sidebarPanel(
                                                   uiOutput('dateRangeFilter_'),
                                                   uiOutput('labCodesDropped_'),
                                                   checkboxGroupInput('repFilter', "Filter Reps", 
                                                                      choices = c('R','S1', 'S2'), selected = 'R'),
                                                   width = 3),
                                                 mainPanel(
                                                   tabsetPanel(
                                                     tabPanel('Station Data',
                                                              
                                                              #verbatimTextOutput('test'),
                                                              
                                                              
                                                              h4('Field and Chemistry Data Combined'),
                                                              helpText("This table presents all available field and analyte data 
                                                                        filtered by the date filter in the sidebar panel. Analyte 
                                                                        data collected more than once for a given sample event 
                                                                        (e.g. more than one lab parameter per date time combination)
                                                                       are presented as separate rows and can duplicate the field data associated with a given date time. Where these 
                                                                       instances occurr, the additional rows are highlighted in yellow. If you wish to consolidate this information into a single 
                                                                       measure by averaging these values, please choose the `Average similar parameters by sample date time`
                                                                       option below."),
                                                              radioButtons('averageParameters', strong("Display Options"), 
                                                                           choices = c('Report all available parameters and highlight rows with duplicate analyte measures. The first field named
                                                                                       `Associated Analyte Records` identifies when multiple analytes with the same lab name are returned
                                                                                       for a single sample event date time.', 'Average parameters by sample date time.'),
                                                                           width = '100%'),
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
                                                                        # fluidRow(column(3, 
                                                                        selectInput('parameterPlotlySelection', 'Select a Parameter to Visualize', choices = unique(filter(unitData, !is.na(AltName))$AltName)),#)
                                                                        #          column(3, uiOutput('noParameterDataWarning')),
                                                                         plotlyOutput('parameterPlot'), br(), br(), br()),
                                                                tabPanel('Probabilistic Estimates',
                                                                         h4('Comparison to Freshwater Probabilistic Estimates'),
                                                                         helpText(span('By comparing the average and median parameter measures from the Simplified Field and 
                                                                                  Chemistry Dataset to weighted probabilistic estimates generated by the 
                                                                                  Freshwater Probabilistic Monitoring Program, we can give additional 
                                                                                  context to the station data. These estimates place average and median parameter measures
                                                                                  from the selected station in a statewide, suprbasin (where applicable), basin, and ecoregional context.'),
                                                                                  strong('Probabilistic estimates are based on WADEABLE datasets only.'),
                                                                                  "Superbasins are visible when stations belong to smaller basins. To increase the number of data points
                                                                                  in the underlying dataset, certain basins are combined to create a 'Superbasin' comprised of more data
                                                                                  to improve statistical power. Examples include the Tennessee basin (Clinch-Powell, Holston, and Big Sandy),
                                                                                  Potomac-Shenandoah, and Rappahannock-York. When a superbasin plot is not visible, that means the station
                                                                                  did not fall into an applicable basin."),
                                                                         h5('Central Tendencies'),
                                                                         #helpText('Parameters that have a Benthic Stressor Analysis threshold are colored accordingly. Please see the ',
                                                                        #          strong(tags$a(href = 'http://www.deq.virginia.gov/Programs/Water/WaterQualityInformationTMDLs/WaterQualityMonitoring/ProbabilisticMonitironing.aspx',
                                                                        #                 'Stressor Analysis in Virginia: Data Collection and Stressor Thresholds (DEQ 2017) report',target='_blank')),
                                                                        #          ' for more information regarding these stressor threshold recommendations.'),
                                                                         DT::dataTableOutput('centralTendencies'), br(),
                                                                         h5("Parameter CDF Results"),
                                                                         selectInput('parameterSelect', 'Select a Parameter to Plot', choices = probIndicators$AltName, selected = NULL),
                                                                         #verbatimTextOutput('test1'),
                                                                         #verbatimTextOutput('test2'),
                                                                         fluidRow(column(6, plotOutput('Statewide')),
                                                                                  column(6, plotOutput('SuperBasin'))),
                                                                         fluidRow(column(6, plotOutput('Basin')),
                                                                                  column(6, plotOutput('Ecoregion'))),
                                                                         br(),br(), br()) )), # a little breathing room
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
                                                     
                                                   