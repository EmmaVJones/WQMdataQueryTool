shinyUI(fluidPage(tags$head(
  tags$style(
    HTML(".shiny-notification {position:fixed; top: calc(60%); left: calc(10%); }"))),
  theme= "yeti.css",
                  navbarPage("CEDS Water Quality Monitoring Data Query Tool", id = 'someID',  # key for passing URL to specific Tab
                             
                            #  tabPanel('How To',
                            #           h2(strong('This project is still in beta testing phase.')),
                            #           h5('Please report any data or application issues to Emma Jones emma.jones@deq.virginia.gov. See the 
                            #              CEDS Benthic Data Query App for more detailed instructions on how to use the query features of this application.'),
                            #           h6('the Single Station Query tab allows you to query stations in CEDS for field and analyte data and presents various
                            #              data visualization tools to bring more context to data retrieved.'),
                            #           h6('The multiple station query tab queries the same dataset as the Single Station Query Tab but offers spatial, temporal, 
                            #              and specific analyte querying methods not offered in other query tools. The spatial data is rebuilt on the R server weekly, so
                            #              new stations entered in CEDS are available the following Monday morning for use in this tool.'),
                            #           h4(strong('All data is retrieved from CEDS ODS, which is refreshed nightly. Any data entered in CEDS is available the next
                            #                     morning in this query tool.'))), 
                            # #          htmlOutput("BenthicQueryToolHowTo") ),
                            
                            
                            tabPanel('How To',
                                     includeMarkdown("CEDS WQM Query Tool How To.md")),
                            #includeHTML("BenthicQueryToolHowTo.html")),#htmlOutput("BenthicQueryToolHowTo") ), # this was a hot mess. when trying to update the BenthicQueryHowTo rmd and rendering to html, the app would not take any user inputs. so weird and wasted hours on this problem. ultimately had to go with rendering the .md in the app to have any semblance of a solution
                            
                             
                             tabPanel("Single Station Query (Live CEDS Connection)", value = 'SingleStation', # key for passing URL to specific Tab
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
                                                   #verbatimTextOutput('test'),
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
                                                   uiOutput('depthFilter_'),
                                                   checkboxGroupInput('repFilter', "Filter Reps",
                                                                      choices = c('R','S1', 'S2'), selected = 'R'),
                                                   width = 3),
                                                 mainPanel(
                                                   tabsetPanel(
                                                     tabPanel('Station Data',
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
                                                                tabPanel('Parameter Scatter Plot',
                                                                         h4('Interactive Parameter Scatter Plot'),
                                                                         helpText('Based on the Simplified Field and Chemistry Dataset, users may plot
                                                                                  all available parameters in the selected data window to visualize temporal changes.',
                                                                                  span(strong('Where the information is available, the appropriate Water Quality Standard is
                                                                                              plotted with the station data'))),
                                                                         helpText('The plot below can display the Benthic Stressor Analysis color palette to offer additional context for the
                                                                                 selected water quality data. For more information on the Benthic Stressor Analysis parameter thresholds and data assumptions',
                                                                                  span(strong(a('click here.', href='https://www.deq.virginia.gov/home/showpublisheddocument/4313/637461491358800000',
                                                                                                target='_blank')))),
                                                                         fluidRow(column(3, selectInput('parameterPlotlySelection', 'Select a Parameter to Visualize', choices = unique(filter(unitData, !is.na(AltName))$AltName))),
                                                                                  column(3, checkboxInput('addBSAcolors', 'Display Benthic Stressor Analysis Colors on Plot')),
                                                                                  column(3, actionButton('smoothModal', 'Plot with loess smoothing function'))),
                                                                         plotlyOutput('parameterPlot'), br(), br(), br()),
                                                                tabPanel('Parameter Boxplot',
                                                                         h4('Interactive Parameter Boxplot'),
                                                                         helpText('Based on the Simplified Field and Chemistry Dataset, users may plot
                                                                                                     all available parameters in the selected data window to visualize individual station parameter ranges and statistics.',
                                                                                  span(strong('Where the information is available, the appropriate Water Quality Standard is
                                                                                                                 plotted with the station data'))),
                                                                         fluidRow(column(3, selectInput('parameterBoxPlotlySelection', 'Select a Parameter to Visualize', choices = unique(filter(unitData, !is.na(AltName))$AltName))),
                                                                                  column(3, checkboxInput('addJitter', 'Add jittered raw data'))),
                                                                         plotlyOutput('parameterBoxplot'), br(), br(), br()),
                                                                tabPanel('Freshwater Probabilistic Estimates',
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
                                                                         br(),br(), br()),# a little breathing room
                                                                tabPanel('Dissolved Metals Criteria',
                                                                         dissolvedMetalsCriteriaUI('metals'))
                                                                )), 
                                                     tabPanel('Data Download Formats',
                                                              tabsetPanel(
                                                                tabPanel('Raw Field Data',
                                                                         helpText('This table displays field data for the selected station and filters exactly how the data are
                                                                       stored in CEDS.'),
                                                                         DT::dataTableOutput('fieldDataRaw'), br(),br(),br()), # a little breathing room
                                                                tabPanel('Raw Analyte Data',
                                                                         #h4('Analyte Data'),
                                                                         helpText('This table displays analyte data for the selected station and filters exactly how the data are
                                                                       stored in CEDS.'),
                                                                         DT::dataTableOutput('analyteDataRaw'),br(),br(),br()), # a little breathing room
                                                                tabPanel('LRBS Results',
                                                                         #h4('Analyte Data'),
                                                                         helpText('This table displays Log Relative Stability (LRBS) results and key sediment metrics
                                                                         for the selected station and filters.'),
                                                                         DT::dataTableOutput('LRBSdataRaw'),br(),br(),br()), # a little breathing room
                                                                tabPanel('Benthic Stressor Analysis Data Formats',
                                                                         helpText('This table displays field, analyte, habitat, and metals data for the selected station and filters 
                                                                                  as required for upload into the Benthic Stressor Analysis Tool.'),
                                                                         h4(strong('The BSA tool only accepts .csv data format for this dataset.')),
                                                                         DT::dataTableOutput('BSAtemplateData'), br(),
                                                                         helpText('This table displays dissolved metals data for the selected station and filters 
                                                                                  as required for upload into the dissolved metals portion of theBenthic Stressor Analysis Tool.'),
                                                                         h4(strong('The BSA tool only accepts .csv data format for this dataset.')),
                                                                         DT::dataTableOutput('BSAmetalsTemplateData'), br(),br(),br())))
                                                              
                                                     )
                                                 ))
                                      )),
                            tabPanel('Multiple Station Query (Archived Spatial Data Refreshed Weekly)',
                                     tabsetPanel(
                                       tabPanel("Station Query",
                                                sidebarPanel(
                                                  helpText("Query pulls data directly from CEDS (data is refreshed nightly). All spatial data 
                                                           used to assist spatial querying methods are stored on R server (spatial data is refreshed weekly)."),
                                                  radioButtons('queryType', "How would you like to query stations?",
                                                               choices = c('Spatial Filters', 'Wildcard Selection', 
                                                                           'Manually Specify Stations (requires a few seconds for the station text box to appear)')),
                                                
                                                  # Spatial filters
                                                  conditionalPanel(condition = "input.queryType == 'Spatial Filters'",
                                                    uiOutput('spatialFilters_assessmentRegion'),
                                                    uiOutput('spatialFilters_subbasin'),
                                                    uiOutput('spatialFilters_VAHU6')),
                                                  # Wildcard Selection
                                                  conditionalPanel(condition = "input.queryType == 'Wildcard Selection'",
                                                    uiOutput('wildcardSelection')),
                                                  # Manually Specify Stations Selection
                                                  conditionalPanel(condition = "input.queryType == 'Manually Specify Stations (takes a few seconds for the station text box to appear)'",
                                                    uiOutput('manualSelectionUI')),
                                                  hr(), # keep these at the top level to allow reuse of same filter parameters
                                                  helpText("Additional filter(s) applied on 'Pull Stations' request. "),
                                                  uiOutput('spatialFilters_Ecoregion'),
                                                  uiOutput('spatialFilters_County'),
                                                  uiOutput('dateRange_multistationUI'),
                                                  uiOutput('analyte_FilterUI'),
                                                  # add in appropriate pull data button based on query type
                                                  conditionalPanel(condition = "input.queryType == 'Spatial Filters'",
                                                                   actionButton('begin_multistation_spatial', 'Pull Stations',class='btn-block')),
                                                  conditionalPanel(condition = "input.queryType == 'Wildcard Selection'",
                                                                   actionButton('begin_multistation_wildcard', 'Pull Stations',class='btn-block')),
                                                  conditionalPanel(condition = "input.queryType == 'Manually Specify Stations (takes a few seconds for the station text box to appear)'",
                                                                   actionButton('begin_multistation_manual', 'Pull Stations',class='btn-block')) ),
                                                 mainPanel(
                                                  leafletOutput('multistationMap'),
                                                  helpText('Stations identified in the spatial filter are displayed below unless user further refines
                                                          selected stations with polygon and rectangle drawing tools in map.'),
                                                  br(),
                                                  h4('Station Information'),
                                                  DT::dataTableOutput('multistationInfoTable'),
                                                  br(),
                                                  h4('Sampling Summary'),
                                                  helpText('The records listed below are limited to records with associated sample codes. Additional
                                                            (older) samples could be lacking the sample code but have benthic data for exploration
                                                            in subsequent sections of the application.'),
                                                  DT::dataTableOutput('multistationInfoSampleMetrics'),
                                                   br(), br(), br() )), # a little breathing room
                                       tabPanel("Water Quality Data",
                                                sidebarPanel(
                                                  uiOutput('multistationDateRangeFilter_'),
                                                  uiOutput('multistationLabCodesDropped_'),
                                                  uiOutput('multistationDepthFilter_'),
                                                  checkboxGroupInput('multistationRepFilter', "Filter Reps",
                                                                     choices = c('R','S1', 'S2'), selected = 'R'), width = 3),
                                                mainPanel(
                                                  tabsetPanel(
                                                    tabPanel('Station Data',
                                                             h4('Field and Chemistry Data Combined'),
                                                             helpText("This table presents all available field and analyte data for each station
                                                                                           filtered by the date filter in the sidebar panel. Analyte
                                                                                           data collected more than once for a given sample event
                                                                                           (e.g. more than one lab parameter per date time combination)
                                                                                          are presented as separate rows and can duplicate the field data associated with a given date time. Where these
                                                                                          instances occurr, the additional rows are highlighted in yellow. If you wish to consolidate this information into a single
                                                                                          measure by averaging these values, please choose the `Average similar parameters by sample date time`
                                                                                          option below."),
                                                             radioButtons('multistationAverageParameters', strong("Display Options"),
                                                                          choices = c('Report all available parameters and highlight rows with duplicate analyte measures. The first field named
                                                                                                          `Associated Analyte Records` identifies when multiple analytes with the same lab name are returned
                                                                                                          for a single sample event date time.', 'Average parameters by sample date time.'),
                                                                          width = '100%'),
                                                             DT::dataTableOutput('multistationFieldAnalyte'),
                                                             br(),
                                                             h4('Sample Metrics'),
                                                             fluidRow(
                                                               column(6,h5('Collector Information'),
                                                                      DT::dataTableOutput('multistationCollectorSummary')),
                                                               column(6, h5('Sample Code Summary'),
                                                                      DT::dataTableOutput('multistationSampleCodeSummary'))),
                                                             h5('Sample Comment Summary'),
                                                             DT::dataTableOutput('multistationSampleCommentSummary'),br(), br(), br()),
                                                    tabPanel('Visualization Tools',
                                                             tabsetPanel(
                                                               tabPanel('Simplified Dataset',
                                                                        h4('Simplified Field and Chemistry Dataset'),
                                                                        helpText('This dataset cleans up the CEDS default parameter names and simplifies
                                                                                            results to one measure per sample event (using a median statistic where more than
                                                                                                     one measure per sampel event is available).'),
                                                                        DT::dataTableOutput('multistationBasicSummary'), br(), br(), br()),
                                                               tabPanel('Parameter Scatter Plot',
                                                                        h4('Interactive Parameter Scatter Plot'),
                                                                        helpText('Based on the Simplified Field and Chemistry Dataset, users may plot
                                                                                                     all available parameters in the selected data window to visualize temporal changes.',
                                                                                 span(strong('Where the information is available, the appropriate Water Quality Standard is
                                                                                                                 plotted with the station data'))),
                                                                        helpText('The plot below can display the Benthic Stressor Analysis color palette to offer additional context for the
                                                                                 selected water quality data. For more information on the Benthic Stressor Analysis parameter thresholds and data assumptions',
                                                                                 span(strong(a('click here.', href='https://www.deq.virginia.gov/home/showpublisheddocument/4313/637461491358800000',
                                                                                               target='_blank')))),
                                                                        fluidRow(column(3, selectInput('multistationParameterPlotlySelection', 'Select a Parameter to Visualize', choices = unique(filter(unitData, !is.na(AltName))$AltName))),
                                                                                 column(3, checkboxInput('multistationAddBSAcolors', 'Display Benthic Stressor Analysis Colors on Plot')),
                                                                                 column(3, actionButton('multistationSmoothModal', 'Plot with loess smoothing function'))),
                                                                        plotlyOutput('multistationParameterPlot'), br(), br(), br()),
                                                               tabPanel('Parameter Boxplot',
                                                                        h4('Interactive Parameter Boxplot'),
                                                                        helpText('Based on the Simplified Field and Chemistry Dataset, users may plot
                                                                                                     all available parameters in the selected data window to visualize individual station parameter ranges and statistics.',
                                                                                 span(strong('Where the information is available, the appropriate Water Quality Standard is
                                                                                                                 plotted with the station data'))),
                                                                        fluidRow(column(3, selectInput('multistationParameterBoxPlotlySelection', 'Select a Parameter to Visualize', choices = unique(filter(unitData, !is.na(AltName))$AltName))),
                                                                                 column(3, checkboxInput('multistationAddJitter', 'Add jittered raw data'))),
                                                                        plotlyOutput('multistationParameterBoxplot'), br(), br(), br()))),
                                                    tabPanel('Data Download Formats',
                                                             tabsetPanel(
                                                               tabPanel('Raw Field Data',
                                                                        helpText('This table displays field data for the selected stations and filters exactly how the data are
                                                                       stored in CEDS.'),
                                                                        DT::dataTableOutput('multistationFieldDataRaw'), br(),br(),br()), # a little breathing room
                                                               tabPanel('Raw Analyte Data',
                                                                        helpText('This table displays analyte data for the selected stations and filters exactly how the data are
                                                                       stored in CEDS.'),
                                                                        DT::dataTableOutput('multistationAnalyteDataRaw'),br(),br(),br()), # a little breathing room
                                                               tabPanel('LRBS Results',
                                                                        #h4('Analyte Data'),
                                                                        helpText('This table displays Log Relative Stability (LRBS) results and key sediment metrics
                                                                         for the selected stations and filters.'),
                                                                        DT::dataTableOutput('multistationLRBSdataRaw'),br(),br(),br()))) # a little breathing room
                                                             
                                                  
                                                    
                                                    
                                                    ))) 
                                                               
                                                               
                                                    
                                                    #verbatimTextOutput('test'),
                                                    
                                                    
                                                    
                                                                   
                                                                    
                                                                        
                                       
                                       
                                       
                                       ))
                                       
                                       )))
                                         
# verbatimTextOutput('test1'),
# verbatimTextOutput('test2')),        
                                                     
                                                   