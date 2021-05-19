---
title: "CEDS WQM Query Tool How To"
output:
  html_document:
    theme: yeti
  pdf_document: default
---

<br>
<h2><b>This project is still in beta testing phase.</b></h2>
<h4>Please report any data or application issues to Emma Jones emma.jones@deq.virginia.gov.</h4>
<h4><b>All data is retrieved from CEDS ODS, which is refreshed nightly. Any data entered into CEDS is available the next morning in this query tool.</b></h4>
<br>

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;This tool is designed to assist users with water quality monitoring data acquisition and reporting. The application is divided into two sections to meet analytical needs. The `Single Station Query (Live CEDS Connection)` tab allows single station queries directly from the production ODS environment. The `Multiple Station Query (Archived Data Refreshed Weekly)` tab allows for more complicated spatial and temporal analyses of monitoring data using the same live connection to the production ODS environment, but with archived spatial information saved on the R server to expedite queries. The spatial data associated with the multistation utility is refreshed weekly.

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;All tables feature interactive column sorting, column visibility, and data download features. Tables can be sorted by specific column using the gray arrows next to the column of interest. The `Column Visibility` button allows users to turn columns on or off to aid data visualization. The `Copy` button copies table data to the user's clipboard for use in other applications. Some tables feature `Search` boxes where users may enter character strings to interactively filter table contents. Some tables feature data download features that export data to spreadsheets (.xlsx or .csv) for further local analyses. 

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Most plot are interactive with additional information presented by hovering the mouse over a data point. Turning on the "compare data on hover" feature in the top right corner of the plot (hover mouse on plot to see additional tools) allows user to see multiple popup textboxes with additional information for data near the closest datapoint to the mouse on hover. Plots can be zoomed in/out by dragging the mouse over the desired area (double click in the map to zoom to full extent). Different data layers may be turned on/off by clicking the object in the legend on the top right of the map (plot autoscales to the new extent). Use the camera button (top left choice when hovering mouse over plot) to download the selected view as a png. Tooltips identify additional built in plot features to assist data exploration.


### Single Station Query (Live CEDS Connection)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Queries from this tab are restricted to single stations, allowing users access to the most recent data available. Nightly database rebuilds ensure data entered the previous day are available the follow morning.

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Users begin the query process by typing a DEQ Station ID into the text box on the `Station Data` tab and a data window in the `Filter Data By Sample Date Range (YYYY-MM-DD)` fields. After clicking the `Pull Station` button, the application compiles available station data from the production CEDS environment and GIS REST services. If an invalid DEQ Station ID and data window combination is entered, a notification will appear alerting users that data does not exist. Upon retrieving data, the application plots the station on an interactive map, identifies key station information in the `Station Information` table, and compiles recent sampling history in the `Sampling Summary` table. The interactive map has a Level III Ecoregion and DEQ Assessment Region layers hidden that can be plotted by hovering the mouse over the layers button in the top left corner of the map. Links to the chosen station in CEDS and in the GIS Staff app are available in the `Station Information` table.

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;After a station is retrieved, the user may use the `Water Quality Data` tab to further investigate field and analyte data in various data visualization tools. The left sidebar panel offers multiple ways to further filter the data retrieved from CEDS. The date range filter allows users to further refine the window of data analyzed. Lab codes are important ways DCLS communicates information regarding a sample that could warrant removal from further analyses. You may remove any lab codes that do not meet your analytical needs by checking them under `Lab Codes Revoved From Futher Analyses` and you can always review the lab code meanings by clicking the `Lab Code Table` button. The `Only Analyze Surface Measurements (Depth <= 0.3 m)` checkboxes removes all data from further analyses that are reported as > 0.3 meters depth. The `Filter Reps` defaults to "R" samples only, but you may include replicates and split samples using the additional checkbox options.

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;The `Station Data` tab allows users to explore all field and analyte parameters retrieved from CEDS for the selected filter options as well as summarizes collector, sample code, and sample comment information. Please read the help text regarding the `Field and Chemistry Data Combined` for more information of table reporting formats.

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;The `Visualization Tools` tab offers a simplified dataset for easier data exploration as well as various data visualization tools to bring more context to data retrieved. Interactive scatterplots and boxplots refresh based on the selected parameter from the respective tabs and identifies the appropriate Water Quality Standards to use where data are available from the Water Quality Assessments program. The Freshwater Probabilistic Estimates tab compares select parameter means and medians to baseline condition estimates for Virginia as well as appropriate marjor basin (and superbasin if applicable) and Level 3 Ecoregion.
 
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;The `Data Download Formats` tab provides multiple data formats for download from raw field and analyte data, LRBS results (not currently stored in CEDS), as well as a data format that matches the required template for the Benthic Stressor Analysis Tool. 

Tools may be added to this area by contacting Emma Jones (emma.jones@deq.virginia.gov) with specific examples and use cases.


### Multiple Station Query (Archived Spatial Data Refreshed Weekly)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;The multiple station query tab queries the same dataset as the Single Station Query Tab but offers more flexible spatial, temporal, and specific analyte querying methods that are not offered in other query tools. The spatial data is rebuilt on the R server weekly, so new stations entered in CEDS are available the following Monday morning for use in this tool. Users begin by choosing how they wish to query data, either by spatial filters, wildcard selection, or manually specifying stations.

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;With the `Spatial Filters` radio button selected, users may enter query terms into three cross validated filter boxes (e.g. if a Basin is entered, then only VAHU6s inside that basin can be chosen or if an Assessment Region is selected, only Basins and VAHU6s within that Assessment Region can be selected). More than one selection can be entered into any of these boxes. Additionally below the linebreak, additional Level 3 Ecoregion, County/City, Sample Date Range, Program Code, Lab Media/Sample Group Codes, and Collected Analyte filters may be applied on the results at the time of querying. These filters first take all results from the stations that apply to the interactive cross validated selection and then apply a spatial intersection on the chosen Level 3 Ecoregion(s) and County/City(ies) and filters stations to contain sites sampled within the date range that have data for the selected Program Codes, Lab Media/Sample Group Codes, and analyte(s). Any filters left blank do not apply to the selection. After all the desired filters are chosen, press the `Pull Stations` button to run the query. Wildcard selection does not apply to the results from the `Spatial Filters` query method.

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;With the `Wildcard Query` radio button selected, a wildcard query can be entered in the text box to find StationID's like the entered text. **Remember to use the % character for any wildcard requests**. If an invalid query is entered, a notification in the bottom right corner will appear alerting users that the entered text does not match any DEQ StationID's. Level 3 Ecoregion, County/City, Sample Date Range, and Collected Analyte filters may be applied on the results at the time of querying. These filters first take all results from the stations that apply to the wildcard selection and then apply a spatial intersection on the chosen Level 3 Ecoregion(s) and County/City(ies) and filters stations to contain sites sampled within the date range that have data for the selected Program Codes, Lab Media/Sample Group Codes, and analyte(s). Any filters left blank do not apply to the selection. After all the desired filters are chosen, press the `Pull Stations` button to run the query.

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;With the `Manually Specify Stations` radio button selected, specific stations can be entered in the text box to query those StationIDs. The application will begin to filter available stations based on user input text into the field. Multiple stations can be entered into the single text field. Level 3 Ecoregion, County/City, Sample Date Range,  Program Code, Lab Media/Sample Group Codes,  Program Code, Lab Media/Sample Group Codes, and Collected Analyte filters may be applied on the results at the time of querying. These filters first take all results from the stations that apply to the wildcard selection and then apply a spatial intersection on the chosen Level 3 Ecoregion(s) and County/City(ies) and filters stations to contain sites sampled within the date range that have data for the selected Program Codes, Lab Media/Sample Group Codes, and analyte(s). Any filters left blank do not apply to the selection. After all the desired filters are chosen, press the `Pull Stations` button to run the query.

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Upon retrieving data, the application plots the stations on an interactive map, identifies key station information in the `Station Information` table, and compiles recent sampling history in the `Sampling Summary` table. The interactive map has Level III Ecoregion, DEQ Assessment Region, and VAHU6 layers hidden that can be plotted by hovering the mouse over the layers button in the top left corner of the map. Links to the chosen station in CEDS and in the GIS Staff app are available in the `Station Information` table. 

#### Multistation Selection Map Draw Feature

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;To further restrict stations, use the map drawing polygon or rectangle buttons in the left side of the interactive map to draw features around the stations of interest. The polygon draw tool allows users to draw specific shapes to select just the stations desired. To finish a shape, double click on the first vertex. To use the rectangle draw tool, the user first selects the rectangle tool, then clicks on the map holding the mouse until the shape contains the stations desired. Releasing the mouse finishes the rectangle. Selected stations turn yellow and the selections are reflected in the `Station Information` and `Sampling Summary` tables. To remove selections, click the trash can button on the left side of the interactive map. The user can either clear all to remove all selections at once, or the user can click individual rectangles and polygons to remove. 

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;After the stations are retrieved, the user may use the `Station Data` tab to explore all field and analyte parameters retrieved from CEDS for the selected filter options as well as summarizes collector, sample code, and sample comment information by each unique station. Please read the help text regarding the `Field and Chemistry Data Combined` for more information of table reporting formats.

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;The `Visualization Tools` tab offers a simplified dataset for easier data exploration as well as various data visualizations to bring more context to data retrieved. Interactive scatterplots and boxplots refresh based on the selected parameter from the respective tabs and the appropriate Water Quality Standards to use where data are presented as available from the Water Quality Assessments program. 
 
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;The `Data Download Formats` tab provides multiple data formats for download from raw field and analyte data as well as LRBS results (not currently stored in CEDS).

Tools may be added to this area by contacting Emma Jones (emma.jones@deq.virginia.gov) with specific examples and use cases.


<br><br>

### Contact Information

Please contact Emma Jones (emma.jones@deq.virginia.gov) for questions regarding the application.

<br><br><br>


