library(tidyverse)
library(pool)
library(config)
library(sf)
library(plotly)
library(lubridate)
library(pool)
library(geojsonsf)
library(pins)
library(sqldf)
library(config)



# Server connection things
conn <- config::get("connectionSettings") # get configuration settings

## For testing: connect to ODS production
pool <- dbPool(
  drv = odbc::odbc(),
  Driver = "SQL Server Native Client 11.0", 
  Server= "DEQ-SQLODS-PROD,50000",
  dbname = "ODS",
  trusted_connection = "yes"
)

# For deployment on the R server: Set up pool connection to production environment
#pool <- dbPool(
#  drv = odbc::odbc(),
#  Driver = "SQLServer",   # note the LACK OF space between SQL and Server ( how RStudio named driver)
#  # Production Environment
#  Server= "DEQ-SQLODS-PROD,50000",
#  dbname = "ODS",
#  UID = conn$UID_prod, 
#  PWD = conn$PWD_prod,
#  #UID = Sys.getenv("userid_production"), # need to change in Connect {vars}
#  #PWD = Sys.getenv("pwd_production")   # need to change in Connect {vars}
#  # Test environment
#  #Server= "WSQ04151,50000",
#  #dbname = "ODS_test",
#  #UID = Sys.getenv("userid"),  # need to change in Connect {vars}
#  #PWD = Sys.getenv("pwd"),  # need to change in Connect {vars}
#  trusted_connection = "yes"
#)




# Quick Station Sampling Summary Information
stationSummarySampingMetrics <- function(stationInfo_sf){
  stationInfo_sf %>%
    group_by(STATION_ID) %>%
    mutate(`Years Sampled` = WQM_YRS_YEAR) %>% #paste0(year(WQM_YRS_YEAR))) %>% # for when column coming in as date
    dplyr::select(STATION_ID, WQM_YRS_SPG_CODE,WQM_YRS_YEAR,`Years Sampled`,WQM_SPG_DESCRIPTION) %>%
    st_drop_geometry() %>%
    group_by(STATION_ID, `Years Sampled`) %>%
    summarise(`Sample Codes` = paste0(WQM_YRS_SPG_CODE, collapse = ' | '))
} 