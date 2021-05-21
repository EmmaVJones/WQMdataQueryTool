# Try with DBI
library(DBI)
con4 <- dbConnect(odbc::odbc(), .connection_string = "driver={ODBC Driver 11 for SQL Server};server={DEQ-SQLODS-PROD,50000};database={ODS};trusted_connection=yes")
dbListTables(con4)


dbListTables(con4)

library(dbplyr)
tbl(con4, in_schema("wqm", "WQM_Stations_View")) %>%
  head()

tbl(con4, in_schema("wqm", "WQM_STATIONS")) %>%
  head()



# with pool
library(tidyverse)
library(pool)

pool <- dbPool(
  drv = odbc::odbc(),
  Driver = "ODBC Driver 11 for SQL Server",#Driver = "SQL Server Native Client 11.0",
  Server= "DEQ-SQLODS-PROD,50000",
  dbname = "ODS", #"ODS.wqm",
  trusted_connection = "yes"
)


# 
# WQM_Sta_GIS_View_Stations <- pool %>% tbl("WQM_FIELD_DATA") %>% 
#   as_tibble()


testTest <- pool %>% tbl(in_schema("wqm", "Wqm_Field_Data_View")) %>% 
  head() %>% 
  as_tibble()



station <- '2-SKC001.17'#'2-JKS023.61'#'4AROA175.63'##'4ASRE043.54'#'2-JKS028.69'#'4AROA202.20'#'4ATKR000.08'#'4ADEE000.06'##'4ATKR003.03'#'2-JKS023.61'#'4ADEE000.06'##'2-JKS018.68'#'1BNFS011.81'#'2-PWT003.98'#'2-JKS023.61'#'2-JKS067.00'#'2-JKS023.61'#'1AOCC002.47'##'2-JKS006.67'#'2-JKS023.61'#'4AROA217.38'# not in WQM_full on REST service '2-JKS023.61'#

dbGetQuery(pool, "SELECT * FROM wqm.WQM_Stations_View") %>% 
  filter(Sta_Id %in% !! toupper(station)) 
dbListTables(pool)

con4 <- dbConnect(odbc::odbc(), .connection_string = "driver={ODBC Driver 11 for SQL Server};server={DEQ-SQLODS-PROD,50000};database={ODS};trusted_connection=yes")


dbGetQuery(pool, "SELECT TOP 10 * FROM wqm.WQM_STATIONS_VIEW") 

dbGetQuery(pool, "SELECT TOP 10 * FROM wqm.WQM_FIELD_DATA_VIEW")

# works

dbGetQuery(pool, "SELECT  TOP 10 * FROM wqm.WQM_Stations_View") # works


show_query(pool %>% tbl("WQM_STA_GIS")) %>% 
  head(5) %>% 
  as_tibble() %>% show_query()


show_query(pool %>% tbl("Wqm_Lab_Cds_Codes_Wqm_View") %>% as_tibble())


con_ods <- dbPool(
  
  drv = odbc::odbc(),
  
  Driver = "ODBC Driver 11 for SQL Server",
  
  Server= "DEQ-SQLODS-PROD,50000",
  
  dbname = "ODS",
  
  trusted_connection = "yes"
  
)

con_ods %>% tbl("Wqm_Sta_GIS_View") %>% 
  head(5) %>% 
  as_tibble() %>% show_query()
