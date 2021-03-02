library(tidyverse)
library(pool)
library(config)
library(sf)

# Server connection things
conn <- config::get("connectionSettings") # get configuration settings

## For testing: connect to ODS production
pool <- dbPool(
  drv = odbc::odbc(),
  Driver = "ODBC Driver 11 for SQL Server",#"SQL Server Native Client 11.0",
  Server= "DEQ-SQLODS-PROD,50000",
  dbname = "ODS",
  trusted_connection = "yes"
)

station <- '2-JKS023.61'#'4AROA217.38'# not in WQM_full on REST service '2-JKS023.61'#
dateRange <- c(as.Date('1970-01-01'), as.Date(Sys.Date()))


### Field Data Information

stationFieldData <- pool %>% tbl("Wqm_Field_Data_View") %>%
  filter(Fdt_Sta_Id %in% !! station &
           between(as.Date(Fdt_Date_Time), !! dateRange[1], !! dateRange[2]) ) %>% # x >= left & x <= right
  as_tibble()


### Analyte information

stationAnalyteData <- pool %>% tbl("Wqm_Analytes_View") %>%
  filter(Ana_Sam_Fdt_Id %in% !! stationFieldData$Fdt_Id &
           between(as.Date(Ana_Received_Date), !! dateRange[1], !! dateRange[2]) ) %>% # x >= left & x <= right
  as_tibble() %>%
  left_join(dplyr::select(stationFieldData, Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time), by = c("Ana_Sam_Fdt_Id" = "Fdt_Id")) 

x <- stationAnalyteData %>%
  filter(Ana_Sam_Mrs_Container_Id_Desc == 'R') %>%
  group_by(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc) %>% #, Ana_Sam_Mrs_Lcc_Parm_Group_Cd) %>%
  dplyr::select(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc, #Ana_Sam_Mrs_Lcc_Parm_Group_Cd, 
                Ana_Parameter_Name, Ana_Value) %>%
  group_by(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc, #Ana_Sam_Mrs_Lcc_Parm_Group_Cd, 
           Ana_Parameter_Name) %>%
  filter(!is.na(Ana_Parameter_Name) & Ana_Parameter_Name != 'STORET CONVERSION') %>%
  mutate(n = n()) %>%
  arrange(Fdt_Date_Time, desc(n))



  pivot_wider(names_from = 'Ana_Parameter_Name', #c('Ana_Parameter_Name','Ana_Sam_Mrs_Lcc_Parm_Group_Cd'), names_sep = " | ", 
              values_from = "Ana_Value")

z <- full_join(stationFieldData,#dplyr::select(stationFieldData, Fdt_Id 
               x, by = c("Fdt_Id" = "Ana_Sam_Fdt_Id", 'Fdt_Sta_Id', 'Fdt_Date_Time'))

dplyr::select(z1, Fdt_Date_Time, `AMMONIA, TOTAL (MG/L AS N)`, `PHOSPHORUS, TOTAL (MG/L AS P)`)$`AMMONIA, TOTAL (MG/L AS N)`

glimpse(z)

z <- full_join(stationFieldData,
          stationAnalyteData %>%
           group_by(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc, Ana_Sam_Mrs_Lcc_Parm_Group_Cd) %>%
           dplyr::select(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc, Ana_Sam_Mrs_Lcc_Parm_Group_Cd, Ana_Parameter_Name, Ana_Value) %>%
           pivot_wider(names_from = 'Ana_Parameter_Name', #c('Ana_Parameter_Name','Ana_Sam_Mrs_Lcc_Parm_Group_Cd'), names_sep = " | ", 
                       values_from = "Ana_Value"),
         by = c("Fdt_Id" = "Ana_Sam_Fdt_Id", 'Fdt_Sta_Id', 'Fdt_Date_Time'))


z <- stationAnalyteData %>%
  #filter(Ana_Sam_Mrs_Container_Id_Desc %in% repFilter) %>%
  group_by(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc) %>%
  dplyr::select(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc, #Ana_Sam_Mrs_Lcc_Parm_Group_Cd,
                Ana_Parameter_Name, Ana_Value) %>%
  pivot_wider(names_from = c('Ana_Parameter_Name'), names_sep = " | ", 
              values_from = "Ana_Value")#,
              #values_fn = list(Ana_Value = mean))



# Join field and Analyte information
full_join(stationFieldData,
          stationAnalyteData %>%
            filter(Ana_Sam_Mrs_Container_Id_Desc %in% repFilter) %>%
            group_by(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc) %>%
            dplyr::select(Ana_Sam_Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time, Ana_Sam_Mrs_Container_Id_Desc, #Ana_Sam_Mrs_Lcc_Parm_Group_Cd,
                          Ana_Parameter_Name, Ana_Value) %>%
            pivot_wider(names_from = c('Ana_Parameter_Name'), names_sep = " | ", 
                        values_from = "Ana_Value",
                        values_fn = list(Ana_Value = mean)),
          by = c("Fdt_Id" = "Ana_Sam_Fdt_Id", 'Fdt_Sta_Id', 'Fdt_Date_Time'))