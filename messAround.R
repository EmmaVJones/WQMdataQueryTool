wqm_samples_view <- pool %>% tbl()


samplesView <- pool %>% tbl(in_schema("wqm", "Wqm_Samples_View")) %>%
  distinct(Sam_Mrs_Container_Id_Desc) %>% 
  as_tibble()
  
analytesView <- pool %>% tbl(in_schema("wqm", "Wqm_Analytes_View")) %>%
  distinct(Ana_Sam_Mrs_Container_Id_Desc) %>% 
  as_tibble()

commentsView <- pool %>% tbl(in_schema("wqm", "Wqm_Field_Data_View")) %>%
  distinct(Fdt_Comment) %>% 
  as_tibble()

commentsView2 <- commentsView %>% 
  filter( str_detect(Fdt_Comment, '= S')) %>%
  filter( str_detect(Fdt_Comment, '=S')) %>%
  filter( str_detect(Fdt_Comment, '= s')) %>%
  filter( str_detect(Fdt_Comment, '=s')) %>%
  filter( str_detect(Fdt_Comment, 'STORM EVENT')) %>%
  filter( str_detect(Fdt_Comment, 'storm event')) %>%
  filter( str_detect(Fdt_Comment, 'STORM SAMPL')) %>%
  filter( str_detect(Fdt_Comment, 'storm sampl')) %>%
  filter( str_detect(Fdt_Comment, 'TARGETED')) %>%
  filter( str_detect(Fdt_Comment, 'targeted')) %>%
  filter( str_detect(Fdt_Comment, 'DUPLICATE')) %>%
  filter( str_detect(Fdt_Comment, 'duplicate')) %>%
  filter( str_detect(Fdt_Comment, 'BLANK')) %>%
  filter( str_detect(Fdt_Comment, 'blank'))

  #filter(Ana_Sam_Fdt_Id %in% !! stationFieldData$Fdt_Id  &
           between(as.Date(Ana_Received_Date), !! dateRange[1], !! dateRange[2]) & # x >= left & x <= right
           Pg_Parm_Name != "STORET STORAGE TRANSACTION DATE YR/MO/DAY") %>% 
  as_tibble() 



# Fix field measurement to NA if remark field indicates QC failure
View(dplyr::select(stationFieldDataUserFilter, Fdt_Temp_Celcius:Fdt_Gauge_Height, Fdt_Do_Satr_Per:Fdt_Secchi_Depth ) %>% 
  select(sort(tidyselect::peek_vars())))
# need to remove Fdt?

# flip long by parameter and remark?
