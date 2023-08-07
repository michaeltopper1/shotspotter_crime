## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-08-07
##

library(tidyverse)

sst <- read_csv("created_data/sst_dispatch_cpd.csv")

sst <- sst %>% 
  mutate(district = parse_number(district)) %>% 
  mutate(date = as_date(entry_received_date),
         year = year(date)) %>% 
  group_by(date, district) %>% 
  summarize(number_sst_dispatches = n()) %>% 
  ungroup() %>% 
  mutate(year = year(date))

write_csv(sst, "analysis_data/sst_dispatches_cpd.csv")  
