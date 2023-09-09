## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-09-05
##

library(tidyverse)


sst <- read_csv("created_data/sst_dispatch_cpd.csv")


sst_arrests <- readxl::read_excel("raw_data/shotspotter_arrests_events.xlsx",
                                  sheet = "Data Set II") %>% 
  janitor::clean_names() %>% 
  mutate(event_number = as.double(event_number),
         associated_event_number = as.double(associated_event_number))



sst %>% 
  left_join(sst_arrests, join_by(event_number == associated_event_number)) %>% 
  distinct(event_number, .keep_all =  T) %>% 
  drop_na(associated_event_type)


