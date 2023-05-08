## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-05-04
##

library(tidyverse)

files <- list.files("raw_data/dispatches_event_type/")

dispatches <- map_df(files, ~read_csv(paste0("raw_data/dispatches_event_type/",.))) %>% 
  janitor::clean_names()

dispatches <- dispatches %>% 
  mutate(across(c(entry_date, dispatched, onscene), ~mdy_hms(.)))

dispatches <- dispatches %>% 
  mutate(time_to_dispatch = seconds(seconds_to_period(dispatched - entry_date)) %>% 
           second(),
         time_dispatch_to_onscene = seconds(seconds_to_period(onscene - dispatched)) %>% 
           second(),
         time_entry_to_onscene = seconds(seconds_to_period(onscene - entry_date)) %>% 
           second(),
         .before = 1) 

dispatches %>% 
  write_csv("created_data/dispatches_19_23_eventnumber.csv")
