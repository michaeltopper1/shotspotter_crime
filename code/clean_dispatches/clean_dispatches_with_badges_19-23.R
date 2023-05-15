## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-04-11
##

library(tidyverse)

files <- list.files("raw_data/dispatches/")

dispatches <- map_df(files, ~read_csv(paste0("raw_data/dispatches/", .)) %>% 
         janitor::clean_names())

dispatches <- dispatches %>% 
  mutate(across(c(entry_date,
                  dispatched, enroute,
                  onscene), ~mdy_hms(.)))


dispatches %>% 
  write_csv("created_data/dispatches.csv")
