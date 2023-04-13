## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-04-13
##

library(tidyverse)



# attaching the shotspotter alerts dispatches -----------------------------

## This attaches the dispatch badge number to the shotspotter alerts
## these can be reattached together which I will do at the end

dispatches_cleaned <- read_csv("created_data/dispatches.csv")


shotspotter_dispatches <- read_csv("/Users/michaeltopper/Downloads/cpd_foia231378 2.csv",
                                   skip = 1) %>% 
  slice(-1, -2)

shotspotter_dispatches <- shotspotter_dispatches %>% janitor::clean_names() %>% 
  mutate(across(c(entry_date,
                  dispatched, enroute,
                  onscene), ~mdy_hms(.)))

dispatches_cleaned <- dispatches_cleaned %>% 
  mutate(event_number = as.character(event_number))

shotspotter_dispatches <- shotspotter_dispatches %>% 
  mutate(shotspoter_dispatch = 1) %>% 
  left_join(dispatches_cleaned) 

shotspotter_dispatches %>% 
  write_csv("created_data/shotspotter_dispatches.csv")