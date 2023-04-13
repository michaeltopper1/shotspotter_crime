## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-04-13
##

library(tidyverse)


# 911 matt chapman data ---------------------------------------------------

## I inadvertantly named this dispatches when it is in fact 911 metadata
## this is all similar to the dispatch data (see event number) although
## it does not have many of the needed columns for dispatching

files <- list.files("raw_data/dispatches_nobadge/")

dispatches <- map_df(files, ~read_csv(paste0("raw_data/dispatches_nobadge/",.),
                                      col_types = list(EventNumber = col_character(),
                                      EntryDate = col_character(),
                                      CloseDate = col_character(),
                                      FinalEventType = col_character(),
                                      FinalDisposition = col_character(),
                                      CaseNumberList = col_character()))) 

dispatches <- dispatches %>% janitor::clean_names()

dispatches <- dispatches %>% distinct()

dispatches <- dispatches %>% 
  mutate(across(ends_with("date"), ~mdy_hms(.)))

dispatches %>% 
  write_csv("created_data/911_16_20_cleaned.csv")





