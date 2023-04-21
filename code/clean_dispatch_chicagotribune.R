## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-04-20
##

library(tidyverse)

files <- list.files("raw_data/911_chicago_tribune//")

nineone <- map_df(files, ~read_csv(paste0("raw_data/911_chicago_tribune/",.)))

nineone <- nineone %>% 
  janitor::clean_names() %>% 
  distinct()

nineone <- nineone %>% 
  mutate(date_received = mdy_hms(received),
         entry_date = mdy_hms(entry_date),
         first_dispatch_date = mdy_hms(first_dispatch_date)) %>% 
  select(-received)

nineone <- nineone %>% 
  filter(entry_date > as_date("2015-12-31"))

nineone %>% 
  write_csv("created_data/dispatches_16_20.csv")
