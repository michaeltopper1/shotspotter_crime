## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-05-08
##

library(tidyverse)
library(readlxl)

files <- list.files("raw_data/shifts_20-23/")
bad_file <- files[8]
files <- files[!files %in% bad_file]


shifts_20_23 <- map_df(files, ~readxl::read_xlsx(paste0("raw_data/shifts_20-23/", .))) %>% 
  janitor::clean_names()

shifts_20_23 <- shifts_20_23 %>% 
  mutate(aa_date = if_else(is.na(aa_date), date, aa_date)) 
  
shifts_20_23 <- shifts_20_23 %>% 
  mutate(aa_date = as_date(aa_date))

bad_file <- read_xlsx(paste0("raw_data/shifts_20-23/", bad_file)) %>% 
  janitor::clean_names()

bad_file <- bad_file %>% 
  mutate(aa_date = dmy(date), .before = 1)
  
shifts_20_23 <- shifts_20_23 %>% 
  mutate(date = as.character(date)) %>% 
  bind_rows(bad_file)


shifts_20_23 <- shifts_20_23 %>% 
  filter(sworn == "Y") %>% 
  filter(present_for_duty == "Y") %>% 
  distinct()




shifts_20_23 %>% 
  write_csv("created_data/shifts_20_23.csv")
