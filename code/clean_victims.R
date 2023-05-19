## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-05-17
##

library(tidyverse)

victims <- read_csv("raw_data/gun_victims.csv") %>% 
  janitor::clean_names()

victims <- victims %>% 
  mutate(date = mdy_hms(date) %>% 
           as_date()) %>% 
  mutate(gunshot_injury_i = if_else(gunshot_injury_i == "YES", 1, 0))

victims <- victims %>% 
  mutate(year = year(date)) %>% 
  filter(year > 2016 & year < 2022) %>%
  distinct(case_number, .keep_all = T)

victims_aggregated <- victims %>% 
  group_by(district, date) %>% 
  summarize(number_gun_injury_victims = sum(gunshot_injury_i, na.rm = T),
            number_gun_victims = n()) %>% ungroup()

## creating panels for each of the districts
panel_dates <- seq(as_date("2016-01-01"), as_date("2022-12-31") , by= "day") %>% 
  as_tibble() %>% 
  rename(date = value) 

districts <- c(1:20, 22, 24, 25) %>% 
  as_tibble() %>% 
  rename(district = value) %>% 
  filter(district != 13)

panel_dates <- panel_dates %>% 
  cross_join(districts)

victims_panel <- panel_dates %>% 
  left_join(victims_aggregated, join_by(date == date,
                                        district == district)) %>% 
  mutate(across(starts_with("number"), ~if_else(is.na(.), 0, .)))

victims_panel %>% 
  write_csv("analysis_data/victims_panel.csv")
