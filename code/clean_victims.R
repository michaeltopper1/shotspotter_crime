## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-05-17
##

library(tidyverse)

victims <- read_csv("raw_data/all_victims.csv") %>% 
  janitor::clean_names()

victims <- victims %>% 
  mutate(date = mdy_hms(date) %>% 
           as_date()) 

victims <- victims %>% 
  mutate(year = year(date)) %>% 
  filter(year >= 2016 & year <= 2022) %>%
  distinct(case_number, .keep_all = T)


victims <- victims %>%
  mutate(
    any_gunshot_victim = if_else(gunshot_injury_i == "YES", 1, 0),
    gunshot_homicide = if_else(gunshot_injury_i == "YES" & incident_primary == "HOMICIDE", 1, 0),
    non_gun_homicide = if_else(gunshot_injury_i == "NO" & incident_primary == "HOMICIDE", 1, 0),
    any_homicide = if_else(incident_primary == "HOMICIDE", 1, 0),
    non_gunshot_victim = if_else(gunshot_injury_i == "NO", 1, 0),
    gun_robbery = if_else(gunshot_injury_i == "YES" & incident_primary == "ROBBERY", 1, 0),
    
    gun_battery = if_else(gunshot_injury_i == "YES" & incident_primary == "BATTERY", 1, 0),
  )


victims_aggregated <- victims %>%
  group_by(district, date) %>%
  summarize(
    num_any_victim = n(),
    num_victim_no_gun = sum(non_gunshot_victim, na.rm = T),
    num_any_gunshot_victim = sum(any_gunshot_victim, na.rm = T),
    num_gunshot_homicide = sum(gunshot_homicide, na.rm = T),
    num_non_gun_homicide = sum(non_gun_homicide, na.rm = T),
    num_any_homicide = sum(any_homicide, na.rm = T),
    
    num_gun_robbery = sum(gun_robbery, na.rm = T),
    
    num_gun_battery = sum(gun_battery, na.rm = T),
    
  ) %>% ungroup()

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
  mutate(across(starts_with("num"), ~if_else(is.na(.), 0, .)))


victims_panel %>% 
  write_csv("analysis_data/victims_panel.csv")
