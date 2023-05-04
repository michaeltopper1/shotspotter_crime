## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-04-26
##

library(tidyverse)
library(fixest)

calls <- read_csv("created_data/dispatches_appended_16_23.csv")
rollout_dates <- read_csv("created_data/rollout_dates.csv")


calls <- calls %>% 
  mutate(shotspotter_alert = ifelse(event_type == "SST", 1, 0))

calls <- calls %>% 
  distinct(event_number, shotspotter_alert, district, entry_date)

calls <- calls %>% 
  mutate(year = year(entry_date),
         month = month(entry_date),
         date = as_date(entry_date))

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

aggregated_calls <- calls %>% 
  mutate(district = parse_number(district)) %>% 
  filter(district %in% panel_dates$district) %>% 
  group_by(date, district) %>% 
  summarize(num_calls = n(),
            num_shotspotter = sum(shotspotter_alert, na.rm = T))

calls_panel <- panel_dates %>% 
  left_join(aggregated_calls, join_by(district == district,
                                      date == date)) 

calls_panel %>% 
  left_join(rollout_dates, join_by(district == district) ) %>% 
  mutate(treatment = ifelse(date >= shotspot_activate, 1, 0)) %>% 
  mutate(treatment = ifelse(is.na(treatment), 0, treatment)) %>% 
  fepois(num_calls ~ treatment | district + date,
        cluster = ~district)


