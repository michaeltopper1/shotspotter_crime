## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-04-11
##

library(tidyverse)

shifts <- read_csv("created_data/shifts_appended.csv")

shifts <- shifts %>% 
  mutate(date = as_date(aa_date))

shifts <- shifts %>% 
  mutate(start_time = parse_time(start_time, format = "%H%M")) %>% 
  mutate(end_time  = parse_time(end_time, format = "%H%M"))


shifts_aggregated <- shifts %>% 
  group_by(date, unit) %>% 
  summarize(num_shifts = n()) %>% 
  ungroup()


## creating panels for each of the districts
panel_dates <- seq(as_date("2016-01-01"), as_date("2019-12-31") , by= "day") %>% 
  as_tibble() %>% 
  rename(date = value) 

districts <- c(1:20, 22, 24, 25) %>% 
  as_tibble() %>% 
  rename(district = value) %>% 
  filter(district != 13)

panel_dates <- panel_dates %>% 
  cross_join(districts)

shifts_panel <- panel_dates %>% 
  left_join(shifts_aggregated, by = join_by(date == date,
                                            district == unit))


rollout_dates <- tribble(~district, ~shotspot_activate,
                         2, mdy("5-6-2018"), 
                         3, mdy("1-29-2018"), 
                         4, mdy("1-29-2018"),
                         5, mdy("3-13-2018"),
                         6, mdy("9-23-2017"),
                         7, mdy("1-13-2017"),
                         8, mdy("3-30-2018"),## differes from 4/1/2018. there are some beforehand, but not as consistent..looks like bleed form district 7
                         9, mdy("6-16-2017"), ## differences from 3/1/2017
                         10, mdy("10-12-2017"),
                         11, mdy("3-1-2017"), ## probably later, actually loosk ok
                         15, mdy("5-11-2017"), ## differs from 3/1/2018
                         25, mdy("4-1-2017"))

shifts_panel <- shifts_panel %>% 
  left_join(rollout_dates) %>% 
  mutate(treatment = ifelse(date >= shotspot_activate, 1, 0),
         .by = district) %>% 
  mutate(treatment = ifelse(is.na(treatment), 0, treatment))


library(fixest)
stops <- read_csv("analysis_data/stops_panel.csv")

shifts_panel <- shifts_panel %>% 
  left_join(stops, join_by(date == date,
                           district == district))
shifts_panel %>% 
  feols(number_stops ~ num_shifts | date + district, 
        cluster = ~district, data = .)
