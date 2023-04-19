## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-04-01
##

library(tidyverse)

stops <- read_csv("created_data/stops_cleaned.csv")


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



# aggregating the stops --------------------------------------------------
  
stops <- stops %>% 
  mutate(black_male_stop = ifelse(person_stopped_race_black ==1 & person_stopped_sex_m == 1, 1, 0)) 
 
stops_aggregated <- stops %>%
  group_by(stop_district, stop_date) %>% 
  summarize(number_stops = n(),
            number_black_stops = sum(person_stopped_race_black, na.rm = T),
            number_investigatory_stops = sum(stop_type_investigatory_stop, na.rm = T),
            number_hispanic_stops = sum(person_stopped_race_hispanic, na.rm = T),
            number_asian_stops = sum(person_stopped_race_asian, na.rm = T),
            number_males_stopped = sum(person_stopped_sex_m, na.rm = T),
            number_gang_stops = sum(stop_type_gang_and_narcotics_related_loitering,na.rm = T),
            number_firearm_found = sum(firearm_found, na.rm = T),
            number_pat_down = sum(pat_down_y_n, na.rm = T),
            number_black_males_stops = sum(black_male_stop, na.rm = T),
            number_search_beyond_patdown = sum(search_beyond_a_protective_pat_down_conducted_of_the_person_y_n, na.rm = T)) %>% 
  mutate(across(starts_with("number"), ~ifelse(is.na(.), 0, .))) %>% 
  ungroup()

stops_panel <- panel_dates %>% 
  left_join(stops_aggregated, join_by(district == stop_district,
                                     date == stop_date)) %>% 
  mutate(across(starts_with("number"), ~ifelse(is.na(.), 0, .)))



# rollout dates -----------------------------------------------------------


rollout_dates <- tribble(~district, ~shotspot_activate,
                         2, mdy("5-16-2018"), 
                         3, mdy("1-29-2018"), 
                         4, mdy("1-29-2018"),
                         5, mdy("3-16-2018"),
                         6, mdy("10-6-2017"),
                         7, mdy("1-13-2017"),
                         8, mdy("3-28-2018"),## differes from 4/1/2018. there are some beforehand, but not as consistent..looks like bleed form district 7
                         9, mdy("6-16-2017"), ## differences from 3/1/2017
                         10, mdy("10-12-2017"),
                         11, mdy("3-1-2017"), ## partially treated in january 1/2017 less extreme than 25.
                         15, mdy("5-11-2017"), ## differs from 3/1/2018
                         25, mdy("4-24-2018")) ### partiailly treated as early as january 2017

stops_panel <- stops_panel %>% 
  left_join(rollout_dates)

stops_panel <- stops_panel %>% 
  mutate(treatment = ifelse(shotspot_activate <= date, 1, 0), .by = district) %>% 
  mutate(never_treated = ifelse(is.na(treatment),1, 0), .by = district) %>% 
  mutate(treatment = ifelse(is.na(treatment), 0, treatment
                            ), .by = district)


stops_panel <- stops_panel %>% 
  mutate(month = month(date),
         year = year(date),
         year_month = mdy(paste0(month, "-1-", year)))

stops_panel %>% 
  write_csv("analysis_data/stops_panel.csv")

