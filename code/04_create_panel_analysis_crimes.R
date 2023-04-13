## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-04-08
##

library(tidyverse)

crimes <- read_csv("created_data/crimes_cleaned.csv")

## getting firearm crimes and firearm arrests
## may want to change this down the line by looking at some of the descriptions.
## some of the descriptions include thigns that we don't really care about with ShotSpotter like
## Unlawful Sales/School Offenses/Registration etc.
crimes <- crimes %>% 
  mutate(firearm = ifelse(str_detect(description, "FIREARM|GUN"), 1, 0)) %>% 
  mutate(firearm_arrest = ifelse(firearm == 1 & arrest == 1, 1, 0))
  

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


# aggregating the crimes data ---------------------------------------------

crimes_aggregated <- crimes %>% 
  mutate(other_crime = ifelse(firearm == 0, 1, 0),
         other_arrest = ifelse(arrest == 1 & firearm_arrest !=1, 1, 0)) %>% 
  group_by(date, district) %>% 
  summarize(across(c(firearm, firearm_arrest, 
                     other_crime, other_arrest), ~sum(.,na.rm = T), .names = "number_{.col}")) %>% 
  ungroup()

crimes_panel <- panel_dates %>% 
  left_join(crimes_aggregated, by = join_by(date == date, 
                                       district == district))


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





crimes_panel <- crimes_panel %>% 
  left_join(rollout_dates) %>% 
  mutate(treatment = ifelse(shotspot_activate <= date, 1, 0), .by = district) %>% 
  mutate(never_treated = ifelse(is.na(treatment),1, 0), .by = district) %>% 
  mutate(treatment = ifelse(is.na(treatment), 0, treatment
  ), .by = district)


crimes_panel %>% 
  write_csv("analysis_data/crimes_panel.csv")

