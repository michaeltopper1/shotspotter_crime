## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-05-19
##

library(tidyverse)

sst_alerts <- read_csv("created_data/shotspotter_cleaned.csv")
dispatch_panel <- read_csv("analysis_data/dispatches_all.csv")
officer_hours <- read_csv("analysis_data/officer_hours.csv")
border_districts <- read_csv("created_data/border_districts_final.csv")


sst_alerts <- sst_alerts %>% 
  filter(year < 2023)

sst_alerts <- sst_alerts %>% 
  group_by(date, district) %>% 
  summarize(number_sst_alerts = n()) %>% 
  ungroup()

dispatch_panel <- dispatch_panel %>% 
  mutate(across(starts_with("number_dispatches"),
                ~ifelse(is.na(.), 0, .)))

dispatch_panel <- dispatch_panel %>% 
  left_join(sst_alerts, join_by(date == date, 
                                district == district))

dispatch_panel <- dispatch_panel %>% 
  left_join(officer_hours, join_by(date == date,
                                   district == district))

dispatch_panel <- dispatch_panel %>% 
  mutate(number_sst_alerts =if_else(is.na(number_sst_alerts), 0, number_sst_alerts))


dispatch_panel <- dispatch_panel %>% 
  left_join(border_districts, join_by(district == border_district))

dispatch_panel <- dispatch_panel %>% 
  group_by(district) %>% 
  mutate(shotspot_border_treatment = ifelse(date >= border_treatment,1 ,0 )) %>% 
  ungroup() %>% 
  mutate(shotspot_border_treatment = ifelse(is.na(shotspot_border_treatment), 0, shotspot_border_treatment))


# getting rid of july 4th/december 31/january 1 ---------------------------

dispatch_panel <- dispatches_panel %>% 
  mutate(day = day(date)) %>% 
  filter(!(month ==7 & day == 4)) %>% 
  filter(!(month == 1 & day == 1)) %>% 
  filter(!(month == 12 & day == 31))



dispatch_panel %>% 
  write_csv("analysis_data/xxdispatch_panel.csv")
