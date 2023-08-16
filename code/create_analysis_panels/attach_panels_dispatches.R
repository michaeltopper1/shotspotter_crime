## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-05-19
##

library(tidyverse)

sst_alerts <- read_csv("analysis_data/sst_dispatches_cpd.csv") 
dispatch_panel <- read_csv("analysis_data/dispatches_all.csv") 
officer_hours <- read_csv("analysis_data/officer_hours.csv")
border_districts <- read_csv("created_data/border_districts_final.csv")
victimization <- read_csv("analysis_data/xxvictim_panel.csv")


sst_alerts <- sst_alerts %>% 
  filter(year < 2023)

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
  mutate(number_sst_dispatches =if_else(is.na(number_sst_dispatches), 0, number_sst_dispatches))

dispatch_panel <- dispatch_panel %>% 
  mutate(across(starts_with("arrests_made"), ~if_else(is.na(.), 0, .)))

dispatch_panel <- dispatch_panel %>% 
  mutate(across(ends_with("num"), ~if_else(is.na(.), 0, .)),
         across(ends_with("arrest"), ~if_else(is.na(.), 0, .))) %>% 
  mutate(domestic_disturb_p1_arrestrate = domestic_disturb_p1_arrest/domestic_disturb_p1_num,
         domestic_battery_p1_arrestrate = domestic_battery_p1_arrest/domestic_battery_p1_num,
         battery_ip_p1_arrestrate = battery_ip_p1_arrest/battery_ip_p1_num,
         ems_p1_arrestrate = ems_p1_arrest/ems_p1_num,
         robbery_jo_p1_arrestrate = robbery_jo_p1_arrest/robbery_jo_p1_num,
         assault_ip_p1_arrestrate = assault_ip_p1_arrest/assault_ip_p1_num,
         shots_fired_p1_arrestrate = shots_fired_p1_arrest/shots_fired_p1_num,
         check_well_p1_arrestrate = check_well_p1_arrest/check_well_p1_num,
         fire_p1_arrestrate = fire_p1_arrest/fire_p1_num,
         person_gun_arrestrate = person_gun_p1_arrest/person_gun_p1_num,
         gun_crime_arrestrate_1 = number_guncrime_arrest_1/number_guncrime_1,
         non_gun_crime_arrestrate_1 = number_no_guncrime_arrest_1/number_no_guncrime_1,
         prob_victim_injury_1 = number_victim_injury_ts_1/number_time_sensitive_calls_1,
         prob_victim_injury_2 = number_victim_injury_2/number_time_sensitive_calls_2,
         prob_victim_injury_3 = number_victim_injury_3/number_time_sensitive_calls_3,
         prob_victim_injury = number_victim_injury_ts/number_time_sensitive_calls,
         prob_victim_injury_no_guncrime = number_victim_injury_no_gun_crime/number_time_sensitive_calls_no_gun,
         prob_victim_injury_guncrime = number_victim_injury_ts_gun/number_time_sensitive_calls_gun,
         prob_victim_injury_guncrime_1 = number_victim_injury_ts_gun_1/number_time_sensitive_calls_gun_1,
         prob_victim_injury_no_guncrime_1 = number_victim_injury_ts_no_gun_1/number_time_sensitive_calls_no_gun_1) 


dispatch_panel <- dispatch_panel %>% 
  left_join(border_districts, join_by(district == border_district)) 

dispatch_panel <- dispatch_panel %>% 
  group_by(district) %>% 
  mutate(border_treatment = mdy(border_treatment)) %>% 
  mutate(shotspot_border_treatment = ifelse(date >= border_treatment,1 ,0 )) %>% 
  ungroup() %>% 
  mutate(shotspot_border_treatment = ifelse(is.na(shotspot_border_treatment), 0, shotspot_border_treatment))

dispatch_panel <- dispatch_panel %>% 
  left_join(victimization, join_by(district == district, date == date), suffix = c("", ".y")) %>%
  select(-ends_with(".y"))



# getting rid of july 4th/december 31/january 1 ---------------------------

dispatch_panel <- dispatch_panel %>% 
  mutate(day = day(date)) %>% 
  filter(!(month ==7 & day == 4)) %>% 
  filter(!(month == 1 & day == 1)) %>% 
  filter(!(month == 12 & day == 31)) 


# creating arrest rates ---------------------------------------------------

dispatch_panel <- dispatch_panel %>% 
  rowwise() %>% 
  mutate(arrest_rate = arrests_made/number_dispatches,
         arrest_rate_1 = arrests_made_1/number_dispatches_1,
         arrest_rate_2 = arrests_made_2/number_dispatches_2,
         arrest_rate_3 = arrests_made_3/number_dispatches_3) %>% 
  ungroup()


dispatch_panel %>% 
  write_csv("analysis_data/xxdispatch_panel.csv")
