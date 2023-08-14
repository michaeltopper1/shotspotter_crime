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
  mutate(number_isr_stops_gun_crime = if_else(is.na(number_isr_stops_gun_crime), 0, number_isr_stops_gun_crime))

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
         domestic_disturb_p1_a_arrestrate = domestic_disturb_p1_a_arrest/domestic_disturb_p1_num,
         domestic_disturb_p1_c_arrestrate = domestic_disturb_p1_c_arrest/domestic_disturb_p1_num,
         domestic_battery_p1_arrestrate = domestic_battery_p1_arrest/domestic_battery_p1_num,
         domestic_battery_p1_a_arrestrate = domestic_battery_p1_a_arrest/domestic_battery_p1_num,
         domestic_battery_p1_c_arrestrate = domestic_battery_p1_c_arrest/domestic_battery_p1_num,
         battery_ip_p1_arrestrate = battery_ip_p1_arrest/battery_ip_p1_num,
         battery_ip_p1_a_arrestrate = battery_ip_p1_a_arrest/battery_ip_p1_num,
         battery_ip_p1_c_arrestrate = battery_ip_p1_c_arrest/battery_ip_p1_num,
         ems_p1_arrestrate = ems_p1_arrest/ems_p1_num,
         ems_p1_a_arrestrate = ems_p1_a_arrest/ems_p1_num,
         ems_p1_c_arrestrate = ems_p1_c_arrest/ems_p1_num,
         robbery_jo_p1_arrestrate = robbery_jo_p1_arrest/robbery_jo_p1_num,
         robbery_jo_p1_a_arrestrate = robbery_jo_p1_a_arrest/robbery_jo_p1_num,
         robbery_jo_p1_c_arrestrate = robbery_jo_p1_c_arrest/robbery_jo_p1_num,
         assault_ip_p1_arrestrate = assault_ip_p1_arrest/assault_ip_p1_num,
         assault_ip_p1_a_arrestrate = assault_ip_p1_a_arrest/assault_ip_p1_num,
         assault_ip_p1_c_arrestrate = assault_ip_p1_c_arrest/assault_ip_p1_num,
         shots_fired_p1_arrestrate = shots_fired_p1_arrest/shots_fired_p1_num,
         shots_fired_p1_a_arrestrate = shots_fired_p1_a_arrest/shots_fired_p1_num,
         shots_fired_p1_c_arrestrate = shots_fired_p1_c_arrest/shots_fired_p1_num,
         check_well_p1_arrestrate = check_well_p1_arrest/check_well_p1_num,
         fire_p1_arrestrate = fire_p1_arrest/fire_p1_num,
         fire_p1_a_arrestrate = fire_p1_a_arrest/fire_p1_num,
         fire_p1_c_arrestrate = fire_p1_c_arrest/fire_p1_num,
         person_gun_arrestrate = person_gun_p1_arrest/person_gun_p1_num,
         person_gun_a_arrestrate = person_gun_p1_a_arrest/person_gun_p1_num,
         person_gun_c_arrestrate = person_gun_p1_c_arrest/person_gun_p1_num,
         gun_crime_report_arrestrate = gun_crime_report_arrest/number_guncrime,
         gun_crime_report_a_arrestrate = gun_crime_report_a_arrest/number_guncrime,
         gun_crime_report_c_arrestrate = gun_crime_report_c_arrest/number_guncrime,
         isr_firearm_found_rate = number_firearm_found_isr_stops/number_isr_stops,
         isr_black_stop_rate = number_black_isr_stops/number_isr_stops,
         isr_firearm_found_gun_crime_rate = number_firearm_found_isr_stops_gun_crime/number_isr_stops_gun_crime) 


# injury probabilties -----------------------------------------------------

dispatch_panel <- dispatch_panel %>%
  mutate(injury_prob = number_victim_injury/number_dispatches,
         injury_prob_1 = number_victim_injury_1/number_dispatches_1,
         injury_domestic_disturb_1 = number_victim_injury_domestic_disturb_p1/domestic_disturb_p1_num) 



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
         arrest_rate_a = arrests_made_arrest_data/number_dispatches,
         arrest_rate_c = arrests_made_crimes_data/number_dispatches,
         arrest_rate_1 = arrests_made_1/number_dispatches_1,
         arrest_rate_2 = arrests_made_2/number_dispatches_2,
         arrest_rate_3 = arrests_made_3/number_dispatches_3) %>% 
  ungroup()


dispatch_panel %>% 
  write_csv("analysis_data/xxdispatch_panel.csv")
