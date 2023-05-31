## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-05-14
##

library(tidyverse)

dispatches <- read_csv("created_data/dispatches_innerjoin_16_22.csv")
rollout_dates <- read_csv("created_data/rollout_dates.csv")
crimes_panel <- read_csv("analysis_data/crimes_panel.csv")



dispatches <- dispatches %>% 
  mutate(district_owm = if_else(str_detect(district.x,"^O|C"), 1, 0)) 


dispatches <- dispatches %>% 
  mutate(district = case_when(
   district_owm == 1 ~ NA,
   .default = district.x
  )) %>% 
  mutate(district = parse_number(district)) 


dispatches_filtered <- dispatches %>% 
  filter(district %in% crimes_panel$district)


## changing the SST to priority 1A
dispatches_filtered <- dispatches_filtered %>%
  mutate(priority_code = if_else(final_dispatch_code == "SST",
                                "1A", priority_code))
  
# going to be using the CPD data as the final priority
# most of them are the same
# changing the missing ones to 
dispatches_filtered <- dispatches_filtered %>% 
  mutate(priority_code = if_else(is.na(priority_code), priority_oemc, priority_code)) %>% 
  mutate(priority_code = parse_number(priority_code))


## getting rid fo the one priority 4 one
dispatches_filtered <- dispatches_filtered %>% 
  filter(priority_code != 4) 

## filtering to only call sources that are from 911 calls
dispatches_filtered <- dispatches_filtered %>% 
  filter(call_source_description == "E-911") 

dispatches_filtered <- dispatches_filtered %>% 
  mutate(entry_to_dispatch = time_length(first_dispatch_date - entry_received_date, "seconds"),
         entry_to_onscene = time_length(on_scene_date - entry_received_date, "seconds"),
         dispatch_to_onscene = time_length(on_scene_date - first_dispatch_date, "seconds"),
         entry_to_close = time_length(close_completed_date - entry_received_date, "seconds"))


## flooring the entry date and the dispatch date 
dispatches_filtered <- dispatches_filtered %>% 
  mutate(first_dispatch_date_floor = floor_date(first_dispatch_date, "minutes"),
         entry_date_floor = floor_date(entry_date, "minutes"),
         dispatch_to_onscene_floor = time_length(on_scene_date - first_dispatch_date_floor, "seconds"),
         entry_to_onscene_floor = time_length(on_scene_date - entry_date_floor, "seconds"))


dispatches_filtered <- dispatches_filtered %>% 
  mutate(dispatch_to_onscene_g1 = if_else(dispatch_to_onscene > 60, 1, 0),
         dispatch_to_onscene_g2 = if_else(dispatch_to_onscene > 120, 1, 0),
         entry_to_dispatch_g1 = if_else(entry_to_dispatch > 60, 1, 0),
         entry_to_dispatch_g2 = if_else(entry_to_dispatch > 120, 1, 0))

dispatches_filtered <- dispatches_filtered %>% 
  mutate(sst_dispatch = if_else(final_dispatch_code == "SST", 1, 0))

## need to deleted these sst
dispatches_filtered <- dispatches_filtered %>% 
  filter(sst_dispatch !=1)

## deleting the negative entry_to_dispatches - only 126 of these
dispatches_filtered <- dispatches_filtered %>% 
  filter(entry_to_dispatch > 0) 

## There are 38892 dispatch to onscenes that are less than or equal to 0.
## I am creating other columns that get rid of this.
dispatches_filtered <- dispatches_filtered %>% 
  mutate(dispatch_to_onscene_less_than_zero = if_else(dispatch_to_onscene <=0, 1, 0))

dispatches_filtered <- dispatches_filtered %>% 
  mutate(dispatch_to_onscene_filtered = if_else(dispatch_to_onscene_less_than_zero ==1,
                                                NA, dispatch_to_onscene),
         dispatch_to_onscene_filtered_floor = if_else(dispatch_to_onscene_less_than_zero ==1,
                                                      NA, dispatch_to_onscene_floor))



# creating indicators for certain crimes we care about --------------------

dispatches_filtered <- dispatches_filtered %>% 
  mutate(shots_fired =if_else(final_dispatch_description == "SHOTS FIRED", 1, 0),
         domestic_distrub = if_else(final_dispatch_description == "DOMESTIC DISTURBANCE", 1, 0),
         person_wgun = if_else(final_dispatch_description == "PERSON WITH A GUN", 1, 0),
         person_shot = if_else(final_dispatch_description == "PERSON SHOT", 1, 0),
         domestic_battery = if_else(final_dispatch_description == "DOMESTIC BATTERY", 1, 0)) %>% 
  mutate(gun_crime_report = if_else(shots_fired == 1 | person_wgun == 1 | person_shot == 1, 1, 0))

dispatches_filtered <- dispatches_filtered %>% 
  mutate(domestic_disturb_p1 = if_else(final_dispatch_description == "DOMESTIC DISTURBANCE" &
                                         priority_code == 1, 1, 0),
         check_well_p1 = if_else(final_dispatch_description == "CHECK WELL BEING" &
                                   priority_code ==1 , 1, 0),
         battery_ip_p1 = if_else(final_dispatch_description == "BATTERY IP" &
                                   priority_code == 1, 1, 0),
         suspicious_person_p1 = if_else(final_dispatch_description == "SUSPICIOUS PERSON" &
                                          priority_code ==1, 1, 0),
         ems_p1 = if_else(final_dispatch_description == "EMS" &
                            priority_code == 1, 1, 0),
         alarm_burglar_p2 = if_else(final_dispatch_description == "ALARM BURGLAR" &
                                      priority_code ==2, 1, 0),
         alarm_commercial_p2 = if_else(final_dispatch_description == "ALARM COMMERCIAL" &
                                         priority_code == 2, 1, 0),
         suspicious_auto_occ_p2 = if_else(final_dispatch_description == "SUSPICIOUS AUTO WITH OCC" &
                                            priority_code ==2, 1, 0),
         person_down_p2 = if_else(final_dispatch_description == "PERSON DOWN" &
                                    priority_code == 2, 1, 0),
         battery_jo_p2 = if_else(final_dispatch_description == "BATTERY JO" &
                                   priority_code == 2, 1, 0),
         disturbance_p3 = if_else(final_dispatch_description == "DISTURBANCE" &
                                    priority_code == 3, 1, 0),
         parking_violation_p3 = if_else(final_dispatch_description == "PARKING VIOL. 1" &
                                          priority_code == 3, 1, 0),
         disturbance_noise_p3 = if_else(final_dispatch_description == "DISTURBANCE - MUSIC/NOISE" &
                                          priority_code ==3, 1, 0),
         parking_violation_2_p3 = if_else(final_dispatch_description == "PARKING VIOL. 2" &
                                            priority_code == 3, 1, 0),
         sell_narcotics_p3 = if_else(final_dispatch_description == "SELLING NARCOTICS" &
                                       priority_code == 3, 1, 0))



# aggregating by month ----------------------------------------------------

## creating by priority
aggregated_monthly <- dispatches_filtered %>% 
  mutate(date = as_date(entry_received_date),
         year = year(date),
         month = month(date),
         year_month = mdy(paste0(month, "-1-", year))) %>% 
  group_by(year_month, district, priority_code) %>% 
  summarize(across(c(entry_to_dispatch,
                     entry_to_onscene,
                     dispatch_to_onscene,
                     entry_to_close,
                     dispatch_to_onscene_filtered,
                     dispatch_to_onscene_filtered_floor), ~mean(.,na.rm = T)),
            number_dispatches = n(),
            across(c(dispatch_to_onscene_g1,
                     dispatch_to_onscene_g2,
                     entry_to_dispatch_g1,
                     entry_to_dispatch_g2), ~sum(., na.rm = T), .names = "number_{.col}")) %>% ungroup()
## not doing it by priority
aggregated_monthly_nopriority <- dispatches_filtered %>% 
  mutate(date = as_date(entry_received_date),
         year = year(date),
         month = month(date),
         year_month = mdy(paste0(month, "-1-", year))) %>% 
  group_by(year_month, district) %>% 
  summarize(across(c(entry_to_dispatch,
                     entry_to_onscene,
                     dispatch_to_onscene,
                     entry_to_close,
                     dispatch_to_onscene_filtered,
                     dispatch_to_onscene_filtered_floor), ~mean(.,na.rm = T)),
            number_dispatches = n(),
            across(c(dispatch_to_onscene_g1,
                     dispatch_to_onscene_g2,
                     entry_to_dispatch_g1,
                     entry_to_dispatch_g2), ~sum(., na.rm = T), .names = "number_{.col}")) %>% ungroup()

## pivoting priority to fit dimensions
aggregated_monthly <- aggregated_monthly %>% 
  pivot_wider(names_from = priority_code, 
              values_from = c(entry_to_dispatch,
                              entry_to_onscene,
                              dispatch_to_onscene,
                              entry_to_close,
                              number_dispatches,
                              dispatch_to_onscene_filtered,
                              dispatch_to_onscene_filtered_floor,
                              number_dispatch_to_onscene_g1,
                              number_dispatch_to_onscene_g2,
                              number_entry_to_dispatch_g1,
                              number_entry_to_dispatch_g2))

## joining the-non priority and priority datas
aggregated_monthly <- aggregated_monthly %>% 
  left_join(aggregated_monthly_nopriority) 


## going to be defining treatment by first full month of treatment.
aggregated_monthly <- aggregated_monthly %>% 
  left_join(rollout_dates, join_by(district == district)) %>% 
  mutate(treatment = if_else(shotspot_activate <= year_month, 1, 0), .by = district) %>% 
  mutate(never_treated = if_else(is.na(treatment),1, 0), .by = district) %>% 
  mutate(treatment = if_else(is.na(treatment), 0, treatment
  ), .by = district)



# aggregating at daily level ----------------------------------------------


aggregated <- dispatches_filtered %>% 
  mutate(date = as_date(entry_received_date)) %>% 
  group_by(date, district, priority_code) %>% 
  summarize(across(c(entry_to_dispatch,
                     entry_to_onscene,
                     dispatch_to_onscene,
                     entry_to_close,
                     dispatch_to_onscene_filtered,
                     dispatch_to_onscene_filtered_floor), ~mean(.,na.rm = T)),
            number_dispatches = n(),
            across(c(dispatch_to_onscene_g1,
                     dispatch_to_onscene_g2,
                     entry_to_dispatch_g1,
                     entry_to_dispatch_g2), ~sum(., na.rm = T), .names = "number_{.col}")) %>% ungroup()

aggregated_nopriority <- dispatches_filtered %>% 
  filter(priority_code !=0) %>% 
  mutate(date = as_date(entry_received_date)) %>% 
  group_by(date, district) %>% 
  summarize(across(c(entry_to_dispatch,
                     entry_to_onscene,
                     dispatch_to_onscene,
                     entry_to_close,
                     dispatch_to_onscene_filtered,
                     dispatch_to_onscene_filtered_floor), ~mean(.,na.rm = T)),
            number_dispatches = n(),
            across(c(dispatch_to_onscene_g1,
                     dispatch_to_onscene_g2,
                     entry_to_dispatch_g1,
                     entry_to_dispatch_g2), ~sum(., na.rm = T), .names = "number_{.col}")) %>% ungroup()

aggregated_top_5 <- dispatches_filtered %>% 
  mutate(crime_type = case_when(
     final_dispatch_description == "DOMESTIC DISTURBANCE" &
                                    priority_code == 1 ~ "domestic_disturb_p1",
    final_dispatch_description == "CHECK WELL BEING" &
                              priority_code ==1 ~ "check_well_p1",
   final_dispatch_description == "BATTERY IP" &
                              priority_code == 1 ~ "battery_ip_p1",
    final_dispatch_description == "SUSPICIOUS PERSON" &
                                     priority_code ==1 ~ "suspicious_person_p1",
    final_dispatch_description == "EMS" &
                       priority_code == 1 ~ "ems_p1",
    final_dispatch_description == "ALARM BURGLAR" &
                                 priority_code ==2 ~ "alarm_burglar_p2",
   final_dispatch_description == "ALARM COMMERCIAL" &
                                    priority_code == 2 ~ "alarm_commercial_p2",
    final_dispatch_description == "SUSPICIOUS AUTO WITH OCC" &
                                       priority_code ==2 ~ "suspicious_auto_p2",
    final_dispatch_description == "PERSON DOWN" &
                               priority_code == 2 ~ "person_down_p2",
    final_dispatch_description == "BATTERY JO" &
                              priority_code == 2 ~"battery_jo_p2",
    final_dispatch_description == "DISTURBANCE" &
                               priority_code == 3 ~"disturbance_p3",
    final_dispatch_description == "PARKING VIOL. 1" &
                                     priority_code == 3 ~ "parking_violation1_p3",
    final_dispatch_description == "DISTURBANCE - MUSIC/NOISE" &
                                     priority_code ==3 ~ "disturbance_music_p3",
    final_dispatch_description == "PARKING VIOL. 2" &
                                       priority_code == 3 ~ "parking_violation2_p3",
    final_dispatch_description == "SELLING NARCOTICS" &
                                  priority_code == 3 ~ "selling_narcotics_p3",
   .default = NA
  )) %>% 
  filter(!is.na(crime_type)) %>% 
  mutate(date = as_date(entry_received_date)) %>% 
  group_by(date, district, crime_type) %>% 
  summarize(across(c(entry_to_dispatch,
                     entry_to_onscene,
                     dispatch_to_onscene,
                     entry_to_close,
                     dispatch_to_onscene_filtered,
                     dispatch_to_onscene_filtered_floor), ~mean(.,na.rm = T))) %>% ungroup()

aggregated_nopriority_types <- dispatches_filtered %>% 
  mutate(date = as_date(entry_received_date)) %>% 
  filter(gun_crime_report ==1 | domestic_distrub == 1 | domestic_battery == 1) %>% 
  mutate(crime_type = case_when(
    gun_crime_report == 1 ~ "gun_crime",
    domestic_distrub == 1 ~"domestic_disturb",
    domestic_battery == 1 ~"domestic_battery",
    .default = NA
  )) %>% 
  group_by(date, district, crime_type) %>% 
  summarize(across(c(entry_to_dispatch,
                     entry_to_onscene,
                     dispatch_to_onscene,
                     entry_to_close,
                     dispatch_to_onscene_filtered,
                     dispatch_to_onscene_filtered_floor), ~mean(.,na.rm = T))) %>% ungroup()


aggregated <- aggregated %>% 
  pivot_wider(names_from = priority_code, 
              values_from = c(entry_to_dispatch,
                              entry_to_onscene,
                              dispatch_to_onscene,
                              entry_to_close,
                              number_dispatches,
                              dispatch_to_onscene_filtered,
                              dispatch_to_onscene_filtered_floor,
                              number_dispatch_to_onscene_g1,
                              number_dispatch_to_onscene_g2,
                              number_entry_to_dispatch_g1,
                              number_entry_to_dispatch_g2))

aggregated_nopriority_types <- aggregated_nopriority_types %>% 
  pivot_wider(names_from = crime_type,
              values_from = c(entry_to_dispatch,
                              entry_to_onscene,
                              dispatch_to_onscene,
                              entry_to_close,
                              dispatch_to_onscene_filtered,
                              dispatch_to_onscene_filtered_floor))

aggregated_top_5 <- aggregated_top_5 %>% 
  pivot_wider(names_from = crime_type,
              values_from = c(entry_to_dispatch,
                              entry_to_onscene,
                              dispatch_to_onscene,
                              entry_to_close,
                              dispatch_to_onscene_filtered,
                              dispatch_to_onscene_filtered_floor))

## joining the no priority and priority
aggregated <- aggregated %>% 
  left_join(aggregated_nopriority) %>% 
  left_join(aggregated_nopriority_types) %>% 
  left_join(aggregated_top_5) 

aggregated <- aggregated %>% 
  left_join(rollout_dates) %>% 
  mutate(treatment = if_else(shotspot_activate <= date, 1, 0), .by = district) %>% 
  mutate(never_treated = if_else(is.na(treatment),1, 0), .by = district) %>% 
  mutate(treatment = if_else(is.na(treatment), 0, treatment
  ), .by = district)


## WARNING missing 2021-06-17, 2021-06-18 in the raw data. 
## changing NAs to 0s
# aggregated <- aggregated %>% 
#   mutate(across(starts_with("entry"), ~ifelse(is.na(.), 0, .)),
#          across(starts_with("number"), ~ifelse(is.na(.), 0, .)))

aggregated %>% 
  write_csv("analysis_data/dispatches_all.csv")

aggregated_monthly %>% 
  write_csv("analysis_data/dispatches_all_monthly.csv")
