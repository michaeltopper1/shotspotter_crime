## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-05-14
##

library(tidyverse)

dispatches <- read_csv("created_data/dispatches_all.csv") %>% 
  janitor::clean_names()

rollout_dates <- read_csv("created_data/rollout_dates.csv")
rollout_dates <- rollout_dates %>% mutate(across(starts_with("shotspot"), ~mdy(.)))
crimes_panel <- read_csv("analysis_data/crimes_panel.csv")


# filtering ---------------------------------------------------------------

## getting only 911 calls
dispatches_filtered <- dispatches %>% 
  filter(call_source_description == "E-911")

## filtering to only districts we have
dispatches_filtered <- dispatches_filtered %>% 
  mutate(district = parse_number(district)) %>% 
  filter(district %in% crimes_panel$district)


## parsing the priority codes
dispatches_filtered <- dispatches_filtered %>% 
  mutate(priority_code = parse_number(priority_code)) 


## getting rid fo the one priority 4 one
dispatches_filtered <- dispatches_filtered %>% 
  filter(priority_code != 4) 

## Note that 52% of the data is missing on-scene


# break point - can load data in from here --------------------------------
## The break point was created in case you want to go in and create more variables
## without having to load the 5 gigabyte data anymore

# dispatches_filtered %>% 
#   write_csv("created_data/dispatches_filtered_cpd.csv")
# 
dispatches_filtered <- read_csv("created_data/dispatches_filtered_cpd.csv")

rollout_dates <- read_csv("created_data/rollout_dates.csv")
rollout_dates <- rollout_dates %>% mutate(across(starts_with("shotspot"), ~mdy(.)))



# creating variables ------------------------------------------------------

dispatches_filtered <- dispatches_filtered %>% 
  mutate(entry_to_dispatch = time_length(dispatch_date - entry_received_date, "seconds"),
         entry_to_onscene = time_length(on_scene_date - entry_received_date, "seconds"),
         dispatch_to_onscene = time_length(on_scene_date - dispatch_date, "seconds"),
         entry_to_close = time_length(close_completed_date - entry_received_date, "seconds"))

## there were some final codes missing. replaced with the initial codes
dispatches_filtered <- dispatches_filtered %>% 
  mutate(final_dispatch_code = if_else(is.na(final_dispatch_code), initial_dispatch_code,
                                       final_dispatch_code),
         final_dispatch_description = if_else(is.na(final_dispatch_description),
                                              initial_dispatch_description,
                                              final_dispatch_description))

## getting rid of any SST alert dispatches
dispatches_filtered <- dispatches_filtered %>% 
  filter(final_dispatch_code != "SST")

## getting rid of priority 0
# dispatches_filtered <- dispatches_filtered %>% 
#   filter(priority_code != 0)

## deleting the negative entry_to_dispatches - only 130 of these
dispatches_filtered <- dispatches_filtered %>% 
  filter(entry_to_dispatch > 0) 

## There are 3256 dispatch to onscenes that are less than or equal to 0.
## As of 6-1-23, I am deleting these. THey represent such a small portion of the data.
## I am creating other columns that get rid of this.
dispatches_filtered <- dispatches_filtered %>% 
  mutate(dispatch_to_onscene_less_than_zero = if_else(dispatch_to_onscene <=0, 1, 0))


dispatches_filtered <- dispatches_filtered %>% 
  filter(dispatch_to_onscene_less_than_zero ==0 | is.na(dispatch_to_onscene_less_than_zero))


# outliers ----------------------------------------------------------------

outliers <- dispatches_filtered %>% 
  summarize(across(c(entry_to_dispatch, entry_to_onscene),
                   list(mean = ~mean(., na.rm = T), sd = ~sd(., na.rm = T))),
            .by = priority_code) %>% 
  mutate(entry_to_dispatch_outlier = entry_to_dispatch_mean + 3 * entry_to_dispatch_sd,
         entry_to_onscene_outlier = entry_to_onscene_mean + 3 *entry_to_onscene_sd,
         .by = priority_code) %>% 
  select(priority_code,ends_with("outlier"))

dispatches_filtered <- dispatches_filtered %>% 
  left_join(outliers)

dispatches_filtered <- dispatches_filtered %>% 
  group_by(priority_code) %>% 
  mutate(entry_to_dispatch_outlier = if_else(entry_to_dispatch > entry_to_dispatch_outlier, 1, 0),
         entry_to_onscene_outlier = if_else(entry_to_onscene > entry_to_onscene_outlier, 1, 0)) %>% 
  ungroup()


## this will filter out any of the outliers
## you can remove this if you do not want the outliers
dispatches_filtered <- dispatches_filtered %>% 
  mutate(entry_to_onscene_outlier = if_else(is.na(entry_to_onscene_outlier),
                                            0, entry_to_onscene_outlier))


dispatches_filtered <- dispatches_filtered %>% 
  filter(entry_to_onscene_outlier != 1) %>% 
  filter(entry_to_dispatch_outlier != 1)


# creating indicators for certain crimes we care about --------------------

dispatches_filtered <- dispatches_filtered %>% 
  mutate(shots_fired =if_else(final_dispatch_description == "SHOTS FIRED", 1, 0),
         domestic_distrub = if_else(final_dispatch_description == "DOMESTIC DISTURBANCE", 1, 0),
         person_wgun = if_else(final_dispatch_description == "PERSON WITH A GUN", 1, 0),
         person_shot = if_else(final_dispatch_description == "PERSON SHOT", 1, 0),
         domestic_battery = if_else(final_dispatch_description == "DOMESTIC BATTERY", 1, 0)) %>% 
  mutate(gun_crime_report = if_else(shots_fired == 1 | person_wgun == 1 | person_shot == 1, 1, 0))


dispatches_filtered <- dispatches_filtered %>% 
  mutate(arrest_made = if_else(!is.na(rd), 1, 0)) 


## creating intervals by first watch, second watch, third watch.

dispatches_filtered <- dispatches_filtered %>% 
  mutate(time = hms::as_hms(entry_received_date), .before = 1) %>% 
  mutate(first_watch = if_else(between(time, hms::as_hms("00:00:00"), hms::as_hms("07:59:59")), 1, 0),
         second_watch = if_else(between(time, hms::as_hms("08:00:00"), hms::as_hms("15:59:59")), 1, 0),
         third_watch = if_else(between(time, hms::as_hms("16:00:00"), hms::as_hms("23:59:59")), 1, 0),
         .before = 1)

dispatches_filtered <- dispatches_filtered %>% 
  mutate(watch = case_when(
    first_watch == 1 ~1,
    second_watch == 1 ~2, 
    third_watch == 1 ~3,
    .default = NA
  )) 


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
                     entry_to_close), ~mean(.,na.rm = T)),
            number_dispatches = n(),
            number_shotsfired = sum(shots_fired, na.rm = T)) %>% ungroup()


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
                     entry_to_close), ~mean(.,na.rm = T)),
            number_dispatches = n()) %>% ungroup()

## pivoting priority to fit dimensions
aggregated_monthly <- aggregated_monthly %>% 
  pivot_wider(names_from = priority_code, 
              values_from = c(entry_to_dispatch,
                              entry_to_onscene,
                              dispatch_to_onscene,
                              entry_to_close,
                              number_dispatches,
                              number_shotsfired))

## joining the-non priority and priority datas
aggregated_monthly <- aggregated_monthly %>% 
  left_join(aggregated_monthly_nopriority) 


## going to be defining treatment by first full month of treatment.
aggregated_monthly <- aggregated_monthly %>% 
  left_join(rollout_dates, join_by(district == district)) %>% 
  mutate(treatment = if_else(shotspot_activate <= year_month, 1, 0),
         treatment_official = if_else(shotspot_activate_official <= year_month, 1, 0),
         treatment_first_shot = if_else(shotspot_activate_first_shot <= year_month, 1, 0),
         .by = district) %>% 
  mutate(never_treated = if_else(is.na(treatment),1, 0), .by = district) %>% 
  mutate(treatment = if_else(is.na(treatment), 0, treatment
  ),
  treatment_official = if_else(is.na(treatment_official), 0, treatment_official),
  treatment_first_shot = if_else(is.na(treatment_first_shot), 0, treatment_first_shot),
  .by = district)



# aggregating at daily level ----------------------------------------------

dispatches_filtered <- dispatches_filtered %>% 
  mutate(date = as_date(entry_received_date)) 

aggregated <- dispatches_filtered %>% 
  group_by(date, district, priority_code) %>% 
  summarize(across(c(entry_to_dispatch,
                     entry_to_onscene,
                     dispatch_to_onscene,
                     entry_to_close), ~mean(.,na.rm = T)),
            number_dispatches = n(),
            number_shotsfired = sum(shots_fired, na.rm = T),
            number_guncrime = sum(gun_crime_report, na.rm = T),
            arrests_made = sum(arrest_made,na.rm = T)) %>% ungroup()


aggregated_nopriority <- dispatches_filtered %>% 
  filter(priority_code !=0) %>% 
  group_by(date, district) %>% 
  summarize(across(c(entry_to_dispatch,
                     entry_to_onscene,
                     dispatch_to_onscene,
                     entry_to_close), ~mean(.,na.rm = T)),
            number_dispatches = n(),
            arrests_made = sum(arrest_made,na.rm = T)) %>% ungroup()

aggregated_watch <- dispatches_filtered %>% 
  group_by(date, district, watch) %>% 
  summarize(across(c(entry_to_dispatch,
                     entry_to_onscene,
                     dispatch_to_onscene,
                     entry_to_close), ~mean(.,na.rm = T)),
            number_dispatches = n()) %>% ungroup()



# breakdown of arrest rates -----------------------------------------------
aggregate_arrests_top <- dispatches_filtered %>% 
  mutate(domestic_disturb_p1_arrest = if_else(priority_code == 1 &
                                                arrest_made == 1 &
                                                final_dispatch_description == "DOMESTIC DISTURBANCE",
                                              1, 0),
         domestic_disturb_p1_num = if_else(priority_code == 1 &
                                             final_dispatch_description == "DOMESTIC DISTURBANCE",
                                           1, 0),
         domestic_battery_p1_arrest = if_else(priority_code == 1 &
                                                arrest_made == 1 &
                                                final_dispatch_description == "DOMESTIC BATTERY",
                                              1, 0),
         domestic_battery_p1_num = if_else(priority_code == 1 &
                                             final_dispatch_description == "DOMESTIC BATTERY",
                                           1, 0),
         battery_ip_p1_arrest = if_else(priority_code == 1 &
                                          arrest_made == 1 &
                                          final_dispatch_description == "BATTERY IP",
                                        1, 0),
         battery_ip_p1_num = if_else(priority_code == 1 &
                                       final_dispatch_description == "BATTERY IP",
                                     1, 0),
         ems_p1_arrest = if_else(priority_code == 1 &
                                   arrest_made == 1 &
                                   final_dispatch_description == "EMS",
                                 1, 0),
         ems_p1_num = if_else(priority_code == 1 &
                                final_dispatch_description == "EMS",
                              1, 0),
         robbery_jo_p1_arrest = if_else(priority_code == 1 &
                                          arrest_made == 1 &
                                          final_dispatch_description == "ROBBERY JO",
                                        1, 0),
         robbery_jo_p1_num = if_else(priority_code == 1 &
                                       final_dispatch_description == "ROBBERY JO",
                                     1, 0),
         assault_ip_p1_arrest = if_else(priority_code == 1 &
                                          arrest_made == 1 &
                                          final_dispatch_description == "ASSAULT IP",
                                        1, 0),
         assault_ip_p1_num = if_else(priority_code == 1 &
                                       final_dispatch_description == "ASSAULT IP",
                                     1, 0),
         shots_fired_p1_arrest = if_else(priority_code == 1 &
                                           arrest_made == 1 &
                                           final_dispatch_description == "SHOTS FIRED",
                                         1, 0),
         shots_fired_p1_num = if_else(priority_code == 1 &
                                        final_dispatch_description == "SHOTS FIRED",
                                      1, 0),
         person_gun_p1_arrest = if_else(priority_code == 1 &
                                          arrest_made == 1 &
                                          final_dispatch_description == "PERSON WITH A GUN",
                                        1, 0),
         person_gun_p1_num = if_else(priority_code == 1 &
                                       final_dispatch_description == "PERSON WITH A GUN",
                                     1, 0),
         fire_p1_arrest = if_else(priority_code == 1 &
                                    arrest_made == 1 &
                                    final_dispatch_description == "FIRE",
                                  1, 0),
         fire_p1_num = if_else(priority_code == 1 &
                                 final_dispatch_description == "FIRE",
                               1, 0),
         check_well_p1_arrest = if_else(priority_code == 1 &
                                          arrest_made == 1 &
                                          final_dispatch_description == "CHECK WELL BEING",
                                        1, 0),
         check_well_p1_num = if_else(priority_code == 1 &
                                       final_dispatch_description == "SHOTS FIRED",
                                     1, 0)) %>% 
  filter(entry_to_dispatch !=1 ) %>% 
  filter(entry_to_onscene_outlier != 1) %>% 
  group_by(date, district) %>% 
  summarize(across(ends_with("arrest"), ~sum(.,na.rm = T)),
            across(ends_with("num"), ~sum(.,na.rm = T))) %>% ungroup() 





# breakdown of top 5 crimes -----------------------------------------------

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
    final_dispatch_description == "AUTO ACCIDENT PI" &
                              priority_code == 2 ~"auto_accident_pi_p2",
    final_dispatch_description == "DISTURBANCE" &
                               priority_code == 3 ~"disturbance_p3",
    final_dispatch_description == "PARKING VIOL. 1" &
                                     priority_code == 3 ~ "parking_violation1_p3",
    final_dispatch_description == "DISTURBANCE - MUSIC/NOISE" &
                                     priority_code ==3 ~ "disturbance_music_p3",
    final_dispatch_description == "PARKING VIOL. 2" &
                                       priority_code == 3 ~ "parking_violation2_p3",
    final_dispatch_description == "AUTO ACCIDENT PD" &
                                  priority_code == 3 ~ "auto_accident_pd_p3",
   .default = NA
  )) %>% 
  filter(!is.na(crime_type)) %>% 
  group_by(date, district, crime_type) %>% 
  summarize(across(c(entry_to_dispatch,
                     entry_to_onscene,
                     dispatch_to_onscene,
                     entry_to_close), ~mean(.,na.rm = T))) %>% ungroup()

aggregated_nopriority_types <- dispatches_filtered %>% 
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
                     entry_to_close), ~mean(.,na.rm = T))) %>% ungroup()


aggregated <- aggregated %>% 
  pivot_wider(names_from = priority_code, 
              values_from = c(entry_to_dispatch,
                              entry_to_onscene,
                              dispatch_to_onscene,
                              entry_to_close,
                              number_dispatches,
                              arrests_made,
                              number_shotsfired,
                              number_guncrime))

aggregated_nopriority_types <- aggregated_nopriority_types %>% 
  pivot_wider(names_from = crime_type,
              values_from = c(entry_to_dispatch,
                              entry_to_onscene,
                              dispatch_to_onscene,
                              entry_to_close))

aggregated_watch <- aggregated_watch %>% 
  mutate(watch = glue::glue("watch{watch}")) %>% 
  pivot_wider(names_from = watch, 
              values_from = c(entry_to_dispatch,
                              entry_to_onscene,
                              dispatch_to_onscene,
                              entry_to_close,
                              number_dispatches)) 

aggregated_top_5 <- aggregated_top_5 %>% 
  pivot_wider(names_from = crime_type,
              values_from = c(entry_to_dispatch,
                              entry_to_onscene,
                              dispatch_to_onscene,
                              entry_to_close))

## joining the no priority and priority
aggregated <- aggregated %>% 
  left_join(aggregated_nopriority) %>% 
  left_join(aggregated_nopriority_types) %>% 
  left_join(aggregated_watch) %>% 
  left_join(aggregated_top_5) %>% 
  left_join(aggregate_arrests_top)

aggregated <- aggregated %>% 
  left_join(rollout_dates) %>% 
  mutate(treatment = if_else(shotspot_activate <= date, 1, 0),
         treatment_official = if_else(shotspot_activate_official <= date, 1, 0),
         treatment_first_shot = if_else(shotspot_activate_first_shot <= date, 1, 0),
         .by = district) %>% 
  mutate(never_treated = if_else(is.na(treatment),1, 0), .by = district) %>% 
  mutate(treatment = if_else(is.na(treatment), 0, treatment
  ),
  treatment_official = if_else(is.na(treatment_official), 0, treatment_official),
  treatment_first_shot = if_else(is.na(treatment_first_shot), 0, treatment_first_shot),
  .by = district)


aggregated %>% 
  write_csv("analysis_data/dispatches_all.csv")

aggregated_monthly %>% 
  write_csv("analysis_data/dispatches_all_monthly.csv")
