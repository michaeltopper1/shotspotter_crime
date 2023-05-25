## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-04-01
##

library(tidyverse)

stops <- read_csv("created_data/stops_cleaned.csv")


## changing the NA in race to White. This is corroborated by the ISR files on the Chicago Police Website
## For some reason, in our FOIA request, they put in NA as white

stops <- stops %>% 
  mutate(person_stopped_race_white = if_else(is.na(person_stopped_race_white), 1, person_stopped_race_white)) 


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
  mutate(black_male_stop = ifelse(person_stopped_race_black ==1 & 
                                    person_stopped_sex_m == 1, 1, 0)) 


## consent and beyond patdown by race
stops <- stops %>% 
  mutate(black_search_beyond_patdown = if_else(person_stopped_race_black==1 &
                                                search_beyond_a_protective_pat_down_conducted_of_the_person_y_n ==1,
                                              1, 0),
         hispanic_search_beyond_patdown = if_else(person_stopped_race_hispanic == 1 &
                                                   search_beyond_a_protective_pat_down_conducted_of_the_person_y_n == 1,
                                                 1, 0),
         nonblack_search_beyond_patdown = if_else(person_stopped_race_black == 0 & 
                                                   search_beyond_a_protective_pat_down_conducted_of_the_person_y_n == 1,
                                                 1, 0),
         white_search_beyond_patdown = if_else(person_stopped_race_white == 1 & 
                                                search_beyond_a_protective_pat_down_conducted_of_the_person_y_n == 1,
                                              1, 0),
         black_hispanic_search = if_else((person_stopped_race_black == 1 | person_stopped_race_hispanic == 1) & 
                                          search_beyond_a_protective_pat_down_conducted_of_the_person_y_n == 1,
                                        1, 0)) %>% 
  mutate(black_search_no_consent = if_else(person_stopped_race_black == 1 &
                                            search_based_on_consent_y_n == 0, 
                                          1, 0),
         nonblack_search_no_consent = if_else(person_stopped_race_black == 0 &
                                               search_based_on_consent_y_n == 0,
                                             1, 0))


stops <- stops %>% 
  mutate(search_noconsent = if_else(search_based_on_consent_y_n == 0, 1, 0)) %>% 
  mutate(patdown_noconsent = if_else(pat_down_consent_y_n == 0, 1, 0))


stops_aggregated <- stops %>%
  group_by(stop_district, stop_date) %>% 
  summarize(number_stops = n(),
            number_black_stops = sum(person_stopped_race_black, na.rm = T),
            number_investigatory_stops = sum(stop_type_investigatory_stop, na.rm = T),
            number_hispanic_stops = sum(person_stopped_race_hispanic, na.rm = T),
            number_asian_stops = sum(person_stopped_race_asian, na.rm = T),
            number_white_stops = sum(person_stopped_race_white, na.rm = T),
            number_males_stopped = sum(person_stopped_sex_m, na.rm = T),
            number_gang_stops = sum(stop_type_gang_and_narcotics_related_loitering,na.rm = T),
            number_firearm_found = sum(firearm_found, na.rm = T),
            number_firearm_found_search = sum(firearm_found_as_a_result_of_the_search_y_n, na.rm = T),
            number_firearm_found_patdown = sum(firearm_found_as_a_result_of_the_protective_pat_down_y_n, na.rm = T),
            number_pat_down = sum(pat_down_y_n, na.rm = T),
            number_black_males_stops = sum(black_male_stop, na.rm = T),
            number_search = sum(search_beyond_a_protective_pat_down_conducted_of_the_person_y_n, na.rm = T),
            number_black_search = sum(black_search_beyond_patdown, na.rm = T),
            number_hispanic_search = sum(hispanic_search_beyond_patdown, na.rm = T),
            number_black_hispanic_search = sum(black_hispanic_search, na.rm = T),
            number_white_search = sum(white_search_beyond_patdown, na.rm = T),
            number_nonblack_search = sum(nonblack_search_beyond_patdown, na.rm = T),
            number_search_consent = sum(search_based_on_consent_y_n),
            number_search_noconsent = sum(search_noconsent, na.rm = T),
            number_nonblack_search_noconsent = sum(nonblack_search_no_consent, na.rm = T),
            number_black_search_noconsent = sum(black_search_no_consent,na.rm = T),
            number_patdown_consent = sum(pat_down_consent_y_n, na.rm = T),
            number_patdown_noconsent = sum(patdown_noconsent, na.rm = T),
            number_stops_probable_cause = sum(factor_that_led_to_stop_proble_cause_y_n, na.rm = T),
            number_stops_proximity = sum(factor_that_led_to_stop_proximity_to_crime_y_n, na.rm = T)) %>% 
  mutate(across(starts_with("number"), ~ifelse(is.na(.), 0, .))) %>% 
  ungroup()

stops_panel <- panel_dates %>% 
  left_join(stops_aggregated, join_by(district == stop_district,
                                     date == stop_date)) %>% 
  mutate(across(starts_with("number"), ~ifelse(is.na(.), 0, .)))




# creating new columns for rates ------------------------------------------

stops_panel <- stops_panel %>% 
  rowwise() %>% 
  mutate(patdown_rate = number_pat_down/number_stops,
         search_rate = number_search/number_stops,
         firearm_found_rate = number_firearm_found/number_stops,
         black_stop_rate = number_black_stops/number_stops,
         hispanic_stop_rate = number_hispanic_stops/number_stops,
         asian_stop_rate = number_asian_stops/number_stops,
         white_stop_rate = number_white_stops/number_stops,
         nonblack_stop_rate = (number_stops -number_black_stops)/number_stops,
         black_search_rate = number_black_search/number_stops,
         nonblack_search_rate = number_nonblack_search/number_stops,
         black_hispanic_search_rate = number_black_hispanic_search/number_stops,
         white_search_rate = number_white_search/number_stops,
         probable_cause_stop_rate = number_stops_probable_cause/number_stops,
         proximity_stop_rate = number_stops_proximity/number_stops,
         firearm_found_patdown_rate = number_firearm_found_patdown/number_pat_down,
         firearm_found_search_rate = number_firearm_found_search/number_search) %>% 
  ungroup()


stops_panel <- stops_panel %>% 
  mutate(across(ends_with("rate"), ~ifelse(is.na(.), 0, .)))



# rollout dates -----------------------------------------------------------


rollout_dates <- read_csv("created_data/rollout_dates.csv")


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

