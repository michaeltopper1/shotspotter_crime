## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-05-08
##

library(tidyverse)


rollout_dates <- read_csv("created_data/rollout_dates.csv")
crimes_panel <- read_csv("analysis_data/crimes_panel.csv")
dispatches_all <- read_csv("created_data/dispatches_appended_16_23.csv")

dispatches_all <- dispatches_all %>% 
  mutate(date = as_date(entry_date)) %>% 
  mutate(priority_1 = ifelse(str_detect(priority, "^1"), 1, 0),
         priority_2 = ifelse(str_detect(priority, "^2"), 1, 0),
         priority_3 = ifelse(str_detect(priority, "^3"), 1, 0),
         priority_4 = ifelse(str_detect(priority, "^4"), 1, 0),
         priority_5 = ifelse(str_detect(priority, "^5"), 1, 0),
         priority_0 = ifelse(str_detect(priority, "^0"), 1, 0),
         shotspotter_dispatch = ifelse(event_type == "SST", 1, 0),
         shotsf_dispatch = ifelse(event_type == "SHOTSF", 1, 0)) 

dispatches_all <- dispatches_all %>% 
  mutate(priority = str_extract(priority,"^.")) 


aggregated <- dispatches_all %>% 
  group_by(date, district, priority) %>% 
  summarize(mean_time = mean(time_to_dispatch, na.rm = T),
            number_shotsf = sum(shotsf_dispatch,na.rm = T),
            number_sst = sum(shotspotter_dispatch, na.rm = T)) %>% ungroup()


aggregated <- aggregated %>% 
  pivot_wider(names_from = priority, 
              values_from = c(mean_time, number_shotsf, number_sst))

aggregated <- aggregated %>% 
  filter(district %in% c(1:20, 22, 24, 25)) %>% 
  filter(district != 13)


aggregated <- aggregated %>% 
  left_join(rollout_dates) %>% 
  mutate(treatment = ifelse(shotspot_activate <= date, 1, 0), .by = district) %>% 
  mutate(never_treated = ifelse(is.na(treatment),1, 0), .by = district) %>% 
  mutate(treatment = ifelse(is.na(treatment), 0, treatment
  ), .by = district)



aggregated_crime_join <- aggregated %>% 
  left_join(crimes_panel, join_by(date == date,
                                  district == district)) %>% 
  filter(!is.na(number_other_crime)) 
