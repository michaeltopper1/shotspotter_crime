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
  mutate(district_owm = ifelse(str_detect(district.x,"^O|C"), 1, 0)) 


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
  mutate(priority_code = ifelse(final_dispatch_code == "SST",
                                "1A", priority_code))
  
# going to be using the CPD data as the final priority
# most of them are the same
# changing the missing ones to 
dispatches_filtered <- dispatches_filtered %>% 
  mutate(priority_code = ifelse(is.na(priority_code), priority_oemc, priority_code)) %>% 
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

dispatches_filtered <- dispatches_filtered %>% 
  mutate(dispatch_to_onscene_g1 = ifelse(dispatch_to_onscene > 60, 1, 0),
         dispatch_to_onscene_g2 = ifelse(dispatch_to_onscene > 120, 1, 0),
         entry_to_dispatch_g1 = ifelse(entry_to_dispatch > 60, 1, 0),
         entry_to_dispatch_g2 = ifelse(entry_to_dispatch > 120, 1, 0))

dispatches_filtered <- dispatches_filtered %>% 
  mutate(sst_dispatch = if_else(final_dispatch_code == "SST", 1, 0))

## need to deleted these sst
dispatches_filtered <- dispatches_filtered %>% 
  filter(sst_dispatch !=1)



aggregated <- dispatches_filtered %>% 
  mutate(date = as_date(entry_received_date)) %>% 
  group_by(date, district, priority_code) %>% 
  summarize(across(c(entry_to_dispatch,
                     entry_to_onscene,
                     dispatch_to_onscene,
                     entry_to_close), ~mean(.,na.rm = T)),
            number_dispatches = n(),
            across(c(dispatch_to_onscene_g1,
                     dispatch_to_onscene_g2,
                     entry_to_dispatch_g1,
                     entry_to_dispatch_g2), ~sum(., na.rm = T), .names = "number_{.col}")) %>% ungroup()


aggregated <- aggregated %>% 
  pivot_wider(names_from = priority_code, 
              values_from = c(entry_to_dispatch,
                              entry_to_onscene,
                              dispatch_to_onscene,
                              entry_to_close,
                              number_dispatches,
                              number_dispatch_to_onscene_g1,
                              number_dispatch_to_onscene_g2,
                              number_entry_to_dispatch_g1,
                              number_entry_to_dispatch_g2))



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

