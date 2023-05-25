## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-05-15
##

library(tidyverse)

dispatches <- read_csv("created_data/dispatches_innerjoin_16_22.csv")
rollout_dates <- read_csv("created_data/rollout_dates.csv")

dispatches <- dispatches %>% 
  mutate(date = as_date(entry_received_date))


## finding the districts that are on the radio OWM
dispatches <- dispatches %>% 
  mutate(district_owm = if_else(str_detect(district.x,"^O|C"), 1, 0)) 

## if they are over the radio districts, give them NAs
## Otherwise, keep them the same as the CPD
dispatches <- dispatches %>% 
  mutate(district = case_when(
    district_owm == 1 ~ NA,
    .default = district.x
  )) %>% 
  mutate(district = parse_number(district)) 


## changing SSTs to priority 1
dispatches <- dispatches %>% 
  mutate(priority_code = if_else(final_dispatch_code == "SST",
                                "1A", priority_code))


## getting priority codes (from CPD) to doubles
dispatches <- dispatches %>% 
  mutate(priority_code = parse_number(priority_code)) %>% 
  filter(priority_code != 4)

## filtering to only call sources that are from 911 calls
dispatches <- dispatches %>% 
  filter(call_source_description == "E-911") 


# creating outcomes ------------------------------------------------------

dispatches <- dispatches %>% 
  mutate(entry_to_dispatch = time_length(first_dispatch_date - entry_received_date, "seconds"),
         entry_to_onscene = time_length(on_scene_date - entry_received_date, "seconds"),
         dispatch_to_onscene = time_length(on_scene_date - first_dispatch_date, "seconds"),
         entry_to_close = time_length(close_completed_date - entry_received_date, "seconds"))

dispatches <- dispatches %>% 
  mutate(first_dispatch_date_floor = floor_date(first_dispatch_date, "minutes"),
         dispatch_to_onscene_floor = time_length(on_scene_date - first_dispatch_date_floor, "seconds"))


dispatches <- dispatches %>% 
  mutate(dispatch_to_onscene_g1 = if_else(dispatch_to_onscene > 60, 1, 0),
         dispatch_to_onscene_g2 = if_else(dispatch_to_onscene > 120, 1, 0),
         entry_to_dispatch_g1 = if_else(entry_to_dispatch > 60, 1, 0),
         entry_to_dispatch_g2 = if_else(entry_to_dispatch > 120, 1, 0))

dispatches <- dispatches %>% 
  mutate(sst_dispatch = if_else(final_dispatch_code == "SST", 1, 0))

## need to deleted these sst
dispatches <- dispatches %>% 
  filter(sst_dispatch !=1)

## deleting the negative entry_to_dispatches - only 126 of these
dispatches <- dispatches %>% 
  filter(entry_to_dispatch > 0) 

## There are 38892 dispatch to onscenes that are less than or equal to 0.
## I am creating other columns that get rid of this.
dispatches <- dispatches %>% 
  mutate(dispatch_to_onscene_less_than_zero = if_else(dispatch_to_onscene <=0, 1, 0))

dispatches <- dispatches %>% 
  mutate(dispatch_to_onscene_filtered = if_else(dispatch_to_onscene_less_than_zero ==1,
                                                NA, dispatch_to_onscene),
         dispatch_to_onscene_filtered_floor = if_else(dispatch_to_onscene_less_than_zero ==1,
                                                      NA, dispatch_to_onscene_floor))

dispatches <- dispatches %>% 
  select(-c(district.x,
            district.y,
            district_owm,
            area_of_service,
            sector_of_service,
            ))

dispatches <- dispatches %>% 
  left_join(rollout_dates,
            join_by(district == district))

dispatches <- dispatches %>% 
  mutate(treatment = if_else(shotspot_activate <= date, 1, 0), .by = district) %>% 
  mutate(never_treated = if_else(is.na(treatment),1, 0), .by = district) %>% 
  mutate(treatment = if_else(is.na(treatment), 0, treatment
  ), .by = district)

## saving
dispatches %>% 
  write_csv("analysis_data/dispatches_clevel.csv")
