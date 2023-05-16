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
  mutate(district_owm = ifelse(str_detect(district.x,"^O|C"), 1, 0)) 

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
  mutate(priority_code = ifelse(final_dispatch_code == "SST",
                                "1A", priority_code))


## getting priority codes (from CPD) to doubles
dispatches <- dispatches %>% 
  mutate(priority_code = parse_number(priority_code)) %>% 
  filter(priority_code != 4)

dispatches %>% 
  colnames()

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
  mutate(treatment = ifelse(shotspot_activate <= date, 1, 0), .by = district) %>% 
  mutate(never_treated = ifelse(is.na(treatment),1, 0), .by = district) %>% 
  mutate(treatment = ifelse(is.na(treatment), 0, treatment
  ), .by = district)

## saving
dispatches %>% 
  write_csv("analysis_data/dispatches_clevel.csv")
