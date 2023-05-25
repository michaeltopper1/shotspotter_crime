## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-05-22
##

library(tidyverse)

dispatches <- read_csv("analysis_data/dispatches_clevel.csv")
rollout_dates <- read_csv("created_data/rollout_dates.csv")
officer_hours <- read_csv("analysis_data/officer_hours.csv")
border_districts <- read_csv("created_data/border_districts_final.csv")


initial_final <- read_csv("created_data/change_dispatch.csv") %>% 
  filter(use == 1)

call_changes <- dispatches %>% 
  filter(initial_dispatch_description %in% initial_final$initial_dispatch_description) %>% 
  filter(final_dispatch_description %in% initial_final$final_dispatch_description)

call_changes <- call_changes %>% 
  filter(initial_dispatch_description != final_dispatch_description)

call_changes <- call_changes %>% 
  filter(call_source_description == "E-911")

# call_changes %>% 
#   count(initial_dispatch_description, final_dispatch_description)


call_changes_aggregated <- call_changes %>% 
  mutate(date = as_date(entry_received_date)) %>% 
  group_by(date, district) %>% 
  summarize(number_call_changes = n()) %>% 
  ungroup() 

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


call_change_panel <- panel_dates %>% 
  left_join(call_changes_aggregated,
            join_by(date == date,
                    district == district)) %>% 
  mutate(call_change_occur = if_else(number_call_changes > 0,
                                     1, 0)) %>% 
  mutate(across(c(number_call_changes,
                  call_change_occur), ~if_else(is.na(.), 0, .))) 


# joining rollout dates ---------------------------------------------------

call_change_panel <- call_change_panel %>% 
  left_join(rollout_dates,
            join_by(district == district)) %>% 
  mutate(treatment = ifelse(shotspot_activate <= date, 1, 0), .by = district) %>% 
  mutate(never_treated = ifelse(is.na(treatment),1, 0), .by = district) %>% 
  mutate(treatment = ifelse(is.na(treatment), 0, treatment
  ), .by = district)


# joining officer hours and border districts ------------------------------

call_change_panel <- call_change_panel %>% 
  left_join(officer_hours, join_by(date == date,
                                   district == district)) %>% 
  left_join(border_districts, join_by(district == border_district))


# create border treatment -------------------------------------------------

call_change_panel <- call_change_panel %>% 
  group_by(district) %>% 
  mutate(shotspot_border_treatment = ifelse(date >= border_treatment,1 ,0 )) %>% 
  ungroup() %>% 
  mutate(shotspot_border_treatment = ifelse(is.na(shotspot_border_treatment), 0, shotspot_border_treatment))


call_change_panel <- call_change_panel %>% 
  left_join(dispatch_panel,
            join_by(district == district,
                    date == date))

call_change_panel %>% 
  feols(call_change_occur ~treatment.x +
          officer_hours +
          number_dispatches_1 +
          number_dispatches_2 + 
          number_dispatches_3 +
          shotspot_border_treatment| district + date)
