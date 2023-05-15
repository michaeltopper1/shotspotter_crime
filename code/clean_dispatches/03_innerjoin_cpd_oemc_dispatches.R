## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-05-12
##

library(tidyverse)


# data --------------------------------------------------------------------

dispatches <- read_csv('created_data/dispatches_all.csv')

dispatches_previous <- read_csv("created_data/dispatches_appended_16_23.csv")


# getting rid of unncessary columns ---------------------------------------

## this helps for computational purposes
dispatches <- dispatches %>% 
  select(-starts_with("service"),
         -starts_with("occurrence"),
         -radio_zone)

dispatches_previous <- dispatches_previous %>% 
  select(-c(x_coord, y_coord)) %>% 
  rename(onscene_oemc = onscene,
         priority_oemc = priority,
         enroute_oemc = enroute)

# filters -----------------------------------------------------------------
## only years 2016-2022
dispatches_previous <- dispatches_previous %>% 
  mutate(year = year(entry_date)) %>% 
  filter(year > 2015 & year < 2023)

## getting rid of things that were not dispatched
dispatches_previous <- dispatches_previous %>% 
  filter(!is.na(first_dispatch_date))
dispatches_previous <- dispatches_previous %>% 
  filter(priority_oemc != 4)

## getting rid of things that were not dispatched
dispatches <- dispatches %>% 
  drop_na(dispatch_date)

## getting rid of dispatch priority 4 from the CPD data
## priority 4 means administrative dispatch - initiating an administrative meetup
dispatches <- dispatches %>% 
  filter(priority_code != 4 )

# creating new variables --------------------------------------------------

# dispatches <- dispatches %>% 
#   mutate(dispatch_to_onscene = time_length(on_scene_date - dispatch_date, unit = "seconds"),
#          entry_to_close = time_length(close_completed_date - entry_received_date, unit = "seconds"),
#          entry_to_dispatch = time_length(dispatch_date - entry_received_date, unit = "seconds")) 

# dispatches <- dispatches %>% 
#   mutate(priority_code_r = parse_number(priority_code)) %>% 
#   mutate(dgreater_one= ifelse(entry_to_dispatch > 60, 1, 0),
#          dgreater_two= ifelse(entry_to_dispatch > 60, 1, 0),
#          dgreater_three = ifelse(entry_to_dispatch > 60, 1, 0)) 


dispatches_shared <- dispatches %>% 
  inner_join(dispatches_previous, join_by(event_number == event_number))


## removing leading zeros fromt he district
dispatches_shared <- dispatches_shared %>% 
  mutate(district.x = str_remove(district.x, "^0{1,2}")) 



# appending the SSts that were removed ------------------------------------

sst_not_included <- dispatches %>% 
  filter(final_dispatch_code == "SST") %>% 
  filter(!event_number %in% dispatches_shared$event_number) %>% 
  mutate(not_in_innerjoin = 1)


dispatches_shared <- dispatches_shared %>% 
  bind_rows(sst_not_included)


## note that district from CPD and OEMC do not always match.
## I feel that that CPD data is more complete and we should use the CPD district

dispatches_shared %>% 
  write_csv("created_data/dispatches_innerjoin_16_22.csv")
