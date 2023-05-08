library(tidyverse)

dispatches <- read_csv("created_data/dispatches_19_23_eventnumber.csv")
dispatches_ols <- read_csv("created_data/dispatches_16_20.csv")

dispatches_ols <- dispatches_ols %>% 
  select(-agency, -location)
dispatches <- dispatches %>% 
  select(-time_to_dispatch, -time_dispatch_to_onscene,-time_entry_to_onscene )

dispatches_all <- dispatches_ols %>% 
  bind_rows(dispatches)

dispatches_all <- dispatches_all %>% 
  distinct(event_number, .keep_all = T)

priority <- read_csv("created_data/priority_event_types.csv")
dispatches_ols %>% 
  count(event_type) %>% 
  left_join(priority) %>% View()
priority <- priority %>% 
  rename(priority_mf = priority)

dispatches_all <- dispatches_all %>% 
  left_join(priority)

## dispatches that don't have event numbers are things like hangups, or transfers to another service
dispatches_all <- dispatches_all %>% 
  mutate(priority = if_else(is.na(priority), priority_mf, priority)) 


dispatches_all <- dispatches_all %>% 
  select(-casenumber, -priority_mf) %>% 
  mutate(district = parse_number(district))



##fitlering anything that didn't get dispatched
dispatches_all <- dispatches_all %>% 
  mutate(first_dispatch_date = if_else(is.na(first_dispatch_date), dispatched, first_dispatch_date)) 


dispatches_all <- dispatches_all %>% 
  mutate(time_to_dispatch = time_length(first_dispatch_date - entry_date ,unit = "seconds"),
         .before = 1)

dispatches_all %>% 
  write_csv("created_data/dispatches_appended_16_23.csv")
