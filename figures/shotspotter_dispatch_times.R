## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-04-15
##

library(tidyverse)
library(dtplyr)

dispatches <- read_csv("created_data/dispatches.csv")
shotspotter_dispatches <- read_csv("created_data/shotspotter_dispatches.csv")

all_dispatches <- dispatches %>% 
  left_join(shotspotter_dispatches)

all_dispatches <- all_dispatches %>% 
  mutate(shotspotter_dispatch = ifelse(shotspoter_dispatch == 1, 1, 0)) %>% 
  mutate(shotspotter_dispatch = ifelse(is.na(shotspotter_dispatch), 0, shotspotter_dispatch)) %>% 
  select(-shotspoter_dispatch)


## final priority doesn't have any useful information
## same information as the shotspotter column. 1 if shotspotter dispatch.
all_dispatches <- all_dispatches %>% 
  select(-final_priority)

all_dispatches <- all_dispatches %>% 
  mutate(event_number = as.character(event_number))
small <- all_dispatches %>% 
  head(1000)
 
all_dispatches <- all_dispatches %>% 
  mutate(dispatch_to_onscene = onscene - dispatched,
         call_to_dispatch = dispatched - entry_date,
         dispatch_to_enroute = enroute - dispatched, .before = 1) 

distinct_dispatches <- all_dispatches %>% 
  distinct(event_number, dispatch_to_onscene,
           call_to_dispatch, dispatch_to_enroute,
           priority, district, shotspotter_dispatch)


distinct_dispatches <- distinct_dispatches %>% 
  mutate(district = parse_number(district)) %>% 
  mutate(shotspotter_district = ifelse(district %in% c(2:11, 15, 25), 1, 0)) 

distinct_dispatches %>% 
  filter(str_detect(priority, "^1")) %>% 
  group_by(shotspotter_dispatch, shotspotter_district, priority) %>% 
  summarize(across(c(call_to_dispatch, dispatch_to_onscene, dispatch_to_enroute),
                   ~mean(.,na.rm = T), .names = "avg_{.col}")) %>% 
  ggplot(aes(avg_call_to_dispatch, priority, fill = as.factor(shotspotter_dispatch))) +
  geom_col(position = position_dodge()) +
  facet_wrap(~shotspotter_district)
  
