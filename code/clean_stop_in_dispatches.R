## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-08-03
##

library(tidyverse)

stops <- read_csv("created_data/stops_cleaned.csv")

## this one takes a long time to run.
# dispatches_filtered <- read_csv("created_data/dispatches_filtered_cpd.csv")

stops_in_dispatches <- stops %>% 
  drop_na(event_no) %>% 
  mutate(event_no = parse_number(event_no)) %>% 
  semi_join(dispatches_filtered, join_by(event_no == event_number))

stops_in_dispatches <- stops_in_dispatches %>% 
  select(event_no, firearm_found, black_stop = person_stopped_race_black, 
         search_conducted = search_beyond_a_protective_pat_down_conducted_of_the_person_y_n) %>% 
  mutate(isr_stop = 1)


write_csv(stops_in_dispatches, file = "created_data/stops_cleaned_in_dispatches.csv")

