## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-03-24
##

library(tidyverse)

stops <- readxl::read_excel("raw_data/stops.xlsx",
                            sheet = "ISR Data",
                            col_types = "text") 

stops <- stops %>% 
  janitor::clean_names() %>% 
  mutate(datetime = dmy_hm(datetime)) %>% 
  mutate(stop_year = year(datetime),
         stop_month = month(datetime),
         stop_year_month = mdy(paste0(stop_month, "-1-", stop_year))) 

stops <- stops %>% 
  mutate(stop_type = contact_type_description) %>% 
  fastDummies::dummy_cols(select_columns = "stop_type") %>%
  select(-stop_type) %>% 
  janitor::clean_names() 

stops <- stops %>% 
  fastDummies::dummy_cols(select_columns = "person_stopped_sex") %>% 
  janitor::clean_names()

stops <- stops %>% 
  mutate(person_stopped_race = case_when(
    person_stopped_race == "AMER IND/ALASKAN NATIVE" ~ "Other",
    person_stopped_race == "ASIAN/PACIFIC ISLANDER" ~ "Asian",
    person_stopped_race == "BLACK" ~ "Black",
    person_stopped_race == "WHITE" ~ "White",
    person_stopped_race == "WHITE HISPANIC" ~ "Hispanic",
    person_stopped_race == "UNKNOWN" ~"Other"
  )) %>% 
  fastDummies::dummy_cols(select_columns = "person_stopped_race") %>% 
  janitor::clean_names()

stops <- stops %>% 
  mutate(across(ends_with("y_n"), ~ ifelse(. == "Y", 1, 0))) 
  

stops <- stops %>% 
  mutate(firearm_found = ifelse(firearm_found_as_a_result_of_the_protective_pat_down_y_n == 1 | firearm_found_as_a_result_of_the_search_y_n == 1,
                                1, 0)) %>% 
  mutate(firearm_found = ifelse(is.na(firearm_found), 0, firearm_found)) 


stops <- stops %>% 
  mutate(stop_district = as.double(stop_district)) %>%
  mutate(stop_date = as_date(datetime)) 


## these are shotspotter dates that I can see from the data.
## these are slightly different than the dates that I see on the public records request
## anything that is not the first of hte month was changed from the public records request
rollout_dates <- read_csv("created_data/rollout_dates.csv")


stops <- stops %>% 
  left_join(rollout_dates, by = join_by(stop_district == district))

stops %>% 
  write_csv("created_data/stops_cleaned.csv")


# ## Trend Graph
# stops %>% 
#   group_by(stop_year_month) %>% 
#   summarize(n = n()) %>% 
#   ggplot(aes(year_month, n)) +
#   geom_line() +
#   theme_minimal()
