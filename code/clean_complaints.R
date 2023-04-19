## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-04-15
##

library(tidyverse)

complaints <- read_csv("raw_data/complaints.csv") %>% janitor::clean_names()

complaints <- complaints %>% 
  mutate(datetime = mdy_hms(complaint_date),
         date = as_date(datetime),
         year = year(date),
         month = month(date),
         year_month = mdy(paste0(month, "-1-", year))) 


complaints %>% 
  count(year)
complaints <- complaints %>% 
  mutate(race_of_complainants = race_of_complainants %>% 
           str_to_lower() %>% str_trim()) %>% 
  mutate(number_black_complainants = str_count(race_of_complainants,"black or african american"),
         number_white_complainants = str_count(race_of_complainants, "white"),
         number_hispanic_complainants = str_count(race_of_complainants, "hispanic"),
         number_asian_complaintants = str_count(race_of_complainants, "asian"),
         number_other_complainants = str_count(race_of_complainants, "other|american|middle|hawaiian")) %>% 
  mutate(race_na_complainants = ifelse(is.na(race_of_complainants) |
                                                str_detect(race_of_complainants, "unknown|prefer not to say"),
                                              1, 0)) 


complaints <- complaints %>% 
  mutate(race_of_involved_officers = race_of_involved_officers %>% 
           str_to_lower() %>% str_trim()) %>% 
  mutate(number_black_officer_complaints = str_count(race_of_involved_officers,"black or african american"),
         number_white_officer_complaints = str_count(race_of_involved_officers, "white"),
         number_hispanic_officer_complaints = str_count(race_of_involved_officers, "hispanic"),
         number_asian_officer_complaints = str_count(race_of_involved_officers, "asian"),
         number_other_officer_complaints = str_count(race_of_involved_officers, "other|american|middle|hawaiian")) 



complaints <- complaints %>% 
  separate_wider_delim(cols = "beat",
                        delim = "|",
                       names = c("beat_1", "beat_2", "beat_3",
                                 "beat_4", "beat_5"),
                       too_few = "align_start") %>% 
  mutate(across(starts_with("beat"), ~paste0("0", .))) %>% 
  mutate(across(starts_with("beat"), ~str_sub(., start = -4))) %>% 
  mutate(across(starts_with("beat"), ~str_sub(., start = 1, end = 2), .names = "district_{.col}")) %>% 
  mutate(across(starts_with('district'), ~parse_number(.))) %>% 
  mutate(across(starts_with("district"), ~ifelse(. == 0, NA, .))) 

complaints %>% 
  filter(if_all(starts_with("district"), ~is.na(.)))
  count(beat_4, sort = T)
  mutate(district = ifelse(!is.na(district_beat_1), district_beat_1, district_beat_2)) %>% 
  count(district) %>% View()
## looks like some people don't put in the race of the officer or anything
## one complaint may have many officers in it.
## one complaint may have many complainants in it as well.
complaints_aggregated <- complaints %>% 
  group_by(date) %>% 
  summarize(across(starts_with("number"), ~sum(., na.rm = T)),
            number_complaints_filed = n()) %>% 
  ungroup() %>% 
  mutate(year = year(date),
         month = month(date),
         year_month = mdy(paste0(month, "-1-", year)))

complaints_aggregated <- complaints_aggregated %>% 
  filter(year > 2015)


complaints_aggregated %>% 
  write_csv("created_data/complaints_cleaned")
