## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-05-19
##

library(tidyverse)

dispatch_monthly <- read_csv("analysis_data/dispatches_all_monthly.csv")
officer_hours <- read_csv("analysis_data/officer_hours.csv")
border_districts <- read_csv("created_data/border_districts_final.csv")
sst_alerts <- read_csv("created_data/shotspotter_cleaned.csv")


# collapsing officer hours and sst alerts ---------------------------------

officer_hours <- officer_hours %>% 
  group_by(year_month, district) %>% 
  summarize(officer_hours = sum(officer_hours, na.rm = T)) %>% 
  ungroup()

sst_alerts <- sst_alerts %>% 
  mutate(year_month = mdy(paste0(month, "-1-", year)), .before = 1) %>% 
  group_by(year_month, district) %>% 
  summarize(number_sst_alerts = n()) %>% 
  ungroup() 



# joining data sets -------------------------------------------------------

dispatch_monthly <- dispatch_monthly %>% 
  left_join(officer_hours) %>% 
  left_join(border_districts, join_by(district == border_district))

dispatch_monthly <- dispatch_monthly %>% 
  left_join(sst_alerts) %>% 
  mutate(across(number_sst_alerts, ~if_else(is.na(.),0, .)))

dispatch_monthly <- dispatch_monthly %>% 
  group_by(district) %>% 
  mutate(shotspot_border_treatment = ifelse(year_month >= border_treatment,1 ,0 )) %>% 
  ungroup() %>% 
  mutate(shotspot_border_treatment = ifelse(is.na(shotspot_border_treatment), 0, shotspot_border_treatment))



# changing NAs to 0s for number dispatches --------------------------------

dispatch_monthly <- dispatch_monthly %>% 
  mutate(across(starts_with("number_dispatches"), ~if_else(is.na(.), 0, .)))


## getting a year_month column
dispatch_monthly <- dispatch_monthly %>% 
  mutate(year = year(year_month))



# creating event study time indicator -------------------------------------

dispatch_monthly <- dispatch_monthly %>% 
  mutate(shotspot_activate_ceiling = ceiling_date(shotspot_activate, unit = "month")) %>% 
  mutate(time_to_treat = time_length(year_month - shotspot_activate_ceiling,
                                     unit = "months") %>% round(), .by = district,
         .before = 1) 

dispatch_monthly <- dispatch_monthly %>% 
  arrange(year_month) %>% 
  mutate(year_month_seq = c(1:84), .by = district, .before = 1 ) %>% 
  mutate(year_month_treat = if_else(time_to_treat == 0, year_month_seq, NA), .by = district,
         .before = 1)  %>% 
  group_by(district) %>% 
  fill(year_month_treat, .direction = "down") %>%
  fill(year_month_treat, .direction = "up") %>% 
  ungroup() 




dispatch_monthly %>% 
  write_csv("analysis_data/xxdispatch_panel_monthly.csv")







