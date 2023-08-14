## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-08-14
##

library(tidyverse)

adult_1 <- readxl::read_excel("raw_data/arrests_injuries.xlsx", sheet = "Adult Victims 1")

adult_2 <- readxl::read_excel("raw_data/arrests_injuries.xlsx", sheet = "Adult Victims 2")

all_victims <- adult_1 %>% 
  bind_rows(adult_2)

all_victims <- all_arrests %>% janitor::clean_names()


# This is a trend graph to make certain all the data is there.
# all_victims %>% 
#   mutate(date_time = mdy_hm(date_time)) %>% 
#   mutate(date = as_date(date_time),
#          year = year(date),
#          month = month(date),
#          day = day(date),
#          year_month = mdy(paste0(month, "-1-", year))) %>% 
#   distinct(rd_no, .keep_all = T) %>% 
#   group_by(year_month) %>% 
#   summarize(n = n()) %>% 
#   ggplot(aes(year_month, n)) +
#   geom_line()

## since I am making all rds distinct, I am only counting if there was an occurrence of 
## an injury from ANYONE involved in the incident.
## This follows the deangelo paper 2023
all_victims <- all_victims %>% 
  distinct(rd_no, victim_injury) %>% 
  pivot_wider(names_from = victim_injury, values_from = victim_injury) %>% 
  mutate(victim_injury = if_else(`Y` == "Y", 1, 0),
         victim_injury = replace_na(victim_injury, 0)) %>% 
  select(victim_injury, rd_no) %>% 
  distinct(rd_no, .keep_all = T) 

all_victims <- all_victims %>% 
  drop_na()

write_csv(all_victims, file = "created_data/victim_injuries.csv")


  
