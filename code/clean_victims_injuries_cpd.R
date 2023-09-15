## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-08-14
##

library(tidyverse)

adult_1 <- readxl::read_excel("raw_data/victim_injuries.xlsx", sheet = "Adult Victims 1")

adult_2 <- readxl::read_excel("raw_data/victim_injuries.xlsx", sheet = "Adult Victims 2")

juvenile <- readxl::read_excel("raw_data/victim_injuries.xlsx", sheet = "Juvenile Data") %>% 
  janitor::clean_names()

all_victims <- adult_1 %>% 
  bind_rows(adult_2)

all_victims <- all_victims %>% janitor::clean_names()


# finding amount of juveniles ---------------------------------------------

# 
# victims_adults <- all_victims %>% 
#   mutate(date_time = mdy_hm(date_time),
#          date = as_date(date_time), 
#          year = year(date)) %>%
#   mutate(victim_injury = if_else(victim_injury == "Y", 1, 0)) %>% 
#   filter(victim_injury == 1) %>% 
#   summarize(total_victims = sum(victim_injury), .by = year)
# victims_juvenile <- juvenile %>% 
#   filter(victim_injury == "Y") %>% 
#   summarize(juvenile_total = sum(total, na.rm = T), .by = year)
# 
# victims_adults %>% 
#   bind_cols(victims_juvenile) %>% 
#   janitor::adorn_totals() %>% 
#   mutate(frac_juv = juvenile_total / total_victims)
# 

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


  
