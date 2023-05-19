## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-05-14
##

library(tidyverse)
library(fixest)
library(panelsummary)
library(kableExtra)
library(did2s)

victim_panel <- read_csv("analysis_data/victims_panel.csv")
officer_hours <- read_csv("analysis_data/officer_hours.csv")
crimes <- read_csv("analysis_data/crimes_panel.csv")

victim_panel <- victim_panel %>% 
  left_join(officer_hours, join_by(date == date,
                                   district == district)) %>% 
  left_join(crimes, join_by(date == date,
                            district == district))


victim_panel <- victim_panel %>% 
  group_by(district, date) %>% 
  mutate(gun_victim_occur = ifelse(number_gun_victims > 0, 1, 0),
         gun_injury_occur = ifelse(number_gun_injury_victims > 0, 1, 0)) %>% 
  ungroup()
  


victim_panel %>% 
  write_csv("analysis_data/xxvictim_panel.csv")

