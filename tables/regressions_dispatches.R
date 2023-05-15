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


dispatch_panel <- read_csv("analysis_data/dispatches_all.csv")
officer_hours <- read_csv("analysis_data/officer_hours.csv")

dispatch_panel <- dispatch_panel %>% 
  left_join(officer_hours, join_by(date == date,
                                   district == district))

dispatch_panel %>% 
  filter(is.na(officer_hours)) %>% 
  distinct(date) %>% View()
dispatch_panel %>% 
  feols(c(entry_to_dispatch_0,
          entry_to_dispatch_1,
          entry_to_dispatch_2,
          entry_to_dispatch_3)~treatment + officer_hours | district + date)

dispatch_panel %>% 
  feols(c(entry_to_onscene_0,
          entry_to_onscene_1,
          entry_to_onscene_2,
          entry_to_onscene_3)~treatment + officer_hours| district + date)

dispatch_panel %>% 
  feols(c(entry_to_close_0,
          entry_to_close_1,
          entry_to_close_2,
          entry_to_close_3)~treatment + officer_hours| district + date)



