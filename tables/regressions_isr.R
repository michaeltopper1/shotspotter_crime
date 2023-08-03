## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-06-16
##

library(tidyverse)
library(fixest)
library(modelsummary)
library(panelsummary)
library(kableExtra)
library(did2s)

if (!exists("dispatch_panel")){
  dispatch_panel <- read_csv(here::here("analysis_data/xxdispatch_panel.csv"))
}

dispatch_panel <- dispatch_panel %>% 
  mutate(isr_stop_rate_gun_crime =number_isr_stops_gun_crime/number_guncrime)


setFixest_fml(..ctrl = ~officer_hours +
                number_dispatches_1 + number_dispatches_2 + 
                number_dispatches_3 + number_dispatches_0| district + date)

dispatch_panel %>% 
  feols(isr_stop_rate_gun_crime ~ treatment  + ..ctrl)

dispatch_panel %>% 
  feols(isr_firearm_found_gun_crime_rate ~ treatment + ..ctrl)
