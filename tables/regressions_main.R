## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-04-01
##

library(tidyverse)
library(fixest)
library(modelsummary)

stops_panel <- read_csv("analysis_data/stops_panel.csv")


stops_panel %>% 
  mutate(treatment = ifelse(district == 16 | district == 17, 0, treatment)) %>% 
  feols(c(number_stops,
          number_black_stops,
          number_firearm_found) ~ treatment | district + date,
        cluster = ~district) %>% 
  modelsummary::modelsummary()