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
  feols(c(number_stops,
          number_black_stops,
          number_firearm_found,
          number_investigatory_stop,
          number_asian_stops,
          number_hispanic_stops) ~ treatment | district + date,
        cluster = ~district) %>% 
  modelsummary::modelsummary(stars = T)
