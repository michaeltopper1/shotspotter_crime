## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-04-08
##

library(tidyverse)
library(fixest)
library(modelsummary)

crimes <- read_csv("analysis_data/crimes_panel.csv")
stops <- read_csv("analysis_data/stops_panel.csv")

## investigatory black stops - something to think about
stops_ddd <- stops %>% 
  rowwise() %>% 
  mutate(number_non_black_stops = number_stops -number_black_stops, .before = 1) %>% 
  select(district, date, number_black_stops, number_non_black_stops,
         shotspot_activate)

stops_ddd <- stops_ddd %>% 
  pivot_longer(c(number_black_stops, number_non_black_stops),
               values_to = "number_stops",
               names_to  = "type_stop") %>% 
  mutate(type_stop = ifelse(type_stop == "number_black_stops", "black", "other")) %>% 
  mutate(treated = ifelse(is.na(shotspot_activate), 0, 1)) %>% 
  mutate(treated = ifelse(district == 16 | district == 17, 0, treated)) %>% 
  mutate(post = ifelse(date >= shotspot_activate, 1, 0), .by = district) %>% 
  mutate(post = ifelse(is.na(post), 0, post))

stops_ddd %>% 
  count(treated, district, post) %>% View()

stops_ddd %>% 
  count(post, district) %>% View()
  
stops_ddd %>% 
  mutate(treated = ifelse(district == 16 | district == 17, 0, treated)) %>% 
  mutate(black_stop = ifelse(type_stop == "black",1 ,0)) %>% 
  feols(number_stops ~ treated + post + black_stop +
          treated*post + treated*black_stop + post*black_stop +
          treated*post*black_stop | district^post + district^treated +
          district^black_stop +
        + date^treated + date^post + date^black_stop,
        cluster = ~district, data = .) %>% summary()

stops_ddd_twfe <- stops_ddd %>% 
  mutate(treatment = ifelse(date >= shotspot_activate, 1, 0)) %>% 
  mutate(treatment = ifelse(is.na(treatment), 0, treatment)) %>% 
  mutate(treatment = ifelse(district == 16 | district == 17, 0, treatment)) 







stops_ddd_twfe %>% 
  mutate(black_stop = ifelse(type_stop == "black",1 ,0)) %>%
  feols(number_stops ~treatment*black_stop | district^black_stop + date^black_stop, 
        cluster = ~district)

