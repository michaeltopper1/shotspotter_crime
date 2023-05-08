## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-05-04
##

library(tidyverse)

stops_panel <- read_csv("analysis_data/stops_panel.csv")


trend_data <- stops_panel %>% 
  group_by(year_month, never_treated) %>% 
  summarize(across(starts_with("number"), ~sum(.,na.rm = T))) %>% 
  ungroup() %>% 
  mutate(never_treated = ifelse(never_treated == 0, 
                                "Treated", "Never Treated"))


trend_data %>% 
  ggplot(aes(year_month, number_search, color = never_treated)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  labs(color = "", x = "", y = "Number Searches") +
  theme(legend.position = "bottom")

trend_data %>% 
  ggplot(aes(year_month, number_stops, color = never_treated)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  labs(color = "", x = "", y = "Number Stop") +
  theme(legend.position = "bottom")

trend_data %>% 
  ggplot(aes(year_month, number_firearm_found, color = never_treated)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  labs(color = "", x = "", y = "Number Firearms Found") +
  theme(legend.position = "bottom")
