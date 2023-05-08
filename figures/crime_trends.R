## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-05-04
##

library(tidyverse)

crime_panel <- read_csv("analysis_data/crimes_panel.csv")

crime_panel <- crime_panel %>% 
  mutate(year = year(date),
         month = month(date),
         year_month = mdy(paste0(month, "-1-", year)))

trend_data <- crime_panel %>% 
  group_by(year_month, never_treated) %>% 
  summarize(across(starts_with("number"), ~sum(.,na.rm = T))) %>% 
  ungroup() %>% 
  mutate(never_treated = ifelse(never_treated == 0, 
                                "Treated", "Never Treated"))


trend_data %>% 
  ggplot(aes(year_month, number_gun_involved_arrest, color = never_treated)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  labs(color = "", x = "", y = "Number Gun Involved Arrests") +
  theme(legend.position = "bottom")

trend_data %>% 
  ggplot(aes(year_month, number_gun_involved, color = never_treated)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  labs(color = "", x = "", y = "Number Gun Involved Crime") +
  theme(legend.position = "bottom")

trend_data %>% 
  ggplot(aes(year_month, number_other_arrest, color = never_treated)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  labs(color = "", x = "", y = "Number Other Arrest") +
  theme(legend.position = "bottom")

trend_data %>% 
  ggplot(aes(year_month, number_other_crime, color = never_treated)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  labs(color = "", x = "", y = "Number Other Crime") +
  theme(legend.position = "bottom")
