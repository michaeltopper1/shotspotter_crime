## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-05-03
##

library(tidyverse)

stops_panel <- read_csv("analysis_data/stops_panel.csv")

graph_data <- stops_panel %>% 
  group_by(year_month, district, shotspot_activate) %>% 
  summarize(across(c(number_stops,
                     number_firearm_found,
                     number_search,
                     number_pat_down), ~sum(.,na.rm = T))) %>% 
  ungroup()

stops_trend <- graph_data %>% 
  ggplot(aes(year_month, number_stops)) +
  geom_line() +
  geom_vline(aes(xintercept = shotspot_activate), linetype = "dashed", color = "dark red") +
  facet_wrap(~district) +
  theme_minimal() +
  labs(x = "", y = "Number of Stops (Month)") +
  theme(axis.text.x = element_text(angle = 90))

firearm_trend <- graph_data %>% 
  ggplot(aes(year_month, number_firearm_found)) +
  geom_line() +
  geom_vline(aes(xintercept = shotspot_activate), linetype = "dashed", color = "dark red") +
  facet_wrap(~district) +
  theme_minimal() +
  labs(x = "", y = "Number of Firearms Found (Month)") +
  theme(axis.text.x = element_text(angle = 90))

search_trend <- graph_data %>% 
  ggplot(aes(year_month, number_search)) +
  geom_line() +
  geom_vline(aes(xintercept = shotspot_activate), linetype = "dashed", color = "dark red") +
  facet_wrap(~district) +
  theme_minimal() +
  labs(x = "", y = "Number of Searches (Month)") +
  theme(axis.text.x = element_text(angle = 90))

patdown_trend <- graph_data %>% 
  ggplot(aes(year_month, number_pat_down)) +
  geom_line() +
  geom_vline(aes(xintercept = shotspot_activate), linetype = "dashed", color = "dark red") +
  facet_wrap(~district) +
  theme_minimal() +
  labs(x = "", y = "Number of Pat Downs (Month)") +
  theme(axis.text.x = element_text(angle = 90))
