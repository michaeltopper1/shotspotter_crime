## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-03-30
##

library(tidyverse)
library(fixest)
shotspotter <- read_csv("analysis_data/shotspotter_cleaned.csv")
# chicago <- st_read("raw_data/Boundaries - Police Districts (current)/geo_export_2af99ea9-183c-440b-82a5-67f4a40e66d4.shp")

stops <- read_csv("created_data/stops_cleaned.csv")

stops_year_month <- stops %>% 
  mutate(stop_district = as.double(stop_district)) %>% 
  group_by(stop_district,stop_year_month,person_stopped_race) %>% 
  summarize(number_stops = n()) %>% 
  ungroup()

shotspotter_year_month <- shotspotter %>% 
  group_by(district, shotspot_year_month) %>% 
  summarize(number_shots = n())
shotspotter_districts <- c(2, 3, 4, 5, 6, 7, 8, 9,
                           10, 11, 15, 16, 17, 25)
rollout_dates <- tribble(~district, ~shotspot_activate,
        2, mdy("5-1-2018"),
        3, mdy("1-1-2018"),
        4, mdy("2-1-2018"),
        5, mdy("3-1-2018"),
        6, mdy("3-1-2017"),
        7, mdy("1-13-2017"),
        8, mdy("4-1-2018"),
        9, mdy("3-1-2017"),
        10, mdy("3-1-2017"),
        11, mdy("3-1-2017"),
        15, mdy("3-1-2017"),
        16, mdy("9-1-2020"),
        17, mdy("9-1-2020"),
        25, mdy("4-1-2018"))

stops_shots_year_month <- stops_year_month %>% 
  left_join(shotspotter_year_month,
            by = join_by(stop_year_month == shotspot_year_month,
                         stop_district == district)) %>% 
  left_join(rollout_dates, join_by(
                                   stop_district == district))


stops_shots_year_month <- stops_shots_year_month %>% 
  mutate(treatment = ifelse(stop_year_month >= shotspot_activate, 1, 0)) %>% 
  mutate(treatment = ifelse(is.na(treatment), 0, treatment)) 

stops %>% 
  mutate(stop_date = as_date(datetime)) %>% 
  group_by(stop_date, stop_district, person_stopped_race) %>% 
  summarize(number_stops = n()) %>% 
  mutate(stop_district = as.double(stop_district)) %>% 
  left_join(rollout_dates, join_by(stop_district == district)) %>% 
  mutate(treatment = ifelse(stop_date >= shotspot_activate, 1, 0)) %>% 
  mutate(treatment = ifelse(is.na(treatment), 0, treatment)) %>% 
  filter(stop_district %in% shotspotter_districts) %>% 
  filter(person_stopped_race == "Black") %>% 
  feols(number_stops ~ treatment | stop_date + stop_district,
        cluster = ~stop_district,
        data = .)
  
stops %>% 
  mutate(year = year(stop_year_month)) %>% 
  filter(stop_district %in% shotspotter_districts) %>% 
  feols(number_stops ~ treatment | stop_year_month + stop_district,
        cluster = ~stop_district,
        data = .)
stops_shots_year_month %>% 
  filter(stop_district %in% shotspotter_districts) %>% 
  ggplot(aes(stop_year_month, number_stops, color = person_stopped_race)) +
  geom_line() +
  geom_vline(aes(xintercept = shotspot_activate),
             linetype = "dashed",
             color = "dark red") +
  facet_wrap(~stop_district) +
  labs(color = "Race", x = "Time", y = "Number of Stops") +
  theme_minimal() +
  theme(legend.position = "bottom")
merged <- shotspotter %>% 
  group_by(shotspot_year, district) %>% 
  mutate(district = as.character(district)) %>% 
  summarize(number_shots  = n()) %>% 
  ungroup() %>% 
  left_join(chicago,
            by = join_by(district == "dist_num")) %>% 
  st_as_sf()
merged %>% 
  ggplot() +
  geom_sf(aes(fill = number_shots)) +
  geom_sf_label(aes(label = dist_label)) +
  gganimate::transition_time(shotspot_year) + 
  ease_aes('linear')
chicago %>% 
  
  ggplot() +
  geom_sf() +
  geom_sf_label(aes(label = dist_label)) +
  ggthemes::theme_map()
