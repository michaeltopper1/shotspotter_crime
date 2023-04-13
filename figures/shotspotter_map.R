## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-04-12
##

library(tidyverse)
library(sf)
library(mapview)
library(gridExtra)


shotspotter <- read_csv("created_data/shotspotter_cleaned.csv")

chicago_lines <- st_read("raw_data/chicago_boundary/geo_export_2af99ea9-183c-440b-82a5-67f4a40e66d4.shp")
chicago_beats <- st_read("raw_data/boundaries_beats/geo_export_470ee887-3356-4951-a604-91d3441678cf.shp")

shotspotter_sf <- shotspotter %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(chicago_beats)) 


joined <- shotspotter_sf %>% select(date) %>% 
  st_join(chicago_beats, join = st_within)

joined_aggregated <- joined %>% 
  st_drop_geometry() %>% 
  group_by(beat_num, district) %>% 
  summarize(number_alerts = n()) %>% 
  ungroup()

table_values <- shotspotter %>% 
  drop_na(district) %>% 
  count(district) %>% 
  mutate(district = ifelse(n < 1000, glue::glue("{district}*"), district)) %>% 
  mutate(n = scales::comma(n)) %>% 
  rename(`CPD District` = district,
         `ShotSpotter Alerts` = n)

district_graph <- chicago_beats %>% 
  left_join(joined_aggregated)
  

x <- district_graph %>% 
  mutate(number_alerts = ifelse(is.na(number_alerts), 0, number_alerts)) %>% 
  ggplot() +
  geom_sf(aes(fill = number_alerts), color = "light grey") +
  geom_sf(data = chicago_lines, fill = NA, color = "black") +
  geom_sf_label(data = chicago_lines %>% 
                 filter(dist_label != "31ST"), aes(label = dist_label),
               nudge_x = 0.005,
               size = 2.3,
               alpha = 0.3) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(fill = "ShotSpotter Alerts in\nChicago from 2016-2022") +
  ggthemes::theme_map() 



x + ggthemes::theme_map() + annotation_custom(tableGrob(table_values,rows=NULL, 
                                                        theme = ggpp::ttheme_gtlight(base_size = 7)),
                                              xmin = -87.98011,
                                              xmax = -87.88,
                                              ymin = 41.74455,
                                              ymax = 41.83455) +
  theme(legend.position = "bottom")


# ## this gets the scales of the ggplot
# layer_scales(x)$y$range$range

