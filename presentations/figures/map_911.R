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

## to create the crime graph
## just need to connect an aggregate number of 911 calls
## to the beat_num in the beat data.
## then can follow the shotspotter map 

if (!exists("dispatch_panel")){
  dispatch_panel <- read_csv(here::here("analysis_data/xxdispatches_clevel.csv"))
  ## priority 1 dispatches only
  dispatch_panel_p1 <- dispatch_panel %>% 
    filter(priority_code ==1)
}

sst <- read_csv("created_data/sst_dispatch_cpd.csv")
chicago_lines <- st_read("raw_data/chicago_boundary/geo_export_2af99ea9-183c-440b-82a5-67f4a40e66d4.shp")
chicago_beats <- st_read("raw_data/boundaries_beats/geo_export_470ee887-3356-4951-a604-91d3441678cf.shp")

aggregate_sst <- sst %>% 
  mutate(sst = 1) %>% 
  group_by(beat_of_occurrence) %>% 
  summarize(number_sst = n()) %>% 
  ungroup()

aggregate_911 <- dispatch_panel_p1 %>% 
  group_by(beat_of_occurrence) %>% 
  summarize(number_911 = n(), number_gun = sum(gun_crime_report)) %>% 
  ungroup()

crime_locations <- chicago_beats %>% 
  left_join(aggregate_911, join_by("beat_num" == "beat_of_occurrence")) %>% 
  left_join(aggregate_sst, join_by("beat_num" == "beat_of_occurrence"))


map_sst <- crime_locations %>% 
  mutate(number_sst = replace_na(number_sst, 0)) %>% 
  ggplot() +
  geom_sf(aes(fill = number_sst), color = "light grey") +
  geom_sf(data = chicago_lines, fill = NA, color = "black") +
  geom_sf_label(data = chicago_lines %>% 
                  filter(dist_label != "31ST"), aes(label = dist_label),
                nudge_x = 0.005,
                size = 2.5,
                alpha = 0.3) +
  scale_fill_gradient(low = "white", high = "dark red") +
  labs(fill = "SST Dispatches") +
  ggthemes::theme_map() +
  ggtitle("Number of ShotSpotter Dispatches", subtitle = "Chicago (2016-2022)") +
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())



map_911 <- crime_locations %>% 
  ggplot() +
  geom_sf(aes(fill = number_911), color = "light grey") +
  geom_sf(data = chicago_lines, fill = NA, color = "black") +
  geom_sf_label(data = chicago_lines %>% 
                  filter(dist_label != "31ST"), aes(label = dist_label),
                nudge_x = 0.005,
                size = 2.5,
                alpha = 0.3) +
  scale_fill_gradient(low = "white", high = "dark red") +
  labs(fill = "911 Calls") +
  ggthemes::theme_map() +
  ggtitle("Number of Priority 1 911 Calls", subtitle = "Chicago (2016-2022)") +
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

map_911_gun <- crime_locations %>% 
  ggplot() +
  geom_sf(aes(fill = number_gun), color = "light grey") +
  geom_sf(data = chicago_lines, fill = NA, color = "black") +
  geom_sf_label(data = chicago_lines %>% 
                  filter(dist_label != "31ST"), aes(label = dist_label),
                nudge_x = 0.005,
                size = 2.5,
                alpha = 0.3) +
  scale_fill_gradient(low = "white", high = "dark red") +
  labs(fill = "Gun-Related Calls") +
  ggthemes::theme_map() +
  ggtitle("Number of Gun-related 911 Calls", subtitle = "Chicago (2016-2022)") +
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

ggsave(map_sst, filename = "presentations/figures/map_sst.jpeg",
       width = 5, height = 7)
ggsave(map_911, filename = "presentations/figures/map_911.jpeg",
       width = 5, height = 7)
ggsave(map_911_gun, filename = "presentations/figures/map_911_gun.jpeg",
       width = 5, height = 7)
