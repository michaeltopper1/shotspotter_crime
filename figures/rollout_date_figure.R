## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-04-20
##

library(tidyverse)

stops_panel <- read_csv("analysis_data/stops_panel.csv")

districts <- stops_panel %>% 
  distinct(district)

rollouts <- read_csv("created_data/rollout_dates.csv")

graph_data <- districts %>% 
  left_join(rollouts)

graph_data %>% 
  mutate(treatment_days = days(as_date("2023-01-01") - shotspot_activate) %>% 
           day() %>% 
           scales::comma()) %>% 
  mutate(treatment_days = glue::glue("{treatment_days} days")) %>%  
  mutate(district_plot = glue::glue("District {district}")) %>% 
  mutate(district_plot = fct_reorder(district_plot, -district)) %>% 
  ggplot(aes(shotspot_activate, district_plot)) +
  geom_segment(aes(x = shotspot_activate, xend = as_date("2022-12-31"),
                   yend = district_plot),lineend = "square", linewidth = 2,
               color = "dark red", alpha = 0.8) +
  geom_point(color = "dark green", alpha  = 0.8) +
  geom_text(aes(label = treatment_days), vjust = -.8, size = 3) +
  scale_x_date(limits = c(as_date("2016-01-01"), as_date("2022-12-31")),
               date_breaks = "1 year", date_labels = "%Y") +
  labs(y = "", x = "") +
  theme_minimal()
