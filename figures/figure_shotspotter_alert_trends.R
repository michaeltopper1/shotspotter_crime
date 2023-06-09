library(tidyverse)
library(patchwork)
library(gridExtra)

if(!exists("dispatch_panel")) {
  dispatch_panel <- read_csv(here::here("analysis_data/xxdispatch_panel.csv"))
}


rollout_dates <- read_csv("created_data/rollout_dates.csv") %>% 
  mutate(across(starts_with("shotspot"), ~mdy(.)))



table_shots <- dispatch_panel %>% 
  group_by(district) %>% 
  filter(date >= shotspot_activate) %>% 
  ungroup() %>% 
  summarize(avg_shots = mean(number_sst_alerts), .by = district) %>% 
  arrange(district) %>% 
  mutate(avg_shots = sprintf(fmt = "%.3f", avg_shots)) %>% 
  rename(District = district,
         `Mean Shots` = avg_shots)

time_graph <- dispatch_panel %>% 
  group_by(year_month, district) %>% 
  summarize(number_shots = sum(number_sst_alerts)) %>% 
  left_join(rollout_dates) %>% 
  filter(!is.na(shotspot_activate)) %>% 
  ggplot(aes(year_month, number_shots)) +
  geom_line() +
  scale_x_date(date_breaks =  "2 years",
               date_labels = "%Y") +
  geom_vline(aes(xintercept = shotspot_activate_official), linetype = "dashed", color = "blue") +
  geom_vline(aes(xintercept = shotspot_activate), linetype = "dashed", color = "dark red") +
  facet_wrap(~district) +
  labs(x = "", y = "Number of ShotSpotter Alerts") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))


shotspotter_trend <- time_graph + gridExtra::tableGrob(table_shots, rows=NULL, 
                                  theme = ggpp::ttheme_gtlight(base_size = 9)) + plot_layout(widths = c(2, 1))
  

