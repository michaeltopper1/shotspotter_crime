library(tidyverse)
library(patchwork)

rollout_dates <- read_csv(here::here("created_data/rollout_dates.csv")) %>% 
  mutate(across(starts_with("shotspot"), ~mdy(.)))
shotspotter_alerts <- read_csv(here::here("created_data/shotspotter_cleaned.csv"))


## creating panels for each of the districts
panel_dates <- seq(as_date("2016-01-01"), as_date("2022-12-31") , by= "day") %>% 
  as_tibble() %>% 
  rename(date = value) 

districts <- c(1:20, 22, 24, 25) %>% 
  as_tibble() %>% 
  rename(district = value) %>% 
  filter(district != 13)

panel_dates <- panel_dates %>% 
  cross_join(districts)



# aggregating -------------------------------------------------------------

agg_shotspotter_alerts <- shotspotter_alerts %>% 
  group_by(date, district) %>% 
  count() %>% ungroup()

shotspot_panel <- panel_dates %>% 
  left_join(agg_shotspotter_alerts) %>% 
  left_join(rollout_dates) %>% 
  mutate(n = ifelse(is.na(n), 0, n))


table_shots <- shotspot_panel %>% 
  group_by(district) %>% 
  filter(date >= shotspot_activate) %>% 
  ungroup() %>% 
  summarize(avg_shots = mean(n), .by = district) %>% 
  arrange(district) %>% 
  mutate(avg_shots = sprintf(fmt = "%.3f", avg_shots)) %>% 
  rename(District = district,
         `Mean Shots` = avg_shots)

time_graph <- shotspot_panel %>% 
  mutate(year = year(date),
         month =  month(date)) %>% 
  filter(year <= 2022) %>% 
  mutate(year_month = mdy(paste0(month, "-1-", year))) %>% 
  group_by(year_month, district) %>% 
  summarize(number_shots = sum(n)) %>% 
  left_join(rollout_dates) %>% 
  filter(!is.na(shotspot_activate)) %>% 
  ggplot(aes(year_month, number_shots)) +
  geom_line() +
  scale_x_date(date_breaks =  "2 years",
               date_labels = "%Y") +
  geom_vline(aes(xintercept = shotspot_activate), linetype = "dashed", color = "dark red") +
  facet_wrap(~district) +
  labs(x = "", y = "Number of ShotSpotter Alerts") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))


shotspotter_trend <- time_graph + gridExtra::tableGrob(table_shots, rows=NULL, 
                                  theme = ggpp::ttheme_gtlight(base_size = 9)) + plot_layout(widths = c(2, 1))
  