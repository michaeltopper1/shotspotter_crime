library(tidyverse)
library(patchwork)
library(gridExtra)

if(!exists("dispatch_panel")) {
  dispatch_panel <- read_csv(here::here("analysis_data/xxdispatches_clevel_sf.csv"))
}


rollout_dates <- read_csv("created_data/rollout_dates.csv") %>% 
  mutate(across(starts_with("shotspot"), ~mdy(.)))



table_shots <- dispatch_panel %>% 
  group_by(district) %>% 
  filter(date >= shotspot_activate) %>% 
  ungroup() %>% 
  summarize(avg_shots = mean(number_sst_dispatches), .by = c(district, shotspot_activate)) %>% 
  arrange(district) %>% 
  mutate(avg_shots = sprintf(fmt = "%.2f", avg_shots)) %>% 
  rename(District = district,
         `Enactment` = shotspot_activate,
         `Daily Alerts` = avg_shots) %>% 
  select(-`Daily Alerts`)

time_graph <- dispatch_panel %>% 
  mutate(shots_fired = if_else(final_dispatch_description == "SHOTS FIRED",
                                      1, 0)) %>% 
  mutate(shots_fired = sum(shots_fired), .by = c(district, year_month)) %>% 
  distinct(date, district, number_sst_dispatches, year_month, shots_fired) %>% 
  select(district, number_sst_dispatches, date, year_month,shots_fired) %>% 
  arrange(desc(number_sst_dispatches)) %>% 
  group_by(year_month, district) %>% 
  summarize(number_shots = sum(number_sst_dispatches),
            number_shots_fired = mean(shots_fired)) %>% 
  pivot_longer(cols = c(number_shots, number_shots_fired),
               values_to = "number", names_to = "type") %>% 
  left_join(rollout_dates) %>% 
  mutate(district_label = glue::glue("District {district}") %>% 
           fct_reorder(district)) %>%
  filter(!is.na(shotspot_activate)) %>% 
  mutate(type = if_else(type =="number_shots", "ShotSpotter", 
                        "Civilian Reports of Gunshots")) %>% 
  ggplot(aes(year_month, number, color = type)) +
  geom_line() +
  scale_x_date(date_breaks =  "2 years",
               date_labels = "%Y") +
  scale_y_continuous(labels = scales::comma) +
  geom_vline(aes(xintercept = shotspot_activate), linetype = "dashed", color = "dark red") +
  facet_wrap(~district_label) +
  labs(x = "", y = "Number of Dispatches",
       color = "") +
  scale_color_manual(values = c( "grey", "black")) +
  scale_alpha_manual(values = c( 0.2, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1),
        legend.position = "bottom")


shotspotter_trend <- time_graph + gridExtra::tableGrob(table_shots, rows=NULL, 
                                  theme = ggpp::ttheme_gtlight(base_size = 9)) + plot_layout(widths = c(2, 1))
  
ggsave(shotspotter_trend, filename = "paper/figures/shotspotter_trend.jpeg",
       width = 7, height = 5)
