## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-05-22
##

library(tidyverse)

if(!exists("dispatch_panel")) {
  dispatch_panel <- read_csv(here::here("analysis_data/xxdispatch_panel.csv"))
}


raw_evidence_figure <- dispatch_panel %>% 
  mutate(district = as_factor(district)) %>% 
  mutate(treatment = if_else(treatment == 1, "ShotSpotter Active",
                             "ShotSpotter Inactive")) %>% 
  mutate(never_treated = if_else(never_treated == 1,
                                 "Never Treated Districts", "Treated Districts")) %>% 
  group_by(district, never_treated,
           treatment) %>% 
  summarize(avg_entry_to_dispatch_1 = mean(entry_to_dispatch_1, na.rm = T),
            avg_entry_to_onscene_1 = mean(entry_to_onscene_1, na.rm = T)) %>% 
  pivot_longer(cols = starts_with("avg_"), 
               values_to = "avg_time",
               names_to = "outcome") %>% 
  mutate(outcome = if_else(outcome == "avg_entry_to_dispatch_1",
                           "Call-to-Dispatch", "Call-to-On-Scene")) %>% 
  ggplot(aes(district, avg_time, fill = treatment)) +
  geom_col(position = "dodge") +
  facet_wrap(~outcome) +
  labs(fill = "", x = "Police District", y = "Mean of Outcome (seconds)") +
  ggthemes::scale_fill_stata() +
  theme_minimal() +
  theme(legend.position = "bottom")

