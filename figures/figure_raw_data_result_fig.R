## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-05-22
##

library(tidyverse)

if (!exists("dispatch_panel")){
  dispatch_panel <- read_csv(here::here("analysis_data/xxdispatches_clevel.csv"))
  dispatch_panel_p1 <- dispatch_panel %>% 
    filter(priority_code == 1)
}


raw_evidence_figure <-dispatch_panel_p1 %>% 
  mutate(district = as_factor(district)) %>% 
  mutate(treatment = if_else(treatment == 1, "ShotSpotter Active",
                             "ShotSpotter Inactive")) %>% 
  mutate(never_treated = if_else(never_treated == 1,
                                 "Never Treated Districts", "Treated Districts")) %>% 
  group_by(district, never_treated,
           treatment) %>% 
  summarize(avg_entry_to_dispatch_1 = mean(entry_to_dispatch, na.rm = T),
            avg_entry_to_onscene_1 = mean(entry_to_onscene, na.rm = T)) %>% 
  pivot_longer(cols = starts_with("avg_"), 
               values_to = "avg_time",
               names_to = "outcome") %>% 
  mutate(treatment = if_else(never_treated == "Never Treated Districts", 
                             "Never Treated", treatment)) %>% 
  mutate(district = fct_rev(district)) %>% 
  mutate(outcome = if_else(outcome == "avg_entry_to_dispatch_1",
                           "Call-to-Dispatch", "Call-to-On-Scene")) %>% 
  ggplot(aes(district, avg_time, fill = treatment, alpha = treatment)) +
  geom_col(position = "dodge") +
  facet_wrap(~outcome, scales = "free") +
  labs(fill = "", x = "Police District", y = "Mean of Outcome (seconds)",
       alpha = "") +
  scale_fill_manual(values =c("#808080","#1a476f", "#90353b")) +
  scale_alpha_discrete(range = c(0.4, 1, 1)) + 
  theme_minimal() +
  coord_flip() +
  theme(legend.position = "bottom", panel.grid.major =  element_blank())

ggsave(raw_evidence_figure, filename = "paper/figures/raw_evidence_figure.jpeg",
       width = 7, height = 5)


