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


start <- dispatch_panel_p1 %>% 
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
  mutate(outcome = if_else(outcome == "avg_entry_to_dispatch_1",
                           "Call-to-Dispatch", "Call-to-On-Scene")) %>% 
  mutate(district_label = glue::glue("District {district}")) %>% 
  mutate(district_label = fct_reorder(district_label, district),
         district_label = fct_rev(district_label)) %>% 
  mutate(never_treated = fct_rev(never_treated))

raw_evidence_figure <- start %>% 
  ggplot(aes(district_label, avg_time, color = treatment)) +
  geom_pointrange(aes(xmin = district_label,xmax = district_label, ymin = 0, ymax = avg_time),
                  position = position_dodge(width = 0.7),
                  fatten = 1.5) +
  facet_grid(fct_rev(never_treated)~outcome, scales = "free_y") +
  coord_flip() +
  labs(color = "", x = "", y = "Mean of Response Time (seconds)",
       alpha = "") +
  scale_color_manual(values =c("#808080","#1a476f", "#90353b")) +
  scale_alpha_discrete(range = c(0.5, 1, 1)) + 
  theme_minimal() +
  theme(legend.position = "bottom", panel.grid.major =  element_blank())

ggsave(raw_evidence_figure, filename = "paper/figures/raw_evidence_figure.jpeg",
       width = 7, height = 5)


