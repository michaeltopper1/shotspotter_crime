
library(tidyverse)
library(fixest)
library(panelsummary)
library(kableExtra)
library(did2s)

if (!exists("dispatch_panel")){
  dispatch_panel <- read_csv(here::here("analysis_data/xxdispatch_panel.csv"))
}


watch_reg <- dispatch_panel %>% 
  feols(c(entry_to_dispatch_watch1_1,
          entry_to_dispatch_watch2_1,
          entry_to_dispatch_watch3_1,
          entry_to_onscene_watch1_1,
          entry_to_onscene_watch2_1,
          entry_to_onscene_watch3_1) ~ treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 + number_dispatches_3 +
          number_dispatches_0| district + date,
        cluster = ~district) %>% 
  map_df(~broom::tidy(.,conf.int = T),.id = "type") %>% 
  filter(term == "treatment") %>% 
  mutate(group = "Shift Watch") %>% 
  mutate(outcome = if_else(str_detect(type, "onscene"), "Call-to-On-Scene", "Call-to-Dispatch")) %>% 
  mutate(type = case_when(
    str_detect(type, "watch3") ~ "Watch 3:\n(4pm - 12am)",
    str_detect(type, "watch2") ~ "Watch 2:\n(8am - 4pm)",
    str_detect(type, "watch1") ~ "Watch 1:\n(12am - 8am)"
  ))


dispatch_reg <- dispatch_panel %>% 
  mutate(median_priority_1 = median(number_dispatches_1), .by = district) %>% 
  filter(number_dispatches_1 > median_priority_1) %>% 
  feols(c(entry_to_dispatch_1,
          entry_to_onscene_1) ~ treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 + number_dispatches_3 +
          number_dispatches_0| district + date,
        cluster = ~district) %>% 
  map_df(~broom::tidy(., conf.int = T), .id = "type") %>% 
  filter(term == "treatment") %>% 
  mutate(group = "Number of 911 Dispatches") %>% 
  mutate(outcome = if_else(str_detect(type, "onscene"), "Call-to-On-Scene", "Call-to-Dispatch")) %>% 
  mutate(type = if_else(type == "lhs:entry_to_dispatch_1", "Above Median",
                       "Above Median"))


dispatch_reg_below <- dispatch_panel %>% 
  mutate(median_priority_1 = median(number_dispatches_1), .by = district) %>% 
  filter(number_dispatches_1 <= median_priority_1) %>% 
  feols(c(entry_to_dispatch_1,
          entry_to_onscene_1) ~ treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 + number_dispatches_3 +
          number_dispatches_0| district + date,
        cluster = ~district) %>% 
  map_df(~broom::tidy(., conf.int = T), .id = "type") %>% 
  filter(term == "treatment") %>% 
  mutate(group = "Number of 911 Dispatches") %>% 
  mutate(outcome = if_else(str_detect(type, "onscene"), "Call-to-On-Scene", "Call-to-Dispatch")) %>% 
  mutate(type = if_else(type == "lhs:entry_to_dispatch_1", "Below Median",
                        "Below Median"))



hetero_resource <- watch_reg %>% 
  bind_rows(dispatch_reg, 
            dispatch_reg_below) %>% 
  mutate(type = factor(type, levels = c("Watch 3:\n(4pm - 12am)",
                                        "Watch 2:\n(8am - 4pm)",
                                        "Watch 1:\n(12am - 8am)",
                                        "Below Median",
                                        "Above Median"))) %>% 
  mutate(resource_constraint = case_when(
    type == "Above Median" ~ "More Resource Constrained",
    type == "Watch 3:\n(4pm - 12am)" ~ "More Resource Constrained",
    .default = "Less Resource Constrained"
  )) %>% 
  ggplot(aes(type, estimate, color = resource_constraint)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
  facet_wrap(outcome~group, scales = "free") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "dark red") +
  coord_flip() +
  theme_minimal() +
  ggthemes::scale_color_stata() +
  labs(x = "", y = "Point Estimate (seconds) and 95% Confidence Interval", color = "") +
  theme(legend.position = "bottom")

