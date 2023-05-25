## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-05-22
##

library(tidyverse)

dispatch_panel %>% 
  mutate(district = as_factor(district)) %>% 
  mutate(treatment = if_else(treatment == 1, "ShotSpotter Active",
                             "ShotSpotter Inactive")) %>% 
  group_by(district, never_treated,
           treatment) %>% 
  filter(never_treated == 0) %>% 
  summarize(avg_p1 = mean(entry_to_dispatch_1, na.rm = T),
            avg_p2 = mean(entry_to_dispatch_2, na.rm = T),
            avg_p3 = mean(entry_to_dispatch_3, na.rm = T)) %>% 
  pivot_longer(cols = starts_with("avg_"), 
               values_to = "entry_to_dispatch",
               names_to = "priority") %>% 
  mutate(priority = case_when(
    priority == "avg_p1" ~ "Priority 1",
    priority == "avg_p2" ~ "Priority 2",
    priority == "avg_p3" ~ "Priority 3"
  )) %>% 
  ggplot(aes(district, entry_to_dispatch, fill = treatment)) +
  geom_col(position = "dodge") +
  facet_wrap(~priority) +
  labs(fill = "", x = "Police District", y = "Mean of Average Daily Entry to Dispatch P1 (seconds)") +
  ggthemes::scale_fill_stata() +
  theme_minimal() +
  theme(legend.position = "bottom")

