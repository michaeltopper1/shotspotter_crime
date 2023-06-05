## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-06-01
##

library(tidyverse)

if (!exists("dispatch_panel")){
  dispatch_panel <- read_csv(here::here("analysis_data/xxdispatch_panel.csv"))
}


distribution_outcomes <- dispatch_panel %>% 
  mutate(entry_to_dispatch_1 = if_else(entry_to_dispatch_1 >= 2500, 2500,
                                       entry_to_dispatch_1),
         entry_to_onscene_1 = if_else(entry_to_onscene_1 >= 2500,
                                      2500, entry_to_onscene_1)) %>% 
  select(entry_to_dispatch_1, entry_to_onscene_1) %>% 
  pivot_longer(cols = everything(),names_to = "outcome", values_to = "time") %>% 
  mutate(mean = mean(time, na.rm = T), .by = outcome) %>% 
  mutate(outcome = if_else(outcome == "entry_to_dispatch_1",
                           "Call to Dispatch",
                           "Call to On-Scene")) %>% 
  ggplot(aes(time, fill = outcome)) + 
  geom_histogram(position = "identity", alpha = 0.6) +
  geom_vline(aes(xintercept = mean, color = outcome), linetype = "dashed") +
  scale_x_continuous(breaks = c(500, 1000, 1500, 2000, 2500),
                     labels = c("500", "1000", "1500", "2000", "2500+")) +
  scale_y_continuous(labels = scales::comma) +
  ggthemes::scale_fill_stata() +
  ggthemes::scale_color_stata() +
  labs(x = "Time (seconds)", y = "Count",
       color = "", fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom")

