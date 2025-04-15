## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-06-01
##

library(tidyverse)

if (!exists("dispatch_panel")){
  dispatch_panel <- read_csv(here::here("analysis_data/xxdispatches_clevel.csv"))
  dispatch_panel_p1 <- dispatch_panel %>% 
    filter(priority_code == 1)
}

means <- dispatch_panel_p1 %>% 
  select(entry_to_dispatch, entry_to_onscene) %>% 
  pivot_longer(cols = everything(),names_to = "outcome", values_to = "time") %>% 
  mutate(mean = mean(time, na.rm = T), .by = outcome) %>% 
  distinct(mean) %>% pull()

mean_call_dispatch <- means[1]
mean_call_onscene <- means[2]

distribution_outcomes <- dispatch_panel_p1 %>% 
  select(entry_to_dispatch, entry_to_onscene) %>% 
  pivot_longer(cols = everything(),names_to = "outcome", values_to = "time") %>% 
  mutate(mean = mean(time, na.rm = T), .by = outcome) %>% 
  mutate(outcome = if_else(outcome == "entry_to_dispatch",
                           "Call-to-Dispatch",
                           "Call-to-On-Scene")) %>% 
  mutate(time = if_else(time >= 3000, 3000, time)) %>% 
  ggplot(aes(time, fill = outcome)) + 
  geom_histogram(position = "identity", alpha = 0.6) +
  geom_vline(aes(xintercept = mean, color = outcome), linetype = "dashed") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = c(0, round(mean_call_dispatch,0), round(mean_call_onscene,0),1000,  2000, 3000),
                     labels = c("0",round(mean_call_dispatch,0),round(mean_call_onscene,0),"1000", "2000", "3000+")) +
  ggthemes::scale_fill_stata() +
  ggthemes::scale_color_stata() +
  labs(x = "Response Time (seconds)", y = "Count",
       color = "", fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom", panel.grid.major = element_blank())

ggsave(distribution_outcomes, filename = "paper/figures/distribution_outcomes.jpeg",
       width = 7, height = 5)

