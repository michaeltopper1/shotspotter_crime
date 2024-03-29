
library(tidyverse)
library(fixest)
library(panelsummary)
library(kableExtra)
library(did2s)
library(patchwork)

if (!exists("dispatch_panel")){
  dispatch_panel <- read_csv(here::here("analysis_data/xxdispatches_clevel.csv"))
  ## priority 1 dispatches only
  dispatch_panel_p1 <- dispatch_panel %>% 
    filter(priority_code ==1)
}

sst <- read_csv(here::here("created_data/sst_dispatch_cpd.csv"))

setFixest_fml(..ctrl = ~0| district + date +
                final_dispatch_description + hour)

dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  mutate(entry_to_dispatch_watch1_1 = if_else(watch == 1, 
                                              entry_to_dispatch,
                                              NA),
         entry_to_dispatch_watch2_1 = if_else(watch ==2,
                                              entry_to_dispatch,
                                              NA),
         entry_to_dispatch_watch3_1 = if_else(watch == 3,
                                              entry_to_dispatch,
                                              NA),
         entry_to_onscene_watch1_1 = if_else(watch == 1,
                                             entry_to_onscene,
                                             NA),
         entry_to_onscene_watch2_1 = if_else(watch == 2, 
                                             entry_to_onscene,
                                             NA),
         entry_to_onscene_watch3_1= if_else(watch == 3, 
                                            entry_to_onscene,
                                            NA)) 

watch_reg <- dispatch_panel_p1 %>% 
  feols(c(entry_to_dispatch_watch1_1,
          entry_to_dispatch_watch2_1,
          entry_to_dispatch_watch3_1,
          entry_to_onscene_watch1_1,
          entry_to_onscene_watch2_1,
          entry_to_onscene_watch3_1) ~ treatment + ..ctrl,
        cluster = ~district) %>% 
  map_df(~broom::tidy(.,conf.int = T),.id = "type") %>% 
  filter(term == "treatment") %>% 
  mutate(group = "Shift Watch") %>% 
  mutate(outcome = if_else(str_detect(type, "onscene"), "Call-to-On-Scene", "Call-to-Dispatch")) %>% 
  mutate(type = case_when(
    str_detect(type, "watch3") ~ "Watch 3:\n(3pm - 11pm)",
    str_detect(type, "watch2") ~ "Watch 2:\n(7am - 3pm)",
    str_detect(type, "watch1") ~ "Watch 1:\n(11pm - 7am)"
  ))

watch_means <- dispatch_panel_p1 %>% 
  summarize(across(c(entry_to_dispatch_watch1_1,
                     entry_to_dispatch_watch2_1,
                     entry_to_dispatch_watch3_1,
                     entry_to_onscene_watch1_1,
                     entry_to_onscene_watch2_1,
                     entry_to_onscene_watch3_1), ~mean(., na.rm = T))) %>% 
  pivot_longer(cols = everything(), names_to = "term", values_to = "mean_dependent") %>% 
  mutate(outcome = if_else(str_detect(term, "onscene"), "Call-to-On-Scene", "Call-to-Dispatch")) %>% 
  mutate(type = case_when(
    str_detect(term, "watch3") ~ "Watch 3:\n(3pm - 11pm)",
    str_detect(term, "watch2") ~ "Watch 2:\n(7am - 3pm)",
    str_detect(term, "watch1") ~ "Watch 1:\n(11pm - 7am)"
  ))


watch_reg <- watch_reg %>% 
  left_join(watch_means, join_by(type == type, outcome == outcome)) %>% 
  mutate(percent_change = estimate/mean_dependent %>% round(2)) %>% 
  mutate(mean_dependent = round(mean_dependent, 2))

hetero_resource <- watch_reg %>% 
  mutate(percent_change = percent_change %>% round(2) %>% 
           scales::percent()) %>% 
  mutate(type = factor(type, levels = c("Watch 3:\n(3pm - 11pm)",
                                        "Watch 2:\n(7am - 3pm)",
                                        "Watch 1:\n(11pm - 7am)"))) %>% 
  mutate(resource_constraint = case_when(
    type == "Watch 3:\n(3pm - 11pm)" ~ "More Resource Constrained",
    type == "Watch 2:\n(7am - 3pm)" ~ "Lo",
    .default = "Less Resource Constrained"
  )) %>%
  ggplot(aes(type, estimate, color = resource_constraint)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
  facet_wrap(~outcome, scales = "free") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_label(aes(label = percent_change), nudge_x = .15) +
  scale_fill_manual(values =c("#808080","#1a476f", "#90353b")) +
  coord_flip() +
  theme_minimal() +
  ggthemes::scale_color_stata() +
  labs(x = "", y = "Point Estimate (seconds) and 95% Confidence Interval", color = "") +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))


sst_by_hour <- sst %>% 
  mutate(date = as_date(entry_received_date),
         year = year(date),
         month = month(date),
         day = day(date),
         year_month = mdy(paste0(month, "-1-", year)),
         hour = hour(entry_received_date)) %>% 
  summarize(n = n(), .by = hour) %>% 
  mutate(watch = case_when(
    hour < 7 | hour == 23 ~ "Watch 1",
    hour < 15 & hour >=7 ~ "Watch 2",
    hour >= 15 & hour < 23 ~ "Watch 3")) %>% 
  mutate(average_watch = mean(n), .by = watch, .before = 1) %>% 
  ggplot(aes(hour,n, fill = watch)) +
  geom_col(alpha = 0.8) +
  scale_x_continuous(breaks = c(0, 7, 15, 23),
                     labels = c("00:00",
                                "07:00",
                                "15:00",
                                "23:00")) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  ggthemes::scale_fill_stata() +
  labs(x = "Hour of the Day", y = "ShotSpotter Dispatches", fill = "") +
  theme(legend.position = "bottom",
        axis.title.x = element_text(size =9),
        panel.grid.major =  element_blank(),
        axis.title.y = element_text(size = 9))



watch_graph <- sst_by_hour + hetero_resource  + patchwork::plot_layout(ncol = 2) 

ggsave(watch_graph, filename = "presentations/figures/watch_graph.jpeg",
       width = 10, height = 5)
