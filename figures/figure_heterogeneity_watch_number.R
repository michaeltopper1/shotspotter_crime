
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
    str_detect(type, "watch3") ~ "Watch 3:\n(4pm - 12am)",
    str_detect(type, "watch2") ~ "Watch 2:\n(8am - 4pm)",
    str_detect(type, "watch1") ~ "Watch 1:\n(12am - 8am)"
  ))


hetero_resource <- watch_reg %>% 
  mutate(type = factor(type, levels = c("Watch 3:\n(4pm - 12am)",
                                        "Watch 2:\n(8am - 4pm)",
                                        "Watch 1:\n(12am - 8am)"))) %>% 
  mutate(resource_constraint = case_when(
    type == "Watch 3:\n(4pm - 12am)" ~ "More Resource Constrained",
    type == "Watch 2:\n(8am - 4pm)" ~ "Lo",
    .default = "Less Resource Constrained"
  )) %>%
  ggplot(aes(type, estimate, color = resource_constraint)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
  facet_wrap(outcome~group, scales = "free") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
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
    hour < 8 ~ "Watch 1",
    hour < 16 & hour >=8 ~ "Watch 2",
    hour >= 16 ~ "Watch 3")) %>% 
  mutate(average_watch = mean(n), .by = watch, .before = 1) %>% 
  ggplot(aes(hour,n, fill = watch)) +
  geom_col(alpha = 0.8) +
  scale_x_continuous(breaks = c(0:23)) +
  theme_minimal() +
  ggthemes::scale_fill_stata() +
  labs(x = "Hour of the Day", y = "SST Dispatches", fill = "") +
  theme(legend.position = "bottom",
        axis.title.x = element_text(size =9),
        panel.grid.major =  element_blank(),
        axis.title.y = element_text(size = 9))



