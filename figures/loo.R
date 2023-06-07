library(tidyverse)
library(fixest)
library(panelsummary)
library(kableExtra)
library(did2s)


dispatch_panel <- read_csv(here::here("analysis_data/xxdispatch_panel.csv"))


districts <- dispatch_panel %>% 
  distinct(district) %>% 
  pull() 



loo <- map_df(districts, ~ feols(entry_to_dispatch ~treatment + 
                        number_dispatches_1 + 
                        number_dispatches_2 + 
                        number_dispatches_3 +
          officer_hours | district + date,
        cluster = ~district,
        data = dispatch_panel %>% 
          filter(district != .x)) %>% 
      broom::tidy(conf.int = T) %>% 
        filter(term == "treatment"))

loo_r <- map_df(districts, ~did2s(data = dispatch_panel %>% filter(district != .x),
      yname = "entry_to_dispatch_1",
      first_stage = ~ officer_hours + number_dispatches_1 + 
        number_dispatches_2 + 
        number_dispatches_3 | district + date,
      second_stage = ~treatment,
      treatment = "treatment",
      cluster_var = "district") %>% 
      broom::tidy(conf.int = T))

loo_r <- loo_r %>% 
  mutate(id = row_number(),
         method = "Gardner (2021)")
loo %>% 
  mutate(id = row_number(),
         method = "OLS") %>% 
  bind_rows(loo_r) %>% 
  group_by(method) %>% 
  mutate(mean_estimate = mean(estimate)) %>% 
  ungroup() %>% 
  ggplot(aes(id, estimate, color = method)) +
  geom_point(aes(id, estimate), position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),position = position_dodge(width = 0.5)) +
  geom_hline(aes(yintercept = 0), color = "dark red") +
  geom_hline(aes(yintercept = mean_estimate, color = method), linetype = "dashed") +
  ylim(c(-200, 300)) +
  ggthemes::scale_color_stata() +
  labs(color = "", x = "", y = "95% Confidence Interval and Point Estimate") +
  theme_minimal() +
  theme(legend.position = "bottom")

