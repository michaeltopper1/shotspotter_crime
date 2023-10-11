library(tidyverse)
library(fixest)
library(panelsummary)
library(kableExtra)
library(did2s)


if (!exists("dispatch_panel")){
  dispatch_panel <- read_csv(here::here("analysis_data/xxdispatches_clevel.csv"))
  ## priority 1 dispatches only
  dispatch_panel_p1 <- dispatch_panel %>% 
    filter(priority_code ==1)
}

setFixest_fml(..ctrl = ~0| district + date +
                final_dispatch_description + hour)


districts <- dispatch_panel_p1 %>% 
  distinct(district) %>% 
  pull() 



loo <- map_df(districts, ~ feols(entry_to_dispatch ~treatment +..ctrl,
        cluster = ~district,
        data = dispatch_panel_p1 %>% 
          filter(district != .x)) %>% 
      broom::tidy(conf.int = T) %>% 
        filter(term == "treatment"))

loo_os <- map_df(districts, ~ feols(entry_to_onscene ~treatment + 
                                   ..ctrl,
                                 cluster = ~district,
                                 data = dispatch_panel_p1 %>% 
                                   filter(district != .x)) %>% 
                broom::tidy(conf.int = T) %>% 
                filter(term == "treatment"))

loo_r <- map_df(districts, ~did2s(data = dispatch_panel_p1 %>% filter(district != .x),
      yname = "entry_to_dispatch",
      first_stage = ~ ..ctrl,
      second_stage = ~treatment,
      treatment = "treatment",
      cluster_var = "district") %>% 
      broom::tidy(conf.int = T))

loo_os_r <- map_df(districts, ~did2s(data = dispatch_panel_p1 %>% filter(district != .x),
                                  yname = "entry_to_onscene",
                                  first_stage = ~ ..ctrl,
                                  second_stage = ~treatment,
                                  treatment = "treatment",
                                  cluster_var = "district") %>% 
                  broom::tidy(conf.int = T))



# delineating each set ----------------------------------------------------

loo <- loo %>% 
  mutate(id = row_number(),
         method = "OLS",
         outcome = "Call-to-Dispatch") 

loo_r <- loo_r %>% 
  mutate(id = row_number(),
         method = "Gardner (2021)",
         outcome = "Call-to-Dispatch")

loo_os <- loo_os %>% 
  mutate(id = row_number(),
                 method = "OLS",
                 outcome = "Call-to-On-Scene") 

loo_os_r <- loo_os_r %>% 
  mutate(id = row_number(),
         method = "Gardner (2021)",
         outcome = "Call-to-On-Scene") 


loo_figure <- loo %>% 
  bind_rows(loo_r, loo_os, loo_os_r) %>% 
  group_by(method, outcome) %>% 
  mutate(mean_estimate = mean(estimate)) %>% 
  ungroup() %>% 
  ggplot(aes(id, estimate, color = method, shape = method)) +
  geom_point(aes(id, estimate), position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),position = position_dodge(width = 0.5)) +
  geom_hline(aes(yintercept = 0), color = "dark red", alpha = 0.5) +
  ylim(c(-200, 300)) +
  facet_wrap(~outcome) +
  ggthemes::scale_color_stata() +
  labs(color = "",shape = "", x = "", y = "95% Confidence Interval and Point Estimate") +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank())

ggsave(loo_figure, filename = "paper/appendix_figures/loo.jpeg",
       width = 7, height = 5)
