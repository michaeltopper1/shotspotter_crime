## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-05-18
##

library(tidyverse)
library(fixest)
library(panelsummary)
library(kableExtra)
library(did2s)

dispatch_panel <- read_csv("analysis_data/dispatches_all.csv")
officer_hours <- read_csv("analysis_data/officer_hours.csv")
border_districts <- read_csv("created_data/border_districts_final.csv")

dispatch_panel <- dispatch_panel %>% 
  mutate(across(starts_with("number_dispatches"), ~ifelse(is.na(.), 0, .)))

dispatch_panel <- dispatch_panel %>% 
  left_join(officer_hours, join_by(date == date,
                                   district == district))

dispatch_panel <- dispatch_panel %>% 
  left_join(border_districts, join_by(district == border_district))


# priority 1 --------------------------------------------------------------

d1_main <- dispatch_panel %>% 
  feols(entry_to_dispatch_1 ~ treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 + 
          number_dispatches_3 + number_dispatches_0| district + date) %>% 
  broom::tidy(conf.int = T) %>% 
  filter(term == "treatment") %>% 
  mutate(priority = "Priority 1",
         sample = "Main Sample")

d1_2s <- did2s(dispatch_panel,
      yname = "entry_to_dispatch_1",
      first_stage = ~ officer_hours +
        number_dispatches_1 + number_dispatches_2 + 
        number_dispatches_3 + number_dispatches_0| district + date,
      second_stage = ~treatment,
      treatment = "treatment",
      cluster_var = "district") %>% 
  broom::tidy(conf.int = T) %>% 
  filter(term == "treatment") %>% 
  mutate(priority = "Priority 1",
         sample = "Main Sample\n(2-Stage DID)")

d1_nocontrol <- dispatch_panel %>% 
  feols(entry_to_dispatch_1 ~ treatment | district + date) %>% 
  broom::tidy(conf.int = T) %>% 
  filter(term == "treatment") %>% 
  mutate(priority = "Priority 1", 
         sample = "No Controls")

d1_2020 <- dispatch_panel %>%
  filter(year != 2020) %>% 
  feols(entry_to_dispatch_1 ~ treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 + 
          number_dispatches_3 + number_dispatches_0| district + date) %>% 
  broom::tidy(conf.int = T) %>% 
  filter(term == "treatment") %>% 
  mutate(priority = "Priority 1",
         sample = "Omitting 2020")

d1_nevertreat <- dispatch_panel %>%
  filter(never_treated == 0) %>% 
  feols(entry_to_dispatch_1 ~ treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 + 
          number_dispatches_3 + number_dispatches_0| district + date) %>% 
  broom::tidy(conf.int = T) %>% 
  filter(term == "treatment") %>% 
  mutate(priority = "Priority 1",
         sample = "Omitting\nNever-Treated")

# priority 2 --------------------------------------------------------------

d2_main <- dispatch_panel %>% 
  feols(entry_to_dispatch_2 ~ treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 + 
          number_dispatches_3 + number_dispatches_0| district + date) %>% 
  broom::tidy(conf.int = T) %>% 
  filter(term == "treatment") %>% 
  mutate(priority = "Priority 2",
         sample = "Main Sample")

d2_2s <- did2s(dispatch_panel,
               yname = "entry_to_dispatch_2",
               first_stage = ~ officer_hours +
                 number_dispatches_1 + number_dispatches_2 + 
                 number_dispatches_3 + number_dispatches_0| district + date,
               second_stage = ~treatment,
               treatment = "treatment",
               cluster_var = "district") %>% 
  broom::tidy(conf.int = T) %>% 
  filter(term == "treatment") %>% 
  mutate(priority = "Priority 2",
         sample = "Main Sample\n(2-Stage DID)")

d2_nocontrol <- dispatch_panel %>% 
  feols(entry_to_dispatch_2 ~ treatment | district + date) %>% 
  broom::tidy(conf.int = T) %>% 
  filter(term == "treatment") %>% 
  mutate(priority = "Priority 2",
         sample = "No Controls")

d2_2020 <- dispatch_panel %>%
  filter(year != 2020) %>% 
  feols(entry_to_dispatch_2 ~ treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 + 
          number_dispatches_3 + number_dispatches_0| district + date) %>% 
  broom::tidy(conf.int = T) %>% 
  filter(term == "treatment") %>% 
  mutate(priority = "Priority 2",
         sample = "Omitting 2020")

d2_nevertreat <- dispatch_panel %>%
  filter(never_treated == 0) %>% 
  feols(entry_to_dispatch_2 ~ treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 + 
          number_dispatches_3 + number_dispatches_0| district + date) %>% 
  broom::tidy(conf.int = T) %>% 
  filter(term == "treatment") %>% 
  mutate(priority = "Priority 2",
         sample = "Omitting\nNever-Treated")

# priority 3 --------------------------------------------------------------

d3_main <- dispatch_panel %>% 
  feols(entry_to_dispatch_3 ~ treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 + 
          number_dispatches_3 + number_dispatches_0| district + date) %>% 
  broom::tidy(conf.int = T) %>% 
  filter(term == "treatment") %>% 
  mutate(priority = "Priority 3",
         sample = "Main Sample")

d3_2s <- did2s(dispatch_panel,
               yname = "entry_to_dispatch_3",
               first_stage = ~ officer_hours +
                 number_dispatches_1 + number_dispatches_2 + 
                 number_dispatches_3 + number_dispatches_0| district + date,
               second_stage = ~treatment,
               treatment = "treatment",
               cluster_var = "district") %>% 
  broom::tidy(conf.int = T) %>% 
  filter(term == "treatment") %>% 
  mutate(priority = "Priority 3",
         sample = "Main Sample\n(2-Stage DID)")

d3_nocontrol <- dispatch_panel %>% 
  feols(entry_to_dispatch_3 ~ treatment | district + date) %>% 
  broom::tidy(conf.int = T) %>% 
  filter(term == "treatment") %>% 
  mutate(priority = "Priority 3",
         sample = "No Controls")

d3_2020 <- dispatch_panel %>%
  filter(year != 2020) %>% 
  feols(entry_to_dispatch_3 ~ treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 + 
          number_dispatches_3 + number_dispatches_0| district + date) %>% 
  broom::tidy(conf.int = T) %>% 
  filter(term == "treatment") %>% 
  mutate(priority = "Priority 3",
         sample = "Omitting 2020")

d3_nevertreat <- dispatch_panel %>%
  filter(never_treated == 0) %>% 
  feols(entry_to_dispatch_3 ~ treatment + officer_hours +
          number_dispatches_1 + number_dispatches_2 + 
          number_dispatches_3 + number_dispatches_0| district + date) %>% 
  broom::tidy(conf.int = T) %>% 
  filter(term == "treatment") %>% 
  mutate(priority = "Priority 3",
         sample = "Omitting\nNever-Treated")

entry_forest <- d1_main %>% 
  bind_rows(d1_2s, d1_nocontrol, d1_2020, d1_nevertreat,
            d2_main, d2_2s, d2_nocontrol, d2_2020, d2_nevertreat,
            d3_main, d3_2s, d3_nocontrol, d3_2020, d3_nevertreat) %>% 
  mutate(estimate = round(estimate, 2)) %>%
  mutate(estimate_label = sprintf("%.3f", estimate))

d1_labs <- entry_forest %>% 
  mutate(sample = factor(sample, levels = c("Main Sample", "Main Sample\n(2-Stage DID)",
                                            "No Controls", "Omitting 2020",
                                            "Omitting\nNever-Treated")) %>% 
           fct_rev()) %>% 
  pull(sample) %>% rev()
d1_labs2 <- rev(sprintf("%.3f",entry_forest$estimate))



entry_forest %>% 
  mutate(sample = factor(sample, levels = c("Main Sample", "Main Sample\n(2-Stage DID)",
                                   "No Controls", "Omitting 2020",
                                   "Omitting\nNever-Treated")) %>% 
           fct_rev()) %>% 
  mutate(y_ax1 = c(1:15) %>% rev()) %>% 
  ggplot(aes(estimate, y_ax1)) +
  geom_point(aes(shape = sample), size = 3) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "dark red") +
  facet_wrap(~priority, scales = "free_y") +
    scale_y_continuous(breaks = 1:length(d1_labs2),
                       labels = d1_labs,
                       sec.axis = sec_axis(~., breaks = 1:length(d1_labs2),
                                           labels = d1_labs2, 
                                           name = expression(paste("Estimate (", hat(beta), ")")))) +
  labs(y = "Sample", x = "Time (seconds)", shape = "") +
  ggthemes::scale_color_stata() +
  theme_minimal() +
  theme(axis.line.y = element_blank(),
        axis.text.y = element_text(hjust = 1),
        axis.ticks.y = element_blank(),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 1), 
        axis.text.y.left =element_text(size = 9),
        legend.position = "none",
        axis.line.x = element_line(color = "black"),
        panel.border = element_blank(),
        panel.background = element_rect(color = NA),
        strip.text = element_text(size = 9))

