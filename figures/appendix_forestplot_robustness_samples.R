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

if(!exists("dispatch_panel")) {
  dispatch_panel <- read_csv(here::here("analysis_data/xxdispatch_panel.csv"))
}

dispatch_panel_noshots <- read_csv(here::here("analysis_data/xxdispatch_panel_noshotsfired.csv"))
dispatch_panel_outliers <- read_csv(here::here("analysis_data/xxdispatch_panel_outliers.csv"))

setFixest_fml(..ctrl = ~officer_hours +
                number_dispatches_1 + number_dispatches_2 + 
                number_dispatches_3 + number_dispatches_0| district + date)

tidy_reg <- function(reg, outcome, sample, estimator){
  tidy <- reg %>% 
    broom::tidy(conf.int = T) %>% 
    filter(term == "treatment" | term == "treatment_official") %>% 
    mutate(outcome = outcome,
           sample = sample,
           estimator = estimator) 
  return(tidy)
}

# entry to dispatch--------------------------------------------------------------

d1_main <- dispatch_panel %>% 
  feols(entry_to_dispatch_1 ~ treatment + ..ctrl, cluster = ~district) %>% 
  tidy_reg(outcome = "Call-to-Dispatch",
           sample = "Main Sample",
           estimator = "OLS")
  

d1_main_2s <- did2s(dispatch_panel,
               yname = "entry_to_dispatch_1",
               first_stage = ~ ..ctrl,
               second_stage = ~treatment,
               treatment = "treatment",
               cluster_var = "district") %>% 
  tidy_reg(outcome = "Call-to-Dispatch",
           sample = "Main Sample",
           estimator = "Gardner (2021)")


d1_2020 <- dispatch_panel %>%
  filter(year != 2020) %>% 
  feols(entry_to_dispatch_1 ~ treatment + ..ctrl, cluster = ~district) %>% 
  tidy_reg(outcome = "Call-to-Dispatch",
           sample = "Omitting 2020",
           estimator = "OLS")
  

d1_2020_2s <- did2s(dispatch_panel %>% filter(year !=2020),
               yname = "entry_to_dispatch_1",
               first_stage = ~ ..ctrl,
               second_stage = ~treatment,
               treatment = "treatment",
               cluster_var = "district") %>% 
  tidy_reg(outcome = "Call-to-Dispatch",
           sample = "Omitting 2020",
           estimator = "Gardner (2021)")

d1_nevertreat <- dispatch_panel %>%
  filter(never_treated == 0) %>% 
  feols(entry_to_dispatch_1 ~ treatment + ..ctrl, cluster ~district) %>% 
  tidy_reg(outcome = "Call-to-Dispatch",
           sample = "Omitting Never-Treated",
           estimator = "OLS")
 

d1_nevertreat_2s <- did2s(dispatch_panel %>% filter(never_treated == 0),
               yname = "entry_to_dispatch_1",
               first_stage = ~ ..ctrl,
               second_stage = ~treatment,
               treatment = "treatment",
               cluster_var = "district") %>% 
  tidy_reg(outcome = "Call-to-Dispatch",
           sample = "Omitting Never-Treated",
           estimator = "Gardner (2021)")

d1_outliers <- dispatch_panel_outliers %>% 
  feols(entry_to_dispatch_1 ~ treatment + ..ctrl, cluster = ~district) %>% 
  tidy_reg(outcome = "Call-to-Dispatch",
           sample = "Including Outliers",
           estimator = "OLS")

d1_outliers_2s <- did2s(dispatch_panel_outliers,
               yname = "entry_to_dispatch_1",
               first_stage = ~ ..ctrl,
               second_stage = ~treatment,
               treatment = "treatment",
               cluster_var = "district") %>% 
  tidy_reg(outcome = "Call-to-Dispatch",
           sample = "Including Outliers",
           estimator = "Gardner (2021)")

d1_no_shots <- dispatch_panel_noshots %>% 
  feols(entry_to_dispatch_1 ~ treatment + ..ctrl, cluster = ~district) %>% 
  tidy_reg(outcome = "Call-to-Dispatch",
           sample = "Omitting Shots Fired",
           estimator = "OLS")

d1_no_shots_2s <- did2s(dispatch_panel_noshots,
               yname = "entry_to_dispatch_1",
               first_stage = ~ ..ctrl,
               second_stage = ~treatment,
               treatment = "treatment",
               cluster_var = "district") %>% 
  tidy_reg(outcome = "Call-to-Dispatch",
           sample = "Omitting Shots Fired",
           estimator = "Gardner (2021)")

d1_official_treat <- dispatch_panel %>% 
  feols(entry_to_dispatch_1 ~ treatment_official + ..ctrl, cluster = ~district) %>% 
  tidy_reg(outcome = "Call-to-Dispatch",
           sample = "Official Activate Dates",
           estimator = "OLS")

d1_official_treat_2s <- did2s(dispatch_panel,
                        yname = "entry_to_dispatch_1",
                        first_stage = ~ ..ctrl,
                        second_stage = ~treatment_official,
                        treatment = "treatment_official",
                        cluster_var = "district") %>% 
  tidy_reg(outcome = "Call-to-Dispatch",
           sample = "Official Activate Dates",
           estimator = "Gardner (2021)")

# call to on scene --------------------------------------------------------

os_main <- dispatch_panel %>% 
  feols(entry_to_onscene_1 ~ treatment + ..ctrl, cluster = ~district) %>% 
  tidy_reg(outcome = "Call-to-On-Scene",
           sample = "Main Sample",
           estimator = "OLS")

os_main_2s <- did2s(dispatch_panel,
                    yname = "entry_to_onscene_1",
                    first_stage = ~ ..ctrl,
                    second_stage = ~treatment,
                    treatment = "treatment",
                    cluster_var = "district") %>% 
  tidy_reg(outcome = "Call-to-On-Scene",
          sample = "Main Sample",
          estimator = "Gardner (2021)")

os_2020 <- dispatch_panel %>%
  filter(year != 2020) %>% 
  feols(entry_to_onscene_1 ~ treatment + ..ctrl, cluster = ~district) %>% 
  tidy_reg(outcome = "Call-to-On-Scene",
           sample = "Omitting 2020",
           estimator = "OLS")


os_2020_2s <- did2s(dispatch_panel %>% filter(year !=2020),
               yname = "entry_to_onscene_1",
               first_stage = ~..ctrl,
               second_stage = ~treatment,
               treatment = "treatment",
               cluster_var = "district") %>% 
  tidy_reg(outcome = "Call-to-On-Scene",
           sample = "Omitting 2020",
           estimator = "Gardner (2021)")

os_nevertreat <- dispatch_panel %>%
  filter(never_treated == 0) %>% 
  feols(entry_to_onscene_1 ~ treatment + ..ctrl, cluster = ~district) %>% 
  tidy_reg(outcome = "Call-to-On-Scene",
           sample = "Omitting Never-Treated",
           estimator = "OLS")

os_nevertreat_2s <- did2s(dispatch_panel %>% filter(never_treated == 0),
               yname = "entry_to_onscene_1",
               first_stage = ~ ..ctrl,
               second_stage = ~treatment,
               treatment = "treatment",
               cluster_var = "district") %>% 
  tidy_reg(outcome = "Call-to-On-Scene",
           sample = "Omitting Never-Treated",
           estimator = "Gardner (2021)")

os_outliers <- dispatch_panel_outliers %>% 
  feols(entry_to_onscene_1 ~ treatment + ..ctrl, cluster = ~district) %>% 
  tidy_reg(outcome = "Call-to-On-Scene",
           sample = "Including Outliers",
           estimator = "OLS")
  

os_outliers_2s <- did2s(dispatch_panel_outliers,
               yname = "entry_to_onscene_1",
               first_stage = ~  ..ctrl,
               second_stage = ~treatment,
               treatment = "treatment",
               cluster_var = "district") %>% 
  tidy_reg(outcome = "Call-to-On-Scene",
           sample = "Including Outliers",
           estimator = "Gardner (2021)")

os_no_shots <- dispatch_panel_noshots %>% 
  feols(entry_to_onscene_1 ~ treatment + ..ctrl, cluster = ~district) %>% 
  tidy_reg(outcome = "Call-to-On-Scene",
           sample = "Omitting Shots Fired",
           estimator = "OLS")

os_no_shots_2s <- did2s(dispatch_panel_noshots,
               yname = "entry_to_onscene_1",
               first_stage = ~..ctrl,
               second_stage = ~treatment,
               treatment = "treatment",
               cluster_var = "district") %>% 
  tidy_reg(outcome = "Call-to-On-Scene",
           sample = "Omitting Shots Fired",
           estimator = "Gardner (2021)")

os_official_treat <- dispatch_panel %>% 
  feols(entry_to_onscene_1 ~ treatment_official + ..ctrl, cluster = ~district) %>% 
  tidy_reg(outcome = "Call-to-On-Scene",
           sample = "Official Activate Dates",
           estimator = "OLS")

os_official_treat_2s <- did2s(dispatch_panel,
                        yname = "entry_to_onscene_1",
                        first_stage = ~..ctrl,
                        second_stage = ~treatment_official,
                        treatment = "treatment_official",
                        cluster_var = "district") %>% 
  tidy_reg(outcome = "Call-to-On-Scene",
           sample = "Official Activate Dates",
           estimator = "Gardner (2021)")

forest_samples <- d1_main %>% 
  bind_rows(d1_main_2s, d1_2020, d1_2020_2s,
            d1_no_shots, d1_no_shots_2s,
            d1_outliers, d1_outliers_2s,
            d1_official_treat, d1_official_treat_2s,
            d1_nevertreat, d1_nevertreat_2s,
            os_main, os_main_2s,
            os_2020, os_2020_2s,
            os_no_shots, os_no_shots_2s,
            os_outliers, os_outliers_2s,
            os_official_treat, os_official_treat_2s,
            os_nevertreat, os_nevertreat_2s) %>% 
  mutate(estimate = round(estimate, 2)) %>%
  mutate(estimate_label = sprintf("%.3f", estimate))

forest_sample_plot <- forest_samples %>% 
  mutate(sample = fct_reorder(sample, -row_number())) %>% 
  ggplot(aes(sample, estimate, color = estimator, shape = estimator)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                position = position_dodge(width = 0.5),
                fatten = 4) +
  facet_wrap(~outcome) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  coord_flip() +
  labs(color = "", shape = "",
       x = "95% Confidence Interval and Point Estimate",
       y = "") +
  ggthemes::scale_color_stata() +
  theme_minimal() +
  theme(legend.position = "bottom")

