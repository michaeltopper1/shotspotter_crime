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

if (!exists("dispatch_panel")){
  dispatch_panel <- read_csv(here::here("analysis_data/xxdispatches_clevel.csv"))
  dispatch_panel_p1 <- dispatch_panel %>% 
    filter(priority_code == 1)
}

dispatch_panel_outliers <- read_csv(here::here("analysis_data/xxdispatches_clevel_outliers.csv")) %>% 
  filter(priority_code == 1)

dispatch_panel_outliers <- dispatch_panel_outliers %>% 
  mutate(date = as_date(entry_received_date),
          year = year(date),
          month = month(date),
          day = day(date),
          year_month = mdy(paste0(month, "-1-", year)))

setFixest_fml(..ctrl = ~0| district + date +
                final_dispatch_description + hour)

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

d1_main <- dispatch_panel_p1 %>% 
  feols(entry_to_dispatch ~ treatment + ..ctrl, cluster = ~district) %>% 
  tidy_reg(outcome = "Call-to-Dispatch",
           sample = "Main Sample",
           estimator = "OLS")
  

d1_main_2s <- did2s(dispatch_panel_p1,
               yname = "entry_to_dispatch",
               first_stage = ~ ..ctrl,
               second_stage = ~treatment,
               treatment = "treatment",
               cluster_var = "district") %>% 
  tidy_reg(outcome = "Call-to-Dispatch",
           sample = "Main Sample",
           estimator = "Gardner (2021)")


d1_2020 <- dispatch_panel_p1 %>%
  filter(year != 2020) %>% 
  feols(entry_to_dispatch ~ treatment + ..ctrl, cluster = ~district) %>% 
  tidy_reg(outcome = "Call-to-Dispatch",
           sample = "Omitting 2020",
           estimator = "OLS")
  

d1_2020_2s <- did2s(dispatch_panel_p1 %>% filter(year !=2020),
               yname = "entry_to_dispatch",
               first_stage = ~ ..ctrl,
               second_stage = ~treatment,
               treatment = "treatment",
               cluster_var = "district") %>% 
  tidy_reg(outcome = "Call-to-Dispatch",
           sample = "Omitting 2020",
           estimator = "Gardner (2021)")

d1_nevertreat <- dispatch_panel_p1 %>%
  filter(never_treated == 0) %>% 
  feols(entry_to_dispatch ~ treatment + ..ctrl, cluster ~district) %>% 
  tidy_reg(outcome = "Call-to-Dispatch",
           sample = "Omitting Never-Treated",
           estimator = "OLS")
 

d1_nevertreat_2s <- did2s(dispatch_panel %>% filter(never_treated == 0),
               yname = "entry_to_dispatch",
               first_stage = ~ ..ctrl,
               second_stage = ~treatment,
               treatment = "treatment",
               cluster_var = "district") %>% 
  tidy_reg(outcome = "Call-to-Dispatch",
           sample = "Omitting Never-Treated",
           estimator = "Gardner (2021)")

d1_outliers <- dispatch_panel_outliers %>% 
  filter(!(month ==7 & day == 4)) %>% 
  filter(!(month == 1 & day == 1)) %>% 
  filter(!(month == 12 & day == 31)) %>% 
  feols(entry_to_dispatch ~ treatment + ..ctrl, cluster = ~district) %>% 
  tidy_reg(outcome = "Call-to-Dispatch",
           sample = "Including Outliers",
           estimator = "OLS")

d1_outliers_2s <- did2s(dispatch_panel_outliers %>% 
                          filter(!(month ==7 & day == 4)) %>% 
                          filter(!(month == 1 & day == 1)) %>% 
                          filter(!(month == 12 & day == 31)),
               yname = "entry_to_dispatch",
               first_stage = ~ ..ctrl,
               second_stage = ~treatment,
               treatment = "treatment",
               cluster_var = "district") %>% 
  tidy_reg(outcome = "Call-to-Dispatch",
           sample = "Including Outliers",
           estimator = "Gardner (2021)")

d1_no_shots <- dispatch_panel_p1 %>%
  filter(gun_crime_report != 1) %>% 
  feols(entry_to_dispatch ~ treatment + ..ctrl, cluster = ~district) %>% 
  tidy_reg(outcome = "Call-to-Dispatch",
           sample = "Omitting Shots Fired",
           estimator = "OLS")

d1_no_shots_2s <- did2s(dispatch_panel_p1 %>%
                          filter(gun_crime_report != 1),
               yname = "entry_to_dispatch",
               first_stage = ~ ..ctrl,
               second_stage = ~treatment,
               treatment = "treatment",
               cluster_var = "district") %>% 
  tidy_reg(outcome = "Call-to-Dispatch",
           sample = "Omitting Shots Fired",
           estimator = "Gardner (2021)")

d1_official_treat <- dispatch_panel_p1 %>% 
  feols(entry_to_dispatch ~ treatment_official + ..ctrl, cluster = ~district) %>% 
  tidy_reg(outcome = "Call-to-Dispatch",
           sample = "Official Activate Dates",
           estimator = "OLS")

d1_official_treat_2s <- did2s(dispatch_panel_p1,
                        yname = "entry_to_dispatch",
                        first_stage = ~ ..ctrl,
                        second_stage = ~treatment_official,
                        treatment = "treatment_official",
                        cluster_var = "district") %>% 
  tidy_reg(outcome = "Call-to-Dispatch",
           sample = "Official Activate Dates",
           estimator = "Gardner (2021)")

d1_include_nye <- dispatch_panel_outliers %>% 
  filter(entry_to_onscene_outlier != 1) %>% 
  filter(entry_to_dispatch_outlier != 1) %>% 
  feols(entry_to_dispatch ~ treatment + ..ctrl, cluster = ~district) %>% 
  tidy_reg(outcome = "Call-to-Dispatch",
           sample = "Including July 4/NYE",
           estimator = "OLS")

d1_include_nye_2s <- did2s(dispatch_panel_outliers %>% 
        filter(entry_to_onscene_outlier != 1) %>% 
        filter(entry_to_dispatch_outlier != 1),
      yname = "entry_to_dispatch",
      first_stage = ~ ..ctrl,
      second_stage = ~treatment,
      treatment = "treatment",
      cluster_var = "district") %>% 
  tidy_reg(outcome = "Call-to-Dispatch",
           sample = "Including July 4/NYE",
           estimator = "Gardner (2021)")

# call to on scene --------------------------------------------------------

os_main <- dispatch_panel_p1 %>% 
  feols(entry_to_onscene ~ treatment + ..ctrl, cluster = ~district) %>% 
  tidy_reg(outcome = "Call-to-On-Scene",
           sample = "Main Sample",
           estimator = "OLS")

os_main_2s <- did2s(dispatch_panel_p1,
                    yname = "entry_to_onscene",
                    first_stage = ~ ..ctrl,
                    second_stage = ~treatment,
                    treatment = "treatment",
                    cluster_var = "district") %>% 
  tidy_reg(outcome = "Call-to-On-Scene",
          sample = "Main Sample",
          estimator = "Gardner (2021)")

os_2020 <- dispatch_panel_p1 %>%
  filter(year != 2020) %>% 
  feols(entry_to_onscene ~ treatment + ..ctrl, cluster = ~district) %>% 
  tidy_reg(outcome = "Call-to-On-Scene",
           sample = "Omitting 2020",
           estimator = "OLS")


os_2020_2s <- did2s(dispatch_panel_p1 %>% filter(year !=2020),
               yname = "entry_to_onscene",
               first_stage = ~..ctrl,
               second_stage = ~treatment,
               treatment = "treatment",
               cluster_var = "district") %>% 
  tidy_reg(outcome = "Call-to-On-Scene",
           sample = "Omitting 2020",
           estimator = "Gardner (2021)")

os_nevertreat <- dispatch_panel_p1 %>%
  filter(never_treated == 0) %>% 
  feols(entry_to_onscene ~ treatment + ..ctrl, cluster = ~district) %>% 
  tidy_reg(outcome = "Call-to-On-Scene",
           sample = "Omitting Never-Treated",
           estimator = "OLS")

os_nevertreat_2s <- did2s(dispatch_panel_p1 %>% filter(never_treated == 0),
               yname = "entry_to_onscene",
               first_stage = ~ ..ctrl,
               second_stage = ~treatment,
               treatment = "treatment",
               cluster_var = "district") %>% 
  tidy_reg(outcome = "Call-to-On-Scene",
           sample = "Omitting Never-Treated",
           estimator = "Gardner (2021)")

os_outliers <- dispatch_panel_outliers %>% 
  filter(!(month ==7 & day == 4)) %>% 
  filter(!(month == 1 & day == 1)) %>% 
  filter(!(month == 12 & day == 31)) %>% 
  feols(entry_to_onscene ~ treatment + ..ctrl, cluster = ~district) %>% 
  tidy_reg(outcome = "Call-to-On-Scene",
           sample = "Including Outliers",
           estimator = "OLS")
  

os_outliers_2s <- did2s(dispatch_panel_outliers %>% 
                        filter(!(month ==7 & day == 4)) %>% 
                          filter(!(month == 1 & day == 1)) %>% 
                          filter(!(month == 12 & day == 31)),
               yname = "entry_to_onscene",
               first_stage = ~  ..ctrl,
               second_stage = ~treatment,
               treatment = "treatment",
               cluster_var = "district") %>% 
  tidy_reg(outcome = "Call-to-On-Scene",
           sample = "Including Outliers",
           estimator = "Gardner (2021)")

os_no_shots <- dispatch_panel_p1 %>%
  filter(gun_crime_report != 1) %>% 
  feols(entry_to_onscene ~ treatment + ..ctrl, cluster = ~district) %>% 
  tidy_reg(outcome = "Call-to-On-Scene",
           sample = "Omitting Shots Fired",
           estimator = "OLS")

os_no_shots_2s <- did2s(dispatch_panel_p1 %>%
                          filter(gun_crime_report != 1) ,
               yname = "entry_to_onscene",
               first_stage = ~..ctrl,
               second_stage = ~treatment,
               treatment = "treatment",
               cluster_var = "district") %>% 
  tidy_reg(outcome = "Call-to-On-Scene",
           sample = "Omitting Shots Fired",
           estimator = "Gardner (2021)")

os_official_treat <- dispatch_panel_p1 %>% 
  feols(entry_to_onscene ~ treatment_official + ..ctrl, cluster = ~district) %>% 
  tidy_reg(outcome = "Call-to-On-Scene",
           sample = "Official Activate Dates",
           estimator = "OLS")

os_official_treat_2s <- did2s(dispatch_panel_p1,
                        yname = "entry_to_onscene",
                        first_stage = ~..ctrl,
                        second_stage = ~treatment_official,
                        treatment = "treatment_official",
                        cluster_var = "district") %>% 
  tidy_reg(outcome = "Call-to-On-Scene",
           sample = "Official Activate Dates",
           estimator = "Gardner (2021)")

os_include_nye <- dispatch_panel_outliers %>% 
  filter(entry_to_onscene_outlier != 1) %>% 
  filter(entry_to_dispatch_outlier != 1) %>% 
  feols(entry_to_onscene ~ treatment + ..ctrl, cluster = ~district) %>% 
  tidy_reg(outcome = "Call-to-On-Scene",
           sample = "Including July 4/NYE",
           estimator = "OLS")

os_include_nye_2s <- did2s(dispatch_panel_outliers %>% 
                             filter(entry_to_onscene_outlier != 1) %>% 
                             filter(entry_to_dispatch_outlier != 1),
                           yname = "entry_to_onscene",
                           first_stage = ~ ..ctrl,
                           second_stage = ~treatment,
                           treatment = "treatment",
                           cluster_var = "district") %>% 
  tidy_reg(outcome = "Call-to-On-Scene",
           sample = "Including July 4/NYE",
           estimator = "Gardner (2021)")




forest_samples <- d1_main %>% 
  bind_rows(d1_main_2s, d1_2020, d1_2020_2s,
            d1_no_shots, d1_no_shots_2s,
            d1_outliers, d1_outliers_2s,
            d1_official_treat, d1_official_treat_2s,
            d1_include_nye, d1_include_nye_2s,
            d1_nevertreat, d1_nevertreat_2s,
            os_main, os_main_2s,
            os_2020, os_2020_2s,
            os_no_shots, os_no_shots_2s,
            os_outliers, os_outliers_2s,
            os_official_treat, os_official_treat_2s,
            os_include_nye, os_include_nye_2s,
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

ggsave(forest_sample_plot, filename = "paper/appendix_figures/forest_sample_plot.jpeg",
       width = 7, height = 5)
