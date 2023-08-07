## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-05-16
##

library(tidyverse)
library(fixest)
library(panelsummary)
library(kableExtra)
library(modelsummary)
library(did2s)

if(!exists("dispatch_panel")) {
  dispatch_panel <- read_csv(here::here("analysis_data/xxdispatch_panel.csv"))
}
## use the months(1) to change how many months you want in a bin
## use the months to treat to give the amount of leads/lags
es_data_dispatch <- dispatch_panel %>% 
  mutate(time_to_treat = time_length(as_date(date) - shotspot_activate,
                                     "month") %>% 
           magrittr::add(1) %>% 
           floor() %>%
           magrittr::subtract(1),
         .by = district) %>% 
  mutate(time_to_treat = case_when(
    time_to_treat > 24 ~ 24,
    time_to_treat < -12 ~ -12,
    .default = time_to_treat
  )) %>% 
  mutate(time_to_treat = if_else(is.na(time_to_treat), -1000, time_to_treat)) 

## Column graph numbers
number_sst_dispatches <- es_data_dispatch %>% 
  summarize(number_sst_dispatches = sum(number_sst_dispatches)/sum(number_dispatches_1), .by = c(time_to_treat)) %>% 
  filter(time_to_treat < 12 & time_to_treat > -12)


event_study_graph <- function(x){
  graph <- x %>% 
    ggplot(aes(periods, estimate, color = type, shape = type)) +
    geom_point(position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = 0.5)) +
    geom_hline(yintercept = 0, color = "dark red") +
    labs(x = "Months to Adoption",
         y = "95% Confidence Interval and Point Estimate",
         color = "",
         shape = "") +
    theme_minimal() +
    ggthemes::scale_color_stata() +
    theme(legend.position = "bottom")
  return(graph)
}



# entry to dispatch 1 -----------------------------------------------------


entry_1 <- es_data_dispatch %>% 
  feols(entry_to_dispatch_1 ~ i(time_to_treat, ref = c(-1, -1000)) +
          number_dispatches_1 + number_dispatches_2 + number_dispatches_3 +
          number_dispatches_0 +
          officer_hours |district + date,
        cluster = ~district, data = .) %>% 
  broom::tidy(conf.int = T) %>% 
  add_row(term = "0", 
          estimate = 0,
          .before = 12) %>% 
  slice(1:37) %>% 
  mutate(periods = c(-12:24)) %>% 
  mutate(type = "TWFE")


entry_1_2sdid <- did2s(es_data_dispatch,
                       yname = "entry_to_dispatch_1",
                       first_stage = ~number_dispatches_1 + number_dispatches_2 + number_dispatches_3 +
                         number_dispatches_0 + officer_hours | district + date,
                       second_stage = ~ i(time_to_treat, ref = c(-1, -1000)),
                       treatment = "treatment",
                       cluster_var = "district") %>% 
  broom::tidy(conf.int = T) %>% 
  add_row(term = "0", 
          estimate = 0,
          .before = 12) %>% 
  mutate(periods = c(-12:24)) %>% 
  mutate(type = "Gardner (2021)")

entry_1_es_longer <- entry_1 %>% 
  bind_rows(entry_1_2sdid) %>% 
  filter(periods %in% c(-11:23)) %>% 
  event_study_graph()



# on scene 1----------------------------------------------------------------


eos_1 <- es_data_dispatch %>% 
  feols(entry_to_onscene_1 ~ i(time_to_treat, ref = c(-1, -1000)) +
          number_dispatches_1 + number_dispatches_2 + number_dispatches_3 +
          number_dispatches_0 +
          officer_hours |district + date,
        cluster = ~district, data = .) %>% 
  broom::tidy(conf.int = T) %>% 
  add_row(term = "0", 
          estimate = 0,
          .before = 12) %>% 
  slice(1:37) %>% 
  mutate(periods = c(-12:24)) %>% 
  mutate(type = "TWFE")


eos_1_2sdid <- did2s(es_data_dispatch,
                     yname = "entry_to_onscene_1",
                     first_stage = ~number_dispatches_1 + number_dispatches_2 + number_dispatches_3 +
                       number_dispatches_0 +
                       officer_hours |district + date,
                     second_stage = ~ i(time_to_treat, ref = c(-1, -1000)),
                     treatment = "treatment",
                     cluster_var = "district") %>% 
  broom::tidy(conf.int = T) %>% 
  add_row(term = "0", 
          estimate = 0,
          .before = 12) %>% 
  mutate(periods = c(-12:24)) %>% 
  mutate(type = "Gardner (2021)")

eos_1_es_longer <- eos_1 %>% 
  bind_rows(eos_1_2sdid) %>% 
  filter(periods %in% c(-11:23)) %>% 
  event_study_graph()




# 
# 
# entry_1 %>% 
#   bind_rows(entry_1_2sdid) %>% 
#   filter(periods %in% c(-11:11)) %>% 
#   left_join(number_sst_dispatches, join_by(periods == time_to_treat)) %>% 
#   event_study_graph() +
#   scale_y_continuous("95% Confidence Interval and Point Estimate", sec.axis = sec_axis(~./1000, name = "Number SST Alerts/Number Dispatches")) 
# 
# eos_1 %>% 
#   bind_rows(eos_1_2sdid) %>% 
#   filter(periods %in% c(-11:11)) %>% 
#   filter(periods %in% c(-11:11)) %>% 
#   left_join(number_sst_dispatches, join_by(periods == time_to_treat)) %>% 
#   event_study_graph() +
#   scale_y_continuous("95% Confidence Interval and Point Estimate", sec.axis = sec_axis(~./1000, name = "Number SST Alerts/Number Dispatches")) 
# event_study_graph <- function(x){
#   graph <- x %>% 
#     ggplot(aes(periods, estimate, color = type, shape = type)) +
#     geom_col(aes(x = periods, y = number_sst_dispatches*300, color = NULL,shape = NULL), alpha = 0.3, position = "dodge", fill = "grey") +
#     geom_point(position = position_dodge(width = 0.5)) +
#     geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
#                   position = position_dodge(width = 0.5)) +
#     geom_hline(yintercept = 0, color = "dark red") +
#     labs(x = "Months to Adoption",
#          y = "95% Confidence Interval and Point Estimate",
#          color = "",
#          shape = "") +
#     theme_minimal() +
#     ggthemes::scale_color_stata() +
#     theme(legend.position = "bottom")
#   return(graph)
# }
