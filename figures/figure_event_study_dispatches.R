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

if (!exists("dispatch_panel")){
  dispatch_panel <- read_csv(here::here("analysis_data/xxdispatches_clevel.csv"))
  ## priority 1 dispatches only
  dispatch_panel_p1 <- dispatch_panel %>% 
    filter(priority_code ==1)
}

setFixest_fml(..ctrl = ~0| district + date +
                final_dispatch_description + hour)

## use the months(1) to change how many months you want in a bin
## use the months to treat to give the amount of leads/lags
es_data_dispatch <- dispatch_panel_p1 %>% 
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

# ## Column graph numbers
# number_sst_dispatches <- es_data_dispatch %>% 
#   summarize(fraction_sst_dispatches = sum(number_sst_dispatches)/sum(number_dispatches_1),
#             number_sst_dispatches = sum(number_sst_dispatches, na.rm = T), .by = c(time_to_treat)) %>% 
#   filter(time_to_treat < 12 & time_to_treat > -12)


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
  feols(entry_to_dispatch ~ i(time_to_treat, ref = c(-1, -1000)) +
          ..ctrl,
        cluster = ~district, data = .) %>% 
  broom::tidy(conf.int = T) %>% 
  add_row(term = "0", 
          estimate = 0,
          .before = 12) %>% 
  slice(1:37) %>% 
  mutate(periods = c(-12:24)) %>% 
  mutate(type = "OLS")


entry_1_2sdid <- did2s(es_data_dispatch,
                       yname = "entry_to_dispatch",
                       first_stage = ~..ctrl,
                       second_stage = ~ i(time_to_treat, ref = c(-1, -1000)),
                       treatment = "treatment",
                       cluster_var = "district") %>% 
  broom::tidy(conf.int = T) %>% 
  add_row(term = "0", 
          estimate = 0,
          .before = 12) %>% 
  mutate(periods = c(-12:24)) %>% 
  mutate(type = "Gardner (2022)")

entry_1_es <- entry_1 %>% 
  bind_rows(entry_1_2sdid) %>% 
  filter(periods %in% c(-11:23)) %>% 
  event_study_graph()

  


# on scene 1----------------------------------------------------------------


eos_1 <- es_data_dispatch %>% 
  feols(entry_to_onscene ~ i(time_to_treat, ref = c(-1, -1000)) +
          ..ctrl,
        cluster = ~district, data = .) %>% 
  broom::tidy(conf.int = T) %>% 
  add_row(term = "0", 
          estimate = 0,
          .before = 12) %>% 
  slice(1:37) %>% 
  mutate(periods = c(-12:24)) %>% 
  mutate(type = "OLS")


eos_1_2sdid <- did2s(es_data_dispatch,
                     yname = "entry_to_onscene",
                     first_stage = ~..ctrl,
                     second_stage = ~ i(time_to_treat, ref = c(-1, -1000)),
                     treatment = "treatment",
                     cluster_var = "district") %>% 
  broom::tidy(conf.int = T) %>% 
  add_row(term = "0", 
          estimate = 0,
          .before = 12) %>% 
  mutate(periods = c(-12:24)) %>% 
  mutate(type = "Gardner (2022)")

eos_1_es <- eos_1 %>% 
  bind_rows(eos_1_2sdid) %>% 
  filter(periods %in% c(-11:23)) %>% 
  event_study_graph()




# 
# 
# entry_1 %>% 
#   bind_rows(entry_1_2sdid) %>% 
#   filter(periods %in% c(-11:11)) %>% 
#   left_join(number_sst_alerts, join_by(periods == time_to_treat)) %>% 
#   event_study_graph() +
#   scale_y_continuous("95% Confidence Interval and Point Estimate", sec.axis = sec_axis(~./1000, name = "Number SST Alerts/Number Dispatches")) 
# 
# eos_1 %>% 
#   bind_rows(eos_1_2sdid) %>% 
#   filter(periods %in% c(-11:11)) %>% 
#   filter(periods %in% c(-11:11)) %>% 
#   left_join(number_sst_alerts, join_by(periods == time_to_treat)) %>% 
#   event_study_graph() +
#   scale_y_continuous("95% Confidence Interval and Point Estimate", sec.axis = sec_axis(~./1000, name = "Number SST Alerts/Number Dispatches")) 
event_study_graph_overlay <- function(x){
  graph <- x %>%
    ggplot(aes(periods, estimate, color = type, shape = type)) +
    geom_line(aes(x = periods, y = number_sst_dispatches/10, color = NULL,shape = NULL), alpha = 0.3, position = "dodge", fill = "grey") +
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


# 
# entry_1_es_overlay <- entry_1 %>%
#   bind_rows(entry_1_2sdid) %>%
#   filter(periods %in% c(-11:11)) %>%
#   left_join(number_sst_dispatches, join_by(periods == time_to_treat)) %>%
#   event_study_graph_overlay() +
#   scale_y_continuous("95% Confidence Interval and Point Estimate", sec.axis = sec_axis(~.*10, name = "Number SST Dispatches"))
# 
# eos_1_es_overlay <- eos_1 %>%
#   bind_rows(eos_1_2sdid) %>%
#   filter(periods %in% c(-11:11)) %>%
#   filter(periods %in% c(-11:11)) %>%
#   left_join(number_sst_dispatches, join_by(periods == time_to_treat)) %>%
#   event_study_graph_overlay() +
#   scale_y_continuous("95% Confidence Interval and Point Estimate", sec.axis = sec_axis(~.*10, name = "Number SST Dispatches/Number Dispatches"))
# 
