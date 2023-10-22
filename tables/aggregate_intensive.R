## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-05-17
##

library(tidyverse)
library(fixest)
library(modelsummary)
library(panelsummary)
library(kableExtra)
library(did2s)

if (!exists("dispatch_panel")){
  dispatch_panel <- read_csv(here::here("analysis_data/xxdispatches_clevel.csv"))
  dispatch_panel_p1 <- dispatch_panel %>% 
    filter(priority_code == 1)
}

rollout_dates <- read_csv("created_data/rollout_dates.csv") %>% 
  mutate(across(c(-1), ~mdy(.)))

officer_hours <- read_csv("analysis_data/officer_hours.csv")

sst_alerts <- read_csv("analysis_data/sst_dispatches_cpd.csv") %>% 
  filter(year < 2023) 


aggregate_outcomes <- dispatch_panel_p1 %>% 
  group_by(district, date) %>% 
  summarize(across(c(entry_to_dispatch,
                   entry_to_onscene), ~ mean(.,na.rm = T))) %>% 
  ungroup()


aggregate_outcomes <- aggregate_outcomes %>% 
  left_join(rollout_dates) %>% 
  mutate(treatment = if_else(shotspot_activate <= date, 1, 0), 
         treatment_sdsc = if_else(sdsc <= date, 1, 0 ),
         treatment_official = if_else(shotspot_activate_official <= date, 1, 0),
         treatment_first_shot = if_else(shotspot_activate_first_shot <= date, 1, 0),
         treatment_cpd = if_else(shotspot_activate_cpd <= date, 1, 0),
         .by = district) %>% 
  mutate(never_treated = if_else(is.na(treatment),1, 0), .by = district) %>% 
  mutate(treatment = if_else(is.na(treatment), 0, treatment
  ),
  treatment_sdsc = if_else(is.na(treatment_sdsc), 0 , treatment_sdsc),
  treatment_official = if_else(is.na(treatment_official), 0, treatment_official),
  treatment_first_shot = if_else(is.na(treatment_first_shot), 0, treatment_first_shot),
  .by = district) 

aggregate_outcomes <- aggregate_outcomes %>% 
  left_join(officer_hours) %>% 
  left_join(officer_hours_median)

aggregate_outcomes <- aggregate_outcomes %>% 
  left_join(sst_alerts,by = join_by(district, date))


aggregate_outcomes <- aggregate_outcomes %>%
  mutate(number_sst_dispatches = if_else(is.na(number_sst_dispatches), 0, number_sst_dispatches))

aggregate_outcomes %>% 
  filter(treatment == 1 | never_treated == 1) %>% 
  filter(officer_hours < officer_hours_median) %>%
  feols(entry_to_dispatch ~ number_sst_dispatches | district + date)

  # filter(officer_hours < officer_hours_median) %>% 
  feols(entry_to_onscene ~ number_sst_dispatches | district + date)
