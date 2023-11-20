library(tidyverse)
library(fixest)

if (!exists("dispatch_panel")){
  dispatch_panel <- read_csv(here::here("analysis_data/xxdispatches_clevel.csv"))
  dispatch_panel_p1 <- dispatch_panel %>% 
    filter(priority_code ==1)
}

officer_hours <- read_csv("analysis_data/officer_hours.csv")

rollout_dates <- read_csv("created_data/rollout_dates.csv") %>% 
  mutate(across(c(-1), ~mdy(.)))

sst_alerts <- read_csv("analysis_data/sst_dispatches_cpd.csv") %>% 
  filter(year < 2023) 

# aggregate for intensive margin ------------------------------------------

## aggregating to the district-day level
aggregate_outcomes <- dispatch_panel_p1 %>% 
  group_by(district, date) %>% 
  summarize(across(c(entry_to_dispatch,
                     entry_to_onscene), ~ mean(.,na.rm = T))) %>% 
  ungroup()

## creating the treatment variables at the district-day level
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

## merging in the number of officer hours and officer hours median
aggregate_outcomes <- aggregate_outcomes %>% 
  left_join(officer_hours) %>% 
  left_join(sst_alerts,by = join_by(district, date))

## creating the number of sst dispatches variable for 
## variation 
aggregate_outcomes <- aggregate_outcomes %>%
  mutate(number_sst_dispatches = if_else(is.na(number_sst_dispatches), 0, number_sst_dispatches))

## dispatch results
aggregate_outcomes %>% 
  mutate(officers = officer_hours/8) %>% 
  feols(entry_to_dispatch ~ officers + officers^2 + officers^3|district + date)
dispatch_panel_p1 %>% 
  mutate(officers = officer_hours/8) %>% 
  feols(entry_to_dispatch ~ officers + ..ctrl)



# marginal effect of officer on on-scene time ------------------------------


## these give the marginal effect of an additional police officer
onscene_estimates <- aggregate_outcomes %>% 
  mutate(officers = officer_hours/8) %>% 
  feols(entry_to_onscene ~ officers + officers^2|district + date) %>% 
  broom::tidy()
## 1 additional officer reduces on-scene times by 1.02 seconds
## given that the point estimates show 103.7 second increases this means
number_officers_needed <- 103.7/-(onscene_estimates$estimate[[1]] +2 *onscene_estimates$estimate[[2]])



# average officers within district ----------------------------------------

## getting average number of officer hours
officer_hours_avg <- aggregate_outcomes %>% 
  summarize(mean(officer_hours)) %>% pull()

## getting average number of officers
officer_avg <- officer_hours_avg/8

##finding percentage of force increase to get mitigate
number_officers_needed/officer_avg
