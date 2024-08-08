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

## translate this into code for the final analysis.

disposition_codes <- read_csv("raw_data/Chicago_Police_Department_-_Illinois_Uniform_Crime_Reporting__IUCR__Codes_20240801.csv") %>% 
  janitor::clean_names()

disposition_codes <- disposition_codes %>% 
  mutate(injury_likely = if_else(
    primary_description %in% c("HOMICIDE", "BATTERY", "ASSAULT"),
    1, 0
  )) 

disposition_codes <- disposition_codes %>% 
  mutate(iucr = paste0("0", iucr)) %>% 
  mutate(iucr = str_extract(iucr, ".{4}$")) 

## need to add in other arrest mades. This doesn't change the results as far as I have looked.
## 
dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  tidylog::mutate(arrest_made = if_else(str_detect(final_disposition_code, "R") | arrest_made == 1, 1, 0)) %>% 
  mutate(arrest_made = replace_na(arrest_made, 0)) 


dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  tidylog::left_join(disposition_codes, join_by(final_disposition_code == iucr)) %>% 
  mutate(injury_likely = replace_na(injury_likely, 0))


## adding in victim_injuries from description
dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  tidylog::mutate(victim_injury_descr = if_else(str_detect(final_dispatch_description, "INJ"), 1, 0) %>% 
                    replace_na(0)) %>% 
  tidylog::mutate(victim_injury_dispo = if_else(str_detect(final_disposition_description, "INJ"), 1, 0) %>% 
                    replace_na(0)) %>% 
  tidylog::mutate(victim_injury = if_else(victim_injury_descr== 1, 1, victim_injury),
                  victim_injury = if_else(victim_injury_dispo == 1, 1, victim_injury)) 


## creating new victim_injury_liekly variable which includes dispositions where it's likely person was injured
dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  mutate(victim_injury_likely = if_else(victim_injury == 1 | injury_likely == 1, 1, 0)) 
  

## percentage
dispatch_panel_p1 <- dispatch_panel_p1 %>% 
  mutate(victim_injury_likely_p = victim_injury_likely * 100)

dispatch_panel_p1 %>% 
  summarize(mean(victim_injury_likely, na.rm = T))

# regressions -------------------------------------------------------------

setFixest_fml(..ctrl = ~0| district + date +
                final_dispatch_description + hour)


## full sample of time-sensitive calls
victim_1 <- dispatch_panel_p1 %>% 
  feols(victim_injury_likely*100 ~ treatment + ..ctrl)

## time sensitive call with gun is person with a gun or shots fired
victim_gun <- dispatch_panel_p1 %>% 
  filter(gun_crime_report == 1) %>% 
  feols(victim_injury_likely*100 ~ treatment + ..ctrl)

## non-time sensitive stuff is everything else that can be put into the table
victim_no_gun <- dispatch_panel_p1 %>% 
  filter(gun_crime_report == 0) %>% 
  feols(victim_injury_likely*100~ treatment + ..ctrl)


# 2sdid -------------------------------------------------------------------

victim_1_2s <- did2s(data = dispatch_panel_p1 ,
      yname = "victim_injury_likely_p",
      first_stage = ~0|district + date + 
        final_dispatch_code + hour,
      second_stage = ~treatment,
      treatment = "treatment",
      cluster_var = "district")

victim_gun_2s <- did2s(data = dispatch_panel_p1 %>% 
        filter(gun_crime_report == 1),
      yname = "victim_injury_likely_p",
      first_stage = ~0|district + date + 
        final_dispatch_code + hour,
      second_stage = ~treatment,
      treatment = "treatment",
      cluster_var = "district")

victim_no_gun_2s <- did2s(data = dispatch_panel_p1 %>% 
        filter(gun_crime_report == 0),
      yname = "victim_injury_likely_p",
      first_stage = ~0|district + date + 
        final_dispatch_code + hour,
      second_stage = ~treatment,
      treatment = "treatment",
      cluster_var = "district")


## non-time sensitive stuff is everything else that can be put into the table
dispatch_panel_p1 %>% 
  filter(gun_crime_report == 1) %>% 
  feglm(victim_injury_likely~ treatment + ..ctrl,
        family = "logit")

