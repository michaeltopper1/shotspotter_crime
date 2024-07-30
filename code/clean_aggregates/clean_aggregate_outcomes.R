## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2024-07-29
##

library(tidyverse)



# importing all data necessary --------------------------------------------


arrests <- readxl::read_excel("raw_data/arrests/20237-P833370_Arrest_Records_2016-2022__1___1___1_.xlsx",
                                sheet = "DataSet I") %>% 
  janitor::clean_names()

crimes <- read_csv("created_data/crimes_cleaned.csv")

## does not have all of the sdscs
rollout_dates <- read_csv("created_data/rollout_dates.csv")
rollout_dates <- rollout_dates %>% mutate(across(starts_with("shotspot"), ~mdy(.))) %>% 
  select(-sdsc)

## has the correct Max Kapustein dates for SDSCs
bwc <- read_csv("created_data/bwc_rollout.csv") %>% 
  mutate(bwc_date = mdy(bwc_date),
         sdsc_max = mdy(sdsc_max))

if (!exists("dispatch_panel")){
  dispatch_panel <- read_csv(here::here("analysis_data/xxdispatches_clevel.csv"))
  ## priority 1 dispatches only
  dispatch_panel_p1 <- dispatch_panel %>% 
    filter(priority_code ==1)
}

## sst alerts are the number of shotspotter dispatches
sst_alerts <- read_csv("analysis_data/sst_dispatches_cpd.csv") %>% 
  filter(year < 2023) 

# cleaning columns --------------------------------------------------------

## all of the data here is distinct CB numbers
## this data matches Toshio's arrest data very well, although this data
## actually has district/beat attached to it.


arrests <- arrests %>% 
  mutate(gun_arrest = if_else(str_detect(charge_description, "GUN|FIREARM"), 1,0)) %>% 
  mutate(district = parse_number(cpd_district))

arrests <- arrests %>% 
  mutate(arrest_datetime = mdy_hms(arrestdate),
         arrest_hour = hour(arrest_datetime))

arrests <- arrests %>% 
  mutate(arrest_date = as_date(arrest_datetime))

arrests <- arrests %>% 
  select(-c(arrestdate, arrest_address ))

arrests <- arrests %>% 
  drop_na(district) %>% 
  filter(district <= 25)

arrests <- arrests %>% 
  mutate(date = arrest_date) %>% 
  group_by(district, date) %>% 
  summarize(number_arrests = n(),
            number_gun_arrests = sum(gun_arrest, na.rm = T)) %>% 
  ungroup()


# cleaning victims --------------------------------------------------------

victims <- victims %>% 
  janitor::clean_names()

victims <- victims %>% 
  mutate(date = mdy_hms(date) %>% 
           as_date()) %>% 
  mutate(year = year(date)) %>% 
  filter(year >= 2016 & year <= 2022)

victims <- victims %>% 
  mutate(gun_victim = if_else(str_detect(incident_iucr_secondary, "GUN") | gunshot_injury_i == "YES", 1, 0))

victims <- victims %>% 
  filter(district < 30) %>% 
  drop_na(district) 

## gun involved victims will be those with a gun involved or gun injury.
victims <- victims %>% 
  group_by(district, date) %>% 
  summarize(number_victims = n(),
            number_gun_involved_victims = sum(gun_victim, na.rm = T)) %>% 
  ungroup()



# create panel to merge to ------------------------------------------------

districts <- arrests %>% 
  distinct(district) 

districts_panel <- seq(ymd("2016-01-01"), ymd("2022-12-31"), by = "day") %>% 
  as_tibble() %>% 
  rename(date = value) %>% 
  cross_join(districts)


arrests <- districts_panel %>% 
  left_join(arrests) %>% 
  mutate(across(starts_with("number"), ~if_else(is.na(.), 0, .)))


# combining treatment dates -----------------------------------------------


arrests <-  arrests %>% 
  left_join(rollout_dates) %>% 
  mutate(treatment = if_else(shotspot_activate <= date, 1, 0), 
         .by = district) %>% 
  mutate(never_treated = if_else(is.na(treatment),1, 0), .by = district) %>% 
  mutate(treatment = if_else(is.na(treatment), 0, treatment
  ),
  .by = district) 

arrests <- arrests %>% 
  left_join(bwc) %>% 
  mutate(treatment_bwc = if_else(date >= bwc_date, 1, 0),
         treatment_bwc = if_else(is.na(treatment_bwc), 0, treatment_bwc),
         treatment_sdsc = if_else(date >=sdsc_max, 1, 0),
         treatment_sdsc = if_else(is.na(treatment_sdsc), 0, treatment_sdsc),
         .by = district)


## dispatch panel and dispatch panel_p1 have same number
## of gun-related calls - which makes sense.
number_gun_calls <- dispatch_panel_p1 %>% 
  mutate(gun_call = if_else(str_detect(initial_dispatch_description, 
                               "PERSON WITH A GUN|SHOTS FIRED|PERSON SHOT"),
                            1, 0)) %>% 
  group_by(district, date) %>% 
  summarize(number_gun_calls = sum(gun_call, na.rm = T)) %>% 
  ungroup()


# merging gun calls and sst dispatches-----------------------------------------------------------------

arrests <- arrests %>% 
  left_join(number_gun_calls) %>% 
  mutate(number_gun_calls = if_else(is.na(number_gun_calls),0 , number_gun_calls))


arrests <- arrests %>% 
  left_join(sst_alerts) %>% 
  mutate(number_sst_dispatches = if_else(is.na(number_sst_dispatches), 0, number_sst_dispatches))


## this adds together the number of gun and SST dispatches to show that 
## there actually IS an increase in how much gun-related stuff officers need to respond to
arrests <- arrests %>% 
  mutate(number_gun_and_sst_calls = number_gun_calls + number_sst_dispatches)


# merging victims ---------------------------------------------------------

arrests <- arrests %>% 
  left_join(victims) %>% 
  mutate(across(c(number_victims, number_gun_involved_victims), ~if_else(is.na(.), 0, .)))


arrests %>% 
  write_csv("analysis_data/xxaggregate_outcomes.csv")
