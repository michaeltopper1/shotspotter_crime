## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-09-05
##

library(tidyverse)


# shotspotter dispatches --------------------------------------------------

sst <- read_csv("created_data/sst_dispatch_cpd.csv")

## filtering to only past february which is the first date
## of a shotspotter-related arrest
sst_feb <- sst %>% 
  filter(date >= as_date("2019-02-19"))

# shotspotter evidence found ----------------------------------------------

sst_ev <- readxl::read_excel("raw_data/shotspotter_arrests_events.xlsx",
                                  sheet = "Data Set II") %>% 
  janitor::clean_names() %>% 
  mutate(event_number = as.double(event_number),
         associated_event_number = as.double(associated_event_number))


# shotspotter related arrests ---------------------------------------------

sst_arrests <- readxl::read_excel("raw_data/shotspotter_arrests_events.xlsx",
                                  sheet = "Data Set I") %>% 
  janitor::clean_names()



# all arrests -------------------------------------------------------------

arrests <- read_csv("raw_data/arrests.csv") %>% 
  janitor::clean_names()


# number of sst dispatches ending in arrest -------------------------------
nrow(sst_arrests)/nrow(sst_feb)




# probability of 911 call ending in arrest pre-shotspotter ----------------
## this only looks at shotspotter implemented districts before their
## implementation

dispatch_panel_p1 %>% 
  filter(never_treated == 0) %>% 
  filter(treatment == 0) %>% 
  count(arrest_made,gun_crime_report) %>% 
  mutate(total = sum(n), .by = gun_crime_report) %>% 
  mutate(n/total, .by = arrest_made) %>% 
  arrange(gun_crime_report)



# cleaning sst arrests ----------------------------------------------------

## double booking number, indicator firearm arrest, change to date
sst_arrests <- sst_arrests %>% 
  mutate(cb_no = as.double(cb_no),
         arrest_firearm = if_else(str_detect(armed_with, "FIREARM|GUN|RIFLE"), 
                                  1, 0)) %>% 
  mutate(arrest_datetime = dmy_hm(arrest_datetime))

sst_arrests_merged <- sst_arrests %>% 
  left_join(arrests, join_by(cb_no == cb_no)) %>% 
  mutate(race = case_when(
    race == "ASIAN / PACIFIC ISLANDER" ~ "Asian",
    race == "BLACK" ~ "Black",
    race == "WHITE" ~ "White",
    race == "WHITE HISPANIC" ~ "Hispanic",
    race == "UNKNOWN / REFUSED" ~ NA,
    race == "BLACK HISPANIC" ~ "Black",
    .default = NA
  ))

sst_arrests_merged %>% 
  count(arrest_firearm, race) %>% 
  mutate(total = sum(n),
         fraction = n/total, .by = arrest_firearm) %>% 
  mutate(race = fct_reorder(race, fraction), .by = arrest_firearm) %>% 
  ggplot(aes(race, n)) +
  geom_col() +
  geom_text(aes(label = round(fraction, 2)), vjust = -1) +
  facet_wrap(~arrest_firearm) +
  theme_minimal()
  



sst <- sst %>% 
  mutate(date = as_date(entry_received_date))

  
sst_arrests %>% 
  distinct()
# number of sst dispatches ending in arrest -------------------------------
nrow(sst_arrests)/nrow(sst_feb)

dispatch_panel_p1 %>% 
  summarize(mean(arrest_made))

# number ending in firearm arrest -----------------------------------------
nrow(sst_arrests %>% filter(arrest_firearm == 1))/nrow(sst_feb)

dispatch_panel_p1 %>% 
  filter(never_treated == 0) %>% 
  filter(treatment == 0) %>% 
  count(arrest_made,gun_crime_report) %>% 
  mutate(total = sum(n), .by = gun_crime_report) %>% 
  mutate(n/total, .by = arrest_made) %>% 
  arrange(gun_crime_report)




