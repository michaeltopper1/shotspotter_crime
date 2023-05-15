## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-04-08
##

library(tidyverse)


# loading data ------------------------------------------------------------


crimes <- read_csv("created_data/crimes_cleaned.csv")

rollout_dates <- read_csv("created_data/rollout_dates.csv")

codebook <- read_csv("created_data/crime_codebook_cleaned.csv")


# joining codebook --------------------------------------------------------

crimes <- crimes %>%
  left_join(codebook, join_by(fbi_code == fbi_code,
                              iucr == iucr_code)) 



# getting gun crimes ------------------------------------------------------

crimes <- crimes %>% 
  mutate(across(c(gun_involved, unlawful_sale_gun), ~ifelse(is.na(.), 0, .)))

crimes <- crimes %>% 
  mutate(gun_involved = ifelse(unlawful_sale_gun == 1 & gun_involved ==1, 0, gun_involved)) %>% 
  mutate(gun_involved_arrest = ifelse(gun_involved == 1 & arrest == 1, 1, 0)) %>% 
  mutate(gun_involved_no_arrest = ifelse(gun_involved == 1 & arrest == 0, 1, 0))

## creating panels for each of the districts
panel_dates <- seq(as_date("2016-01-01"), as_date("2022-12-31") , by= "day") %>% 
  as_tibble() %>% 
  rename(date = value) 

districts <- c(1:20, 22, 24, 25) %>% 
  as_tibble() %>% 
  rename(district = value) %>% 
  filter(district != 13)

panel_dates <- panel_dates %>% 
  cross_join(districts)


# aggregating the crimes data ---------------------------------------------

crimes_aggregated <- crimes %>% 
  mutate(other_crime = ifelse(gun_involved == 0, 1, 0),
         other_arrest = ifelse(arrest == 1 & gun_involved_arrest !=1, 1, 0)) %>% 
  group_by(date, district) %>% 
  summarize(across(c(gun_involved, gun_involved_arrest, 
                     other_crime, other_arrest), ~sum(.,na.rm = T), .names = "number_{.col}")) %>% 
  ungroup()

crimes_panel <- panel_dates %>% 
  left_join(crimes_aggregated, by = join_by(date == date, 
                                       district == district))


# rollout dates -----------------------------------------------------------

crimes_panel <- crimes_panel %>% 
  left_join(rollout_dates) %>% 
  mutate(treatment = ifelse(shotspot_activate <= date, 1, 0), .by = district) %>% 
  mutate(never_treated = ifelse(is.na(treatment),1, 0), .by = district) %>% 
  mutate(treatment = ifelse(is.na(treatment), 0, treatment
  ), .by = district)


crimes_panel %>% 
  write_csv("analysis_data/crimes_panel.csv")

