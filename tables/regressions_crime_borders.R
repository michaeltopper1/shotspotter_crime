## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-04-20
##

library(tidyverse)
library(modelsummary)
library(panelsummary)
library(fixest)
library(kableExtra)

# 
# TwoWayFEWeights::twowayfeweights(crimes,
#                                  "number_gun_involved_arrest",
#                                  "district",
#                                  "date",
#                                  "treatment",
#                                   cmd_type = "feTR")

crimes <- read_csv("analysis_data/crimes_panel.csv")

rollout_dates <- read_csv("created_data/rollout_dates.csv")

shotspotter_border_dates <- read_csv("created_data/border_district_key.csv")



# getting the border dates ------------------------------------------------

shotspotter_border_dates <- shotspotter_border_dates %>%
  left_join(rollout_dates) 

shotspotter_border_dates <- shotspotter_border_dates %>% 
  select(-district) %>% 
  rename(border_activate = shotspot_activate) %>% 
  filter(!is.na(border_activate)) %>% 
  arrange(border_district) %>% 
  group_by(border_district) %>% 
  summarize(border_treatment = min(border_activate)) %>% 
  ungroup()

crimes <- crimes %>% 
  left_join(shotspotter_border_dates,
            join_by(district == border_district)) %>% 
  relocate(border_treatment) %>% 
  group_by(district) %>% 
  mutate(shotspot_border_treatment = ifelse(date >= border_treatment,1 ,0 )) %>% 
  ungroup() %>% 
  mutate(shotspot_border_treatment = ifelse(is.na(shotspot_border_treatment), 0, shotspot_border_treatment))

crimes <- crimes %>% 
  mutate(year = year(date), month = month(date))



# number_gun_involved -----------------------------------------------------

c1_gun <- crimes %>% 
  feols(number_gun_involved ~ treatment + shotspot_border_treatment | date + district,
        cluster = ~district)

c2_gun <- crimes %>% 
  fepois(number_gun_involved ~ treatment + shotspot_border_treatment | date + district,
         cluster = ~district)

c3_gun <- crimes %>% 
  filter(year != 2020) %>% 
  feols(number_gun_involved ~ treatment + shotspot_border_treatment | date + district,
        cluster = ~district)

c4_gun <- crimes %>% 
  filter(year != 2020) %>% 
  fepois(number_gun_involved ~ treatment + shotspot_border_treatment | date + district,
         cluster = ~district)



# number_gun_arrested -----------------------------------------------------

c1_guna <- crimes %>% 
  feols(number_gun_involved_arrest ~ treatment + shotspot_border_treatment | date + district,
        cluster = ~district)

c2_guna <- crimes %>% 
  fepois(number_gun_involved_arrest ~ treatment + shotspot_border_treatment | date + district,
         cluster = ~district)

c3_guna <- crimes %>% 
  filter(year != 2020) %>% 
  feols(number_gun_involved_arrest ~ treatment + shotspot_border_treatment | date + district,
        cluster = ~district)

c4_guna <- crimes %>% 
  filter(year != 2020) %>% 
  fepois(number_gun_involved_arrest ~ treatment + shotspot_border_treatment | date + district,
         cluster = ~district)


# number other crime ------------------------------------------------------

c1_crime <- crimes %>% 
  feols(number_other_crime ~ treatment + shotspot_border_treatment | date + district,
        cluster = ~district)

c2_crime <- crimes %>% 
  fepois(number_other_crime ~ treatment + shotspot_border_treatment | date + district,
         cluster = ~district)

c3_crime <- crimes %>% 
  filter(year != 2020) %>% 
  feols(number_other_crime ~ treatment + shotspot_border_treatment | date + district,
        cluster = ~district)

c4_crime <- crimes %>% 
  filter(year != 2020) %>% 
  fepois(number_other_crime ~ treatment + shotspot_border_treatment | date + district,
         cluster = ~district)

# number other arrests ----------------------------------------------------

c1_crimea <- crimes %>% 
  feols(number_other_arrest ~ treatment + shotspot_border_treatment | date + district,
        cluster = ~district)

c2_crimea <- crimes %>% 
  fepois(number_other_arrest ~ treatment + shotspot_border_treatment | date + district,
         cluster = ~district)

c3_crimea <- crimes %>% 
  filter(year != 2020) %>% 
  feols(number_other_arrest ~ treatment + shotspot_border_treatment | date + district,
        cluster = ~district)

c4_crimea <- crimes %>% 
  filter(year != 2020) %>% 
  fepois(number_other_arrest ~ treatment + shotspot_border_treatment | date + district,
         cluster = ~district)


gof_mapping <- tribble(~raw, ~clean, ~fmt,
                       "nobs", "Observations", 0,
                       "FE: date", "FE: Day-by-Month-by-Year", 3,
                       "FE: district", "FE: District", 3)

footnotes <- map(list("* p < 0.1, ** p < 0.05, *** p < 0.01",
                      "Standard errors are clustered by district. 
                  ShotSpotter Border Activated refers to a district that is adjacent to a treated
                  district. A bordering district is considered treated if any of its adjacent
                  districts are treated.
                  Shotspotter is activated in 12 of the 22 police districts in Chicago.
                  Number Gun Crimes refers to all crimes that have a gun involved. For instance,
                  this can include a burglary or aggrevated assault with a firearm.
                  Number of Gun Arrests are the number of gun crimes that end in an arrest.
                  Number of Non-Gun Crimes are all crimes that have no firearm involved. Similarly,
                  not all of these end in arrests. Columns 3 and 4 omit year 2020 (Covid-19).
                  "), ~str_remove_all(., "\n"))

border_table <- panelsummary(list(c1_gun, c2_gun, c3_gun, c4_gun),
                          list(c1_guna, c2_guna, c3_guna, c4_guna),
                          list(c1_crime, c2_crime, c3_crime, c4_crime),
                          list(c1_crimea, c2_crimea, c3_crimea, c4_crimea),
                          mean_dependent = T,
                          collapse_fe =T,
                          panel_labels = c("Panel A: Number Gun Crimes", 
                                           "Panel B: Number Gun Arrests",
                                           "Panel C: Number Non-Gun Crimes",
                                           "Panel D: Number Non-Gun Arrests"),
                          coef_map = c( "treatment" = "ShotSpotter Activated",
                                        "shotspot_border_treatment" = "ShotSpotter Border Activated"),
                          gof_omit = "^R|A|B|S",
                          gof_map = gof_mapping,
                          italic  = T,
                          stars = "econ",
                          caption = "\\label{border_table}Effect of ShotSpotter Activations with Bordering Districts (OLS/Poisson)") %>% 
  add_header_above(c(" " = 1, 
                     "OLS" =1, 
                     "Poisson" =1,
                     "OLS" = 1,
                     "Poisson" = 1)) %>% 
  add_header_above(c(" " = 3, "Omitting 2020" = 2)) %>% 
  footnote(footnotes, threeparttable = T)



