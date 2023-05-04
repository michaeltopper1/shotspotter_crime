## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-04-13
##

library(tidyverse)
library(fixest)
library(modelsummary)
library(panelsummary)
library(kableExtra)

crimes_panel <- read_csv("analysis_data/crimes_panel.csv")

crimes_panel <- crimes_panel %>% 
  mutate(year = year(date),
         month = month(date)) 


## OLS/Poisson (All) OLS Poisson (only treated) OLS Poisson (2016-2019) OLS Poisson (only treated 2016-2019)


# number_gun_involved -----------------------------------------------------

c1_gun <- crimes_panel %>% 
  feols(number_gun_involved ~ treatment | date + district,
        cluster = ~district)

c2_gun <- crimes_panel %>% 
  fepois(number_gun_involved ~ treatment | date + district,
         cluster = ~district)

c3_gun <- crimes_panel %>% 
  filter(never_treated == 0) %>% 
  feols(number_gun_involved ~ treatment | date + district,
        cluster = ~district)

c4_gun <- crimes_panel %>% 
  filter(never_treated == 0) %>% 
  fepois(number_gun_involved ~ treatment | date + district,
         cluster = ~district)

c5_gun <- crimes_panel %>% 
  filter(year < 2020) %>% 
  feols(number_gun_involved ~ treatment | date + district,
        cluster = ~district)

c6_gun <- crimes_panel %>% 
  filter(year < 2020) %>% 
  fepois(number_gun_involved ~ treatment | date + district,
         cluster = ~district)

c7_gun <- crimes_panel %>% 
  filter(never_treated == 0) %>% 
  filter(year < 2020) %>% 
  feols(number_gun_involved ~ treatment | date + district,
        cluster = ~district)

c8_gun <- crimes_panel %>% 
  filter(never_treated == 0) %>% 
  filter(year < 2020) %>% 
  fepois(number_gun_involved ~ treatment | date + district,
         cluster = ~district)


# number_gun_arrested -----------------------------------------------------

c1_guna <- crimes_panel %>% 
  feols(number_gun_involved_arrest ~ treatment | date + district,
        cluster = ~district)

c2_guna <- crimes_panel %>% 
  fepois(number_gun_involved_arrest ~ treatment | date + district,
         cluster = ~district)

c3_guna <- crimes_panel %>% 
  filter(never_treated == 0) %>% 
  feols(number_gun_involved_arrest ~ treatment | date + district,
        cluster = ~district)

c4_guna <- crimes_panel %>% 
  filter(never_treated == 0) %>% 
  fepois(number_gun_involved_arrest ~ treatment | date + district,
         cluster = ~district)

c5_guna <- crimes_panel %>% 
  filter(year < 2020) %>% 
  feols(number_gun_involved_arrest ~ treatment | date + district,
        cluster = ~district)

c6_guna <- crimes_panel %>% 
  filter(year < 2020) %>% 
  fepois(number_gun_involved_arrest ~ treatment | date + district,
         cluster = ~district)

c7_guna <- crimes_panel %>% 
  filter(never_treated == 0) %>% 
  filter(year < 2020) %>% 
  feols(number_gun_involved_arrest ~ treatment | date + district,
        cluster = ~district)

c8_guna <- crimes_panel %>% 
  filter(never_treated == 0) %>% 
  filter(year < 2020) %>% 
  fepois(number_gun_involved_arrest ~ treatment | date + district,
         cluster = ~district)


# number other crime ------------------------------------------------------

c1_crime <- crimes_panel %>% 
  feols(number_other_crime ~ treatment | date + district,
        cluster = ~district)

c2_crime <- crimes_panel %>% 
  fepois(number_other_crime ~ treatment | date + district,
         cluster = ~district)

c3_crime <- crimes_panel %>% 
  filter(never_treated == 0) %>% 
  feols(number_other_crime ~ treatment | date + district,
        cluster = ~district)

c4_crime <- crimes_panel %>% 
  filter(never_treated == 0) %>% 
  fepois(number_other_crime ~ treatment | date + district,
         cluster = ~district)

c5_crime <- crimes_panel %>% 
  filter(year < 2020) %>% 
  feols(number_other_crime ~ treatment | date + district,
        cluster = ~district)

c6_crime <- crimes_panel %>% 
  filter(year < 2020) %>% 
  fepois(number_other_crime ~ treatment | date + district,
         cluster = ~district)

c7_crime <- crimes_panel %>% 
  filter(never_treated == 0) %>% 
  filter(year < 2020) %>% 
  feols(number_other_crime ~ treatment | date + district,
        cluster = ~district)

c8_crime <- crimes_panel %>% 
  filter(never_treated == 0) %>% 
  filter(year < 2020) %>% 
  fepois(number_other_crime ~ treatment | date + district,
         cluster = ~district)

# number other arrests ----------------------------------------------------

c1_crimea <- crimes_panel %>% 
  feols(number_other_arrest ~ treatment | date + district,
        cluster = ~district)

c2_crimea <- crimes_panel %>% 
  fepois(number_other_arrest ~ treatment | date + district,
         cluster = ~district)

c3_crimea <- crimes_panel %>% 
  filter(never_treated == 0) %>% 
  feols(number_other_arrest ~ treatment | date + district,
        cluster = ~district)

c4_crimea <- crimes_panel %>% 
  filter(never_treated == 0) %>% 
  fepois(number_other_arrest ~ treatment | date + district,
         cluster = ~district)

c5_crimea <- crimes_panel %>% 
  filter(year < 2020) %>% 
  feols(number_other_arrest ~ treatment | date + district,
        cluster = ~district)

c6_crimea <- crimes_panel %>% 
  filter(year < 2020) %>% 
  fepois(number_other_arrest ~ treatment | date + district,
         cluster = ~district)

c7_crimea <- crimes_panel %>% 
  filter(never_treated == 0) %>% 
  filter(year < 2020) %>% 
  feols(number_other_arrest ~ treatment | date + district,
        cluster = ~district)

c8_crimea <- crimes_panel %>% 
  filter(never_treated == 0) %>% 
  filter(year < 2020) %>% 
  fepois(number_other_arrest ~ treatment | date + district,
         cluster = ~district)


gof_mapping <- tribble(~raw, ~clean, ~fmt,
                       "nobs", "Observations", 0,
                       "FE: date", "FE: Day-by-Month-by-Year", 3,
                       "FE: district", "FE: District", 3)

footnotes <- map(list("* p < 0.1, ** p < 0.05, *** p < 0.01",
                      "Standard errors are clustered by district. 
                  Shotspotter is activated in 12 of the 22 police districts in Chicago.
                  Number Gun Crimes refers to all crimes that have a gun involved. For instance,
                  this can include a burglary or aggrevated assault with a firearm.
                  Number of Gun Arrests are the number of gun crimes that end in an arrest.
                  Number of Non-Gun Crimes are all crimes that have no firearm involved. Similarly,
                  not all of these end in arrests. Columns 3 and 4 omit year 2020 (Covid-19).
                  "), ~str_remove_all(., "\n"))

gun_table <- panelsummary(list(c1_gun, c2_gun, c3_gun, c4_gun,
                               c5_gun, c6_gun, c7_gun, c8_gun),
                          list(c1_guna, c2_guna, c3_guna, c4_guna,
                               c5_guna, c6_guna, c7_guna, c8_guna),
                          list(c1_crime, c2_crime, c3_crime, c4_crime,
                               c5_crime, c6_crime, c7_crime, c8_crime),
                          list(c1_crimea, c2_crimea, c3_crimea, c4_crimea,
                               c5_crimea, c6_crimea, c7_crimea, c8_crimea),
                          mean_dependent = T,
                          collapse_fe =T,
                          panel_labels = c("Panel A: Number Gun Crimes", 
                                           "Panel B: Number Gun Arrests",
                                           "Panel C: Number Non-Gun Crimes",
                                           "Panel D: Number Non-Gun Arrests"),
                          coef_map = c( "treatment" = "ShotSpotter Activated"),
                          gof_omit = "^R|A|B|S",
                          gof_map = gof_mapping,
                          italic  = T,
                          stars = "econ",
                          caption = "\\label{gun_table}Effect of ShotSpotter Activations on Various Outcomes (OLS/Poisson)") %>% 
  add_header_above(c(" " = 1, 
                     "OLS" =1, 
                     "Poisson" =1,
                     "OLS" = 1,
                     "Poisson" = 1,
                     "OLS" = 1,
                     "Poisson" = 1,
                     "OLS" = 1,
                     "Poisson" = 1)) %>% 
  add_header_above(c(" " = 1, "All" = 2, "Only Treated" =2, "All" =2, "Only Treated" = 2)) %>% 
  add_header_above(c(' ' = 1, "2016-2022" = 4, "2016-2019" = 4)) %>% 
  footnote(footnotes, threeparttable = T)


