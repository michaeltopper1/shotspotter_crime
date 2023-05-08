## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-05-02
##

library(tidyverse)
library(modelsummary)
library(fixest)
library(panelsummary)
library(kableExtra)


stops_panel <- read_csv("analysis_data/stops_panel.csv")

borders <- read_csv("created_data/border_districts_final.csv")

shotspotter_borders <- read_csv("created_data/border_districts_final.csv")

stops_panel <- stops_panel %>% 
  left_join(shotspotter_borders,
            join_by(district == border_district)) %>% 
  relocate(border_treatment) %>% 
  group_by(district) %>% 
  mutate(shotspot_border_treatment = ifelse(date >= border_treatment,1 ,0 )) %>% 
  ungroup() %>% 
  mutate(shotspot_border_treatment = ifelse(is.na(shotspot_border_treatment), 0, shotspot_border_treatment))



# panel A: searches -------------------------------------------------------

c1_stops <- stops_panel %>% 
  feols(number_stops ~treatment | district + date,
        cluster = ~district)

c2_stops <- stops_panel %>% 
  fepois(number_stops ~treatment | district + date,
         cluster = ~district)

c3_stops <- stops_panel %>% 
  filter(never_treated == 0) %>% 
  feols(number_stops ~treatment | district + date,
        cluster = ~district)

c4_stops <-  stops_panel %>% 
  filter(never_treated == 0) %>% 
  fepois(number_stops ~treatment | district + date,
         cluster = ~district)

c5_stops <- stops_panel %>% 
  filter(year < 2020) %>% 
  feols(number_stops ~treatment | district + date,
        cluster = ~district)

c6_stops <- stops_panel %>% 
  filter(year < 2020) %>% 
  fepois(number_stops ~treatment | district + date,
         cluster = ~district)

c7_stops <- stops_panel %>% 
  filter(year < 2020) %>% 
  filter(never_treated == 0) %>% 
  feols(number_stops ~treatment | district + date,
        cluster = ~district)

c8_stops <-  stops_panel %>% 
  filter(year < 2020) %>% 
  filter(never_treated == 0) %>% 
  fepois(number_stops ~treatment | district + date,
         cluster = ~district)


# panel b -----------------------------------------------------------------

c1_pat <- stops_panel %>% 
  feols(number_pat_down ~treatment | district + date,
        cluster = ~district)

c2_pat <- stops_panel %>% 
  fepois(number_pat_down ~treatment | district + date,
         cluster = ~district)

c3_pat <- stops_panel %>% 
  filter(never_treated == 0) %>% 
  feols(number_pat_down ~treatment | district + date,
        cluster = ~district)

c4_pat <-  stops_panel %>% 
  filter(never_treated == 0) %>% 
  fepois(number_pat_down ~treatment | district + date,
         cluster = ~district)

c5_pat <- stops_panel %>% 
  filter(year < 2020) %>% 
  feols(number_pat_down ~treatment | district + date,
        cluster = ~district)

c6_pat <- stops_panel %>% 
  filter(year < 2020) %>% 
  fepois(number_pat_down ~treatment | district + date,
         cluster = ~district)

c7_pat <- stops_panel %>% 
  filter(year < 2020) %>% 
  filter(never_treated == 0) %>% 
  feols(number_pat_down ~treatment | district + date,
        cluster = ~district)

c8_pat <-  stops_panel %>% 
  filter(year < 2020) %>% 
  filter(never_treated == 0) %>% 
  fepois(number_pat_down ~treatment | district + date,
         cluster = ~district)

# panel c: searches -------------------------------------------------------


c1_search <- stops_panel %>% 
  feols(number_search ~treatment | district + date,
        cluster = ~district)

c2_search <- stops_panel %>% 
  fepois(number_search ~treatment | district + date,
         cluster = ~district)

c3_search <- stops_panel %>% 
  filter(never_treated == 0) %>% 
  feols(number_search ~treatment | district + date,
        cluster = ~district)

c4_search <-  stops_panel %>% 
  filter(never_treated == 0) %>% 
  fepois(number_search ~treatment | district + date,
         cluster = ~district)

c5_search <- stops_panel %>% 
  filter(year < 2020) %>% 
  feols(number_search ~treatment | district + date,
        cluster = ~district)

c6_search <- stops_panel %>% 
  filter(year < 2020) %>% 
  fepois(number_search ~treatment | district + date,
         cluster = ~district)

c7_search <- stops_panel %>% 
  filter(year < 2020) %>% 
  filter(never_treated == 0) %>% 
  feols(number_search ~treatment | district + date,
        cluster = ~district)

c8_search <-  stops_panel %>% 
  filter(year < 2020) %>% 
  filter(never_treated == 0) %>% 
  fepois(number_search ~treatment | district + date,
         cluster = ~district)

# panel d: firearm -------------------------------------------------------


c1_firearm <- stops_panel %>% 
  feols(number_firearm_found ~treatment | district + date,
        cluster = ~district)

c2_firearm <- stops_panel %>% 
  fepois(number_firearm_found ~treatment | district + date,
         cluster = ~district)

c3_firearm <- stops_panel %>% 
  filter(never_treated == 0) %>% 
  feols(number_firearm_found ~treatment | district + date,
        cluster = ~district)

c4_firearm <-  stops_panel %>% 
  filter(never_treated == 0) %>% 
  fepois(number_firearm_found ~treatment | district + date,
         cluster = ~district)

c5_firearm <- stops_panel %>% 
  filter(year < 2020) %>% 
  feols(number_firearm_found ~treatment | district + date,
        cluster = ~district)

c6_firearm <- stops_panel %>% 
  filter(year < 2020) %>% 
  fepois(number_firearm_found ~treatment | district + date,
         cluster = ~district)

c7_firearm <- stops_panel %>% 
  filter(year < 2020) %>% 
  filter(never_treated == 0) %>% 
  feols(number_firearm_found ~treatment | district + date,
        cluster = ~district)

c8_firearm <-  stops_panel %>% 
  filter(year < 2020) %>% 
  filter(never_treated == 0) %>% 
  fepois(number_firearm_found ~treatment | district + date,
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
                  Number Stops refers to ISR stops in Chicago. Number of Pat-Downs
                  are the number of pat downs that occurred. Similarly, Number Searches are the number
                  of searches that occurred. Omitting 2020-onwards is our preferred specification due to
                  drastic level changes.
                  "), ~str_remove_all(., "\n"))

stops_pat_search <- panelsummary(list(c1_stops, c2_stops, c3_stops, c4_stops,
                                      c5_stops, c6_stops, c7_stops, c8_stops),
                                 list(c1_pat, c2_pat, c3_pat, c4_pat,
                                      c5_pat, c6_pat, c7_pat, c8_pat),
                                 list(c1_search, c2_search, c3_search, c4_search,
                                      c5_search, c6_search, c7_search, c8_search),
                                 list(c1_firearm, c2_firearm, c3_firearm, c4_firearm,
                                      c5_firearm, c6_firearm, c7_firearm, c8_firearm),
                                 mean_dependent = T,
                                 collapse_fe =T,
                                 panel_labels = c("Panel A: Number Stops", 
                                                  "Panel B: Number Pat-Downs",
                                                  "Panel C: Number Searches",
                                                  "Panel D: Number Firearm Found"),
                                 coef_map = c( "treatment" = "ShotSpotter Activated"),
                                 gof_omit = "^R|A|B|S",
                                 gof_map = gof_mapping,
                                 italic  = T,
                                 stars = "econ",
                                 caption = "\\label{stops_pat_search}Effect of ShotSpotter Activations with Bordering Districts (OLS/Poisson)") %>% 
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
