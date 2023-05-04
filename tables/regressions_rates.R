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

stops_panel <- stops_panel %>% 
  filter(never_treated == 0)
# panel A: searches -------------------------------------------------------

c1_search_rate <- stops_panel %>% 
  feols(search_rate ~treatment | district + date,
        cluster = ~district)


c2_search_rate <- stops_panel %>% 
  filter(year < 2020) %>% 
  feols(search_rate ~treatment | district + date,
        cluster = ~district)




# panel b -----------------------------------------------------------------


c1_pat_rate <- stops_panel %>% 
  feols(patdown_rate ~treatment | district + date,
        cluster = ~district)

c2_pat_rate <- stops_panel %>% 
  filter(year < 2020) %>% 
  feols(patdown_rate ~treatment | district + date,
        cluster = ~district)



# panel c: searches -------------------------------------------------------

c1_firearm_rate <- stops_panel %>% 
  feols(firearm_found_rate ~treatment | district + date,
        cluster = ~district)


c2_firearm_rate <- stops_panel %>% 
  filter(year < 2020) %>% 
  feols(firearm_found_rate ~treatment | district + date,
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

rates <- panelsummary(list(c1_search_rate, c2_search_rate),
                                 list(c1_pat_rate, c2_pat_rate),
                                 list(c1_firearm_rate, c2_firearm_rate),mean_dependent = T,
                                 collapse_fe =T,
                                 panel_labels = c("Panel A: Search Rate", 
                                                  "Panel B: Pat-Down Rate",
                                                  "Panel C: Firearm Found Rate"),
                                 coef_map = c( "treatment" = "ShotSpotter Activated"),
                                 gof_omit = "^R|A|B|S",
                                 gof_map = gof_mapping,
                                 italic  = T,
                                 stars = "econ",
                                 caption = "\\label{rates}Effect of ShotSpotter Activations with Bordering Districts (OLS/Poisson)") %>% 
  add_header_above(c(" " = 2, 
                     "Omitting 2020-onwards" = 1)) %>% 
  footnote(footnotes, threeparttable = T)
