## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-04-01
##

library(tidyverse)
library(fixest)
library(modelsummary)

stops_panel <- read_csv("analysis_data/stops_panel.csv")


# number stops ------------------------------------------------------------

c1_stops <- stops_panel %>% 
  feols(number_stops~ treatment | district + date,
        cluster = ~district)

c2_stops <- stops_panel %>% 
  fepois(number_stops~ treatment | district + date,
        cluster = ~district)

c3_stops <- stops_panel %>% 
  filter(year != 2020) %>% 
  feols(number_stops~ treatment | district + date,
        cluster = ~district)

c4_stops <- stops_panel %>% 
  filter(year != 2020) %>% 
  fepois(number_stops~ treatment | district + date,
         cluster = ~district)


# number black stops ------------------------------------------------------

c1_bstops <- stops_panel %>% 
  feols(number_black_stops~ treatment | district + date,
        cluster = ~district)

c2_bstops <- stops_panel %>% 
  fepois(number_black_stops~ treatment | district + date,
         cluster = ~district)

c3_bstops <- stops_panel %>% 
  filter(year != 2020) %>% 
  feols(number_black_stops~ treatment | district + date,
        cluster = ~district)

c4_bstops <- stops_panel %>% 
  filter(year != 2020) %>% 
  fepois(number_black_stops~ treatment | district + date,
         cluster = ~district)


gof_mapping <- tribble(~raw, ~clean, ~fmt,
                       "nobs", "Observations", 0,
                       "FE: date", "FE: Day-by-Month-by-Year", 3,
                       "FE: district", "FE: District", 3)


footnotes <- map(list("* p < 0.1, ** p < 0.05, *** p < 0.01",
                  "Standard errors are clustered by district. 
                  Shotspotter is activated in 12 of the 22 police districts in Chicago.
                  Stops refer to footstops that have an investigatory stop report (ISR). 
                  Note that these stops do not include juveniles. Columns 3 and 4 omit year 2020 (Covid-19).
                  "), ~str_remove_all(., "\n"))

panelsummary(list(c1_stops, c2_stops, c3_stops, c4_stops),
             list(c1_bstops, c2_bstops, c3_bstops, c4_bstops),
             stars = "econ",
             mean_dependent = T,
             panel_labels = c("Panel A: All Stops",
                              "Panel B: Black Stops"),
             gof_map = gof_mapping,
             coef_map = c("treatment" = "ShotSpotter Activated"),
             collapse_fe = T) %>% 
  add_header_above(c(" " = 1, 
                     "OLS" =1, 
                     "Poisson" =1,
                     "OLS" = 1,
                     "Poisson" = 1)) %>% 
  add_header_above(c(" " = 3, "Omitting 2020" = 2)) %>% 
  footnote(footnotes, threeparttable = T)

