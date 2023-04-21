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


TwoWayFEWeights::twowayfeweights(crimes,
                                 "number_gun_involved_arrest",
                                 "district",
                                 "date",
                                 "treatment",
                                  cmd_type = "feTR")

crimes <- read_csv("analysis_data/crimes_panel.csv")
crimes %>% 
  count(number_other_crime, sort = T
        )
crimes <- crimes %>% 
  mutate(year = year(date),
         month = month(date),
         day_of_week = wday(date, label = T),
         week = week(date), .before = 1)
crimes %>% colnames()
fe <- list(c("district", 
             "day_of_week",
             "month", 
             "year",
             "week"),
           c("district", 
             "day_of_week",
             "month^year",
             "week"),
           c("district", 
             "day_of_week",
             "week^month^year"))
outcomes

panel_1 <- map(fe, ~ifc::reghdfe_pois(data = crimes,
                                        outcome = "number_gun_involved", 
                                        fixed_effects = .,
                                         explanatory_vars = c("treatment"),
                                        cluster = "district"))

panel_2 <- map(fe, ~ifc::reghdfe_pois(data = crimes,
                                        outcome = "number_gun_involved_arrest", 
                                        fixed_effects = .,
                                        explanatory_vars = c("treatment"),
                                        cluster = "district"))

panel_3 <- map(fe, ~ifc::reghdfe_pois(data = crimes,
                                        outcome = "number_other_crime", 
                                        fixed_effects = .,
                                        explanatory_vars = c("treatment"),
                                        cluster = "district"))

panel_4 <- map(fe, ~ifc::reghdfe_pois(data = crimes,
                                        outcome = "number_other_arrest", 
                                        fixed_effects = .,
                                        explanatory_vars = c("treatment"),
                                        cluster = "district"))

gof_mapping <- tribble(~raw, ~clean, ~fmt,
                       "nobs", "Observations", 0,
                       "FE: date", "FE: Day-by-Month-by-Year", 3,
                       "FE: district", "FE: District", 3,
                       "FE: year", "FE: Year", 3,
                       "FE: week", "FE: Week", 3,
                       "FE: month", "FE: Month", 3,
                       "FE: month^year", "FE: Month-by-Year", 3,
                       "FE: week^month^year", "FE: Week-by-Month-by-Year", 3)

footnotes <- map(list("* p < 0.1, ** p < 0.05, *** p < 0.01",
                      "Standard errors are clustered by district. 
                  Shotspotter is activated in 12 of the 22 police districts in Chicago.
                  Number Gun Crimes refers to all crimes that have a gun involved. For instance,
                  this can include a burglary or aggrevated assault with a firearm.
                  Number of Gun Arrests are the number of gun crimes that end in an arrest.
                  Number of Non-Gun Crimes are all crimes that have no firearm involved. Similarly,
                  not all of these end in arrests. Columns 3 and 4 omit year 2020 (Covid-19).
                  "), ~str_remove_all(., "\n"))

panelsummary(panel_1, panel_2, panel_3,panel_4,
             stars = "econ",
             mean_dependent = T,
             collapse_fe = T,
             gof_map = gof_mapping,
             coef_map = c( "treatment" = "ShotSpotter Activated"),
             gof_omit = "R|B|A",
             italic = T,
             caption = "\\label{gun_table}Effect of ShotSpotter Activations on Various Outcomes (OLS/Poisson)") %>% 
  footnote(footnotes, threeparttable = T) %>% 
  kable_paper()
