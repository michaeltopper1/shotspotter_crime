## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-06-16
##

library(tidyverse)
library(fixest)
library(modelsummary)
library(panelsummary)
library(kableExtra)
library(did2s)

if (!exists("dispatch_panel")){
  dispatch_panel <- read_csv(here::here("analysis_data/xxdispatch_panel.csv"))
}

outcomes <- c("Overall Change",
              "Domestic Battery",
              "Domestic Disturbance",
              "Robbery",
              "EMS",
              "Battery",
              "Gun Crimes")


arrest_rates <- feols(c(arrest_rate_1,
                        domestic_battery_p1_arrestrate,
                        domestic_disturb_p1_arrestrate,
                        battery_ip_p1_arrestrate,
                        gun_crime_arrestrate_1) ~treatment  +
                        number_dispatches_1 + number_dispatches_2 +
                        number_dispatches_3 + number_dispatches_0 +
                        officer_hours|district + date,
                      data= dispatch_panel) 


dispatch_panel %>% 
  feols(prob_victim_injury_1 ~ treatment + ..ctrl)
dispatch_panel %>% 
  feols(prob_victim_injury_guncrime_1 ~ treatment + ..ctrl)
dispatch_panel %>% 
  feols(prob_victim_injury_no_guncrime_1 ~ treatment + ..ctrl)

arrest_means <- map_dfr(arrest_rates, ~as.numeric(fitstat(., type = "my"))) %>% 
  mutate(across(everything(), ~sprintf("%.3f",.))) %>% 
  mutate(term = "Mean of Dependent Variable")
  


gof_mapping <- tribble(~raw, ~clean, ~fmt,
                       "nobs", "Observations", 0,
                       "FE: date", "FE: Day-by-Month-by-Year", 3,
                       "FE: district", "FE: District", 3)

footnotes <- map(list("* p < 0.1, ** p < 0.05, *** p < 0.01",
                      "Standard errors are clustered by district. 
                  Arrest Rate is defined as the number of arrests made
                  divided by the number of dispatches. Columns 2-4 report the 
                  top 3 most frequent calls that end in arrests: Domestic Battery,
                  Domestic Disturbance, and Battery. Column 5 reports arrest rates
                  for Gun Crimes which is any call corresponding to a person with a gun,
                  shots fired, or a person shot. Observations are not consistent across each
                  call type since not every type of call occurs on every district-day. Controls
                  of officer hours and number of dispatches
                  are included in all specifications. As mentioned in Section BLANK,
                  not every arrest is included in the data, and therefore, these estimates
                  represent a lower bound. 
                  "), ~str_remove_all(., "\n"))

arrest_rates <- arrest_rates %>% 
  panelsummary_raw(stars = "econ",
               coef_map = c( "treatment" = "ShotSpotter Activated",
                             "shotspot_border_treatment" = "Border Activated"),
               gof_omit = "^R|A|B|S",
               gof_map = gof_mapping) %>% 
  add_row(arrest_means, .before = 3) %>% 
  mutate(across(-c(term), ~if_else(term == "Observations",
                                   . %>% prettyNum(digits = 2, big.mark = ",", format = "f"), .))) %>% 
  clean_raw(caption = "\\label{arrest_rates}Effect of ShotSpotter Enactment on Arrest Rates (OLS)") %>% 
  add_header_above(c(" " = 1,
                     "Arrest Rate" = 1,
                     "Domestic Battery" = 1,
                     "Domestic Disturbance" = 1,
                     "Battery" = 1,
                     "Gun Crimes")) %>% 
  add_header_above(c(" "= 2,
                     "Arrest Rate by Most Frequent Arrest Calls" = 3,
                      " " = 1)) %>% 
  footnote(footnotes, threeparttable = T) %>% 
  kable_styling(latex_options = "HOLD_position", font_size = 11)


