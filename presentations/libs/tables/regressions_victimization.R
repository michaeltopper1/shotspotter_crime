## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-05-14
##

library(tidyverse)
library(fixest)
library(panelsummary)
library(kableExtra)
library(did2s)

victim_panel <- read_csv(here::here("analysis_data/xxvictim_panel.csv"))

if (!exists("dispatch_panel")){
  dispatch_panel <- read_csv(here::here("analysis_data/xxdispatch_panel.csv"))
}

victim_panel <- victim_panel %>% 
  left_join(dispatch_panel, join_by(date == date, district == district),
            suffix = c("", ".y")) %>% 
  select(-ends_with(".y")) 

victim_panel <- victim_panel %>% 
  mutate(across(starts_with("number"), ~if_else(is.na(.), 0, .)))



# panel A -----------------------------------------------------------------

ols_v1 <- victim_panel %>% 
  feols(number_gun_injury_victims ~treatment + officer_hours +
           number_dispatches| district + date)

ols_v2 <- did2s(victim_panel,
                yname = "number_gun_injury_victims",
                first_stage = ~ officer_hours +
                  number_dispatches| district + date,
                second_stage = ~treatment,
                treatment = "treatment",
                cluster_var = "district")

poisson_v1 <- victim_panel %>% 
  fepois(number_gun_injury_victims ~treatment + 
           officer_hours + number_dispatches
         | district + date)




gof_mapping <- tribble(~raw, ~clean, ~fmt,
                       "nobs", "Observations", 0,
                       "FE: date", "FE: Day-by-Month-by-Year", 3,
                       "FE: district", "FE: District", 3)
footnotes <- map(list("* p < 0.1, ** p < 0.05, *** p < 0.01")
                  , ~str_remove_all(., "\n")) 



victimization <- panelsummary_raw(list(ols_v1, ols_v2, poisson_v1),
                 stars = "econ",
                 coef_map = c( "treatment" = "ShotSpotter Activated",
                               "shotspot_border_treatment" = "Border Activated"),
                 gof_omit = "^R|A|B|S",
                 gof_map = gof_mapping,
                 mean_dependent = T) %>% 
  mutate(`Model 2` = if_else(term == "FE: District" |
                               term == "FE: Day-by-Month-by-Year", "X", `Model 2`)) %>% 
  add_row(term = "Gardner (2021) Robust Estimator",
          `Model 1` = "",
          `Model 2` = "X",
          `Model 3` = "") %>% 
  mutate(`Model 2` = if_else(term == "Mean of Dependent Variable", 
                             `Model 1`, `Model 2`)) %>% 
  mutate(across(starts_with("M"), ~if_else(term == "Observations",
                                           . %>% as.double() %>%
                                             as.integer() %>% scales::comma(), .))) %>% 
  clean_raw() %>% 
  add_header_above(c(" " = 1,
                     "OLS" = 2,
                     "Poisson" = 1)) %>% 
  footnote(footnotes, threeparttable = T) %>% 
  kable_classic(full_width = F, html_font = "Cambria")

