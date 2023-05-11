## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-05-08
##

library(tidyverse)
library(fixest)
library(panelsummary)
library(kableExtra)

officer_hours <- read_csv("analysis_data/officer_hours.csv")
dispatch_panel <- read_csv("analysis_data/dispatches.csv")

dispatch_panel <- dispatch_panel %>% 
  rename(treatment = treatment.x) %>% 
  rename(never_treated = never_treated.x) %>% 
  mutate(year = year(date))

dispatch_panel <- dispatch_panel %>% 
  mutate(across(starts_with("mean_time"), ~ifelse(is.na(.), 0, .)))

dispatch_panel <- dispatch_panel %>% 
  left_join(officer_hours, join_by(date == date, district == district))

dispatch_panel <- dispatch_panel %>% 
  rename(year = year.x)
# panel A -----------------------------------------------------------------


c1_0 <- dispatch_panel %>% 
  feols(mean_time_0 ~ treatment | district  + date,
        cluster = ~district)

c2_0 <- dispatch_panel %>% 
  filter(never_treated == 0) %>% 
  feols(mean_time_0 ~ treatment | district  + date,
        cluster = ~district)

c3_0 <- dispatch_panel %>% 
  filter(year < 2020) %>% 
  feols(mean_time_0 ~ treatment + officer_hours| district  + date,
        cluster = ~district)


# panel B -----------------------------------------------------------------


c1_1 <- dispatch_panel %>% 
  feols(mean_time_1 ~ treatment | district  + date,
        cluster = ~district)

c2_1 <- dispatch_panel %>% 
  filter(never_treated == 0) %>% 
  feols(mean_time_1 ~ treatment | district  + date,
        cluster = ~district)

c3_1 <- dispatch_panel %>% 
  filter(year < 2020) %>% 
  feols(mean_time_1 ~ treatment  +officer_hours| district  + date,
        cluster = ~district)


# panel C -----------------------------------------------------------------


c1_2 <- dispatch_panel %>% 
  feols(mean_time_2 ~ treatment | district  + date,
        cluster = ~district)

c2_2 <- dispatch_panel %>% 
  filter(never_treated == 0) %>% 
  feols(mean_time_2 ~ treatment | district  + date,
        cluster = ~district)

c3_2 <- dispatch_panel %>% 
  filter(year < 2020) %>% 
  feols(mean_time_2 ~ treatment  | district  + date,
        cluster = ~district)


# panel D -----------------------------------------------------------------


c1_3 <- dispatch_panel %>% 
  feols(mean_time_3 ~ treatment | district  + date,
        cluster = ~district)

c2_3 <- dispatch_panel %>% 
  filter(never_treated == 0) %>% 
  feols(mean_time_3 ~ treatment | district  + date,
        cluster = ~district)

c3_3 <- dispatch_panel %>% 
  filter(year < 2020) %>% 
  feols(mean_time_3 ~ treatment  + officer_hours| district  + date,
        cluster = ~district)

gof_mapping <- tribble(~raw, ~clean, ~fmt,
                       "nobs", "Observations", 0,
                       "FE: date", "FE: Day-by-Month-by-Year", 3,
                       "FE: district", "FE: District", 3)

footnotes <- map(list("* p < 0.1, ** p < 0.05, *** p < 0.01",
                      "Standard errors are clustered by district. 
                  Shotspotter is activated in 12 of the 22 police districts in Chicago.
                  Priority calls range from 0-5. Only priorities 0-3 are
                  shown here. Priority 0 is highest priority (office calling for help),
                  Priority 1 is second highest etc.
                  "), ~str_remove_all(., "\n"))

panelsummary(list(c1_0, c2_0, c3_0),
             list(c1_1, c2_1, c3_1),
             list(c1_2, c2_2, c3_2),
             list(c1_3, c2_3, c3_3),
             stars = "econ",
             panel_labels = c("Panel A: Priority 0",
                              "Panel B: Priority 1",
                              "Panel C: Priority 2",
                              "Panel D: Priority 3"),
             mean_dependent = T,
             coef_map = c( "treatment" = "ShotSpotter Activated"),
             gof_omit = "^R|A|B|S",
             gof_map = gof_mapping,
             italic  = T) %>% 
  add_header_above(c(" " = 1, " ", "Treated Only Districts" = 1, 
                     "Excluding 2020-onwards" = 1)) %>% 
  footnote(footnotes, threeparttable = T)
