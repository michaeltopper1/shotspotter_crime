library(tidyverse)
library(fixest)
library(panelsummary)
library(kableExtra)
library(modelsummary)
library(did2s)

agg_outcomes <- read_csv('analysis_data/xxaggregate_outcomes.csv')

agg_outcomes <- agg_outcomes %>% 
  mutate(year = year(date),
          month = month(date),
          day = day(date),
          year_month = mdy(paste0(month, "-1-", year)))

## currently using SDSC control. Can change if needed. 
setFixest_fml(..ctrl = ~0| district + date )

gun_sst <- agg_outcomes %>% 
  fepois(number_gun_and_sst_calls ~ treatment + ..ctrl ,
         cluster = ~district, data = .) 

gun_arrest <- agg_outcomes %>% 
  fepois(number_gun_arrests ~ treatment + ..ctrl ,
         cluster = ~district, data = .) 

gun_victim <- agg_outcomes %>% 
  fepois(number_gun_involved_victims ~ treatment + ..ctrl ,
         cluster = ~district, data = .) 

gun_crime_cleared <- agg_outcomes %>% 
  fepois(number_gun_crimes_cleared ~ treatment + ..ctrl ,
         cluster = ~district, data = .) 

aggregate_outcomes <- list(gun_sst, gun_arrest, gun_victim, gun_crime_cleared)

gof_mapping <- tribble(~raw, ~clean, ~fmt,
"nobs", "Observations", 0,
"FE: date", "FE: Day-by-Month-by-Year", 3,
"FE: district", "FE: District", 3,
"FE: final_dispatch_code", "FE: Call-Type", 3,
"FE: hour", "FE: Hour-of-Day", 3)

panelsummary(aggregate_outcomes, mean_dependent = T, stars = "econ",
             pretty_num = T,
             gof_map = gof_mapping,
             coef_map = c("treatment" = "ShotSpotter Activated"),
             caption = "\\label{aggregate_outcomes}Effect of ShotSpotter Technology on Aggregate Gun Outcomes in Chicago (Poisson)") %>% 
  add_header_above(c(" " = 1,
                     "Gun and SST Dispatches" = 1,
                     "Gun Arrests" = 1,
                     "Gun-Involved Victims" = 1,
                     "Gun Crimes Cleared" = 1)) %>% 
  add_header_above(c(" ",
                     "Aggregate Outcomes in Chicago (Counts)" = 4))
  
