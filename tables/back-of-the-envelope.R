library(tidyverse)

if (!exists("dispatch_panel")){
  dispatch_panel <- read_csv(here::here("analysis_data/xxdispatches_clevel.csv"))
  dispatch_panel_p1 <- dispatch_panel %>% 
    filter(priority_code ==1)
}

## dispatch results
dispatch_panel_p1 %>% 
  mutate(officers = officer_hours/8) %>% 
  feols(entry_to_dispatch ~ officers + ..ctrl)



# marginal effect of officer on on-scene time ------------------------------


## these give the marginal effect of an additional police officer
onscene_estimates <- dispatch_panel_p1 %>% 
  mutate(officers = officer_hours/8) %>% 
  feols(entry_to_onscene ~ officers + ..ctrl) %>% 
  broom::tidy()
## 1 additional officer reduces on-scene times by 1.02 seconds
## given that the point estimates show 103.7 second increases this means
number_officers_needed <- 103.7/-onscene_estimates$estimate



# average officers within district ----------------------------------------

## getting average number of officer hours
officer_hours_avg <- dispatch_panel_p1 %>% 
  summarize(mean(officer_hours)) %>% pull()

## getting average number of officers
officer_avg <- officer_hours_avg/8

##finding percentage of force increase to get mitigate
number_officers_needed/officer_avg
