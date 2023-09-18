library(tidyverse)

if (!exists("dispatch_panel")){
  dispatch_panel <- read_csv(here::here("analysis_data/xxdispatches_clevel.csv"))
  dispatch_panel_p1 <- dispatch_panel %>% 
    filter(priority_code ==1)
}


## I am going to be doing a back-of-the-envelope calcuation
## First I am going to take the average amount of officers on duty
## when officer hours are above the median
avg_hour_above_med <- dispatch_panel_p1 %>% 
  filter(officer_hours_median < officer_hours) %>% 
  summarize(avg_hours = mean(officer_hours)) %>% pull()

### Now I am going to take the average number of officer hours that
## are in the main estimates

avg_hour <- dispatch_panel_p1 %>% 
  # filter(officer_hours_median > officer_hours) %>% 
  summarize(avg_hours = mean(officer_hours)) %>% pull()


## substracting the two and dividing by 8 to give number of additional
## officers needed
additional_officers <- (avg_hour_above_med - avg_hour)/8
