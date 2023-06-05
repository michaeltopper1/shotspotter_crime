## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-05-24
##

library(tidyverse)
library(modelsummary)
library(kableExtra)

dispatch_panel <- read_csv("analysis_data/xxdispatch_panel.csv")


summary_stats <- dispatch_panel %>% 
  mutate(across(c(entry_to_dispatch_1,
                  dispatch_to_onscene_1,
                  entry_to_dispatch_2,
                  entry_to_onscene_2,
                  entry_to_dispatch_3,
                  entry_to_onscene_3 ), ~./60, .names = "{.col}_mins")) %>% 
  datasummary((`Call to Dispatch` = entry_to_dispatch_1) +
                entry_to_dispatch_1_mins +
              (`Dispatch to On-Scene` = dispatch_to_onscene_1) +
              dispatch_to_onscene_1_mins +
              (`Number Dispatches` = number_dispatches_1) +
              (`Call to Dispatch` = entry_to_dispatch_2) +
                entry_to_dispatch_2_mins +
              (`Entry to On-Scene` = entry_to_onscene_2) +
              entry_to_onscene_2_mins +
              (`Number Dispatches` = number_dispatches_2) +
              (`Call to Dispatch` = entry_to_dispatch_3) +
                entry_to_dispatch_3_mins +
              (`Entry to On-Scene` = entry_to_onscene_3) +
                entry_to_onscene_3_mins +
              (`Number Dispatches` = number_dispatches_3) + 
              (`Number SST Alerts` = number_sst_alerts) +
                (`Officer Hours` = officer_hours) +
                (`Number Arrests` = arrests_made)~ Mean + SD + Median + Min  +Max,
              data = .,
              output = "data.frame") %>% 
  janitor::clean_names()



footnote <- map(list( "Units are in seconds unless otherwise noted. Data is at
         the district-by-day level. Call to Dispatch represents 
         the amount of time from the 911 call to the dispatcher finding and dispatching
         a police officer to the scene. Dispatch to On-Scene is the time from dispatch to 
         the scene of the reported crime. Priority 1 refers to an immediate dispatch, 
         Priority 2 a rapid dispatch, and Priority 3 a routine dispatch. Officer Hours are the 
         number of working hours sworn police officers work. Number of SST Alerts is the 
         number of ShotSpotter alerts. Note that
         New Years Eve/New Years Day/Fourth of July are excluded from the sample as
         ShotSpotter Alerts can be as high as 392 on these days. 
                  "), ~str_remove_all(., "\n"))


summary_stats <- summary_stats %>% 
  mutate(across(c(-1), ~. %>% as.double() %>% scales::comma(accuracy  = 0.01))) %>% 
  mutate(x = if_else(str_detect(x, "mins$"),
                     "", x)) %>% 
  mutate(across(c(-1), ~if_else(x == "", paste0("(", ., " mins)"), .))) %>% 
  kbl(col.names = c(" ", "Mean", "Std. Dev.", "Median", "Min", "Max"),
      booktabs = T,
      caption = "\\label{summary_stats}Summary Statistics of Response Times (seconds)") %>% 
  kable_styling(latex_options = "HOLD_position") %>% 
  pack_rows("Priority 1 Dispatches:", 1, 5) %>% 
  pack_rows("Priority 2 Dispatches:", 6, 10) %>% 
  pack_rows("Priority 3 Dispatches:", 11, 15) %>% 
  pack_rows("General:", 16, 18) %>% 
  footnote(footnote, threeparttable = T)



         